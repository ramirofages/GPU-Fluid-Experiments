package;

import haxe.Timer;
import snow.modules.opengl.GL;
import shaderblox.uniforms.UTexture;
import snow.Snow;

import snow.types.Types;
import js.Browser;
import gltoolbox.render.RenderTarget;
import shaderblox.ShaderBase;
import shaderblox.uniforms.UVec2.Vector2;
import shaderblox.uniforms.UVec3.Vector3;
import shaderblox.uniforms.UVec4.Vector4;
import hxColorToolkit.spaces.HSB;

using hxColorToolkit.ColorToolkit;


typedef UserConfig = {}

typedef TShader = {
	function activate(_:Bool, _:Bool):Void;
	function deactivate():Void;
}

@:expose class Main extends snow.App{
  var gl = GL;
	//Simulations
	var fluid:GPUFluid;
	var particles:GPUParticles;
	//Geometry
	var textureQuad:GLBuffer = null;
	//Framebuffers
	var screenBuffer:GLFramebuffer = null;	//null for all platforms excluding ios, where it references the defaultFramebuffer (UIStageView.mm)
	//Render Targets
	var offScreenTarget:RenderTarget;
	//Shaders
  var blitTextureShader:BlitTexture;
  var renderFluidShader:FluidRender;
	var renderParticlesShader : ColorParticleMotion;
	var updateDyeShader       : MouseDye;
	var mouseForceShader      : MouseForce;
	//Window
  static inline var MOUSE_ALWAYS_DOWN:Bool = true;

	var isMouseDown:Bool = MOUSE_ALWAYS_DOWN;
	var mousePointKnown:Bool = false;
	var lastMousePointKnown:Bool = false;
	var mouse = new Vector2();
	var mouseFluid = new Vector2();
	var lastMouse = new Vector2();
	var lastMouseFluid = new Vector2();
  var frameRegionLBRT = new Vector4();

	var time:Float;
	var initTime:Float;
	var lastTime:Float;
	//Drawing parameters
	var renderParticlesEnabled:Bool = true;
	var renderFluidEnabled:Bool = true;
  var simulation_enabled:Bool = true;
  var hueCycleEnabled:Bool = MOUSE_ALWAYS_DOWN;
  var dyeColorHSB = new HSB(180, 100, 100);
	var dyeColor = new Vector3();
  var pointSize = 1;

	//
	var performanceMonitor:PerformanceMonitor;
	//Parameters
	var particleCount:Int;
	var fluidScale:Float;
	var fluidIterations(default, set):Int;
	var offScreenScale:Float;
	var offScreenFilter:Int;
	var simulationQuality(default, set):SimulationQuality;

  static var instance : Main;

	public function new () {
    instance = this;
		performanceMonitor = new PerformanceMonitor(35, null, 2000);

		simulationQuality = Medium;
    #if js
			var is_mobile : Bool = (~/(iPad|iPhone|iPod|Android|Windows Phone)/g).match(Browser.navigator.userAgent);
			if(is_mobile)
      {
        simulationQuality = iOS;
			}


		performanceMonitor.fpsTooLowCallback = lowerQualityRequired; //auto adjust quality

		//Extract quality parameter, ?q= and set simulation quality
		var urlParams = js.Web.getParams();
		if(urlParams.exists('q')){
			var q = StringTools.trim(urlParams.get('q').toLowerCase());
			//match enum
			for(e in Type.allEnums(SimulationQuality)){
				var name = Type.enumConstructor(e).toLowerCase();
				if(q == name){
					simulationQuality = e;
					performanceMonitor.fpsTooLowCallback = null; //disable auto quality adjusting
					break;
				}
			}
		}
		//Extract iterations
		if(urlParams.exists('iterations')){
			var iterationsParam = Std.parseInt(urlParams.get('iterations'));
			if(Std.is(iterationsParam, Int))
				fluidIterations = iterationsParam;
		}
		#end
	}

	override function config( config:AppConfig ) : AppConfig {

		#if js
		config.runtime.prevent_default_context_menu = false;
		#end
		config.window.borderless = true;
		config.window.fullscreen = true;
		config.window.title = "GPU Fluid";
		//for some reason, window width and height are set initially from config in browsers and
		//ignores true size
		#if js
		config.window.width = js.Browser.window.innerWidth;
		config.window.height = js.Browser.window.innerHeight;
		#end

		config.render.antialiasing = 0;


	    return config;
	}

	override function ready(){
		init();
	}

	function init():Void {
		//GPUCapabilities.report();


		GL.disable(GL.DEPTH_TEST);
		GL.disable(GL.CULL_FACE);
		GL.disable(GL.DITHER);

        #if ios
				screenBuffer = GL.getParameter(GL.FRAMEBUFFER_BINDING);
				#end

		textureQuad = gltoolbox.GeometryTools.createQuad(0, 0, 1, 1);

		offScreenTarget = new RenderTarget(
			Math.round(app.runtime.window_width()*offScreenScale),
			Math.round(app.runtime.window_height()*offScreenScale),
			gltoolbox.TextureTools.createTextureFactory({
				channelType: GL.RGB,
				dataType: GL.UNSIGNED_BYTE,
				filter: offScreenFilter
			})
		);

    //create shaders
    blitTextureShader = new BlitTexture();
		renderParticlesShader = new ColorParticleMotion();
    renderFluidShader = new FluidRender();

		updateDyeShader = new MouseDye();
		mouseForceShader = new MouseForce();

    //set uniform objects
		updateDyeShader.mouse.data = mouseFluid;
		updateDyeShader.lastMouse.data = lastMouseFluid;
    updateDyeShader.dyeColor.data = dyeColor;

		mouseForceShader.mouse.data = mouseFluid;
		mouseForceShader.lastMouse.data = lastMouseFluid;

    renderFluidShader.regionLBRT.data = frameRegionLBRT;
    blitTextureShader.regionLBRT.data = frameRegionLBRT;
    updatePointSize();

		var cellScale = 32;
		fluid = new GPUFluid(Math.round(app.runtime.window_width()*fluidScale), Math.round(app.runtime.window_height()*fluidScale), cellScale, fluidIterations);
		fluid.updateDyeShader = updateDyeShader;
		fluid.applyForcesShader = mouseForceShader;

		particles = new GPUParticles(particleCount);
		//scale from fluid's velocity field to clipSpace, which the particle velocity uses
		particles.flowScaleX = 1/(fluid.cellSize * fluid.aspectRatio);
		particles.flowScaleY = 1/fluid.cellSize;
		particles.flowIsFloat = fluid.floatVelocity;
		particles.dragCoefficient = 1;
		renderParticlesShader.FLOAT_DATA = particles.floatData ? "true" : "false";

    dyeColor.set(1, 0, 0);

		// #if ios
		// renderParticlesShader.POINT_SIZE = "4.0";
		// #end
    updateFrameRegion();

		initTime = haxe.Timer.stamp();
		lastTime = initTime;
	}

	override function update( dt:Float ){
    if(!simulation_enabled) return;

		time = haxe.Timer.stamp() - initTime;
    performanceMonitor.recordFrameTime(dt);

		dt = 0.016;//@!
		//Physics
		//interaction
		updateDyeShader.isMouseDown.set(isMouseDown && lastMousePointKnown);
		mouseForceShader.isMouseDown.set(isMouseDown && lastMousePointKnown);

		//step physics
		fluid.step(dt);

		particles.flowVelocityField = fluid.velocityRenderTarget.readFromTexture;

    if(renderParticlesEnabled)
			particles.step(dt);

    if(hueCycleEnabled)
      dyeColorHSB.hue += 1.2;

    if(isMouseDown){
    			//cycle further by mouse velocity
          var window_height = app.runtime.window_height();
          var window_width = app.runtime.window_height();

    			if(hueCycleEnabled){
    				var vx = (mouse.x - lastMouse.x)/(dt*window_width);
    				var vy = (mouse.y - lastMouse.y)/(dt*window_height);
    				dyeColorHSB.hue += Math.sqrt(vx*vx + vy*vy)*0.5;
    			}
    			var rgb = dyeColorHSB.toRGB();
    			dyeColor.set(rgb.red/255, rgb.green/255, rgb.blue/255);
    		}

		updateLastMouse();
	}

  override function tick (delta:Float):Void {
    if(!simulation_enabled) return;

		gl.viewport (0, 0, offScreenTarget.width, offScreenTarget.height);
		gl.bindFramebuffer(gl.FRAMEBUFFER, offScreenTarget.frameBufferObject);

		GL.clearColor(0,0,0,1);
		GL.clear(GL.COLOR_BUFFER_BIT);

		if(renderFluidEnabled)
    {
      // renderTexture(fluid.dyeRenderTarget.readFromTexture);
      renderTexture(renderFluidShader, fluid.dyeRenderTarget.readFromTexture);
    }

			//render offscreen texture to screen
		gl.viewport (0, 0, app.runtime.window_width(), app.runtime.window_height());

    gl.bindFramebuffer(gl.FRAMEBUFFER, screenBuffer);

		// renderTexture(offScreenTarget.texture);
    renderTexture(blitTextureShader, offScreenTarget.texture);

		if(renderParticlesEnabled)
		{
			GL.enable(GL.BLEND);
			GL.blendEquation(GL.FUNC_ADD);
      GL.blendFunc( GL.SRC_ALPHA, GL.SRC_ALPHA );

      renderParticlesShader.dye.data = offScreenTarget.texture;
      renderParticles(renderParticlesShader);
			//renderParticles();

			GL.disable(GL.BLEND);
		}
	}
  inline function renderParticles(shader:{>TShader, positionData:UTexture, velocityData:UTexture} ):Void{
    //set vertices
    gl.bindBuffer(gl.ARRAY_BUFFER, particles.particleUVs);

    //set uniforms
    shader.positionData.data = particles.positionData.readFromTexture;
    shader.velocityData.data = particles.velocityData.readFromTexture;

    //draw points
    shader.activate(true, true);
    gl.drawArrays(gl.POINTS, 0, particles.count);
    shader.deactivate();
  }

  inline function renderTexture(shader:{>TShader, texture:UTexture}, texture:GLTexture){
		gl.bindBuffer (gl.ARRAY_BUFFER, textureQuad);

		shader.texture.data = texture;

		shader.activate(true, true);
		gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
		shader.deactivate();
	}


  function updatePointSize(){
		renderParticlesShader.POINT_SIZE = Std.int(pointSize) + ".0";
	}

	function updateSimulationTextures(){
		//only resize if there is a change
		var w:Int, h:Int;
		w = Math.round(app.runtime.window_width()*fluidScale);
		h = Math.round(app.runtime.window_height()*fluidScale);

		if(w != fluid.width || h != fluid.height)
			fluid.resize(w, h);

		w = Math.round(app.runtime.window_width()*offScreenScale);
		h = Math.round(app.runtime.window_height()*offScreenScale);
		if(w != offScreenTarget.width || h != offScreenTarget.height)
			offScreenTarget.resize(w, h);

		if(particleCount != particles.count) particles.setCount(particleCount);

		particles.flowScaleX = 1/(fluid.cellSize * fluid.aspectRatio);
		particles.flowScaleY = 1/fluid.cellSize;
		particles.dragCoefficient = 1;
	}

	function set_simulationQuality(quality:SimulationQuality):SimulationQuality{
		switch (quality) {
			case UltraHigh:
				particleCount = 1 << 20;
				fluidScale = 1/2;
				fluidIterations = 30;
				offScreenScale = 1/1;
				offScreenFilter = GL.NEAREST;
        pointSize = 1;
			case High:
				particleCount = 1 << 20;
				fluidScale = 1/4;
				fluidIterations = 20;
				offScreenScale = 1/1;
				offScreenFilter = GL.NEAREST;
        pointSize = 1;

			case Medium:
				particleCount = 1 << 18;
				fluidScale = 1/4;
				fluidIterations = 18;
				offScreenScale = 1/1;
				offScreenFilter = GL.NEAREST;
        pointSize = 1;

			case Low:
				particleCount = 1 << 16;
				fluidScale = 1/5;
				fluidIterations = 14;
				offScreenScale = 1/1;
				offScreenFilter = GL.NEAREST;
        pointSize = 2;

			case UltraLow:
				particleCount = 1 << 14;
				fluidScale = 1/6;
				fluidIterations = 12;
				offScreenScale = 1/2;
				offScreenFilter = GL.NEAREST;
        pointSize = 2;

			case iOS:
				particleCount = 1 << 14;
				fluidScale = 1/10;
				fluidIterations = 6;
				offScreenScale = 1/2;
				offScreenFilter = GL.LINEAR;
        pointSize = 5;

		}
		renderParticlesEnabled = particleCount > 1;
    // renderParticlesEnabled = false;
		return simulationQuality = quality;
	}

	function set_fluidIterations(v:Int):Int{
		fluidIterations = v;
		if(fluid != null) fluid.solverIterations = v;
		return v;
	}

	var qualityDirection:Int = 0;
	function lowerQualityRequired(magnitude:Float){
		if(qualityDirection>0)return;
		qualityDirection = -1;
		var qualityIndex = Type.enumIndex(this.simulationQuality);
		var maxIndex = Type.allEnums(SimulationQuality).length - 1;
		if(qualityIndex >= maxIndex)return;

		if(magnitude < 0.5) qualityIndex +=1;
		else                qualityIndex +=2;

		if(qualityIndex > maxIndex)qualityIndex = maxIndex;

		var newQuality = Type.createEnumIndex(SimulationQuality, qualityIndex);
		trace('Average FPS: '+performanceMonitor.fpsAverage+', lowering quality to: '+newQuality);
		this.simulationQuality = newQuality;
		updateSimulationTextures();
    updatePointSize();

	}

	//!# Requires better upsampling before use!
	function higherQualityRequired(magnitude:Float){
		if(qualityDirection<0)return;
		qualityDirection = 1;

		var qualityIndex = Type.enumIndex(this.simulationQuality);
		var minIndex = 0;
		if(qualityIndex <= minIndex)return;

		if(magnitude < 0.5) qualityIndex -=1;
		else                qualityIndex -=2;

		if(qualityIndex < minIndex)qualityIndex = minIndex;

		var newQuality = Type.createEnumIndex(SimulationQuality, qualityIndex);
		trace('Raising quality to: '+newQuality);
		this.simulationQuality = newQuality;
		updateSimulationTextures();
    updatePointSize();

	}
  inline function updateLastMouse(){
		lastMouse.set(mouse.x, mouse.y);
		lastMouseFluid.set(
			fluid.clipToAspectSpaceX(windowToClipSpaceX(mouse.x)),
			fluid.clipToAspectSpaceY(windowToClipSpaceY(mouse.y))
		);
		lastMousePointKnown = true && mousePointKnown;
	}
  public function updateFrameRegion()
  {
    var frameEl = js.Browser.document.getElementById('frame');
    var window_height = app.runtime.window_height();
    var window_width = app.runtime.window_height();

    frameRegionLBRT.set(
      frameEl.offsetLeft/window_width,
      1 - (frameEl.offsetTop + frameEl.offsetHeight)/window_height,
      (frameEl.offsetLeft + frameEl.offsetWidth)/window_width,
      1 - frameEl.offsetTop/window_height
    );
  }
	//---- Interface ----//

	function reset():Void{
		particles.reset();
		fluid.clear();
	}

	//coordinate conversion
	inline function windowToClipSpaceX(x:Float) return (x/app.runtime.window_width())*2 - 1;
	inline function windowToClipSpaceY(y:Float) return ((app.runtime.window_height()-y)/app.runtime.window_height())*2 - 1;

	override function onmousedown( x : Float , y : Float , button : Int, _, _){
		this.isMouseDown = true;
    this.hueCycleEnabled = true;

	}
	override function onmouseup( x : Float , y : Float , button : Int, _, _){
    if(!MOUSE_ALWAYS_DOWN){
      this.isMouseDown = false;
    }
	}

	override function onmousemove( x : Float , y : Float , xrel:Int, yrel:Int, _, _) {
		mouse.set(x, y);
		mouseFluid.set(
			fluid.clipToAspectSpaceX(windowToClipSpaceX(x)),
			fluid.clipToAspectSpaceY(windowToClipSpaceY(y))
		);
		mousePointKnown = true;
	}


	override function ontouchdown(x:Float, y:Float, dx:Float, dy:Float, touch_id:Int, timestamp:Float){
    updateTouchCoordinate(x,y);
		updateLastMouse();
		this.isMouseDown = true;
		this.hueCycleEnabled = true;
	}

	override function ontouchup(x:Float, y:Float, dx:Float, dy:Float, touch_id:Int, timestamp:Float){
		updateTouchCoordinate(x,y);
		this.isMouseDown = false;
	}

	override function ontouchmove(x:Float, y:Float, dx:Float, dy:Float, touch_id:Int, timestamp:Float){
		updateTouchCoordinate(x,y);
	}


	function updateTouchCoordinate(x:Float, y:Float){
		x = x*app.runtime.window_width();
		y = y*app.runtime.window_height();
		mouse.set(x, y);
		mouseFluid.set(
			fluid.clipToAspectSpaceX(windowToClipSpaceX(mouse.x)),
			fluid.clipToAspectSpaceY(windowToClipSpaceY(mouse.y))
		);
		mousePointKnown = true;
	}


	var lshiftDown = false;
	var rshiftDown = false;
	override function onkeydown( keyCode : Int, _, _, _, _, _){
		switch (keyCode) {
			case Key.lshift:
				lshiftDown = true;
			case Key.rshift:
				rshiftDown = true;
		}
	}

	override function onkeyup( keyCode : Int , _, _, _, _, _){
		switch (keyCode) {
			case Key.key_r:
				if(lshiftDown || rshiftDown) particles.reset();
				else reset();
			case Key.key_p:
				renderParticlesEnabled = !renderParticlesEnabled;
			case Key.key_d:
				renderFluidEnabled = !renderFluidEnabled;
			case Key.key_s:
				fluid.clear();
			case Key.lshift:
				lshiftDown = false;
			case Key.rshift:
				rshiftDown = false;
		}
	}

}

enum SimulationQuality{
	UltraHigh;
	High;
	Medium;
	Low;
	UltraLow;
	iOS;
}




@:vert('#pragma include("src/shaders/glsl/no-transform.vert")')
@:frag('
	uniform sampler2D texture;
	uniform vec4 regionLBRT;
	varying vec2 texelCoord;

	float isInRegion(in vec2 origin, in vec2 end, in vec2 p){
		vec2 iv = step(origin, p) * (1.0 - step(end, p));
		return iv.x*iv.y;
	}

	void main(void){
		vec2 o = regionLBRT.xy;
		vec2 e = regionLBRT.zw;
		vec2 center = (o+e)*.5;

		float inRegion = isInRegion(o, e, texelCoord);
		float outRegion = 1.0 - inRegion;

		//sample texture
		vec3 c = texture2D(texture, texelCoord).rgb;

		//brightness & contrast
		const float dBrightnessOut  = 0.04;
		c += dBrightnessOut*outRegion;

		//tv glow
		c += max((0.5 - distance(texelCoord, center)) * vec3(0.3), 0.0) * outRegion;

		gl_FragColor = vec4(c, 1.0);
	}
')
class BlitTexture extends ShaderBase {}

@:vert('#pragma include("src/shaders/glsl/no-transform.vert")')
@:frag('
	uniform sampler2D texture;
	uniform vec4 regionLBRT;
	varying vec2 texelCoord;

	float isInRegion(in vec2 origin, in vec2 end, in vec2 p){
		vec2 iv = step(origin, p) * (1.0 - step(end, p));
		return iv.x*iv.y;
	}

	vec3 saturation(in vec3 rgb, in float amount){
		const vec3 CW = vec3(0.299, 0.587, 0.114);//NTSC conversion weights
		vec3 bw = vec3(dot(rgb, CW));
		return mix(bw, rgb, amount);
	}

	void main(void){
		vec2 o = regionLBRT.xy;
		vec2 e = regionLBRT.zw;
		vec2 center = (o+e)*.5;

		float inRegion = isInRegion(o, e, texelCoord);
		float outRegion = 1.0 - inRegion;

		//sample texture
		vec3 c = texture2D(texture, texelCoord).rgb;

		//vignette outside region
		float l = distance(texelCoord, vec2(.5)) - 0.05;
		float vignetteMultiplier = 1.0 - clamp(0., 1.0, 2.0*l*l*l*l)*(outRegion);

		//saturation
		float minSaturation = 0.0;
		c = saturation(c, max(inRegion, minSaturation));

		//tv glow
		c *= vignetteMultiplier;

		gl_FragColor = vec4(c, 1.0);
	}
')
class FluidRender extends ShaderBase {}

@:vert('
	uniform sampler2D dye;

	vec3 saturation(in vec3 rgb, in float amount){
		const vec3 CW = vec3(0.299, 0.587, 0.114);
		vec3 bw = vec3(dot(rgb, CW));//uses NTSC conversion weights
		return mix(bw, rgb, amount);
	}

	const float POINT_SIZE = 1.0;

	void main(){
		vec2 p = unpackParticlePosition(texture2D(positionData, particleUV));
		vec2 v = unpackParticleVelocity(texture2D(velocityData, particleUV));


		gl_PointSize = POINT_SIZE;
		gl_Position = vec4(p, 0.0, 1.0);

		vec3 dyeColor = texture2D(dye, p*.5+.5).rgb;

		float dyeLevel = dot(dyeColor, vec3(1.0));//dot(dyeColor, vec3(0.299, 0.587, 0.114));

		float speed = length(v);
		float x = clamp(speed * 2.0, 0., 1.);

		color.rgb = saturation(dyeColor, 1.0 + x) + (dyeLevel)*.05 + x*x*0.05;
		color.a = clamp(dyeLevel, 0.0, 1.0);
	}
')
class ColorParticleMotion extends GPUParticles.RenderParticles{}

@:frag('
	#pragma include("src/shaders/glsl/geom.glsl")
	uniform bool isMouseDown;
	uniform vec2 mouse; //aspect space coordinates
	uniform vec2 lastMouse;
	uniform vec3 dyeColor;

	vec3 saturation(in vec3 rgb, in float amount){
		const vec3 CW = vec3(0.299, 0.587, 0.114);
		vec3 bw = vec3(dot(rgb, CW));//uses NTSC conversion weights
		return mix(bw, rgb, amount);
	}

	void main(){
		vec4 color = texture2D(dye, texelCoord);
		//darken
		color -= sign(color)*(0.006 - (1.0 - color)*0.004 );

		//saturate, needs to be carefully balanced with darken
		// color.rgb = saturation(color.rgb, 0.999);

		if(isMouseDown){
			vec2 mouseVelocity = (mouse - lastMouse)/dt;

			//compute tapered distance to mouse line segment
			float projection;
			float l = distanceToSegment(mouse, lastMouse, p, projection);
			float taperFactor = 0.6;
			float projectedFraction = 1.0 - clamp(projection, 0.0, 1.0)*taperFactor;

			float speed = 0.016*length(mouseVelocity)/dt;
			float x = speed;

			float R = 0.15;
			float m = 1.0*exp(-l/R);
			float m2 = m*m;
			float m3 = m2*m;
			float m4 = m3*m;
			float m6 = m4*m*m;

			color.rgb +=
				0.009*dyeColor*(16.0*m3*(0.5*x+1.0)+m2) //color
			  + 0.03*m6*m*m*vec3(1.0)*(0.5*m3*x + 1.0);     //white
		}

		gl_FragColor = color;
	}
')
class MouseDye extends GPUFluid.UpdateDye{}

@:frag('
	#pragma include("src/shaders/glsl/geom.glsl")
	// #pragma include("src/shaders/glsl/math.glsl")
	uniform bool isMouseDown;
	uniform vec2 mouse; //aspect space coordinates
	uniform vec2 lastMouse;

	void main(){
		vec2 v = sampleVelocity(velocity, texelCoord);
		// v -= abs(sign(v))*0.2*dt;
		// v -= sign(v)*(0.005 - (1.0 - v)*0.001);
		v *= 0.99;
		if(isMouseDown){
			vec2 mouseVelocity = -(lastMouse - mouse)/dt;
			// mouse = mouse - (lastMouse - mouse) * 2.0;//predict mouse position

			//compute tapered distance to mouse line segment
			float projection;
			float l = distanceToSegment(mouse, lastMouse, p, projection);
			float taperFactor = 0.6;//1 => 0 at lastMouse, 0 => no tapering
			float projectedFraction = 1.0 - clamp(projection, 0.0, 1.0)*taperFactor;
			float R = 0.02;
			float m = exp(-l/R); //drag coefficient
			m *= projectedFraction * projectedFraction;
			vec2 targetVelocity = mouseVelocity * dx * 1.4;

			v += (targetVelocity - v)*(m + m*m*m*8.0)*(0.2);
		}

		//add a wee bit of random noise
		// v += (rand((texelCoord + v))*2.0 - 1.0)*0.5;

		gl_FragColor = packFluidVelocity(v);
	}
')
class MouseForce extends GPUFluid.ApplyForces{}
