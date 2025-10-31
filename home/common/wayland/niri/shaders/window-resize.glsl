#define PI 3.1415926535897932384626433832795
float hash12(vec2 p) {
    float h = dot(p, vec2(127.1, 311.7));
    return fract(sin(h) * 43758.5453);
}

vec3 mod289(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec2 mod289(vec2 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec3 permute(vec3 x) { return mod289(((x*34.0)+1.0)*x); }

float snoise(vec2 v) {
    const vec4 C = vec4(0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439);
    vec2 i  = floor(v + dot(v, C.yy));
    vec2 x0 = v - i + dot(i, C.xx);
    vec2 i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
    vec4 x12 = x0.xyxy + C.xxzz;
    x12.xy -= i1;
    i = mod289(i);
    vec3 p = permute(permute(i.y + vec3(0.0, i1.y, 1.0)) + i.x + vec3(0.0, i1.x, 1.0));
    vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
    m = m*m; m = m*m;
    vec3 x = 2.0 * fract(p * C.www) - 1.0;
    vec3 h = abs(x) - 0.5;
    vec3 ox = floor(x + 0.5);
    vec3 a0 = x - ox;
    m *= 1.79284291400159 - 0.85373472095314 * (a0*a0 + h*h);
    vec3 g;
    g.x = a0.x * x0.x + h.x * x0.y;
    g.yz = a0.yz * x12.xz + h.yz * x12.yw;
    return 130.0 * dot(m, g);
}
float simplex2D(vec2 v) { return snoise(v); }

// Easing functions
float easeOutSine(float x) { return sin((x * PI) / 2.0); }
float easeInSine(float x) { return 1.0 - cos((x * PI) / 2.0); }
float easeOutCubic(float x) { return 1.0 - pow(1.0 - x, 3.0); }

// Alpha compositing
vec4 alphaOver(vec4 bot, vec4 top) {
    return top + bot * (1.0 - top.a);
}

// Hue shifting
vec3 offsetHue(vec3 c, float offset) {
    vec3 k = vec3(0.57735, 0.57735, 0.57735);
    float cosAngle = cos(offset);
    c = c * cosAngle + cross(k, c) * sin(offset) + k * dot(k, c) * (1.0 - cosAngle);
    return c;
}

// Niri texture samplers
vec4 getInputColor(vec2 coords) {
    // For resize, niri_tex_next holds the new window state
    return texture2D(niri_tex_next, coords);
}

vec4 getBlurredInputColor(vec2 uv, float lod, float steps) {
    // niri_tex_next_size is a vec3, we only need xy
    vec2 texel = 1.0 / niri_tex_next_size.xy;
    vec4 color = vec4(0.0);
    float count = 0.0;
    for (float x = -steps; x <= steps; ++x) {
        for (float y = -steps; y <= steps; ++y) {
            color += texture2D(niri_tex_next, uv + vec2(x, y) * texel * lod);
            count += 1.0;
        }
    }
    return color / count;
}

// --- Niri Shader Entry Point ---

vec4 resize_color(vec3 coords_curr_geo, vec3 size_curr_geo) {

    // --- Map niri inputs to BMW shader variables ---
    // Get UV coords for the *next* texture state
    vec3 coords_tex_next = niri_geo_to_tex_next * vec3(coords_curr_geo.xy, 1.0);
    vec2 iTexCoord = coords_tex_next.xy;
    vec2 uSize = size_curr_geo.xy;
    float uProgress = niri_clamped_progress; // This effect fades out, so 0->1 is correct

    const bool uForOpening = false;
    const float uPadding = 0.0;
    const float uSpeed = 5.0;
    const bool uRandomColor = true;
    const float uStartHue = 0.0;
    const float uSaturation = 1.0;
    const float uBlur = 1.0;
    const vec2 uSeed = vec2(42.42, 84.84);
    const float uEdgeSize = 0.1;
    const float uEdgeHardness = 0.5;

    // --- Original 'main' function from BMW shader ---
    float progress = uForOpening ? uProgress : 1.0 - uProgress;
    // This port assumes the effect fades *out* (like pixelate), so we'll
    // use the direct progress. If it should run in reverse, use the line above.
    progress = uProgress;

    vec2 uv = (iTexCoord.st - 0.5) * uSize / (vec2(1.0) - vec2(uPadding * 2.0) / uSize);
    uv       = uv / uSize + 0.5;

    vec2 oneToOneUV = uv;
    oneToOneUV -= 0.5;

    if (uSize.x > uSize.y) {
        oneToOneUV.y *= uSize.y / uSize.x;
    } else {
        oneToOneUV.x *= uSize.x / uSize.y;
    }

    oneToOneUV += 0.5;

    uv = mix(oneToOneUV, uv, progress);

    float shape    = mix(2.0, 100.0, pow(progress, 5.0));
    float gradient = pow(abs(uv.x - 0.5) * 2.0, shape) + pow(abs(uv.y - 0.5) * 2.0, shape);
    gradient += simplex2D((iTexCoord + uSeed)) * 0.5;

    float glowMask = (progress - gradient) / (uEdgeSize + 0.1);
    glowMask       = 1.0 - clamp(glowMask, 0.0, 1.0);

    glowMask *= easeOutSine(min(1.0, (1.0 - progress) * 4.0));

    vec2 windowUV = (iTexCoord.st - 0.5) * mix(1.1, 1.0, easeOutCubic(progress)) + 0.5;
    vec4 windowColor;
    if (uBlur > 0.0) {
        windowColor = getBlurredInputColor(windowUV, (1.0 - progress) * uBlur, 3.0);
    } else {
        windowColor = getInputColor(windowUV);
    }
    glowMask *= windowColor.a;

    vec3 glowColor   = cos(progress * uSpeed + uv.xyx + vec3(0, 2, 4)).xyz;
    float colorOffset = (uRandomColor) ? hash12(uSeed) : uStartHue;
    glowColor       = offsetHue(glowColor, colorOffset + 0.1);
    glowColor       = clamp(glowColor * uSaturation, vec3(0.0), vec3(1.0));

    windowColor.rgb += glowColor * glowMask;

    windowColor = alphaOver(windowColor, vec4(glowColor, glowMask * 0.2));

    float softCrop = 1.0 - smoothstep(progress - 0.5, progress + 0.5, gradient);
    float hardCrop = 1.0 - smoothstep(progress - 0.05, progress + 0.05, gradient);
    float cropMask = mix(softCrop, hardCrop, uEdgeHardness);

    cropMask *= easeInSine(min(1.0, progress * 2.0));

    cropMask = max(cropMask, 1.0 - easeOutSine(min(1.0, (1.0 - progress) * 4.0)));

    windowColor.a *= cropMask;

    return windowColor;
}
