// Simplex noise
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

// Fractal noise
float simplex2DFractal(vec2 p) {
    float value = 0.0;
    float amplitude = 0.5;
    for(int i = 0; i < 4; i++) {
        value += amplitude * (snoise(p) * 0.5 + 0.5);
        p *= 2.0;
        amplitude *= 0.5;
    }
    return value;
}

// Tritone color gradient
vec3 tritone(float val, vec3 c1, vec3 c2, vec3 c3) {
    val = clamp(val, 0.0, 1.0);
    return mix(mix(c1, c2, val * 2.0), mix(c2, c3, val * 2.0 - 1.0), step(0.5, val));
}

// Alpha compositing (straight alpha)
vec4 alphaOver(vec4 bot, vec4 top) {
    return top + bot * (1.0 - top.a);
}

// Edge mask
float getAbsoluteEdgeMask(float scale, float offset, vec2 iTexCoord) {
    return 1.0 - clamp(pow(length(iTexCoord - 0.5) * 2.0, scale) + offset, 0.0, 1.0);
}

// Niri texture sampler
vec4 getInputColor(vec2 coords) {
    return texture2D(niri_tex_next, coords);
}

// This maps a given value in [0..1] to a color from the rgba color ramp
// [transparent black ... semi-transparent uColor ... opaque white].
vec4 getFireColor(float val, vec3 uColor) {
    return vec4(tritone(val, vec3(0.0), uColor, vec3(1.0)), val);
}

// --- Niri Shader Entry Point ---

vec4 close_color(vec3 coords_geo, vec3 size_geo) {
    vec2 iTexCoord = coords_geo.xy;
    float uProgress = niri_clamped_progress;
    vec2 uSize = size_geo.xy;

    const float uDuration = 0.250;
    const bool uForOpening = false;
    const vec2 uSeed = vec2(12.9898, 78.233);
    const vec3 uColor = vec3(1.0, 0.4, 0.0); // Fire color (orange)
    const float uScale = 1.0;
    const float uTurbulence = 0.5;
    const vec2 uStartPos = vec2(0.5, 0.5); // Burn from center

    // --- Original 'main' function from incinerate.frag ---
    float SCORCH_WIDTH = 0.2 * uScale;
    float BURN_WIDTH   = 0.03 * uScale;
    float SMOKE_WIDTH  = 0.9 * uScale;
    float FLAME_WIDTH  = 0.2 * uScale;

    float hideThreshold =
        mix(uForOpening ? 0.0 : -SCORCH_WIDTH, 1.0 + SMOKE_WIDTH, uProgress);

    vec2 scorchRange = uForOpening ? vec2(hideThreshold - SCORCH_WIDTH, hideThreshold)
                                     : vec2(hideThreshold, hideThreshold + SCORCH_WIDTH);
    vec2 burnRange   = vec2(hideThreshold - BURN_WIDTH, hideThreshold + BURN_WIDTH);
    vec2 flameRange  = vec2(hideThreshold - FLAME_WIDTH, hideThreshold);
    vec2 smokeRange  = vec2(hideThreshold - SMOKE_WIDTH, hideThreshold);

    float circle = length((iTexCoord - uStartPos) * (uSize.xy / max(uSize.x, uSize.y)));

    vec2 uv = iTexCoord / uScale * uSize / 1.5;
    float smokeNoise =
        simplex2DFractal(uv * 0.01 + uSeed + uProgress * vec2(0.0, 0.3 * uDuration));

    float gradient =
        mix(circle, smokeNoise, 200.0 * uTurbulence * uScale / max(uSize.x, uSize.y));

    float smokeMask = smoothstep(0.0, 1.0, (gradient - smokeRange.x) / SMOKE_WIDTH) *
                      getAbsoluteEdgeMask(100.0, 0.3, iTexCoord);
    float flameMask = smoothstep(0.0, 1.0, (gradient - flameRange.x) / FLAME_WIDTH) *
                      getAbsoluteEdgeMask(20.0, 0.0, iTexCoord);
    float fireMask   = smoothstep(1.0, 0.0, abs(gradient - hideThreshold) / BURN_WIDTH);
    float scorchMask = smoothstep(1.0, 0.0, (gradient - scorchRange.x) / SCORCH_WIDTH);

    if (uForOpening) {
        scorchMask = 1.0 - scorchMask;
    }

    vec4 oColor = vec4(0.0);

    if ((!uForOpening && gradient > hideThreshold) ||
        (uForOpening && gradient < hideThreshold)) {

        vec2 distort = vec2(0.0);

     if (scorchRange.x < gradient && gradient < scorchRange.y) {
         distort = vec2(dFdx(gradient), dFdy(gradient)) * scorchMask * 5.0;
     }

        oColor = getInputColor(iTexCoord + distort);
    }

    if (smokeRange.x < gradient && gradient < smokeRange.y) {
        float smoke = smokeMask * smokeNoise;
        oColor      = alphaOver(oColor, vec4(0.5 * vec3(smoke), smoke));

        float emberNoise = simplex2DFractal(
            uv * 0.05 + uSeed - smokeNoise * vec2(0.0, 0.3 * smokeMask * uDuration));
        float embers = clamp(pow(emberNoise + 0.3, 100.0), 0.0, 2.0) * smoke;
        oColor += getFireColor(embers, uColor);
    }

    if (scorchRange.x < gradient && gradient < scorchRange.y) {
        oColor.rgb = mix(oColor.rgb, mix(oColor.rgb, vec3(0.1, 0.05, 0.02), 0.4), scorchMask);
    }

    if (min(burnRange.x, flameRange.x) < gradient &&
        gradient < max(burnRange.y, flameRange.y)) {
        float flameNoise =
            simplex2DFractal(uv * 0.02 + uSeed + smokeNoise * vec2(0.0, 1.0 * uDuration) +
                               vec2(0.0, uProgress * uDuration));

        if (flameRange.x < gradient && gradient < flameRange.y) {
            float flame = clamp(pow(flameNoise + 0.3, 20.0), 0.0, 2.0) * flameMask;
            flame += clamp(pow(flameNoise + 0.4, 10.0), 0.0, 2.0) * flameMask * flameMask * 0.1;
            oColor += getFireColor(flame, uColor);
        }

        if (burnRange.x < gradient && gradient < burnRange.y) {
            float fire = fireMask * pow(flameNoise + 0.4, 4.0) * oColor.a;
            oColor += getFireColor(fire, uColor);
        }
    }

    return oColor;
}
