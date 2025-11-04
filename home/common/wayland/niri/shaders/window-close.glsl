vec3 mod289_3(vec3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec2 mod289_2(vec2 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
vec3 permute(vec3 x) { return mod289_3(((x*34.0)+1.0)*x); }

float snoise(vec2 v) {
    const vec4 C = vec4(0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439);
    vec2 i  = floor(v + dot(v, C.yy));
    vec2 x0 = v - i + dot(i, C.xx);
    vec2 i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
    vec4 x12 = x0.xyxy + C.xxzz;
    x12.xy -= i1;
    i = mod289_2(i);
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

float fbm(vec2 p) {
    float value = 0.0;
    float amplitude = 0.5;
    for(int i = 0; i < 4; i++) {
        value += amplitude * (snoise(p) * 0.5 + 0.5);
        p *= 2.0;
        amplitude *= 0.5;
    }
    return value;
}

vec4 getFireColor(float val) {
    vec3 c0 = vec3(0.0, 0.0, 0.0);
    vec3 c1 = vec3(0.8, 0.2, 0.0);
    vec3 c2 = vec3(1.0, 0.6, 0.0);
    vec3 c3 = vec3(1.0, 0.9, 0.3);
    vec3 c4 = vec3(1.0, 1.0, 1.0);

    vec3 color;
    if (val < 0.25) {
        color = mix(c0, c1, val / 0.25);
    } else if (val < 0.5) {
        color = mix(c1, c2, (val - 0.25) / 0.25);
    } else if (val < 0.75) {
        color = mix(c2, c3, (val - 0.5) / 0.25);
    } else {
        color = mix(c3, c4, (val - 0.75) / 0.25);
    }
    return vec4(color, val);
}

vec4 close_color(vec3 coords_geo, vec3 size_geo) {
    float progress = niri_clamped_progress;
    vec2 uv = coords_geo.xy;

    float uScale = 1.0;
    float uTurbulence = 0.5;
    vec2 uSeed = vec2(0.5, 0.5);

    float SCORCH_WIDTH = 0.2 * uScale;
    float BURN_WIDTH = 0.03 * uScale;
    float SMOKE_WIDTH = 0.9 * uScale;
    float FLAME_WIDTH = 0.2 * uScale;

    float hideThreshold = mix(-SCORCH_WIDTH, 1.0 + SMOKE_WIDTH, progress);

    vec2 scorchRange = vec2(hideThreshold, hideThreshold + SCORCH_WIDTH);
    vec2 burnRange = vec2(hideThreshold - BURN_WIDTH, hideThreshold + BURN_WIDTH);
    vec2 flameRange = vec2(hideThreshold - FLAME_WIDTH, hideThreshold);
    vec2 smokeRange = vec2(hideThreshold - SMOKE_WIDTH, hideThreshold);

    vec2 startPos = vec2(0.5, 0.0);
    float circle = length((uv - startPos) * (size_geo.xy / max(size_geo.x, size_geo.y)));
    vec2 noiseUV = uv / uScale * size_geo.xy / 1.5;
    float smokeNoise = fbm(noiseUV * 0.01 + uSeed + progress * vec2(0.0, 0.3));
    float gradient = mix(circle, smokeNoise, 200.0 * uTurbulence * uScale / max(size_geo.x, size_geo.y));

    float smokeMask = smoothstep(0.0, 1.0, (gradient - smokeRange.x) / SMOKE_WIDTH);
    float flameMask = smoothstep(0.0, 1.0, (gradient - flameRange.x) / FLAME_WIDTH);
    float fireMask = smoothstep(1.0, 0.0, abs(gradient - hideThreshold) / BURN_WIDTH);
    float scorchMask = smoothstep(1.0, 0.0, (gradient - scorchRange.x) / SCORCH_WIDTH);

    vec4 oColor = vec4(0.0);

    if (gradient > hideThreshold) {
        oColor = vec4(0.0, 0.0, 0.0, 1.0);
    }

    if (smokeRange.x < gradient && gradient < smokeRange.y) {
        float smoke = smokeMask * smokeNoise;
        oColor.rgb = mix(oColor.rgb, vec3(0.5 * smoke), smoke);
        oColor.a = max(oColor.a, smoke);
    
        float emberNoise = fbm(noiseUV * 0.05 + uSeed - smokeNoise * vec2(0.0, 0.3 * smokeMask));
        float embers = clamp(pow(emberNoise + 0.3, 100.0), 0.0, 2.0) * smoke;
        vec4 emberColor = getFireColor(embers);
        oColor.rgb += emberColor.rgb * emberColor.a;
        oColor.a = max(oColor.a, emberColor.a);
    }

    if (scorchRange.x < gradient && gradient < scorchRange.y) {
        oColor.rgb = mix(oColor.rgb, mix(oColor.rgb, vec3(0.1, 0.05, 0.02), 0.4), scorchMask);
    }

    if (min(burnRange.x, flameRange.x) < gradient && gradient < max(burnRange.y, flameRange.y)) {
        float flameNoise = fbm(noiseUV * 0.02 + uSeed + progress * vec2(0.0, 1.0));
    
        if (flameRange.x < gradient && gradient < flameRange.y) {
            float flame = clamp(pow(flameNoise + 0.3, 20.0), 0.0, 2.0) * flameMask;
            flame += clamp(pow(flameNoise + 0.4, 10.0), 0.0, 2.0) * flameMask * flameMask * 0.1;
            vec4 flameColor = getFireColor(flame);
            oColor.rgb += flameColor.rgb * flameColor.a;
            oColor.a = max(oColor.a, flameColor.a);
        }
    
        if (burnRange.x < gradient && gradient < burnRange.y) {
            float fire = fireMask * pow(flameNoise + 0.4, 4.0);
            vec4 fireColor = getFireColor(fire);
            oColor.rgb += fireColor.rgb * fireColor.a;
            oColor.a = max(oColor.a, fireColor.a);
        }
    }

    return oColor;
}
