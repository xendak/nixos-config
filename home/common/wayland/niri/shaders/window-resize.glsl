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
    for(int i = 0; i < 3; i++) {
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

vec4 resize_color(vec3 coords_curr_geo, vec3 size_curr_geo) {
    vec3 coords_tex_next = niri_geo_to_tex_next * coords_curr_geo;
    vec4 baseColor = texture2D(niri_tex_next, coords_tex_next.st);

    float progress = niri_clamped_progress * 0.3;
    vec2 uv = coords_curr_geo.xy;

    float uScale = 0.8;
    float uTurbulence = 0.3;
    vec2 uSeed = vec2(0.5, 0.5);

    float BURN_WIDTH = 0.05 * uScale;
    float FLAME_WIDTH = 0.15 * uScale;

    vec2 noiseUV = uv / uScale * size_curr_geo.xy / 1.5;
    float smokeNoise = fbm(noiseUV * 0.01 + uSeed + progress * vec2(0.3, 0.0));

    float distFromLeft = uv.x;
    float distFromRight = 1.0 - uv.x;
    float edgeDist = min(distFromLeft, distFromRight);

    float gradient = mix(edgeDist, smokeNoise * 0.5, 100.0 * uTurbulence * uScale / max(size_curr_geo.x, size_curr_geo.y));

    float threshold = progress * 0.3;
    float flameMask = smoothstep(threshold + FLAME_WIDTH, threshold, gradient);
    float fireMask = smoothstep(BURN_WIDTH, 0.0, abs(gradient - threshold));

    vec4 oColor = baseColor;

    if (flameMask > 0.01) {
        float flameNoise = fbm(noiseUV * 0.02 + uSeed + progress * vec2(3.0, 0.0));
        float flame = clamp(pow(flameNoise + 0.3, 15.0), 0.0, 1.5) * flameMask * 0.5;
        vec4 flameColor = getFireColor(flame);
        oColor.rgb = mix(oColor.rgb, oColor.rgb + flameColor.rgb * flameColor.a, flameMask);
    }

    if (fireMask > 0.01) {
        float flameNoise = fbm(noiseUV * 0.02 + uSeed + progress * vec2(3.0, 0.0));
        float fire = fireMask * pow(flameNoise + 0.4, 4.0) * 0.8;
        vec4 fireColor = getFireColor(fire);
        oColor.rgb += fireColor.rgb * fireColor.a * 0.6;
    }

    return oColor;
}
