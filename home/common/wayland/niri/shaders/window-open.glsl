#define PI 3.1415926535897932384626433832795

// Simplex noise (reused from original Burn-My-Windows)
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

// Hexagonal pattern generator (adapted from Burn-My-Windows)
vec4 getHexagons(vec2 p) {
    float edgeLength = sqrt(4.0 / 3.0);
    vec2 scale = vec2(3.0 * edgeLength, 2.0);
    
    vec2 a = mod(p, scale) - scale * 0.5;
    vec2 aAbs = abs(a);
    
    vec2 b = mod(p + scale * 0.5, scale) - scale * 0.5;
    vec2 bAbs = abs(b);
    
    float distA = max(aAbs.x / edgeLength + aAbs.y * 0.5, aAbs.y);
    float distB = max(bAbs.x / edgeLength + bAbs.y * 0.5, bAbs.y);
    
    float dist = 1.0 - min(distA, distB);
    float glow = min(dot(a, a), dot(b, b)) / 1.5;
    vec2 cellCoords = distA < distB ? a : b;
    
    return vec4(cellCoords, dist, glow);
}

vec4 open_color(vec3 coords_geo, vec3 size_geo) {
    float progress = niri_clamped_progress;
    vec2 uv = coords_geo.xy;
    
    // Add noise variation to make tiles appear at different times
    float noise = snoise(uv * 0.5 + vec2(0.5, 0.5)) * 0.5 + 0.5;
    float noisyProgress = clamp(mix(noise - 1.0, noise + 1.0, progress), 0.0, 1.0);
    
    // glowProgress fades in first half, tileProgress fades in second half
    float glowProgress = smoothstep(0.0, 1.0, clamp(noisyProgress / 0.5, 0.0, 1.0));
    float tileProgress = smoothstep(0.0, 1.0, clamp((noisyProgress - 0.5) / 0.5, 0.0, 1.0));
    
    // Scale for hexagon pattern (adjusted for better appearance)
    float hexScale = 8.0;
    vec2 texScale = size_geo.xy / hexScale;
    vec4 hex = getHexagons(uv * texScale);
    
    vec4 oColor = vec4(0.0);
    
    // Only show tiles that have appeared
    if (tileProgress < hex.z) {
        // Sample window content (simulating tile appearance)
        vec4 windowColor = texture2D(niri_tex_next, coords_geo.xy);
        
        // Shrink tiles by offsetting texture lookup
        vec2 lookupOffset = tileProgress * hex.xy / texScale / (1.0 - tileProgress);
        oColor = texture2D(niri_tex_next, coords_geo.xy + lookupOffset);
        
        // Cyan/electric glow colors
        vec4 glowColor = vec4(0.0, 0.8, 1.0, 1.0);
        vec4 lineColor = vec4(0.3, 1.0, 1.0, 1.0);
        
        // Glow intensity (multiple scales for richness)
        glowColor.a *= pow(hex.w, 20.0) * 10.0 + pow(hex.w, 10.0) * 5.0 + pow(hex.w, 2.0) * 0.5;
        
        // Sharp hexagon lines
        float lineWidth = 1.0;
        lineColor.a *= 1.0 - smoothstep(lineWidth * 0.02 * 0.5, lineWidth * 0.02, hex.z);
        
        // Fade in effects based on progress
        glowColor.a *= glowProgress;
        lineColor.a *= glowProgress;
        
        // Apply to visible tiles only
        glowColor *= oColor.a;
        lineColor *= oColor.a;
        
        // Add glow and lines
        oColor.rgb += glowColor.rgb * glowColor.a;
        oColor.rgb = mix(oColor.rgb, lineColor.rgb, lineColor.a);
    }
    
    return oColor;
}
