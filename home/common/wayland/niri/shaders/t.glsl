animations {
    // Hexagon effect for window-open (inspired by Burn-My-Windows)
    window-open {
        duration-ms 300
        curve "ease-out-cubic"
        custom-shader r"
#define PI 3.1415926535897932384626433832795

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

// Hexagonal pattern generator
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
    float noise = snoise(uv + vec2(0.5, 0.5)) * 0.5 + 0.5;
    float noisyProgress = clamp(mix(noise - 1.0, noise + 1.0, progress), 0.0, 1.0);
    
    // Glow fades in first half, tiles appear in second half
    float glowProgress = smoothstep(0.0, 1.0, clamp(noisyProgress / 0.5, 0.0, 1.0));
    float tileProgress = smoothstep(0.0, 1.0, clamp((noisyProgress - 0.5) / 0.5, 0.0, 1.0));
    
    // Scale for hexagon pattern
    float hexScale = 10.0;
    vec2 texScale = 0.1 * size_geo.xy / hexScale;
    vec4 hex = getHexagons(uv * texScale);
    
    vec4 oColor = vec4(0.0);
    
    // Only show tiles that have appeared
    if (tileProgress < hex.z) {
        // Shrink tiles by offsetting texture lookup
        vec2 lookupOffset = tileProgress * hex.xy / texScale / (1.0 - tileProgress);
        
        // For opening, we show the window content as tiles grow
        if (coords_geo.x >= 0.0 && coords_geo.x <= 1.0 && 
            coords_geo.y >= 0.0 && coords_geo.y <= 1.0) {
            oColor = vec4(0.0, 0.0, 0.0, 1.0); // Window will be composited on top
        }
        
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
        
        // Add glow and lines
        oColor.rgb += glowColor.rgb * glowColor.a;
        oColor.rgb += lineColor.rgb * lineColor.a;
        oColor.a = progress;
    }
    
    return oColor;
}
        "
    }
    
    // Incinerate effect for window-close (burns from top to bottom)
    window-close {
        duration-ms 350
        curve "ease-out-quad"
        custom-shader r"
#define PI 3.1415926535897932384626433832795

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

vec4 close_color(vec3 coords_geo, vec3 size_geo) {
    float t = niri_clamped_progress;
    vec2 uv = coords_geo.xy;
    
    // Burn progresses from top (0.0) to bottom (1.0)
    float burnProgress = t * 1.2;
    
    // Vertical position with fade width
    float fadeWidth = 0.15;
    float yPos = uv.y;
    
    // Create burning edge with noise
    vec2 noiseUV = uv * vec2(8.0, 4.0);
    noiseUV.y += t * 2.0; // Move noise upward
    float noise = fbm(noiseUV);
    
    // Jagged burn line
    float burnLine = burnProgress - yPos + (noise - 0.5) * fadeWidth;
    
    // Window visibility (burns away from top)
    float windowMask = 1.0 - smoothstep(-fadeWidth * 0.5, fadeWidth * 0.5, burnLine);
    
    // Fire effect at the burning edge
    float fireIntensity = smoothstep(fadeWidth, 0.0, abs(burnLine)) * (1.0 - t * 0.3);
    
    // More detailed fire noise at the edge
    float fireDetail = fbm(noiseUV * 2.0 + vec2(0.0, t * 5.0));
    fireIntensity *= fireDetail;
    
    // Fire color gradient (dark red -> orange -> yellow -> white)
    vec3 darkRed = vec3(0.2, 0.0, 0.0);
    vec3 red = vec3(0.8, 0.1, 0.0);
    vec3 orange = vec3(1.0, 0.4, 0.0);
    vec3 yellow = vec3(1.0, 0.9, 0.2);
    vec3 white = vec3(1.0, 1.0, 0.8);
    
    vec3 fireColor;
    if (fireIntensity < 0.3) {
        fireColor = mix(darkRed, red, fireIntensity / 0.3);
    } else if (fireIntensity < 0.6) {
        fireColor = mix(red, orange, (fireIntensity - 0.3) / 0.3);
    } else if (fireIntensity < 0.8) {
        fireColor = mix(orange, yellow, (fireIntensity - 0.6) / 0.2);
    } else {
        fireColor = mix(yellow, white, (fireIntensity - 0.8) / 0.2);
    }
    
    // Add embers/sparks
    float ember = step(0.97, fbm(uv * 30.0 + t * 10.0)) * fireIntensity;
    fireColor += ember * vec3(1.0, 0.8, 0.4);
    
    // Smoke above burned area
    float smokeArea = smoothstep(burnProgress, burnProgress - 0.2, yPos);
    float smoke = fbm(noiseUV * 0.5) * smokeArea * 0.3 * (1.0 - t);
    vec3 smokeColor = vec3(0.15, 0.12, 0.1) * smoke;
    
    if (coords_geo.x >= 0.0 && coords_geo.x <= 1.0 && 
        coords_geo.y >= 0.0 && coords_geo.y <= 1.0) {
        
        // Blend fire and smoke
        vec3 finalColor = fireColor * fireIntensity + smokeColor;
        float finalAlpha = windowMask;
        
        return vec4(finalColor, finalAlpha);
    }
    
    return vec4(0.0);
}
        "
    }
    
    // Pixelate effect for window-resize
    window-resize {
        duration-ms 180
        curve "ease-out-cubic"
        custom-shader r"
float hash(vec2 p) {
    return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453);
}

vec4 resize_color(vec3 coords_curr_geo, vec3 size_curr_geo) {
    vec3 coords_tex_next = niri_geo_to_tex_next * coords_curr_geo;
    
    float t = niri_clamped_progress;
    
    // Pixelation intensity based on progress
    float pixelSize = mix(1.0, 32.0, t * 0.6);
    
    // Create pixel grid
    vec2 pixelCoords = floor(coords_tex_next.xy * size_curr_geo.xy / pixelSize) * pixelSize;
    vec2 pixelUV = pixelCoords / size_curr_geo.xy;
    
    // Sample from center of pixel
    vec2 centerUV = (pixelCoords + pixelSize * 0.5) / size_curr_geo.xy;
    vec4 color = texture2D(niri_tex_next, centerUV);
    
    // Random pixel fade out
    float pixelHash = hash(pixelCoords / pixelSize);
    float fadeThreshold = t * 1.2;
    float pixelAlpha = smoothstep(fadeThreshold - 0.2, fadeThreshold, pixelHash);
    
    // Add slight color glitch
    if (pixelAlpha > 0.5 && t > 0.3) {
        float glitch = hash(pixelCoords / pixelSize + vec2(t * 10.0, 0.0));
        if (glitch > 0.9) {
            color.rgb = color.bgr; // Swap color channels
        }
    }
    
    color.a *= pixelAlpha;
    
    return color;
}
        "
    }
}
