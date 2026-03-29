{ lib }:
let
  hexToDecMap = {
    "0" = 0;
    "1" = 1;
    "2" = 2;
    "3" = 3;
    "4" = 4;
    "5" = 5;
    "6" = 6;
    "7" = 7;
    "8" = 8;
    "9" = 9;
    "a" = 10;
    "b" = 11;
    "c" = 12;
    "d" = 13;
    "e" = 14;
    "f" = 15;
    "A" = 10;
    "B" = 11;
    "C" = 12;
    "D" = 13;
    "E" = 14;
    "F" = 15;
  };

  decToHexList = [
    "0"
    "1"
    "2"
    "3"
    "4"
    "5"
    "6"
    "7"
    "8"
    "9"
    "a"
    "b"
    "c"
    "d"
    "e"
    "f"
  ];

  # Math helpers
  mod = base: int: base - (int * (base / int));
  fmod = x: y: x - (y * builtins.floor (x / y)); # Float modulo for hue wrapping
  abs = x: if x < 0.0 then 0.0 - x else x;
  max = a: b: if a > b then a else b;
  min = a: b: if a < b then a else b;

  toHexPair =
    n:
    let
      clipped =
        if n > 255 then
          255
        else if n < 0 then
          0
        else
          n;
    in
    (builtins.elemAt decToHexList (clipped / 16)) + (builtins.elemAt decToHexList (mod clipped 16));

  rgbToHsl =
    {
      r,
      g,
      b,
    }:
    let
      # Normalize RGB to 0.0 - 1.0 floats to avoid integer truncation
      rF = r / 255.0;
      gF = g / 255.0;
      bF = b / 255.0;

      maxC = max rF (max gF bF);
      minC = min rF (min gF bF);
      delta = maxC - minC;

      l = (maxC + minC) / 2.0;

      s = if delta == 0.0 then 0.0 else delta / (1.0 - abs (2.0 * l - 1.0));

      # Standard 0-6 hue scaling
      hRaw =
        if delta == 0.0 then
          0.0
        else if maxC == rF then
          fmod (((gF - bF) / delta) + 6.0) 6.0
        else if maxC == gF then
          ((bF - rF) / delta) + 2.0
        else
          ((rF - gF) / delta) + 4.0;
    in
    {
      h = hRaw * 60.0; # Convert to 0-360 degrees
      s = s * 100.0;
      l = l * 100.0;
    };

  hslToRgb =
    {
      h,
      s,
      l,
    }:
    let
      sF = s / 100.0;
      lF = l / 100.0;

      c = (1.0 - abs (2.0 * lF - 1.0)) * sF;
      x = c * (1.0 - abs (fmod (h / 60.0) 2.0 - 1.0));
      m = lF - c / 2.0;

      res =
        if h < 60.0 then
          {
            r = c;
            g = x;
            b = 0.0;
          }
        else if h < 120.0 then
          {
            r = x;
            g = c;
            b = 0.0;
          }
        else if h < 180.0 then
          {
            r = 0.0;
            g = c;
            b = x;
          }
        else if h < 240.0 then
          {
            r = 0.0;
            g = x;
            b = c;
          }
        else if h < 300.0 then
          {
            r = x;
            g = 0.0;
            b = c;
          }
        else
          {
            r = c;
            g = 0.0;
            b = x;
          };
    in
    {
      # Re-scale to 0-255 and floor back to integers
      r = builtins.floor ((res.r + m) * 255.0);
      g = builtins.floor ((res.g + m) * 255.0);
      b = builtins.floor ((res.b + m) * 255.0);
    };

in
rec {
  toRGB =
    hex:
    let
      h = lib.removePrefix "#" hex;
    in
    {
      r = (hexToDecMap.${builtins.substring 0 1 h} * 16) + hexToDecMap.${builtins.substring 1 1 h};
      g = (hexToDecMap.${builtins.substring 2 1 h} * 16) + hexToDecMap.${builtins.substring 3 1 h};
      b = (hexToDecMap.${builtins.substring 4 1 h} * 16) + hexToDecMap.${builtins.substring 5 1 h};
    };

  toHex = rgb: "#" + (toHexPair rgb.r) + (toHexPair rgb.g) + (toHexPair rgb.b);

  mix =
    hex1: hex2: weight:
    let
      c1 = toRGB hex1;
      c2 = toRGB hex2;
      blend = a: b: builtins.floor (a * (1.0 - weight) + b * weight);
    in
    toHex {
      r = blend c1.r c2.r;
      g = blend c1.g c2.g;
      b = blend c1.b c2.b;
    };

  adjustSaturation =
    hex: amount:
    let
      hsl = rgbToHsl (toRGB hex);
      newS =
        let
          s = hsl.s + amount;
        in
        if s > 100.0 then
          100.0
        else if s < 0.0 then
          0.0
        else
          s;
    in
    toHex (hslToRgb (hsl // { s = newS; }));

  adjustLightness =
    hex: amount:
    let
      hsl = rgbToHsl (toRGB hex);
      newL =
        let
          l = hsl.l + amount;
        in
        if l > 100.0 then
          100.0
        else if l < 0.0 then
          0.0
        else
          l;
    in
    toHex (hslToRgb (hsl // { l = newL; }));

  toneSet =
    type: baseColor:
    let
      vibrant = adjustSaturation baseColor 10;
    in
    {
      fixed = if type == "light" then mix vibrant "#ffffff" 0.85 else mix vibrant "#ffffff" 0.75;
      fixed_dim = if type == "light" then mix vibrant "#ffffff" 0.70 else mix vibrant "#ffffff" 0.55;
      bright =
        if type == "dark" then
          adjustLightness (adjustSaturation baseColor 15) 15
        else
          adjustLightness (adjustSaturation baseColor 10) (-10);
    };
}
