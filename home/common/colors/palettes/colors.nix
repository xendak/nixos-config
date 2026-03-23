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
  mod = base: int: base - (int * (base / int));
  abs = x: if x < 0 then -x else x;
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
      maxC = max r (max g b);
      minC = min r (min g b);
      delta = maxC - minC;
      l = (maxC + minC) / 2;

      s = if delta == 0 then 0 else (delta * 100) / (255 - abs (2 * l - 255));

      h100 =
        if delta == 0 then
          0
        else if maxC == r then
          let
            raw = ((g - b) * 10000) / delta;
            wrapped = mod raw 600;
          in
          if wrapped < 0 then wrapped + 600 else wrapped
        else if maxC == g then
          (((b - r) * 10000) / delta) + 20000
        else
          (((r - g) * 10000) / delta) + 40000;
    in
    {
      h = h100 / 100;
      s = s;
      l = (l * 100) / 255;
    };

  hslToRgb =
    {
      h,
      s,
      l,
    }:
    let
      c = ((100 - abs (2 * l - 100)) * s) / 100;
      h60 = (h * 100) / 60;
      x = (c * (100 - abs (mod h60 200 - 100))) / 100;
      m = l - (c / 2);
      res =
        if h < 60 then
          {
            r = c;
            g = x;
            b = 0;
          }
        else if h < 120 then
          {
            r = x;
            g = c;
            b = 0;
          }
        else if h < 180 then
          {
            r = 0;
            g = c;
            b = x;
          }
        else if h < 240 then
          {
            r = 0;
            g = x;
            b = c;
          }
        else if h < 300 then
          {
            r = x;
            g = 0;
            b = c;
          }
        else
          {
            r = c;
            g = 0;
            b = x;
          };
    in
    {
      r = builtins.floor ((res.r + m) * 255 / 100);
      g = builtins.floor ((res.g + m) * 255 / 100);
      b = builtins.floor ((res.b + m) * 255 / 100);
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

  toKdeRgb =
    hex:
    let
      c = toRGB hex;
    in
    "${toString c.r},${toString c.g},${toString c.b}";

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
        if s > 100 then
          100
        else if s < 0 then
          0
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
        if l > 100 then
          100
        else if l < 0 then
          0
        else
          l;
    in
    toHex (hslToRgb (hsl // { l = newL; }));

  # Produce a tonal set for Material-style roles.
  # Returns: { fixed, fixed_dim, bright }
  #
  # `type` is "light" or "dark" — controls which direction "bright" goes.
  # `baseColor` is any hex string, e.g. "#bf616a"
  # `output` is
  # fixed:     #f4d0d2
  # fixed_dim: #ebb5b8
  # bright:    #d0707a
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
