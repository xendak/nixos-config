import os
import re
import math
from PIL import Image, ImageDraw, ImageFont

# ==========================================
# 1. NIX COLOR MATH PORT
# ==========================================

def hex_to_rgb(hex_color):
    h = hex_color.lstrip('#')
    if len(h) != 6:
        return (136, 136, 136)
    try:
        return tuple(int(h[i:i+2], 16) for i in (0, 2, 4))
    except ValueError:
        return (136, 136, 136)

def rgb_to_hex(rgb):
    return "#{:02x}{:02x}{:02x}".format(*rgb)

def rgb_to_hsl(r, g, b):
    r, g, b = r / 255.0, g / 255.0, b / 255.0
    max_c = max(r, g, b)
    min_c = min(r, g, b)
    delta = max_c - min_c
    l = (max_c + min_c) / 2.0
    if delta == 0:
        h = 0.0
        s = 0.0
    else:
        s = delta / (1.0 - abs(2.0 * l - 1.0))
        if max_c == r:
            h = ((g - b) / delta + 6.0) % 6.0
        elif max_c == g:
            h = (b - r) / delta + 2.0
        else:
            h = (r - g) / delta + 4.0
    return h * 60.0, s * 100.0, l * 100.0

def hsl_to_rgb(h, s, l):
    s /= 100.0
    l /= 100.0
    c = (1.0 - abs(2.0 * l - 1.0)) * s
    x = c * (1.0 - abs((h / 60.0) % 2.0 - 1.0))
    m = l - c / 2.0
    if h < 60: r, g, b = c, x, 0.0
    elif h < 120: r, g, b = x, c, 0.0
    elif h < 180: r, g, b = 0.0, c, x
    elif h < 240: r, g, b = 0.0, x, c
    elif h < 300: r, g, b = x, 0.0, c
    else: r, g, b = c, 0.0, x
    return (
        int(math.floor((r + m) * 255.0)),
        int(math.floor((g + m) * 255.0)),
        int(math.floor((b + m) * 255.0))
    )

def mix(hex1, hex2, weight):
    c1, c2 = hex_to_rgb(hex1), hex_to_rgb(hex2)
    r = int(math.floor(c1[0] * (1.0 - weight) + c2[0] * weight))
    g = int(math.floor(c1[1] * (1.0 - weight) + c2[1] * weight))
    b = int(math.floor(c1[2] * (1.0 - weight) + c2[2] * weight))
    return rgb_to_hex((r, g, b))

def adjust_saturation(hex_color, amount):
    r, g, b = hex_to_rgb(hex_color)
    h, s, l = rgb_to_hsl(r, g, b)
    s = max(0.0, min(100.0, s + amount))
    return rgb_to_hex(hsl_to_rgb(h, s, l))

def adjust_lightness(hex_color, amount):
    r, g, b = hex_to_rgb(hex_color)
    h, s, l = rgb_to_hsl(r, g, b)
    l = max(0.0, min(100.0, l + amount))
    return rgb_to_hex(hsl_to_rgb(h, s, l))

def tone_set(type_str, base_color):
    vibrant = adjust_saturation(base_color, 10)
    if type_str == "light":
        fixed = mix(vibrant, "#ffffff", 0.85)
        fixed_dim = mix(vibrant, "#ffffff", 0.70)
        bright = adjust_lightness(adjust_saturation(base_color, 10), -10)
    else:
        fixed = mix(vibrant, "#ffffff", 0.75)
        fixed_dim = mix(vibrant, "#ffffff", 0.55)
        bright = adjust_lightness(adjust_saturation(base_color, 15), 15)
    return {"fixed": fixed, "fixed_dim": fixed_dim, "bright": bright}


# ==========================================
# 2. FALLBACK M3 DEFAULTS
# ==========================================

def get_default_m3_specs(type_str):
    if type_str == "light":
        return {
            "primary": "#6750A4", "on_primary": "#FFFFFF", "primary_container": "#EADDFF", "on_primary_container": "#21005D",
            "inverse_primary": "#D0BCFF", "primary_fixed": "#EADDFF", "primary_fixed_dim": "#D0BCFF", "on_primary_fixed": "#21005D",
            "on_primary_fixed_variant": "#4F378B", "secondary": "#625B71", "on_secondary": "#FFFFFF", "secondary_container": "#E8DEF8",
            "on_secondary_container": "#1D192B", "secondary_fixed": "#E8DEF8", "secondary_fixed_dim": "#CCC2DC", "on_secondary_fixed": "#1D192B",
            "on_secondary_fixed_variant": "#4A4458", "tertiary": "#7D5260", "on_tertiary": "#FFFFFF", "tertiary_container": "#FFD8E4",
            "on_tertiary_container": "#31111D", "tertiary_fixed": "#FFD8E4", "tertiary_fixed_dim": "#EFB8C8", "on_tertiary_fixed": "#31111D",
            "on_tertiary_fixed_variant": "#633B48", "error": "#B3261E", "on_error": "#FFFFFF", "error_container": "#F9DEDC",
            "on_error_container": "#410E0B", "background": "#FFFBFE", "foreground": "#1C1B1F", "bg": "#FFFBFE", "fg": "#1C1B1F",
            "on_background": "#1C1B1F", "surface": "#FFFBFE", "on_surface": "#1C1B1F", "surface_variant": "#E7E0EC",
            "on_surface_variant": "#49454F", "surface_dim": "#DED8E1", "surface_bright": "#FFFBFE", "surface_container_lowest": "#FFFFFF",
            "surface_container_low": "#F7F2FA", "surface_container": "#F3EDF7", "surface_container_high": "#ECE6F0",
            "surface_container_highest": "#E6E0E9", "dim": "#808080", "outline": "#79747E", "outline_variant": "#CAC4D0",
            "inverse_surface": "#313033", "inverse_on_surface": "#F4EFF4", "surface_tint": "#6750A4", "shadow": "#000000",
            "scrim": "#000000", "source_color": "#6750A4",
            "white": "#fbf1c7", "red": "#cc241d", "green": "#98971a", "yellow": "#d79921", "blue": "#458588",
            "magenta": "#b16286", "cyan": "#689d6a", "gray": "#7c6f64", "orange": "#d65d0e", "black": "#3c3836",
            "brightwhite": "#928374", "brightred": "#9d0006", "brightgreen": "#79740e", "brightyellow": "#b57614",
            "brightblue": "#076678", "brightmagenta": "#8f3f71", "brightcyan": "#427b58", "brightgray": "#928374",
            "brightorange": "#af3a03", "brightblack": "#7c6f64",
        }
    else:
        return {
            "primary": "#D0BCFF", "on_primary": "#381E72", "primary_container": "#4F378B", "on_primary_container": "#EADDFF",
            "inverse_primary": "#6750A4", "primary_fixed": "#EADDFF", "primary_fixed_dim": "#D0BCFF", "on_primary_fixed": "#21005D",
            "on_primary_fixed_variant": "#4F378B", "secondary": "#CCC2DC", "on_secondary": "#332D41", "secondary_container": "#4A4458",
            "on_secondary_container": "#E8DEF8", "secondary_fixed": "#E8DEF8", "secondary_fixed_dim": "#CCC2DC", "on_secondary_fixed": "#1D192B",
            "on_secondary_fixed_variant": "#4A4458", "tertiary": "#EFB8C8", "on_tertiary": "#492532", "tertiary_container": "#633B48",
            "on_tertiary_container": "#FFD8E4", "tertiary_fixed": "#FFD8E4", "tertiary_fixed_dim": "#EFB8C8", "on_tertiary_fixed": "#31111D",
            "on_tertiary_fixed_variant": "#633B48", "error": "#F2B8B5", "on_error": "#601410", "error_container": "#8C1D18",
            "on_error_container": "#F9DEDC", "background": "#1C1B1F", "foreground": "#E6E1E5", "bg": "#1C1B1F", "fg": "#E6E1E5",
            "on_background": "#E6E1E5", "surface": "#1C1B1F", "on_surface": "#E6E1E5", "surface_variant": "#49454F",
            "on_surface_variant": "#CAC4D0", "surface_dim": "#141218", "surface_bright": "#3B383E", "surface_container_lowest": "#0F0D13",
            "surface_container_low": "#1D1B20", "surface_container": "#211F26", "surface_container_high": "#2B2930",
            "surface_container_highest": "#36343B", "dim": "#49454f", "outline": "#938F99", "outline_variant": "#49454F",
            "inverse_surface": "#E6E1E5", "inverse_on_surface": "#313033", "surface_tint": "#D0BCFF", "shadow": "#000000",
            "scrim": "#000000", "source_color": "#6750A4",
            "black": "#353535", "red": "#C4746E", "green": "#8A9A7B", "yellow": "#C4B28A", "blue": "#8BA4B0",
            "magenta": "#A292A3", "cyan": "#8EA4A2", "white": "#C8C093", "gray": "#a89984", "orange": "#d65d0e",
            "brightblack": "#454545", "brightred": "#E46876", "brightgreen": "#87A987", "brightyellow": "#E6C384",
            "brightblue": "#7FB4CA", "brightmagenta": "#938AA9", "brightcyan": "#7AA89F", "brightwhite": "#A6A69C",
            "brightgray": "#928374", "brightorange": "#fe8019",
        }


# ==========================================
# 3. IMPROVED NIX PARSER
# ==========================================

def strip_inline_comment(s):
    """Remove inline comments, respecting string literals."""
    in_string = False
    result = []
    for i, ch in enumerate(s):
        if ch == '"' and (i == 0 or s[i-1] != '\\'):
            in_string = not in_string
        if ch == '#' and not in_string:
            break
        result.append(ch)
    return ''.join(result)

def extract_block_balanced(content, key):
    """Extract a Nix attrset block using balanced brace matching (handles nested braces)."""
    pattern = rf'\b{key}\s*=\s*\{{'
    match = re.search(pattern, content)
    if not match:
        return {}

    start = match.end()  # position right after opening {
    depth = 1
    pos = start
    while pos < len(content) and depth > 0:
        if content[pos] == '{':
            depth += 1
        elif content[pos] == '}':
            depth -= 1
        pos += 1

    if depth != 0:
        print(f"  Warning: Unbalanced braces in '{key}' block")
        return {}

    block_content = content[start:pos - 1]

    raw = {}
    for line in block_content.split('\n'):
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        if '=' not in line:
            continue
        k, v = line.split('=', 1)
        k = k.strip()
        v = strip_inline_comment(v).rstrip(';').strip()
        if k:
            raw[k] = v

    return raw

def extract_let_bindings(content):
    """Extract simple let bindings (hex colors) from the top-level let block."""
    bindings = {}
    let_match = re.search(r'\blet\b(.*?)\bin\b', content, re.DOTALL)
    if not let_match:
        return bindings

    for line in let_match.group(1).split('\n'):
        line = line.strip()
        if '=' not in line or line.startswith('#'):
            continue
        k, v = line.split('=', 1)
        k = k.strip()
        v = v.strip()
        if '{' in v:
            continue  # Skip block definitions like must = { ... }
        v = strip_inline_comment(v).rstrip(';').strip().strip('"')
        if v.startswith('#') and len(v) == 7:
            try:
                int(v[1:], 16)
                bindings[k] = v
            except ValueError:
                pass
    return bindings

def tokenize_args(s):
    """Tokenize function arguments from a string."""
    tokens = []
    i = 0
    while i < len(s):
        while i < len(s) and s[i] in ' \t':
            i += 1
        if i >= len(s):
            break

        if s[i] == '"':
            j = i + 1
            while j < len(s) and s[j] != '"':
                j += 1
            tokens.append(('string', s[i+1:j]))
            i = j + 1
        elif s[i] == '-' or s[i].isdigit():
            j = i + 1 if s[i] == '-' else i
            while j < len(s) and (s[j].isdigit() or s[j] == '.'):
                j += 1
            tokens.append(('number', s[i:j]))
            i = j
        elif s[i].isalpha() or s[i] == '_':
            j = i
            while j < len(s) and (s[j].isalnum() or s[j] in '._'):
                j += 1
            tokens.append(('ident', s[i:j]))
            i = j
        else:
            i += 1  # skip parens, commas, etc.

    return tokens

def _resolve_ident(tok_val, let_bindings, must_res, ov_res, pal_res):
    """Resolve an identifier token to a hex color, or return None."""
    if '.' in tok_val:
        parts = tok_val.split('.')
        ref_block = parts[0]
        ref_key = parts[-1]
        if ref_block == 'must' and ref_key in must_res:
            return must_res[ref_key]
        if ref_block == 'overrides' and ref_key in ov_res:
            return ov_res[ref_key]
        if ref_block == 'palette' and ref_key in pal_res:
            return pal_res[ref_key]
        return None
    # bare variable name
    if tok_val in let_bindings:
        return let_bindings[tok_val]
    if tok_val in must_res:
        return must_res[tok_val]
    if tok_val in ov_res:
        return ov_res[tok_val]
    if tok_val in pal_res:
        return pal_res[tok_val]
    return None

def try_resolve(raw_val, let_bindings, must_res, ov_res, pal_res):
    """Try to resolve a raw value string to a hex color."""
    if raw_val is None:
        return None

    v = raw_val.strip()

    # Handle let ... in ... expressions (strip the binding, keep the body)
    if v.startswith('let '):
        in_match = re.search(r'\bin\b', v)
        if in_match:
            v = v[in_match.end():].strip().rstrip(';').strip()

    # Strip surrounding quotes
    if v.startswith('"') and v.endswith('"'):
        v = v[1:-1]

    # Direct hex color
    if v.startswith('#') and len(v) == 7:
        try:
            int(v[1:], 16)
            return v
        except ValueError:
            return None

    # Reference like must.fg, overrides.red, palette.primary
    ref_match = re.match(r'^(\w+)\.(\w+)$', v)
    if ref_match:
        result = _resolve_ident(v, let_bindings, must_res, ov_res, pal_res)
        return result

    # Simple variable reference (let binding name)
    if re.match(r'^\w+$', v):
        result = _resolve_ident(v, let_bindings, must_res, ov_res, pal_res)
        return result

    # Function calls: mix, adjustLightness, adjustSaturation (with optional colorLib. prefix)
    func_match = re.match(r'^(?:colorLib\.)?(mix|adjustLightness|adjustSaturation)\s+(.+)$', v)
    if func_match:
        func_name = func_match.group(1)
        args_str = func_match.group(2)
        tokens = tokenize_args(args_str)
        args = []
        for tok_type, tok_val in tokens:
            if tok_type == 'string':
                val = tok_val.strip('"')
                if val.startswith('#') and len(val) == 7:
                    try:
                        int(val[1:], 16)
                        args.append(val)
                    except ValueError:
                        return None
                else:
                    return None
            elif tok_type == 'number':
                args.append(tok_val)
            elif tok_type == 'ident':
                resolved = _resolve_ident(tok_val, let_bindings, must_res, ov_res, pal_res)
                if resolved is None:
                    return None
                args.append(resolved)
            else:
                return None

        try:
            if func_name == 'mix' and len(args) >= 3:
                return mix(args[0], args[1], float(args[2]))
            elif func_name == 'adjustLightness' and len(args) >= 2:
                return adjust_lightness(args[0], float(args[1]))
            elif func_name == 'adjustSaturation' and len(args) >= 2:
                return adjust_saturation(args[0], float(args[1]))
        except (ValueError, TypeError):
            return None

    return None

def parse_nix_file(file_path):
    with open(file_path, 'r') as f:
        content = f.read()

    def get_val(key):
        match = re.search(rf'{key}\s*=\s*"([^"]+)"\s*;', content)
        if match:
            return match.group(1)
        match = re.search(rf'{key}\s*=\s*(true|false)\s*;', content)
        if match:
            return match.group(1) == "true"
        return None

    let_bindings = extract_let_bindings(content)
    must_raw = extract_block_balanced(content, 'must')
    overrides_raw = extract_block_balanced(content, 'overrides')
    palette_raw = extract_block_balanced(content, 'palette')

    must_res = {}
    ov_res = {}
    pal_res = {}

    # Iterative resolution: repeat until no more values can be resolved
    changed = True
    iterations = 0
    while changed and iterations < 50:
        changed = False
        iterations += 1

        for k, v in must_raw.items():
            if k not in must_res:
                result = try_resolve(v, let_bindings, must_res, ov_res, pal_res)
                if result:
                    must_res[k] = result
                    changed = True

        for k, v in overrides_raw.items():
            if k not in ov_res:
                result = try_resolve(v, let_bindings, must_res, ov_res, pal_res)
                if result:
                    ov_res[k] = result
                    changed = True

        for k, v in palette_raw.items():
            if k not in pal_res:
                result = try_resolve(v, let_bindings, must_res, ov_res, pal_res)
                if result:
                    pal_res[k] = result
                    changed = True

    # Warn about unresolved values
    for k, v in must_raw.items():
        if k not in must_res:
            print(f"  Warning: Could not resolve must.{k} = '{v}' in {os.path.basename(file_path)}")
    for k, v in overrides_raw.items():
        if k not in ov_res:
            print(f"  Warning: Could not resolve overrides.{k} = '{v}' in {os.path.basename(file_path)}")
    for k, v in palette_raw.items():
        if k not in pal_res:
            print(f"  Warning: Could not resolve palette.{k} = '{v}' in {os.path.basename(file_path)}")

    return {
        "slug": get_val("slug") or "untitled",
        "type": get_val("type") or "dark",
        "isLazy": get_val("isLazy") if get_val("isLazy") is not None else False,
        "must": must_res,
        "overrides": ov_res,
        "palette": pal_res,
    }


# ==========================================
# 4. THEME PROCESSING LOGIC
# ==========================================

def derive_ansi(type_str, m3, extra):
    base_ansi = {
        "white": extra.get("white", m3["on_surface"]),
        "gray": extra.get("gray", m3["outline"]),
        "red": extra.get("red", m3["error"]),
        "green": extra.get("green", m3["primary"]),
        "yellow": extra.get("yellow", m3["tertiary"]),
        "blue": extra.get("blue", m3["secondary"]),
        "magenta": extra.get("magenta", m3["tertiary"]),
        "cyan": extra.get("cyan", m3["primary_container"]),
        "orange": extra.get("orange", m3["tertiary"]),
        "black": extra.get("black", m3["surface"]),
    }

    bright_ansi = {
        "brightwhite": extra.get("brightwhite", tone_set(type_str, base_ansi["white"])["bright"]),
        "brightgray": extra.get("brightgray", tone_set(type_str, base_ansi["gray"])["bright"]),
        "brightred": extra.get("brightred", tone_set(type_str, base_ansi["red"])["bright"]),
        "brightgreen": extra.get("brightgreen", tone_set(type_str, base_ansi["green"])["bright"]),
        "brightyellow": extra.get("brightyellow", tone_set(type_str, base_ansi["yellow"])["bright"]),
        "brightblue": extra.get("brightblue", tone_set(type_str, base_ansi["blue"])["bright"]),
        "brightmagenta": extra.get("brightmagenta", tone_set(type_str, base_ansi["magenta"])["bright"]),
        "brightcyan": extra.get("brightcyan", tone_set(type_str, base_ansi["cyan"])["bright"]),
        "brightorange": extra.get("brightorange", tone_set(type_str, base_ansi["orange"])["bright"]),
        "brightblack": extra.get("brightblack", tone_set(type_str, base_ansi["black"])["bright"]),
    }

    syntax_ansi = {
        "keywords": extra.get("keywords", base_ansi["red"]),
        "labels": extra.get("labels", bright_ansi["brightred"]),
        "strings": extra.get("strings", base_ansi["green"]),
        "alt_functions": extra.get("alt_functions", bright_ansi["brightgreen"]),
        "builtins": extra.get("builtins", base_ansi["yellow"]),
        "types": extra.get("types", bright_ansi["brightyellow"]),
        "functions": extra.get("functions", base_ansi["blue"]),
        "macros": extra.get("macros", bright_ansi["brightblue"]),
        "specials": extra.get("specials", base_ansi["magenta"]),
        "constants": extra.get("constants", bright_ansi["brightmagenta"]),
        "modules": extra.get("modules", base_ansi["cyan"]),
        "tags": extra.get("tags", bright_ansi["brightcyan"]),
        "numeric": extra.get("numeric", base_ansi["orange"]),
        "punctuation": extra.get("punctuation", bright_ansi["brightorange"]),
        "comments": extra.get("comments", base_ansi["gray"]),
        "inlay": extra.get("inlay", base_ansi["gray"]),
    }

    return {**base_ansi, **bright_ansi, **syntax_ansi}

def process_theme(raw):
    type_str = raw["type"]
    must = dict(raw["must"])  # copy so we don't mutate
    overrides = {k: v for k, v in raw["overrides"].items() if v is not None}
    palette = raw.get("palette", {})

    if not raw["isLazy"]:
        # --- Non-lazy: derive everything from must ---
        # Fill missing must keys from M3 defaults so we don't crash
        defaults = get_default_m3_specs(type_str)
        required = ["primary", "secondary", "tertiary", "error", "bg", "fg",
                     "dim", "surface_container", "surface_container_high"]
        for key in required:
            if key not in must:
                print(f"  Warning: must.{key} missing, falling back to default {defaults.get(key, '#888888')}")
                must[key] = defaults.get(key, "#888888")

        fixed_variants = {}
        for prefix in ["primary", "secondary", "tertiary"]:
            tones = tone_set(type_str, must[prefix])
            fixed_variants[f"{prefix}_fixed"] = overrides.get(f"{prefix}_fixed", tones["fixed"])
            fixed_variants[f"{prefix}_fixed_dim"] = overrides.get(f"{prefix}_fixed_dim", tones["fixed_dim"])

        m3_derived = {
            "background": must["bg"], "on_background": must["fg"],
            "surface": must["bg"], "on_surface": must["fg"],
            "foreground": must["fg"],
            "surface_variant": must["surface_container_high"],
            "on_surface_variant": must["fg"],
            "surface_dim": must["bg"],
            "surface_bright": must["surface_container_high"],
            "surface_container_lowest": must["bg"],
            "surface_container_low": must["surface_container"],
            "surface_container": must["surface_container"],
            "surface_container_high": must["surface_container_high"],
            "surface_container_highest": must["surface_container_high"],

            "primary": must["primary"], "on_primary": must["bg"],
            "primary_container": must["surface_container_high"],
            "on_primary_container": must["primary"],
            "inverse_primary": must["bg"],
            "primary_fixed": must["primary"],
            "primary_fixed_dim": must["primary"],
            "on_primary_fixed": must["bg"],
            "on_primary_fixed_variant": must["surface_container_high"],

            "secondary": must["secondary"], "on_secondary": must["bg"],
            "secondary_container": must["surface_container"],
            "on_secondary_container": must["secondary"],
            "secondary_fixed": must["secondary"],
            "secondary_fixed_dim": must["secondary"],
            "on_secondary_fixed": must["bg"],
            "on_secondary_fixed_variant": must["surface_container"],

            "tertiary": must["tertiary"], "on_tertiary": must["bg"],
            "tertiary_container": must["surface_container"],
            "on_tertiary_container": must["tertiary"],
            "tertiary_fixed": must["tertiary"],
            "tertiary_fixed_dim": must["tertiary"],
            "on_tertiary_fixed": must["bg"],
            "on_tertiary_fixed_variant": must["surface_container"],

            "error": must["error"], "on_error": must["bg"],
            "error_container": must["surface_container"],
            "on_error_container": must["error"],
            "outline": must["dim"],
            "outline_variant": must["surface_container_high"],
            "inverse_surface": must["fg"],
            "inverse_on_surface": must["bg"],
            "shadow": "#000000", "scrim": "#000000",
            "surface_tint": must["primary"],
            "source_color": must["primary"],
        }

        m3 = {**m3_derived, **fixed_variants, **overrides}
        ansi = derive_ansi(type_str, m3, overrides)

        return {
            **m3, **ansi,
            "dim": must["dim"], "bg": must["bg"], "fg": must["fg"],
            "cursor_bg": must.get("cursor_bg", must["primary"]),
            "cursor_fg": must.get("cursor_fg", must["bg"]),
            "selection_bg": overrides.get("selection_bg", m3["surface_container_high"]),
            "selection_fg": overrides.get("selection_fg", m3["on_surface_variant"]),
        }
    else:
        # --- Lazy: use defaults, override with palette and overrides ---
        base_spec = get_default_m3_specs(type_str)
        m3 = {**base_spec, **palette, **overrides}
        extra = {**palette, **overrides}
        ansi = derive_ansi(type_str, m3, extra)

        return {
            **m3, **ansi,
            "dim": m3["dim"],
            "cursor_bg": m3.get("cursor_bg", m3["primary"]),
            "cursor_fg": m3.get("cursor_fg", m3["on_primary"]),
            "selection_bg": overrides.get("selection_bg", m3["surface_container_high"]),
            "selection_fg": overrides.get("selection_fg", m3["on_surface_variant"]),
        }


# ==========================================
# 5. IMAGE GENERATION
# ==========================================

def rrect(draw, xy, radius, fill=None, outline=None, width=1):
    try:
        draw.rounded_rectangle(xy, radius=radius, fill=fill, outline=outline, width=width)
    except AttributeError:
        draw.rectangle(xy, fill=fill, outline=outline, width=width)

def text_contrast(rgb):
    return (0, 0, 0) if 0.299 * rgb[0] + 0.587 * rgb[1] + 0.114 * rgb[2] > 140 else (255, 255, 255)

def center_text(draw, x, y, w, h, text, font, fill):
    bb = draw.textbbox((0, 0), text, font=font)
    tw, th = bb[2] - bb[0], bb[3] - bb[1]
    draw.text((x + (w - tw) // 2, y + (h - th) // 2 - 1), text, font=font, fill=fill)

def generate_theme_preview(palette, slug, output_dir="thumbnails", img_width=640, img_height=512):
    def c(key, fallback="#888888"):
        return hex_to_rgb(palette.get(key, fallback))

    fp = "../../../pkgs/useful-fonts/SofiaPro.ttf"
    try:
        ft_title = ImageFont.truetype(fp, 26)
        ft_body  = ImageFont.truetype(fp, 18)
        ft_chip  = ImageFont.truetype(fp, 16)
        ft_label = ImageFont.truetype(fp, 14)
        ft_tiny  = ImageFont.truetype(fp, 10)
        ft_code  = ImageFont.truetype(fp, 15)
    except IOError:
        print(f"Warning: {fp} not found, using default font")
        d = ImageFont.load_default()
        ft_title = ft_body = ft_chip = ft_label = ft_tiny = ft_code = d

    img = Image.new("RGB", (img_width, img_height), color=c("bg"))
    draw = ImageDraw.Draw(img)

    P = 20
    W = img_width - 2 * P
    y = P

    # Title & Sample
    draw.text((P, y), slug, font=ft_title, fill=c("fg"))
    y += 32
    draw.text((P, y), "The quick brown fox jumps over the lazy dog", font=ft_body, fill=c("fg"))
    y += 26

    # Material
    draw.text((P, y), "MATERIAL", font=ft_tiny, fill=c("dim"))
    y += 14
    mat = [("Primary", "primary", "primary_container"), ("Secondary", "secondary", "secondary_container"),
           ("Tertiary", "tertiary", "tertiary_container"), ("Error", "on_error", "error")]
    n, gap, hc, rc = len(mat), 8, 48, 10
    wc = (W - (n - 1) * gap) // n
    for i, (label, fg_k, bg_k) in enumerate(mat):
        x = P + i * (wc + gap)
        rrect(draw, [x, y, x + wc, y + hc], rc, fill=c(bg_k))
        center_text(draw, x, y, wc, hc, label, ft_chip, c(fg_k))
    y += hc + 12

    # Surfaces
    draw.text((P, y), "SURFACES", font=ft_tiny, fill=c("dim"))
    y += 14
    surfs = [("Low", "surface_container_low"), ("Mid", "surface_container"), ("High", "surface_container_high"), ("Dim", "dim")]
    ns, gs, hs, rs = len(surfs), 8, 40, 8
    ws = (W - (ns - 1) * gs) // ns
    ol = c("outline")
    for i, (label, key) in enumerate(surfs):
        x = P + i * (ws + gs)
        rrect(draw, [x, y, x + ws, y + hs], rs, fill=c(key), outline=ol, width=1)
        center_text(draw, x, y, ws, hs, label, ft_label, c("fg"))
    y += hs + 12

    # ANSI
    draw.text((P, y), "ANSI", font=ft_tiny, fill=c("dim"))
    y += 14
    ansi_n = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white", "gray", "orange"]
    ansi_b = ["brightblack", "brightred", "brightgreen", "brightyellow", "brightblue", "brightmagenta", "brightcyan", "brightwhite", "brightgray", "brightorange"]
    lbl_n = ["blk", "red", "grn", "ylw", "blu", "mag", "cyn", "wht", "gry", "org"]
    lbl_b = ["Bblk", "Bred", "Bgrn", "Bylw", "Bblu", "Bmag", "Bcyn", "Bwht", "Bgry", "Borg"]

    na = len(ansi_n)
    ga = 4
    ha = 40
    wa = (W - (na - 1) * ga) // na
    ra = 6

    for colors, labels in [(ansi_n, lbl_n), (ansi_b, lbl_b)]:
        for i, (ck, lb) in enumerate(zip(colors, labels)):
            x = P + i * (wa + ga)
            rgb = c(ck)
            rrect(draw, [x, y, x + wa, y + ha], ra, fill=rgb)
            center_text(draw, x, y, wa, ha, lb, ft_tiny, text_contrast(rgb))
        y += ha + ga
    y += 6

    # Code
    draw.text((P, y), "CODE", font=ft_tiny, fill=c("dim"))
    y += 14
    code_h, code_p = 92, 10
    rrect(draw, [P, y, P + W, y + code_h], 10, fill=c("surface_container"))

    code_lines = [
        [("fn ", "keywords"), ("main", "functions"), ("() {", "punctuation")],
        [("  ", "fg"), ("let ", "keywords"), ("x ", "types"), ("= ", "punctuation"), ("42", "numeric")],
        [("  ", "fg"), ("print", "builtins"), ('("hello")', "strings")],
        [("  ", "fg"), ("// comment", "comments")],
        [("}", "punctuation")],
    ]
    cy = y + code_p
    for parts in code_lines:
        cx = P + code_p
        for text, ck in parts:
            draw.text((cx, cy), text, font=ft_code, fill=c(ck))
            cx += draw.textbbox((0, 0), text, font=ft_code)[2]
        cy += 15
    y += code_h + 12

    # Cursor
    draw.text((P, y), "CURSOR", font=ft_tiny, fill=c("dim"))
    y += 14
    cs, cg, cr = 30, 10, 7
    bar_w, bar_m = 4, 7
    x = P
    rrect(draw, [x, y, x + cs, y + cs], cr, fill=c("cursor_bg"))
    draw.rectangle([x + bar_m, y + bar_m, x + bar_m + bar_w, y + cs - bar_m], fill=c("cursor_fg"))
    x += cs + cg
    rrect(draw, [x, y, x + cs, y + cs], cr, fill=c("cursor_fg"))
    draw.rectangle([x + bar_m, y + bar_m, x + bar_m + bar_w, y + cs - bar_m], fill=c("cursor_bg"))
    x += cs + cg
    draw.text((x, y + (cs - 11) // 2), "bg \u00b7 fg", font=ft_tiny, fill=c("dim"))

    os.makedirs(output_dir, exist_ok=True)
    out = os.path.join(output_dir, f"{slug}.png")
    img.save(out)
    print(f"Generated {out}")


# ==========================================
# 6. MAIN
# ==========================================

if __name__ == "__main__":
    theme_folder = "./palettes"
    W, H = 128 * 5, 128 * 4

    for fn in sorted(os.listdir(theme_folder)):
        if fn.startswith("default") or fn.startswith("colors") or fn.startswith("theme") or not fn.endswith(".nix"):
            continue
        path = os.path.join(theme_folder, fn)
        print(f"Processing {fn}...")

        raw_data = parse_nix_file(path)
        final_palette = process_theme(raw_data)

        generate_theme_preview(final_palette, raw_data["slug"], img_width=W, img_height=H)

    print("Finished thumbnail generation.")
