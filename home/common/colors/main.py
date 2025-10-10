import os
from PIL import Image, ImageDraw, ImageFont
import re

def parse_nix_theme(file_path):
    theme_data = {
        "slug": "default",
        "palette": {}
    }
    current_palette_section = False
    with open(file_path, 'r') as f:
        for line in f:
            line = line.strip()
            if line.startswith('slug = "'):
                theme_data["slug"] = line.split('"')[1]
            elif line.startswith('palette = {'):
                current_palette_section = True
            elif line.startswith('};'):
                current_palette_section = False
            elif current_palette_section and '=' in line and not line.startswith('#'):
                try:
                    key, value = line.split('=', 1)
                    key = key.strip()
                    value = value.strip().replace(';', '')
                    if value.startswith('"') and value.endswith('"'):
                        theme_data["palette"][key] = value[1:-1]
                except ValueError:
                    pass
    return theme_data

def hex_to_rgb(hex_color):
    hex_color = hex_color.lstrip('#')
    return tuple(int(hex_color[i:i+2], 16) for i in (0, 2, 4))

def generate_theme_preview(theme_data, output_dir="thumbnails",
                           img_width=250, img_height=150,
                           top_padding_percent=0.05,
                           text_color_block_padding_percent=0.05,
                           max_colors_per_row=8):

    slug = theme_data.get("slug", "untitled_theme")
    palette = theme_data["palette"]

    bg_color_hex = palette.get("bg", "#181818")
    fg_color_hex = palette.get("fg", "#c1c1c1")

    bg_color_rgb = hex_to_rgb(bg_color_hex)
    fg_color_rgb = hex_to_rgb(fg_color_hex)

    img = Image.new('RGB', (img_width, img_height), color=bg_color_rgb)
    draw = ImageDraw.Draw(img)

    try:
        font_main_text = ImageFont.truetype("../../../pkgs/useful-fonts/SofiaPro.ttf", 28)
        font_highlight_text = ImageFont.truetype("../../../pkgs/useful-fonts/SofiaPro.ttf", 28)
    except IOError:
        print("Warning: Could not load SofiaPro.ttf font.")
        font_main_text = ImageFont.load_default()
        font_highlight_text = ImageFont.load_default()

    top_padding = int(img_height * top_padding_percent)
    text_color_block_padding = int(img_height * text_color_block_padding_percent)

    # BG // FG
    sample_text = "The quick brown fox jumps over the lazy dog"
    text_bbox = draw.textbbox((0,0), sample_text, font=font_main_text)
    text_width = text_bbox[2] - text_bbox[0]
    text_x = (img_width - text_width) // 2
    text_y = top_padding
    draw.text((text_x, text_y), sample_text, font=font_main_text, fill=fg_color_rgb)

    # BG // PER TYPE
    highlight_words = ["Op", "Num", "Type", "Func", "Str", "Keyword", "Var", "Comment"]
    highlight_color_keys = ["base05", "base09", "base0A", "base0D", "base0B", "base0E", "base08", "base03"]

    space_width = draw.textbbox((0,0), " ", font=font_highlight_text)[2]
    total_highlight_width = -space_width
    for word in highlight_words:
        word_bbox = draw.textbbox((0,0), word, font=font_highlight_text)
        total_highlight_width += (word_bbox[2] - word_bbox[0]) + space_width

    highlight_x_start = (img_width - total_highlight_width) // 2
    highlight_y = text_y + (text_bbox[3] - text_bbox[1]) + 15

    # ZIP (word1, color1, word2, color2)
    current_x_pos = highlight_x_start
    for word, color_key in zip(highlight_words, highlight_color_keys):
        hex_code = palette.get(color_key, fg_color_hex)
        rgb_color = hex_to_rgb(hex_code)
        
        draw.text((current_x_pos, highlight_y), word, font=font_highlight_text, fill=rgb_color)
        
        word_bbox = draw.textbbox((0,0), word, font=font_highlight_text)
        current_x_pos += (word_bbox[2] - word_bbox[0]) + space_width

    # Blocks
    highlight_bbox = draw.textbbox((0,0), "A", font=font_highlight_text)
    highlight_line_height = highlight_bbox[3] - highlight_bbox[1]
    blocks_start_y = highlight_y + highlight_line_height + text_color_block_padding
    
    colors_to_display = {k: v for k, v in palette.items() if k not in ["bg", "fg"]}
    sorted_color_keys = sorted(colors_to_display.keys())

    remaining_height = img_height - blocks_start_y
    if remaining_height <= 0:
        print(f"Warning: Not enough vertical space for color blocks in {slug}. Image height might be too small.")
        remaining_height = 1

    num_colors = len(sorted_color_keys)
    if num_colors == 0:
        print(f"No color was read, nothing generated for {slug}.")
        return

    ideal_colors_per_row = min(num_colors, MAX_COLORS_PER_ROW)
    box_width_attempt = img_width // ideal_colors_per_row if ideal_colors_per_row > 0 else img_width
    box_height_attempt = remaining_height // ((num_colors + ideal_colors_per_row - 1) // ideal_colors_per_row) if ideal_colors_per_row > 0 else remaining_height

    box_size = max(1, min(box_width_attempt, box_height_attempt))

    colors_per_row = max(1, img_width // box_size if box_size > 0 else 1)
    num_rows = (num_colors + colors_per_row - 1) // colors_per_row

    if colors_per_row > 0:
        box_size = img_width // colors_per_row

    current_x = 0
    current_y = blocks_start_y

    for i, color_name in enumerate(sorted_color_keys):
        hex_code = colors_to_display[color_name]
        if not hex_code.startswith("#"):
            hex_code = "#" + hex_code

        rgb = hex_to_rgb(hex_code)
        draw.rectangle([current_x, current_y, current_x + box_size, current_y + box_size], fill=rgb)
        current_x += box_size
        if (i + 1) % colors_per_row == 0:
            current_x = 0
            current_y += box_size

    os.makedirs(output_dir, exist_ok=True)
    output_path = os.path.join(output_dir, f"{slug}.png")
    img.save(output_path)
    print(f"Generated {output_path}")

if __name__ == "__main__":
    theme_folder = "./palettes"
    
    TARGET_IMG_WIDTH = 128*5
    TARGET_IMG_HEIGHT = 128*4
    TOP_PADDING_PERCENT = 0.25
    TEXT_COLOR_BLOCK_PADDING_PERCENT = 0.05
    MAX_COLORS_PER_ROW = 8

    for filename in os.listdir(theme_folder):
        if filename.startswith("default"):
            continue
        if filename.endswith(".nix"):
            file_path = os.path.join(theme_folder, filename)
            print(f"Processing {filename}...")
            theme_data = parse_nix_theme(file_path)
            generate_theme_preview(theme_data,
                                   img_width=TARGET_IMG_WIDTH,
                                   img_height=TARGET_IMG_HEIGHT,
                                   top_padding_percent=TOP_PADDING_PERCENT,
                                   text_color_block_padding_percent=TEXT_COLOR_BLOCK_PADDING_PERCENT,
                                   max_colors_per_row=MAX_COLORS_PER_ROW) # MODIFIED: Pass parameter

    print("Finished thumbnail generation.")
