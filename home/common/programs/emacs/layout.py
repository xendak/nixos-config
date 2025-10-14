#!/usr/bin/env python3
"""
Layout Converter
Converts FROM_LAYOUT to TO_LAYOUT
"""

import re
import sys

FROM_LAYOUT   = "qwertyuiop" + "asdfghjkl;" + "zxcvbnm,./"
# TO_LAYOUT     = "poiuytrewq" + ";lkjhgfdsa" + "/.,mnbvcxz"
TO_LAYOUT     = "bflkv`wou." + "nshtmcdaei" + "pxjyq,g;_z"

# Build translation table
FROM_CHARS = FROM_LAYOUT + FROM_LAYOUT.upper()
TO_CHARS = TO_LAYOUT + TO_LAYOUT.upper()

def create_translation_map():
    """Create bidirectional character mapping"""
    trans_map = {}
    for i, char in enumerate(FROM_CHARS):
        if i < len(TO_CHARS):
            trans_map[char] = TO_CHARS[i]
    return trans_map

TRANS_MAP = create_translation_map()

def convert_key_binding(match):
    """Convert a key binding found in quotes"""
    full_match = match.group(0)
    key = match.group(1)
    
    if len(key) > 1 and (key.startswith('<') or key.isupper() or key == 'SPC'):
        return full_match
    
    if len(key) == 1 and key in TRANS_MAP:
        return f'("{TRANS_MAP[key]}"'
    
    return full_match

def convert_char_prefix(match):
    """Convert character with ?prefix (like ?f, ?d)"""
    prefix = match.group(1)
    char = match.group(2)
    
    if char in TRANS_MAP:
        return f'{prefix}{TRANS_MAP[char]}'
    
    return match.group(0)

def convert_file(input_file, output_file):
    """Convert the entire configuration file"""
    try:
        with open(input_file, 'r') as f:
            content = f.read()
        
        # Track prefixed bindings for warnings
        prefix_pattern = r"'(\(\"[;/]\w+\")"
        prefixed_bindings = re.findall(prefix_pattern, content)
        
        # meow-normal-commands
        content = re.sub(r'\("([^"]+)"', convert_key_binding, content)
        
        # meow-char-thing-table
        content = re.sub(r'(\?)([\w;,./])', convert_char_prefix, content)
        
        # general kbs
        content = re.sub(r'\(kbd "([^"]+)"\)', lambda m: f'(kbd "{convert_kbd_string(m.group(1))}")', content)
        
        with open(output_file, 'w') as f:
            f.write(content)
        
        print(f"{input_file} → {output_file}")
        
        if prefixed_bindings:
            print("\nThe following prefixed bindings were found.")
            print("Please manually verify these are correct:")
            for binding in set(prefixed_bindings):
                print(f"  {binding}")
        
        print("\nCharacter Mapping Reference:")
        print(f"FROM: {FROM_LAYOUT}")
        print(f"TO  : {TO_LAYOUT}")
        
    except FileNotFoundError:
        print(f"Error: File '{input_file}' not found")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

def convert_kbd_string(kbd_str):
    """Convert a kbd string character by character"""
    result = ""
    for char in kbd_str:
        if char in TRANS_MAP:
            result += TRANS_MAP[char]
        else:
            result += char
    return result

def main():
    if len(sys.argv) < 2:
        print("Usage: layout.py <input_file> [output_file]")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else "converted.el"
    
    convert_file(input_file, output_file)

if __name__ == "__main__":
    main()
