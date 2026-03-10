// Credits: Mellbourn
// https://github.com/Mellbourn/lsColorsToToml
// TODO: eventually rewrite this in nix
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const MODES = ["bold",
    "underline",
    "blink",
    "blink_rapid",
    "reversed",
    "hidden",
    "crossed"
];
const modes = {
    1: "bold",
    4: "underline",
    5: "blink",
    6: "blink_rapid",
    7: "reversed",
    8: "hidden",
    9: "crossed",
};
const fgColors = {
    30: "black",
    31: "red",
    32: "green",
    33: "yellow",
    34: "blue",
    35: "magenta",
    36: "cyan",
    37: "gray",
    90: "darkgray",
    91: "lightred",
    92: "lightgreen",
    93: "lightyellow",
    94: "lightblue",
    95: "lightmagenta",
    96: "lightcyan",
};
const bgColors = {
    40: "black",
    41: "red",
    42: "green",
    43: "yellow",
    44: "blue",
    45: "magenta",
    46: "cyan",
    47: "gray",
    100: "darkgray",
    101: "lightred",
    102: "lightgreen",
    103: "lightyellow",
    104: "lightblue",
    105: "lightmagenta",
    106: "lightcyan",
};
const lsColorsContent = process.argv[2] || process.env.LS_COLORS;

// Helper function to convert RGB to Hex
function rgbToHex(r, g, b) {
    return `#${[r, g, b].map((x) => x.toString(16).padStart(2, "0")).join("")}`;
}
// Generate the 256-color palette
function generate256ColorPalette() {
    const palette = {};
    // Generate the 6x6x6 color cube
    for (let r = 0; r < 6; r++) {
        for (let g = 0; g < 6; g++) {
            for (let b = 0; b < 6; b++) {
                const index = 16 + r * 36 + g * 6 + b;
                palette[index] = rgbToHex((r ? 55 : 0) + r * 40, (g ? 55 : 0) + g * 40, (b ? 55 : 0) + b * 40);
            }
        }
    }
    // Generate the grayscale spectrum
    for (let i = 0; i < 24; i++) {
        const shade = 8 + i * 10;
        const index = 232 + i;
        palette[index] = rgbToHex(shade, shade, shade);
    }
    return palette;
}
const palette = generate256ColorPalette();
// Function to convert ANSI code to hex, now more robust
function ansiCodeToHex(code) {
    let colors = {};
    if (!code) {
        return {}; // Return empty if no code provided
    }
    const parts = code.split(";").map(p => parseInt(p, 10));
    for (let i = 0; i < parts.length; i++) {
        // Check if the current part is '38', indicating a foreground color code
        if (parts[i] == 38) {
            // 5 indicates 256 color palette
            if (parts[i + 1] == 5) {
                const colorIndex = parts[i + 2];
                if (!isNaN(colorIndex)) {
                    // Ensure that the color index is a number
                    colors.fg = palette[colorIndex] || "#ffffff"; // Assign foreground color
                    i += 2; // Skip the next two parts as they have been processed
                }
            }
            // 2 indicates 24-bit RGB color
            else if (parts[i + 1] == 2) {
                const [r, g, b] = parts.slice(i + 2, i + 5);
                colors.fg = rgbToHex(r, g, b);
                i += 4;
            }
        }
        // Check if the current part is '48', indicating a background color code
        else if (parts[i] == 48) {
            // 5 indicates 256 color palette
            if (parts[i + 1] == 5) {
                const colorIndex = parts[i + 2];
                if (!isNaN(colorIndex)) {
                    // Ensure that the color index is a number
                    colors.bg = palette[colorIndex] || "#ffffff"; // Assign background color
                    i += 2; // Skip the next two parts as they have been processed
                }
            }
            // 2 indicates 24-bit RGB color
            else if (parts[i + 1] == 2) {
                const [r, g, b] = parts.slice(i + 2, i + 5);
                colors.bg = rgbToHex(r, g, b);
                i += 4;
            }
        }
        else if (parts[i] == 0) {
            // reset
            colors = {};
        }
        else if (parts[i] in modes) {
            const mode = modes[parts[i]];
            colors[mode] = true;
        }
        else if (parts[i] in fgColors) {
            const color = fgColors[parts[i]];
            colors.fg = color;
        }
        else if (parts[i] in bgColors) {
            const color = bgColors[parts[i]];
            colors.bg = color;
        }
    }
    return colors;
}
function lsPatternToYazi(lsColorsPattern) {
    if (lsColorsPattern.length < 3) {
        const patternMap = {
            di: { url: "*/" },
            bd: { url: "*", is: "block" },
            cd: { url: "*", is: "char" },
            ex: { url: "*", is: "exec" },
            pi: { url: "*", is: "fifo" },
            ln: { url: "*", is: "link" },
            or: { url: "*", is: "orphan" },
            so: { url: "*", is: "sock" },
            st: { url: "*", is: "sticky" }
        };
        const mappedPattern = patternMap[lsColorsPattern];
        if (mappedPattern) {
            return mappedPattern;
        }
        return { url: "" };
    }
    else {
        return { url: lsColorsPattern };
    }
}
// Parse LS_COLORS and convert to theme.toml content, now handling potential undefined codes
function convertLsColorsToToml(lsColors) {
    const entries = lsColors.split(":");
    const rules = entries
        .map((entry) => {
        const [pattern, codes] = entry.split("=", 2); // Ensure only the first '=' is used to split
        const { url, is } = lsPatternToYazi(pattern);
        if (!url)
            return "";
        const style = ansiCodeToHex(codes);
        const { fg, bg } = style;
        const ruleParts = [];
        ruleParts.push(`url = "${url}"`);
        if (is)
            ruleParts.push(`is = "${is}"`);
        if (fg)
            ruleParts.push(`fg = "${fg}"`);
        if (bg)
            ruleParts.push(`bg = "${bg}"`);
        for (let mode of MODES) {
            if (style[mode])
                ruleParts.push(`${mode} = true`);
        }
        // Only produce a rule if there's more than just the name
        if (ruleParts.length > 1) {
            return `  { ${ruleParts.join(", ")} }`;
        }
        else {
            return null;
        }
    })
        // Filter out entries without colors
        .filter((rule) => rule);
    return rules.join(",\n") + ",";
}
const themeTomlContent = convertLsColorsToToml(lsColorsContent);
console.log(themeTomlContent);
// Optionally, write to a file
// fs.writeFileSync('theme.toml', themeTomlContent);
//# sourceMappingURL=index.js.map
