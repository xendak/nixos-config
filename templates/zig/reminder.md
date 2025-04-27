# Reminders :)

## Inside shell:
zig build run

## Building for windows.
### Build Windows executable
nix build .#windows

### Result will be in:
file ./result/bin/*.exe
##### Example output: PE32+ executable (console) x86-64, for MS Windows

### Test with Wine (if installed)
wine ./result/bin/my-project.exe
