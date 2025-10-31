https://tsawyer87.github.io/posts/using_nushell_with_nixos/
super+Shift+e { spawn  "foot" "-e" "-a" "f_terminal" "fish" "-c" "yazi"; }

syncthing
https://127.0.0.1:8384/#

## nushell learning
 reference:
[nushell](https://www.nushell.sh/book/coming_from_bash.html)

### generating nixpkgs
nix search nixpkgs ^ --json | jq -r 'keys[]' | sed 's/legacyPackages\.x86_64-linux\.//' | sort > $HOME/Flake/bin/nixpkgs_list

### search offline
nix search --offline nixpkgs

### yt-dlp
quick yt-dlp fix for chromes >_>
nix run 'github:NixOS/nixpkgs/master#yt-dlp'

### send files via lan
localsend


### repl
```nix
nix repl
Nix 2.28.4
Type :? for help.
nix-repl> :lf .
warning: input 'nixpkgs-dprint-fix' has an override for a non-existent input 'nixpkgs'
warning: input 'nixpkgs-git-fix' has an override for a non-existent input 'nixpkgs'
Added 19 variables.

nix-repl> inputs.nixpkgs.legacyPackages.
inputs.nixpkgs.legacyPackages.aarch64-darwin     inputs.nixpkgs.legacyPackages.riscv64-linux
inputs.nixpkgs.legacyPackages.aarch64-linux      inputs.nixpkgs.legacyPackages.x86_64-darwin
inputs.nixpkgs.legacyPackages.armv6l-linux       inputs.nixpkgs.legacyPackages.x86_64-freebsd
inputs.nixpkgs.legacyPackages.armv7l-linux       inputs.nixpkgs.legacyPackages.x86_64-linux
inputs.nixpkgs.legacyPackages.i686-linux
inputs.nixpkgs.legacyPackages.powerpc64le-linux
nix-repl> inputs.nixpkgs.legacyPackages.<system>
error: syntax error, unexpected SPATH, expecting ID or OR_KW or DOLLAR_CURLY or '"'
       at «string»:1:31:
            1| inputs.nixpkgs.legacyPackages.<system>
             |                               ^

nix-repl> inputs.nixpkgs.legacyPackages.<system>.helix
error: syntax error, unexpected SPATH, expecting ID or OR_KW or DOLLAR_CURLY or '"'
       at «string»:1:31:
            1| inputs.nixpkgs.legacyPackages.<system>.helix
             |                               ^

nix-repl> inputs.nixpkgs.legacyPackages.<system>.helix.version
error: syntax error, unexpected SPATH, expecting ID or OR_KW or DOLLAR_CURLY or '"'
       at «string»:1:31:
            1| inputs.nixpkgs.legacyPackages.<system>.helix.version
             |                               ^

nix-repl> inputs.nixpkgs.legacyPackages.x86_64-linux.helix.version
"25.07.1"

nix-repl> inputs.nixpkgs.legacyPackages.x86_64-linux.wezterm
«derivation /nix/store/r9pjk9vdg54kgf3jqfsx514d6gbsmhg3-wezterm-0-unstable-2025-07-30.drv»

nix-repl> inputs.nixpkgs.legacyPackages.x86_64-linux.wezterm.version
"0-unstable-2025-07-30"

nix-repl> inputs.nixpkgs-stable.legacyPackages.x86_64-linux.wezterm.version
"0-unstable-2025-01-03"

nix-repl> inputs.nixpkgs-stable.legacyPackages.x86_64-linux.helix.version
"25.01.1"

nix-repl> inputs.nixpkgs-stable.legacyPackages.x86_64-linux.zig.version
"0.13.0"

nix-repl> inputs.nixpkgs.legacyPackages.x86_64-linux.zig.version
"0.14.1"

nix-repl> inputs.nixpkgs.legacyPackages.x86_64-linux.zls.version
"0.14.0"
```
### finding files
```nushell
ls **/*.nix | where name =~ "browser"
```
### operation on each file
```nushell
ls **/*.nix | each { $in.name }
```
### more than one option on where
```nushell
where { |p| ($p =~ "scoop|cmd" ) }
```
### captures stdout/stderr completely
```nushell
^/mnt/c/Windows/System32/cmd.exe /c "echo %PATH%" o+e>| lines | split row ";"
```

## flake update
- do changes on branch flake_update
- git push
- select flake_update (lazygit)
- rebase onto main
- select main
- hover flake_update
- Merge.



### 1. Switch to your `flake_update` branch
```bash
git checkout flake_update
```

### 2. Pull changes from `main` branch
```bash
git pull origin main
# or if working locally:
git merge main
```

### 3. Update flake inputs
```bash
nix flake update
```

### 4. Test the updates
Build and test your system with the updated flake:
```bash
sudo nixos-rebuild switch --flake .#<your-hostname>
# or for home-manager:
home-manager switch --flake .#<your-username>@<your-hostname>
```

### 5. Commit the flake updates
```bash
git add flake.lock
git commit -m "flake: update inputs"
```

### 6. Push to your `flake_update` branch
```bash
git push origin flake_update
```

### 7. If everything works well, merge back to `main`
```bash
git checkout main
git merge flake_update
git push origin main
```

### Optional: Create a PR instead of direct merge
If you prefer GitHub/GitLab workflow:
```bash
git push origin flake_update
# Then create a Pull/Merge Request from flake_update to main
```

### Additional tips:
1. **Test thoroughly** before merging to main
2. **Review changes** with `git diff main..flake_update`
3. **Consider rebasing** instead of merging if you prefer linear history:
   ```bash
   git checkout flake_update
   git rebase main
   ```
4. **Clean up** after successful merge:
   ```bash
   git branch -d flake_update  # delete local branch
   git push origin --delete flake_update  # delete remote branch
   ```

```bash
   #!/usr/bin/env bash

      set -eo pipefail

      FILE="$(realpath "$1")"
      TMPDIR="$(mktemp -d)"
      PDF="$TMPDIR/rendered.pdf"
      trap "pkill -P $$ && rm -rf \"$TMPDIR\"" exit

      regen() {
      	inotifywait -q -e modify "$FILE" "$TMPDIR/closing.signal" > "$TMPDIR/inotify.log"
      	grep -q "$TMPDIR/closing.signal" "$TMPDIR/inotify.log" || {
      		pandoc "$FILE" -o "$PDF"
      		pkill -P "$$" # Kills children of these script, which can only be termpdf.py at this point
      	}
      }

      pushd "$TMPDIR" >/dev/null # termpdf.py's log file will go here and will be deleted on exit
      pandoc "$FILE" -o "$PDF"
      touch closing.signal

      while :
      do
      	regen &
      	termpdf.py "$PDF" && popd>/dev/null && echo 1 >"$TMPDIR/closing.signal" && exit 0 # If the user quits the viewer, we can exit nicely
      done
```

# Moonlander
- screws for tenting are 19mm apart
- m2.6 screws/ 2.5 also should work


To access files within a Windows Subsystem for Linux (WSL) from a natively installed Linux system, you can mount the WSL drive using the guestmount tool. This allows you to treat the WSL file system as if it were a regular drive on your Linux system. 
Here's how to do it:

    Install libguestfs-tools: 

Code

   sudo apt-get install libguestfs-tools

    Create a mount point:

Code

   sudo mkdir -p /mnt/wsl

    Mount the WSL drive:

Code

   sudo guestmount -o allow_other --add /mnt/c/Users/username/AppData/Local/Packages/CanonicalGroupLimited.UbuntuonWindows_79rhkp1fndgsc/LocalState/ext4.vhdx -i /mnt/wsl

    Replace /mnt/c/Users/username/AppData/Local/Packages/CanonicalGroupLimited.UbuntuonWindows_79rhkp1fndgsc/LocalState/ext4.vhdx with the actual path to your WSL's virtual disk image (vhdx file). You can find this path by navigating to the AppData/Local/Packages folder within your Windows user directory and finding the relevant WSL distribution package. 

Replace /mnt/wsl with your desired mount point. 




https://vlaci.github.io/nix.org/posts/niri
https://github.com/blindFS/modern-dot-files/blob/main/nix/hmModules/nushell.nix
https://github.com/Misterio77/nix-config/blob/main/home/gabriel/features/cli/nushell.nix
