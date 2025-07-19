# CONFIG
$env.QMK_USERSPACE = try {
  open ~/.config/qmk/qmk.ini
  | lines
  | parse "{name} = {path}"
  | where path =~ "userspace"
  | get path
  | str trim
  | path dirname
  | to text
} catch {
  ""
}

# Define keyboard configurations
def __get-keyboards [] {
  {
    moonlander: {
      qmk_name: "moonlander"
      path: "keyboards/zsa"
      keymap: "keymaps/xendak"
      flash_tool: "wally-cli"
      flash_args: []
      description: "moves to moonlander directory"
      cmd: []
      args: []
    }
    annepro2: {
      qmk_name: "annepro2"
      path: "keyboards"
      keymap: "keymaps/xendak"
      flash_tool: "annepro2-tools"
      flash_args: []
      description: "moves to annepro directory"
      cmd: []
      args: []
    }
  }
}

# Available Commands.
# TODO: add a proper way to execute with cmd | args
def __get-commands [] {
  {
    compile: {
      cmd: []
      args: []
      description: "Compile firmware for specified keyboard"
    }
    flash: {
      cmd: []
      args: []
      description: "Flash firmware for specified keyboard"
    }
    cf: {
      cmd: []
      args: []
      description: "Compile and flash firmware for specified keyboard"
    }
  } | merge (__get-keyboards)
}

# Custom completions
def "nu-complete __mkb keyboards" [] {
  __get-keyboards | columns
}
def "nu-complete __mkb commands" [] {
  __get-commands | columns
}

# Print Helpers
def __print-commands-help [] {
  let commands = __get-commands
  print "Available commands:"
  $commands | items { |name, c|
    print $"  ($name) -> ($c.description)"
  }
}
def __print-keyboard-help [] {
  let keyboards = __get-keyboards
  print "Available keyboards:"
  $keyboards | items {|name, config|
    print $"  ($name) -> (__get-keyboard-path $config.path $name)"
  }
}

# Utils
def __get-keyboard-path [path: string, name: string] {
  # somehow i need this here so it avoids the \n
  ($env.QMK_USERSPACE | lines | path join $path $name)
}
def __validate-keyboard [keyboard: string] {
  let keyboards = __get-keyboards
  if not ($keyboard in ($keyboards | columns)) {
    print $"Error: Unknown keyboard '($keyboard)'"
    __print-keyboard-help
    return false
  }
  true
}

# detect keyboard from PWD
def __detect-current-keyboard [] {
  let pwd = pwd
  let keyboards = __get-keyboards

  for keyboard in ($keyboards | items {|name, config| {name: $name, config: $config}}) {
    let keyboard_path = __get-keyboard-path $keyboard.config.path $keyboard.name
    if ($pwd | str starts-with $keyboard_path) {
      return $keyboard.name
    }
  }

  return null
}

# Compiling | Flash specifics
def __compile-keyboard [keyboard: string] {
  let keyboards = __get-keyboards
  let config = $keyboards | get $keyboard
  let keyboard_path = __get-keyboard-path $config.path $keyboard

  print $"Compiling ($keyboard)..."
  cd $keyboard_path
  qmk compile -kb $config.qmk_name -km xendak
}
def __flash-keyboard [keyboard: string] {
  let keyboards = __get-keyboards
  let config = $keyboards | get $keyboard
  let keyboard_path = __get-keyboard-path $config.path $keyboard

  print $"Flashing ($keyboard)..."
  cd $keyboard_path
  qmk flash -kb $config.qmk_name -km xendak
}
def __compile-flash-keyboard [keyboard: string] {
  let keyboards = __get-keyboards
  let config = $keyboards | get $keyboard
  let keyboard_path = __get-keyboard-path $config.path $keyboard

  # TODO: fix this eventually, need to do a try catch and maybe check sha
  print $"Compiling and flashing ($keyboard)..."
  cd $keyboard_path
  qmk flash -kb $config.qmk_name -km xendak
}

def --env mkb [
  command?: string@"nu-complete __mkb commands"
  keyboard?: string@"nu-complete __mkb keyboards"
] {
  if ($command | is-empty) {
    print "QMK Keyboard Management Tool"
    print ""
    __print-commands-help
    print ""
    __print-keyboard-help
    return
  }

  let keyboards = __get-keyboards
  let commands = __get-commands

  # Check if first argument is a keyboard
  if ($command in ($keyboards | columns)) {
    let config = $keyboards | get $command
    let keyboard_path = __get-keyboard-path $config.path $command
    let full_path = $keyboard_path | path join $config.keymap

    print $"Navigating to ($command) keyboard directory: ($full_path)"
    cd $full_path
    return
  }

  # Check if first argument is a command
  if ($command in $commands) {
    match $command {
      "compile" => {
        if ($keyboard | is-empty) {
          # Try to detect current keyboard from PWD
          let current_kb = __detect-current-keyboard
          if ($current_kb | is-not-empty) {
            __compile-keyboard $current_kb
          } else {
            print "No keyboard specified and not in a keyboard directory."
            print "Available keyboards for compilation:"
            __print-keyboard-help
          }
        } else {
          if (__validate-keyboard $keyboard) {
            __compile-keyboard $keyboard
          }
        }
      }
      "flash" => {
        if ($keyboard | is-empty) {
          # Try to detect current keyboard from PWD
          let current_kb = __detect-current-keyboard
          if ($current_kb | is-not-empty) {
            __flash-keyboard $current_kb
          } else {
            print "No keyboard specified and not in a keyboard directory."
            print "Available keyboards for flashing:"
            __print-keyboard-help
          }
        } else {
          if (__validate-keyboard $keyboard) {
            __flash-keyboard $keyboard
          }
        }
      }
      "cf" => {
        if ($keyboard | is-empty) {
          # Try to detect current keyboard from PWD
          let current_kb = __detect-current-keyboard
          if ($current_kb | is-not-empty) {
            __compile-flash-keyboard $current_kb
          } else {
            print "No keyboard specified and not in a keyboard directory."
            print "Available keyboards for compile+flash:"
            __print-keyboard-help
          }
        } else {
          if (__validate-keyboard $keyboard) {
            __compile-flash-keyboard $keyboard
          }
        }
      }
    }
    return
  }

  # If we get here, the command is not recognized
  print $"Error: Unknown command or keyboard '($command)'"
  print ""
  __print-commands-help
  print ""
  __print-keyboard-help
}
