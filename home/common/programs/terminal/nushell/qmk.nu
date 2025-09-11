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
      qmk_suffix: ""
      path: "zsa"
      keymap: "xendak"
      flash_tool: "sudo-wally"
      flash_args: []
      description: "moves to moonlander directory"
      cmd: []
      args: []
    }
    annepro2: {
      qmk_name: "annepro2"
      qmk_suffix: "c18"
      path: ""
      keymap: "xendak"
      flash_tool: "annepro2-tools"
      flash_args: []
      description: "moves to annepro directory"
      cmd: []
      args: []
    }
  }
}

# Available Commands.
def __get-commands [] {
  {
    compile: {
      cmd: {|kb| __compile-keyboard $kb }
      args: []
      description: "Compile firmware for specified keyboard"
      help_message: "Available keyboards for compilation:"
    }
    flash: {
      cmd: {|kb| __flash-keyboard $kb }
      args: []
      description: "Flash firmware for specified keyboard"
      help_message: "Available keyboards for flashing:"
    }
    cf: {
      cmd: {|kb| __compile-flash-keyboard $kb }
      args: []
      description: "Compile and flash firmware for specified keyboard"
      help_message: "Available keyboards for compile+flash:"
    }
    clean: {
      cmd: { __clean-files }
      args: []
      description: "Cleans all keyboards firmware files"
      help_message: "Cleans all keyboards firmware files"
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
  if ($path | is-empty) {
    ($env.QMK_USERSPACE | lines | path join "keyboards" $name)
  } else {
    ($env.QMK_USERSPACE | lines | path join "keyboards" $path $name)
  }
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

def __execute-command [command_name: string, keyboard?: string] {
    let commands = __get-commands
    let command_config = $commands | get $command_name
    
    if ($keyboard | is-empty) {
        let current_kb = __detect-current-keyboard
        if ($current_kb | is-not-empty) {
            do $command_config.cmd $current_kb
                  
        } else {
            print $"No keyboard specified and not in a keyboard directory."
            print $command_config.help_message
            __print-keyboard-help
        }
    } else {
        if (__validate-keyboard $keyboard) {
            do $command_config.cmd $keyboard
        }
    }
}

def __clean-files [] {
  let keyboards = __get-keyboards
  let firmware_files = ($keyboards | values | each {
    |kb| __get-firmware-name $kb
  })
  let firmware_paths = [
    ($env.QMK_USERSPACE | str trim | path join "qmk_firmware")
    ($env.QMK_USERSPACE | str trim )
  ]
  mut files_to_clean = []
  for p in $firmware_paths {
    for f in $firmware_files {
      $files_to_clean = ($files_to_clean | append ($p | path join $f))
    }
  }
  for f in $files_to_clean {
    try {
      rm $f
      print $"Deleted file ($f)"
    } catch {
      print $"Could not delete file ($f)"
    }
  }
}

def __get-firmware-name [keyboard: record] {
  mut firmware = []
  if not ($keyboard.path | is-empty) {
      $firmware =  append $keyboard.path
  }
  $firmware = ($firmware | append $keyboard.qmk_name)
  if not ($keyboard.qmk_suffix | is-empty) {
    $firmware = ($firmware | append $keyboard.qmk_suffix)
  }
  $firmware = ($firmware | append $keyboard.keymap)
  let firmware_file = ($firmware | str join "_") + ".bin"
  return $firmware_file
}

# Compiling | Flash specifics
def __compile-keyboard [keyboard: string] {
  let keyboards = __get-keyboards
  let config = $keyboards | get $keyboard
  let keyboard_path = __get-keyboard-path $config.path $keyboard

  print $"Compiling ($keyboard)..."
  cd $keyboard_path
  try {
    mut qmk_command = $config.qmk_name
    if not ($config.qmk_suffix | is-empty) {
      $qmk_command = $qmk_command + "/" + $config.qmk_suffix
    }
    qmk compile -kb $qmk_command -km xendak
    return true
  } catch {
    return false
  }
}

def __flash-keyboard [keyboard: string] {
    let keyboards = __get-keyboards
    let config = $keyboards | get $keyboard
    let keyboard_path = __get-keyboard-path $config.path $keyboard
    let firmware_file = __get-firmware-name $config
    
    print $"Flashing ($keyboard) with firmware: ($firmware_file)"
    cd $env.QMK_USERSPACE
    
    # remove the sudo here, since its from the flash tool command to avoid password for keyboards
    let flash_cmd = [$config.flash_tool] | append $config.flash_args | append $firmware_file
    print $"Command: ($flash_cmd | str join ' ')"
    
    try {
      run-external ($config.flash_tool) ...$config.flash_args $firmware_file
      return true
    } catch {
      return false
    }
}

def __compile-flash-keyboard [keyboard: string] {
  if (__compile-keyboard $keyboard) {
    if (__flash-keyboard $keyboard) {
      return true
    } else {
      print $"Flash failed"
      return false
    }
  } else {
    print $"Compilation failed, skipping flash"
    return false
  }
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
  # TODO: refactor this out..
  # can do cmd closure on qmk entry itself
  if ($command in ($keyboards | columns)) {
    let config = $keyboards | get $command
    let keyboard_path = __get-keyboard-path $config.path $command
    let full_path = $keyboard_path | path join "keymaps" $config.keymap

    print $"Navigating to ($command) keyboard directory: ($full_path)"
    cd $full_path
    return
  }

  if ($command in $commands) {
    __execute-command $command $keyboard
    return
  }

  print $"Error: Unknown command or keyboard '($command)'"
  print ""
  __print-commands-help
  print ""
  __print-keyboard-help
}
