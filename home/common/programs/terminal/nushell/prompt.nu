def create_left_prompt [] {
    let last_exit_code = $env.LAST_EXIT_CODE
    
    # :Face
    let face_text = if $last_exit_code == 0 { "(^.^)" } else { "(x.x)" }
    let face_color = if $last_exit_code == 0 { (ansi green) } else { (ansi red) }
    let face = $"(ansi green)($face_color)($face_text)(ansi reset)"

    # :SSH :ROOT
    let host = sys host | get hostname

    let identity = if (is-admin) {
        $"(ansi red_bold)!@(ansi reset)─"
    } else if ($env.SSH_CONNECTION? | is-not-empty) {
        $"[(ansi purple)($env.USER)@($host)(ansi reset)]─"
    } else { "" }

    # :Directories
    let dir_path = ($env.PWD | str replace $env.HOME "~")
    let truncated_dir = if ($dir_path | path split | length) > 6 {
        $".../($dir_path | path basename)"
    } else {
        $dir_path
    }
    let dir = $"(ansi white)[(ansi cyan)($truncated_dir)(ansi white)]"

    # :GIT
    let git_info = do { git branch --show-current } | complete
    let git_branch = if ($git_info.exit_code == 0 and ($git_info.stdout | str trim | is-not-empty)) {
        $"(ansi white)─[(ansi yellow) ($git_info.stdout | str trim)(ansi white)]"
    } else { "" }

    # :Nixshell :Venv
    let env_prompt = if "VIRTUAL_ENV" in $env {
        $"(ansi white)─[(ansi green_bold) venv(ansi white)]"
    } else if "IN_NIX_SHELL" in $env  {
        $"(ansi white)─[(ansi green_bold)  (ansi white)]"
    } else {
        ""
    }

    # :Character
    let line1 = $"(ansi white)┌─($face)─($identity)($dir)($git_branch)($env_prompt)"
    let line2 = $"\n(ansi white)└─(ansi reset)"

    $line1 + $line2
}

def create_right_prompt [] {
    # :Time
    let duration = ($env.CMD_DURATION_MS | default 0 | into float)
    if $duration > 500 {
        $"(ansi green)󱎫 (($duration / 1000 | math round --precision 2) | into string)s(ansi reset)"
    } else {
        ""
    }
}

$env.PROMPT_COMMAND = { || create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = { || create_right_prompt }
$env.PROMPT_INDICATOR = {|| $"(ansi reset)ᐉ " }
$env.PROMPT_INDICATOR_VI_INSERT = {|| $"(ansi reset)ᐉ " }
$env.PROMPT_INDICATOR_VI_NORMAL = {|| $"(ansi blue)ᐉ " }
$env.PROMPT_MULTILINE_INDICATOR = {|| "::: " }
