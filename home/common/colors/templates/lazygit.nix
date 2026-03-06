{ paletteSet, ... }:
let
  m = paletteSet.palette;
in
{
  "lazygit/config.yml" =
    # yml
    ''
      customCommands:
      - command: git cz
        context: files
        key: C
        loadingText: opening commitizen commit tool
        output: terminal
      - command: git commit --allow-empty -m 'empty commit'
        context: commits
        description: Add empty commit
        key: E
        loadingText: Committing empty commit...
      - command: git difftool -y {{.SelectedLocalCommit.Sha}} -- {{.SelectedCommitFile.Name}}
        context: commitFiles
        description: Compare (difftool) with local copy
        key: f
      - command: sh lazygit-ai-commit "gemini -m gemini-2.5-pro -p"
        context: files
        description: Pick AI commit
        key: <c-a>
        output: terminal
      - command: sh lazygit-ai-commit "gemini -m gemini-2.5-flash -p"
        context: files
        description: Pick AI commit
        key: <c-A>
        output: terminal
      disableStartupPopups: true
      git:
        pagers:
          - pager: delta --${paletteSet.type} --paging=never --side-by-side --line-numbers --hyperlinks --hyperlinks-file-link-format="lazygit-edit://{path}:{line}" --syntax-theme=base16
          - externalDiffCommand: difft --color=always --syntax-highlight=on --tab-width=2
      gui:
        filterMode: fuzzy
        language: en
        nerdFontsVersion: '3'
        theme:
          activeBorderColor:
            - '${m.primary}'
            - bold
          inactiveBorderColor:
            - '${m.selection_fg}'
          searchingActiveBorderColor:
            - '${m.secondary}'
            - bold
          optionsTextColor:
            - '${m.on_surface_variant}'
          selectedLineBgColor:
            - '${m.primary_container}'
          selectedRangeBgColor:
            - '${m.secondary_container}'
          cherryPickedCommitBgColor:
            - '${m.tertiary_container}'
          cherryPickedCommitFgColor:
            - '${m.on_tertiary_container}'
          unstagedChangesColor:
            - '${m.error}'
          defaultFgColor:
            - '${m.on_surface}'
        os:
          copyToClipboardCmd: 'printf "\033]52;c;$(printf {{text}} | base64)\a" > /dev/tty '
          editInTerminal: true
          editPreset: helix (hx)
          open: handlr open {{filename}} >/dev/null
          openLink: handlr open {{link}} >/dev/null
        refresher:
          fetchInverval: 120
          refreshInterval: 120
      promptToReturnFromSubprocess: false
    '';
}
