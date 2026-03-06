{ paletteSet, ... }:
let
  p = paletteSet.palette;
in
{
  #   surface roles    → backgrounds / UI chrome
  #   on_surface*      → foreground text tiers
  #   primary          → keywords, important UI accents
  #   secondary        → functions, identifiers
  #   tertiary         → types, labels
  #   primary_container / secondary_container → selection, visual
  #   error            → errors, warnings (+ container variants)
  #   outline*         → comments, subtle chrome, borders
  "nvim/colors.vim" = ''
    let g:colors_name="nix-${paletteSet.name}"

    set termguicolors

    if exists("syntax_on")
      syntax reset
    endif

    hi clear

    " ── Core ───────────────────────────────────────────────────────────────
    hi Normal        guifg=${p.on_surface}            guibg=${p.background}             gui=NONE
    hi NormalFloat   guifg=${p.on_surface}            guibg=${p.surface_container}      gui=NONE
    hi NormalNC      guifg=${p.on_surface_variant}    guibg=${p.surface_container_low}  gui=NONE
    hi FloatBorder   guifg=${p.outline}               guibg=${p.surface_container}      gui=NONE

    " ── Cursor & selection ─────────────────────────────────────────────────
    hi Cursor        guifg=${p.background}            guibg=${p.on_surface}             gui=NONE
    hi TermCursor    guifg=${p.background}            guibg=${p.primary}                gui=NONE
    hi TermCursorNC  guifg=${p.background}            guibg=${p.outline}                gui=NONE
    hi Visual        guifg=NONE                       guibg=${p.primary_container}      gui=NONE
    hi VisualNOS     guifg=${p.error}                 guibg=NONE                        gui=NONE

    " ── UI elements ────────────────────────────────────────────────────────
    hi ColorColumn   guifg=NONE                       guibg=${p.surface_container_low}  gui=NONE
    hi CursorColumn  guifg=NONE                       guibg=${p.surface_container_low}  gui=NONE
    hi CursorLine    guifg=NONE                       guibg=${p.surface_container}      gui=NONE
    hi CursorLineNr  guifg=${p.primary}               guibg=${p.surface_container}      gui=bold
    hi LineNr        guifg=${p.outline}               guibg=${p.background}             gui=NONE
    hi SignColumn    guifg=${p.outline}               guibg=${p.background}             gui=NONE
    hi FoldColumn    guifg=${p.outline_variant}       guibg=${p.background}             gui=NONE
    hi Folded        guifg=${p.on_surface_variant}    guibg=${p.surface_container_low}  gui=NONE
    hi EndOfBuffer   guifg=${p.surface_container}     guibg=NONE                        gui=NONE
    hi NonText       guifg=${p.outline_variant}       guibg=NONE                        gui=NONE
    hi SpecialKey    guifg=${p.outline}               guibg=NONE                        gui=NONE
    hi Conceal       guifg=${p.outline_variant}       guibg=NONE                        gui=NONE
    hi Directory     guifg=${p.secondary}             guibg=NONE                        gui=bold
    hi Title         guifg=${p.primary}               guibg=NONE                        gui=bold
    hi WildMenu      guifg=${p.on_primary_container}  guibg=${p.primary_container}      gui=NONE
    hi QuickFixLine  guifg=NONE                       guibg=${p.surface_container_high} gui=NONE

    " ── Status / Tab lines ─────────────────────────────────────────────────
    hi StatusLine    guifg=${p.on_surface}            guibg=${p.surface_container_high} gui=NONE
    hi StatusLineNC  guifg=${p.outline}               guibg=${p.surface_container}      gui=NONE
    hi VertSplit     guifg=${p.outline_variant}       guibg=${p.background}             gui=NONE
    hi TabLine       guifg=${p.on_surface_variant}    guibg=${p.surface_container}      gui=NONE
    hi TabLineFill   guifg=${p.outline_variant}       guibg=${p.surface_container_low}  gui=NONE
    hi TabLineSel    guifg=${p.on_primary_container}  guibg=${p.primary_container}      gui=bold

    " ── Popup / completion ─────────────────────────────────────────────────
    hi PMenu         guifg=${p.on_surface}            guibg=${p.surface_container_high} gui=NONE
    hi PMenuSel      guifg=${p.on_primary_container}  guibg=${p.primary_container}      gui=bold
    hi PMenuSbar     guifg=NONE                       guibg=${p.surface_container_highest} gui=NONE
    hi PMenuThumb    guifg=NONE                       guibg=${p.outline}                gui=NONE

    " ── Search ─────────────────────────────────────────────────────────────
    hi Search        guifg=${p.on_primary_container}  guibg=${p.primary_container}      gui=NONE
    hi IncSearch     guifg=${p.on_primary}            guibg=${p.primary}                gui=bold
    hi Substitute    guifg=${p.on_secondary_container} guibg=${p.secondary_container}   gui=NONE

    " ── Messages & prompts ─────────────────────────────────────────────────
    hi ModeMsg       guifg=${p.primary}               guibg=NONE                        gui=bold
    hi MoreMsg       guifg=${p.secondary}             guibg=NONE                        gui=NONE
    hi Question      guifg=${p.secondary}             guibg=NONE                        gui=NONE
    hi ErrorMsg      guifg=${p.error}                 guibg=NONE                        gui=bold
    hi WarningMsg    guifg=${p.tertiary}              guibg=NONE                        gui=NONE
    hi TooLong       guifg=${p.error}                 guibg=NONE                        gui=NONE

    " ── Diff ───────────────────────────────────────────────────────────────
    hi DiffAdd       guifg=${p.on_primary_container}  guibg=${p.primary_container}      gui=NONE
    hi DiffChange    guifg=${p.on_tertiary_container} guibg=${p.tertiary_container}     gui=NONE
    hi DiffDelete    guifg=${p.on_error_container}    guibg=${p.error_container}        gui=NONE
    hi DiffText      guifg=${p.on_secondary_container} guibg=${p.secondary_container}   gui=bold
    hi DiffAdded     guifg=${p.primary}               guibg=${p.background}             gui=NONE
    hi DiffFile      guifg=${p.error}                 guibg=${p.background}             gui=NONE
    hi DiffNewFile   guifg=${p.primary}               guibg=${p.background}             gui=NONE
    hi DiffLine      guifg=${p.secondary}             guibg=${p.background}             gui=NONE
    hi DiffRemoved   guifg=${p.error}                 guibg=${p.background}             gui=NONE

    " ── Spelling ───────────────────────────────────────────────────────────
    hi SpellBad    guifg=NONE guibg=NONE gui=undercurl guisp=${p.error}
    hi SpellLocal  guifg=NONE guibg=NONE gui=undercurl guisp=${p.tertiary}
    hi SpellCap    guifg=NONE guibg=NONE gui=undercurl guisp=${p.secondary}
    hi SpellRare   guifg=NONE guibg=NONE gui=undercurl guisp=${p.outline}

    " ── Misc ───────────────────────────────────────────────────────────────
    hi Bold          guifg=NONE guibg=NONE gui=bold
    hi Italic        guifg=NONE guibg=NONE gui=italic
    hi Underlined    guifg=${p.secondary} guibg=NONE gui=underline
    hi MatchParen    guifg=${p.on_primary} guibg=${p.primary} gui=bold

    " ── Syntax ─────────────────────────────────────────────────────────────
    " comments / docs
    hi Comment       guifg=${p.outline}            guibg=NONE gui=italic
    hi Todo          guifg=${p.on_primary}         guibg=${p.primary} gui=bold

    " strings → primary_container-ish (warm, distinct)
    hi String        guifg=${p.on_primary_container}  guibg=NONE gui=NONE
    hi Character     guifg=${p.on_primary_container}  guibg=NONE gui=NONE

    " keywords → primary
    hi Keyword       guifg=${p.primary}            guibg=NONE gui=bold
    hi Conditional   guifg=${p.primary}            guibg=NONE gui=bold
    hi Repeat        guifg=${p.primary}            guibg=NONE gui=bold
    hi Statement     guifg=${p.primary}            guibg=NONE gui=NONE
    hi Exception     guifg=${p.error}              guibg=NONE gui=bold
    hi Define        guifg=${p.primary}            guibg=NONE gui=NONE
    hi Include       guifg=${p.primary}            guibg=NONE gui=NONE
    hi Macro         guifg=${p.primary}            guibg=NONE gui=NONE
    hi PreProc       guifg=${p.primary}            guibg=NONE gui=NONE
    hi Label         guifg=${p.primary}            guibg=NONE gui=NONE

    " functions → secondary
    hi Function      guifg=${p.secondary}          guibg=NONE gui=NONE
    hi Identifier    guifg=${p.on_surface}         guibg=NONE gui=NONE

    " types → tertiary
    hi Type          guifg=${p.tertiary}           guibg=NONE gui=NONE
    hi Typedef       guifg=${p.tertiary}           guibg=NONE gui=NONE
    hi Structure     guifg=${p.tertiary}           guibg=NONE gui=NONE
    hi StorageClass  guifg=${p.tertiary}           guibg=NONE gui=NONE

    " constants / numbers → tertiary variant
    hi Constant      guifg=${p.on_tertiary_container} guibg=NONE gui=NONE
    hi Boolean       guifg=${p.on_tertiary_container} guibg=NONE gui=NONE
    hi Number        guifg=${p.on_tertiary_container} guibg=NONE gui=NONE
    hi Float         guifg=${p.on_tertiary_container} guibg=NONE gui=NONE

    " operators & delimiters → on_surface
    hi Operator      guifg=${p.on_surface}         guibg=NONE gui=NONE
    hi Delimiter     guifg=${p.on_surface_variant} guibg=NONE gui=NONE

    " special
    hi Special       guifg=${p.on_secondary_container} guibg=NONE gui=NONE
    hi SpecialChar   guifg=${p.on_secondary_container} guibg=NONE gui=NONE
    hi Tag           guifg=${p.secondary}          guibg=NONE gui=NONE
    hi Debug         guifg=${p.error}              guibg=NONE gui=NONE

    " errors
    hi Error         guifg=${p.on_error}           guibg=${p.error}     gui=NONE

    " ── Git commit ─────────────────────────────────────────────────────────
    hi gitcommitOverflow       guifg=${p.error}              guibg=NONE gui=NONE
    hi gitcommitSummary        guifg=${p.on_surface}         guibg=NONE gui=NONE
    hi gitcommitComment        guifg=${p.outline}            guibg=NONE gui=italic
    hi gitcommitUntracked      guifg=${p.outline}            guibg=NONE gui=NONE
    hi gitcommitDiscarded      guifg=${p.outline}            guibg=NONE gui=NONE
    hi gitcommitSelected       guifg=${p.outline}            guibg=NONE gui=NONE
    hi gitcommitHeader         guifg=${p.primary}            guibg=NONE gui=NONE
    hi gitcommitSelectedType   guifg=${p.secondary}          guibg=NONE gui=NONE
    hi gitcommitUnmergedType   guifg=${p.secondary}          guibg=NONE gui=NONE
    hi gitcommitDiscardedType  guifg=${p.secondary}          guibg=NONE gui=NONE
    hi gitcommitBranch         guifg=${p.tertiary}           guibg=NONE gui=bold
    hi gitcommitUntrackedFile  guifg=${p.tertiary}           guibg=NONE gui=NONE
    hi gitcommitUnmergedFile   guifg=${p.error}              guibg=NONE gui=bold
    hi gitcommitDiscardedFile  guifg=${p.error}              guibg=NONE gui=bold
    hi gitcommitSelectedFile   guifg=${p.primary}            guibg=NONE gui=bold

    " ── Git gutter ─────────────────────────────────────────────────────────
    hi GitGutterAdd           guifg=${p.primary}             guibg=${p.background} gui=NONE
    hi GitGutterChange        guifg=${p.tertiary}            guibg=${p.background} gui=NONE
    hi GitGutterDelete        guifg=${p.error}               guibg=${p.background} gui=NONE
    hi GitGutterChangeDelete  guifg=${p.secondary}           guibg=${p.background} gui=NONE

    " ── Diagnostics ────────────────────────────────────────────────────────
    hi DiagnosticError      guifg=${p.error}      guibg=${p.surface_container} gui=NONE
    hi DiagnosticWarn       guifg=${p.tertiary}   guibg=${p.surface_container} gui=NONE
    hi DiagnosticInfo       guifg=${p.secondary}  guibg=${p.surface_container} gui=NONE
    hi DiagnosticHint       guifg=${p.outline}    guibg=${p.surface_container} gui=NONE

    hi DiagnosticUnderlineError   guifg=NONE guibg=NONE gui=undercurl guisp=${p.error}
    hi DiagnosticUnderlineWarning guifg=NONE guibg=NONE gui=undercurl guisp=${p.tertiary}
    hi DiagnosticUnderlineWarn    guifg=NONE guibg=NONE gui=undercurl guisp=${p.tertiary}
    hi DiagnosticUnderlineInfo    guifg=NONE guibg=NONE gui=undercurl guisp=${p.secondary}
    hi DiagnosticUnderlineHint    guifg=NONE guibg=NONE gui=undercurl guisp=${p.outline}

    hi link LspDiagnosticsDefaultError         DiagnosticError
    hi link LspDiagnosticsDefaultWarning       DiagnosticWarn
    hi link LspDiagnosticsDefaultInformation   DiagnosticInfo
    hi link LspDiagnosticsDefaultHint          DiagnosticHint
    hi link LspDiagnosticsUnderlineError       DiagnosticUnderlineError
    hi link LspDiagnosticsUnderlineWarning     DiagnosticUnderlineWarning
    hi link LspDiagnosticsUnderlineInformation DiagnosticUnderlineInfo
    hi link LspDiagnosticsUnderlineHint        DiagnosticUnderlineHint

    hi LspReferenceText  guifg=NONE guibg=NONE gui=underline guisp=${p.outline}
    hi LspReferenceRead  guifg=NONE guibg=NONE gui=underline guisp=${p.outline}
    hi LspReferenceWrite guifg=NONE guibg=NONE gui=underline guisp=${p.primary}

    " ── Treesitter ─────────────────────────────────────────────────────────
    hi TSComment             guifg=${p.outline}                 guibg=NONE gui=italic
    hi TSAnnotation          guifg=${p.on_surface_variant}      guibg=NONE gui=NONE
    hi TSAttribute           guifg=${p.tertiary}                guibg=NONE gui=NONE
    hi TSBoolean             guifg=${p.on_tertiary_container}   guibg=NONE gui=NONE
    hi TSCharacter           guifg=${p.on_primary_container}    guibg=NONE gui=NONE
    hi TSConditional         guifg=${p.primary}                 guibg=NONE gui=bold
    hi TSConstant            guifg=${p.on_tertiary_container}   guibg=NONE gui=NONE
    hi TSConstBuiltin        guifg=${p.on_tertiary_container}   guibg=NONE gui=italic
    hi TSConstMacro          guifg=${p.on_tertiary_container}   guibg=NONE gui=NONE
    hi TSConstructor         guifg=${p.secondary}               guibg=NONE gui=NONE
    hi TSError               guifg=${p.error}                   guibg=NONE gui=NONE
    hi TSException           guifg=${p.error}                   guibg=NONE gui=bold
    hi TSField               guifg=${p.on_surface}              guibg=NONE gui=NONE
    hi TSFloat               guifg=${p.on_tertiary_container}   guibg=NONE gui=NONE
    hi TSFunction            guifg=${p.secondary}               guibg=NONE gui=NONE
    hi TSFuncBuiltin         guifg=${p.secondary}               guibg=NONE gui=italic
    hi TSFuncMacro           guifg=${p.secondary}               guibg=NONE gui=NONE
    hi TSInclude             guifg=${p.primary}                 guibg=NONE gui=NONE
    hi TSKeyword             guifg=${p.primary}                 guibg=NONE gui=bold
    hi TSKeywordFunction     guifg=${p.primary}                 guibg=NONE gui=bold
    hi TSKeywordOperator     guifg=${p.primary}                 guibg=NONE gui=NONE
    hi TSLabel               guifg=${p.primary}                 guibg=NONE gui=NONE
    hi TSMethod              guifg=${p.secondary}               guibg=NONE gui=NONE
    hi TSNamespace           guifg=${p.on_surface_variant}      guibg=NONE gui=NONE
    hi TSNone                guifg=${p.on_surface}              guibg=NONE gui=NONE
    hi TSNumber              guifg=${p.on_tertiary_container}   guibg=NONE gui=NONE
    hi TSOperator            guifg=${p.on_surface}              guibg=NONE gui=NONE
    hi TSParameter           guifg=${p.on_surface}              guibg=NONE gui=NONE
    hi TSParameterReference  guifg=${p.on_surface}              guibg=NONE gui=NONE
    hi TSProperty            guifg=${p.on_surface}              guibg=NONE gui=NONE
    hi TSPunctDelimiter      guifg=${p.outline_variant}         guibg=NONE gui=NONE
    hi TSPunctBracket        guifg=${p.on_surface_variant}      guibg=NONE gui=NONE
    hi TSPunctSpecial        guifg=${p.on_surface_variant}      guibg=NONE gui=NONE
    hi TSRepeat              guifg=${p.primary}                 guibg=NONE gui=bold
    hi TSString              guifg=${p.on_primary_container}    guibg=NONE gui=NONE
    hi TSStringRegex         guifg=${p.on_secondary_container}  guibg=NONE gui=NONE
    hi TSStringEscape        guifg=${p.on_secondary_container}  guibg=NONE gui=NONE
    hi TSSymbol              guifg=${p.primary}                 guibg=NONE gui=NONE
    hi TSTag                 guifg=${p.secondary}               guibg=NONE gui=NONE
    hi TSTagDelimiter        guifg=${p.outline_variant}         guibg=NONE gui=NONE
    hi TSText                guifg=${p.on_surface}              guibg=NONE gui=NONE
    hi TSStrong              guifg=NONE guibg=NONE gui=bold
    hi TSEmphasis            guifg=${p.tertiary}                guibg=NONE gui=italic
    hi TSUnderline           guifg=${p.on_surface}              guibg=NONE gui=underline
    hi TSStrike              guifg=${p.outline}                 guibg=NONE gui=strikethrough
    hi TSTitle               guifg=${p.primary}                 guibg=NONE gui=bold
    hi TSLiteral             guifg=${p.on_primary_container}    guibg=NONE gui=NONE
    hi TSURI                 guifg=${p.secondary}               guibg=NONE gui=underline
    hi TSType                guifg=${p.tertiary}                guibg=NONE gui=NONE
    hi TSTypeBuiltin         guifg=${p.tertiary}                guibg=NONE gui=italic
    hi TSVariable            guifg=${p.on_surface}              guibg=NONE gui=NONE
    hi TSVariableBuiltin     guifg=${p.on_surface}              guibg=NONE gui=italic
    hi TSDefinition          guifg=NONE guibg=NONE gui=underline guisp=${p.outline}
    hi TSDefinitionUsage     guifg=NONE guibg=NONE gui=underline guisp=${p.outline}
    hi TSCurrentScope        guifg=NONE guibg=NONE gui=bold

    if has('nvim-0.8.0')
      highlight! link @annotation        TSAnnotation
      highlight! link @attribute         TSAttribute
      highlight! link @boolean           TSBoolean
      highlight! link @character         TSCharacter
      highlight! link @comment           TSComment
      highlight! link @conditional       TSConditional
      highlight! link @constant          TSConstant
      highlight! link @constant.builtin  TSConstBuiltin
      highlight! link @constant.macro    TSConstMacro
      highlight! link @constructor       TSConstructor
      highlight! link @exception         TSException
      highlight! link @field             TSField
      highlight! link @float             TSFloat
      highlight! link @function          TSFunction
      highlight! link @function.builtin  TSFuncBuiltin
      highlight! link @function.macro    TSFuncMacro
      highlight! link @include           TSInclude
      highlight! link @keyword           TSKeyword
      highlight! link @keyword.function  TSKeywordFunction
      highlight! link @keyword.operator  TSKeywordOperator
      highlight! link @label             TSLabel
      highlight! link @method            TSMethod
      highlight! link @namespace         TSNamespace
      highlight! link @none              TSNone
      highlight! link @number            TSNumber
      highlight! link @operator          TSOperator
      highlight! link @parameter         TSParameter
      highlight! link @parameter.reference TSParameterReference
      highlight! link @property          TSProperty
      highlight! link @punctuation.bracket TSPunctBracket
      highlight! link @punctuation.delimiter TSPunctDelimiter
      highlight! link @punctuation.special TSPunctSpecial
      highlight! link @repeat            TSRepeat
      highlight! link @storageclass      TSStorageClass
      highlight! link @string            TSString
      highlight! link @string.escape     TSStringEscape
      highlight! link @string.regex      TSStringRegex
      highlight! link @symbol            TSSymbol
      highlight! link @tag               TSTag
      highlight! link @tag.delimiter     TSTagDelimiter
      highlight! link @text              TSText
      highlight! link @strike            TSStrike
      highlight! link @math              TSURI
      highlight! link @type              TSType
      highlight! link @type.builtin      TSTypeBuiltin
      highlight! link @uri               TSURI
      highlight! link @variable          TSVariable
      highlight! link @variable.builtin  TSVariableBuiltin
    endif

    " ── Plugins ────────────────────────────────────────────────────────────
    hi IndentBlankLine  guifg=${p.surface_container_high}  guibg=NONE gui=NONE
    hi TreesitterContext guifg=NONE  guibg=${p.surface_container_low} gui=NONE

    hi NvimTreeNormal   guifg=${p.on_surface}  guibg=${p.surface_container_low} gui=NONE

    hi CmpItemAbbr           guifg=${p.on_surface}         guibg=NONE gui=NONE
    hi CmpItemAbbrDeprecated guifg=${p.outline}             guibg=NONE gui=strikethrough
    hi CmpItemAbbrMatch      guifg=${p.primary}             guibg=NONE gui=bold
    hi CmpItemAbbrMatchFuzzy guifg=${p.primary}             guibg=NONE gui=NONE
    hi CmpItemKind           guifg=${p.secondary}           guibg=NONE gui=NONE
    hi CmpItemMenu           guifg=${p.on_surface_variant}  guibg=NONE gui=NONE

    hi BufferCurrent        guifg=${p.on_primary_container}  guibg=${p.primary_container}    gui=bold
    hi BufferCurrentIndex   guifg=${p.on_primary_container}  guibg=${p.primary_container}    gui=NONE
    hi BufferCurrentMod     guifg=${p.tertiary}              guibg=${p.primary_container}    gui=NONE
    hi BufferCurrentSign    guifg=${p.primary}               guibg=${p.primary_container}    gui=NONE
    hi BufferCurrentTarget  guifg=${p.error}                 guibg=${p.primary_container}    gui=NONE
    hi BufferCurrentIcon    guifg=NONE                       guibg=${p.primary_container}    gui=NONE
    hi BufferVisible        guifg=${p.on_surface}            guibg=${p.surface_container}    gui=NONE
    hi BufferVisibleIndex   guifg=${p.on_surface}            guibg=${p.surface_container}    gui=NONE
    hi BufferVisibleMod     guifg=${p.tertiary}              guibg=${p.surface_container}    gui=NONE
    hi BufferVisibleSign    guifg=${p.outline}               guibg=${p.surface_container}    gui=NONE
    hi BufferVisibleTarget  guifg=${p.error}                 guibg=${p.surface_container}    gui=NONE
    hi BufferVisibleIcon    guifg=NONE                       guibg=${p.surface_container}    gui=NONE
    hi BufferInactive       guifg=${p.outline}               guibg=${p.surface_container_low} gui=NONE
    hi BufferInactiveIndex  guifg=${p.outline}               guibg=${p.surface_container_low} gui=NONE
    hi BufferInactiveMod    guifg=${p.outline_variant}       guibg=${p.surface_container_low} gui=NONE
    hi BufferInactiveSign   guifg=${p.outline_variant}       guibg=${p.surface_container_low} gui=NONE
    hi BufferInactiveTarget guifg=${p.error}                 guibg=${p.surface_container_low} gui=NONE
    hi BufferInactiveIcon   guifg=NONE                       guibg=${p.surface_container_low} gui=NONE
    hi BufferTabpages       guifg=${p.outline}               guibg=${p.surface_container_low} gui=NONE
    hi BufferTabpageFill    guifg=${p.outline_variant}       guibg=${p.surface_container_low} gui=NONE

    hi NvimInternalError  guifg=${p.on_error}  guibg=${p.error} gui=NONE

    " ── User highlight groups (statusline segments etc.) ───────────────────
    hi User1  guifg=${p.error}     guibg=${p.surface_container_high} gui=NONE
    hi User2  guifg=${p.tertiary}  guibg=${p.surface_container_high} gui=NONE
    hi User3  guifg=${p.on_surface} guibg=${p.surface_container_high} gui=NONE
    hi User4  guifg=${p.secondary} guibg=${p.surface_container_high} gui=NONE
    hi User5  guifg=${p.outline}   guibg=${p.surface_container_high} gui=NONE
    hi User6  guifg=${p.on_surface} guibg=${p.surface_container_high} gui=NONE
    hi User7  guifg=${p.on_surface} guibg=${p.surface_container_high} gui=NONE
    hi User8  guifg=${p.background} guibg=${p.surface_container_high} gui=NONE
    hi User9  guifg=${p.background} guibg=${p.surface_container_high} gui=NONE

    " ── Terminal colors ────────────────────────────────────────────────────
    let g:terminal_color_background = "${p.background}"
    let g:terminal_color_foreground = "${p.on_surface}"

    let g:terminal_color_0  = "${p.background}"
    let g:terminal_color_1  = "${p.error}"
    let g:terminal_color_2  = "${p.primary}"
    let g:terminal_color_3  = "${p.tertiary}"
    let g:terminal_color_4  = "${p.secondary}"
    let g:terminal_color_5  = "${p.on_secondary_container}"
    let g:terminal_color_6  = "${p.on_primary_container}"
    let g:terminal_color_7  = "${p.on_surface}"
    let g:terminal_color_8  = "${p.outline}"
    let g:terminal_color_9  = "${p.error}"
    let g:terminal_color_10 = "${p.primary}"
    let g:terminal_color_11 = "${p.tertiary}"
    let g:terminal_color_12 = "${p.secondary}"
    let g:terminal_color_13 = "${p.on_secondary_container}"
    let g:terminal_color_14 = "${p.on_primary_container}"
    let g:terminal_color_15 = "${p.inverse_on_surface}"
  '';
}
