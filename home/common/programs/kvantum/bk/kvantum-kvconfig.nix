{ config, ... }:
let 
  inherit (config.colorscheme) colors;
in {
  home.file = {
    "${config.xdg.configHome}/Kvantum/nixos/nixos.kvconfig" = {
      text = ''
        [%General]
          author=Xendak, using Vince Liuice orchis dark
          comment=An uncomplicated theme inspired by the Materia GTK theme
          x11drag=menubar_and_primary_toolbar
          alt_mnemonic=true
          left_tabs=true
          attach_active_tab=true
          mirror_doc_tabs=true
          group_toolbar_buttons=false
          toolbar_item_spacing=0
          toolbar_interior_spacing=2
          spread_progressbar=true
          composite=true
          menu_shadow_depth=5
          spread_menuitems=true
          tooltip_shadow_depth=2
          splitter_width=1
          scroll_width=9
          scroll_arrows=false
          scroll_min_extent=60
          slider_width=2
          slider_handle_width=23
          slider_handle_length=22
          tickless_slider_handle_size=22
          center_toolbar_handle=true
          check_size=16
          textless_progressbar=false
          progressbar_thickness=2
          menubar_mouse_tracking=true
          toolbutton_style=1
          double_click=false
          translucent_windows=false
          blurring=false
          popup_blurring=true
          vertical_spin_indicators=false
          spin_button_width=24
          fill_rubberband=false
          merge_menubar_with_toolbar=true
          small_icon_size=24
          large_icon_size=32
          button_icon_size=16
          toolbar_icon_size=16
          combo_as_lineedit=true
          animate_states=false
          button_contents_shift=false
          combo_menu=true
          hide_combo_checkboxes=true
          combo_focus_rect=false
          groupbox_top_label=true
          inline_spin_indicators=true
          joined_inactive_tabs=false
          layout_spacing=6
          layout_margin=9
          scrollbar_in_view=true
          transient_scrollbar=true
          transient_groove=true
          submenu_overlap=0
          tooltip_delay=0
          tree_branch_line=true
          no_window_pattern=false
          opaque=kaffeine,kmplayer,subtitlecomposer,kdenlive,vlc,smplayer,smplayer2,avidemux,avidemux2_qt4,avidemux3_qt4,avidemux3_qt5,kamoso,QtCreator,VirtualBox,trojita,dragon,digikam
          reduce_window_opacity=0
          respect_DE=true
          scrollable_menu=false
          submenu_delay=150
          no_inactiveness=false
          reduce_menu_opacity=0
          click_behavior=0
          contrast=1.30
          dialog_button_layout=0
          intensity=1.70
          saturation=1.10
          shadowless_popup=false
          drag_from_buttons=false
          menu_blur_radius=0
          tooltip_blur_radius=0

        [GeneralColors]
          window.color=#212121
          base.color=#BG_BASE#
          alt.base.color=#BG_SOFTER1#
          button.color=#IDK#
          light.color=#FG_SOFTER_1#
          mid.light.color=#IDK#
          dark.color=#IDK#
          mid.color=#IDK#
          highlight.color=#ACCENT_1#
          inactive.highlight.color=#ACCENT_2#
          text.color=#DEFAULT_FG#
          window.text.color=#DEFAULT_FG#
          button.text.color=#DEFAULT_FG#
          disabled.text.color=#FG_SOFTER_3#
          tooltip.text.color=#FG_SOFTER_1#
          highlight.text.color=#BG_BASE#
          link.color=#ACCENT_3#
          link.visited.color=#ACCENT_3#
          progress.indicator.text.color=#ACCENT_1#

        [Hacks]
          transparent_ktitle_label=false
          transparent_dolphin_view=false
          transparent_pcmanfm_sidepane=false
          blur_translucent=true
          transparent_menutitle=false
          respect_darkness=true
          kcapacitybar_as_progressbar=true
          force_size_grip=true
          iconless_pushbutton=true
          iconless_menu=false
          disabled_icon_opacity=100
          lxqtmainmenu_iconsize=16
          normal_default_pushbutton=true
          single_top_toolbar=true
          tint_on_mouseover=0
          transparent_pcmanfm_view=false
          no_selection_tint=true
          transparent_arrow_button=true
          middle_click_scroll=false
          opaque_colors=false
          kinetic_scrolling=false
          scroll_jump_workaround=true
          centered_forms=false
          noninteger_translucency=false
          blur_only_active_window=false
          style_vertical_toolbars=false

        [PanelButtonCommand]
          frame=true
          frame.element=button
          frame.top=6
          frame.bottom=6
          frame.left=6
          frame.right=6
          interior=true
          interior.element=button
          indicator.size=8
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#
          text.shadow=0
          text.margin=4
          text.iconspacing=4
          indicator.element=arrow
          frame.expansion=0

        [PanelButtonTool]
          inherits=PanelButtonCommand
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#
          text.bold=false
          indicator.element=arrow
          indicator.size=0
          frame.expansion=0

        [ToolbarButton]
          frame=true
          frame.element=tbutton
          interior.element=tbutton
          frame.top=16
          frame.bottom=16
          frame.left=16
          frame.right=16
          indicator.element=tarrow
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#
          text.bold=false
          frame.expansion=32

        [Dock]
          inherits=PanelButtonCommand
          interior.element=dock
          frame.element=dock
          frame.top=1
          frame.bottom=1
          frame.left=1
          frame.right=1
          text.normal.color=#DEFAULT_FG#

        [DockTitle]
          inherits=PanelButtonCommand
          frame=false
          interior=false
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.bold=false

        [IndicatorSpinBox]
          inherits=PanelButtonCommand
          frame=true
          interior=true
          frame.top=2
          frame.bottom=2
          frame.left=2
          frame.right=2
          indicator.element=spin
          indicator.size=8
          text.normal.color=#DEFAULT_FG#
          text.margin.top=2
          text.margin.bottom=2
          text.margin.left=2
          text.margin.right=2

        [RadioButton]
          inherits=PanelButtonCommand
          frame=false
          interior.element=radio
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          min_width=+0.3font
          min_height=+0.3font

        [CheckBox]
          inherits=PanelButtonCommand
          frame=false
          interior.element=checkbox
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          min_width=+0.3font
          min_height=+0.3font

        [Focus]
          inherits=PanelButtonCommand
          frame=true
          frame.element=focus
          frame.top=2
          frame.bottom=2
          frame.left=2
          frame.right=2
          frame.patternsize=14

        [GenericFrame]
          inherits=PanelButtonCommand
          frame=true
          interior=false
          frame.element=common
          interior.element=common
          frame.top=1
          frame.bottom=1
          frame.left=1
          frame.right=1

        [LineEdit]
          inherits=PanelButtonCommand
          frame.element=lineedit
          interior.element=lineedit
          frame.top=6
          frame.bottom=6
          frame.left=6
          frame.right=6
          text.margin.top=2
          text.margin.bottom=2
          text.margin.left=2
          text.margin.right=2

        [ToolbarLineEdit]
          frame.element=lineedit
          interior.element=lineedit

        [DropDownButton]
          inherits=PanelButtonCommand
          indicator.element=arrow-down

        [IndicatorArrow]
          indicator.element=arrow
          indicator.size=8

        [ToolboxTab]
          inherits=PanelButtonCommand
          text.normal.color=#DEFAULT_FG#
          text.press.color=#PRESSED#
          text.focus.color=#FOCUSED#

        [Tab]
          inherits=PanelButtonCommand
          interior.element=tab
          text.margin.left=8
          text.margin.right=8
          text.margin.top=2
          text.margin.bottom=2
          frame.element=tab
          indicator.element=tab
          indicator.size=22
          frame.top=9
          frame.bottom=3
          frame.left=6
          frame.right=6
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#
          frame.expansion=0
          text.bold=false

        [TabFrame]
          inherits=PanelButtonCommand
          frame.element=tabframe
          interior.element=tabframe
          frame.top=2
          frame.bottom=2
          frame.left=2
          frame.right=2

        [TreeExpander]
          inherits=PanelButtonCommand
          indicator.size=8
          indicator.element=tree

        [HeaderSection]
          inherits=PanelButtonCommand
          interior.element=header
          frame.element=header
          frame.top=0
          frame.bottom=1
          frame.left=1
          frame.right=1
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#
          frame.expansion=0

        [SizeGrip]
          indicator.element=resize-grip

        [Toolbar]
          inherits=PanelButtonCommand
          indicator.element=toolbar
          indicator.size=5
          text.margin=0
          interior.element=menubar
          frame.element=menubar
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#
          frame.left=0
          frame.right=0
          frame.top=0
          frame.bottom=4
          frame.expansion=0

        [Slider]
          inherits=PanelButtonCommand
          frame.element=slider
          focusFrame=true
          interior.element=slider
          frame.top=3
          frame.bottom=3
          frame.left=3
          frame.right=3

        [SliderCursor]
          inherits=PanelButtonCommand
          frame=false
          interior.element=slidercursor

        [Progressbar]
          inherits=PanelButtonCommand
          frame.element=progress
          interior.element=progress
          text.margin=0
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#
          text.bold=false
          frame.expansion=8

        [ProgressbarContents]
          inherits=PanelButtonCommand
          frame=true
          frame.element=progress-pattern
          interior.element=progress-pattern

        [ItemView]
          inherits=PanelButtonCommand
          text.margin=0
          frame.element=itemview
          interior.element=itemview
          frame.top=2
          frame.bottom=2
          frame.left=2
          frame.right=2
          text.margin.top=2
          text.margin.bottom=2
          text.margin.left=4
          text.margin.right=4
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#
          min_width=+0.3font
          min_height=+0.3font
          frame.expansion=0

        [Splitter]
          indicator.size=48

        [Scrollbar]
          inherits=PanelButtonCommand
          indicator.element=arrow
          indicator.size=12

        [ScrollbarSlider]
          inherits=PanelButtonCommand
          frame.element=scrollbarslider
          interior=false
          frame.left=5
          frame.right=5
          frame.top=5
          frame.bottom=5
          indicator.element=grip
          indicator.size=12

        [ScrollbarGroove]
          inherits=PanelButtonCommand
          interior=false
          frame=false

        [Menu]
          inherits=PanelButtonCommand
          frame.top=10
          frame.bottom=10
          frame.left=10
          frame.right=10
          frame.element=menu
          interior.element=menu
          text.normal.color=#DEFAULT_FG#
          text.shadow=false
          frame.expansion=0
          text.bold=false

        [MenuItem]
          inherits=PanelButtonCommand
          frame=true
          frame.element=menuitem
          interior.element=menuitem
          indicator.element=menuitem
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.margin.top=3
          text.margin.bottom=3
          text.margin.left=6
          text.margin.right=6
          frame.top=0
          frame.bottom=0
          frame.left=6
          frame.right=6
          text.bold=false
          frame.expansion=0

        [MenuBar]
          inherits=PanelButtonCommand
          frame.element=menubar
          interior.element=menubar
          frame.bottom=0
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#
          frame.expansion=0
          text.bold=false

        [MenuBarItem]
          inherits=PanelButtonCommand
          interior=true
          interior.element=menubaritem
          frame.element=menubaritem
          frame.top=2
          frame.bottom=2
          frame.left=2
          frame.right=2
          text.margin.left=4
          text.margin.right=4
          text.margin.top=0
          text.margin.bottom=0
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#
          text.bold=false
          min_width=+0.3font
          min_height=+0.3font
          frame.expansion=0

        [TitleBar]
          inherits=PanelButtonCommand
          frame=false
          text.margin.top=2
          text.margin.bottom=2
          text.margin.left=2
          text.margin.right=2
          interior.element=titlebar
          indicator.size=16
          indicator.element=mdi
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.bold=false
          text.italic=true
          frame.expansion=0

        [ComboBox]
          inherits=PanelButtonCommand
          frame.element=combo
          interior.element=combo
          frame.top=6
          frame.bottom=6
          frame.left=6
          frame.right=6
          text.margin.top=2
          text.margin.bottom=2
          text.margin.left=2
          text.margin.right=2
          text.focus.color=#FOCUSED#
          text.press.color=#PRESSED#
          text.toggle.color=#TOGGLE#

        [GroupBox]
          inherits=GenericFrame
          frame=false
          text.shadow=0
          text.margin=0
          text.normal.color=#DEFAULT_FG#
          text.focus.color=#FOCUSED#
          text.bold=false
          frame.expansion=0

        [TabBarFrame]
          inherits=GenericFrame
          frame=true
          frame.element=tabBarFrame
          interior=false
          frame.top=2
          frame.bottom=2
          frame.left=2
          frame.right=2

        [ToolTip]
          inherits=GenericFrame
          frame.top=6
          frame.bottom=6
          frame.left=6
          frame.right=6
          interior=true
          text.shadow=0
          text.margin=0
          interior.element=tooltip
          frame.element=tooltip
          frame.expansion=6

        [StatusBar]
          inherits=GenericFrame
          frame=false
          interior=false

        [Window]
          interior=true
          interior.element=window
          frame=true
          frame.element=window
          frame.bottom=10
          frame.top=10

      '';
    };
  };
}