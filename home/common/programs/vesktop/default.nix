{
  pkgs,
  config,
  ...
}: let
  inherit (config.colorscheme) colors;
in {
  home.packages = [
    (pkgs.vesktop.overrideAttrs (old: {
      patches = (old.patches or []) ++ [./__readonlyFix.patch];
      postFixup = ''
        wrapProgram $out/bin/vesktop \
          --add-flags "--enable-features=UseOzonePlatform --ozone-platform=wayland --enable-accelerated-mjpeg-decode --enable-accelerated-video --ignore-gpu-blacklist --enable-native-gpu-memory-buffers --enable-gpu-rasterization --enable-gpu --enable-features=WebRTCPipeWireCapturer --enable-wayland-ime"
      '';
    }))
  ];

  services.arrpc.enable = true;

  home.persistence."/persist/home/${config.home.username}".directories = [".config/vesktop"];

  xdg.configFile."vesktop/settings.json".text = builtins.toJSON {
    discordBranch = "canary";
    firstLaunch = false;
    minimizeToTray = false;
    arRPC = "on";
    splashColor = "rgb(219, 222, 225)";
    splashBackground = "rgb(49, 51, 56)";
    enableMenu = false;
    staticTitle = false;
  };

  xdg.configFile."vesktop/settings/quickCss.css".source =
    pkgs.writeText "quickCss.css"
    ''
      @import url('https://refact0r.github.io/midnight-discord/midnight.css');
      :root {
        --spacing: 12px;
        --roundness: 16px;
        --font: 'Sofia Pro';
        --font-code: monospace;
        --online-indicator: var(--text-2);
        --moon-icon: block;
        --discord-icon: none;
        --accent-1: #${colors.base0D};
        --accent-2: #${colors.base08};
        --accent-3: #${colors.base0B}60;
        --accent-4: #${colors.base02};
        --accent-5: #${colors.base00};
        --mention: #${colors.base01}80;
        --mention-hover: #${colors.base01}c0;
        --text-1: #${colors.base07};
        --text-2: #${colors.base0B};
        --text-3: #${colors.base05};
        --text-4: #${colors.base05}c0;
        --text-5: #${colors.base03};
        --bg-1: #${colors.base02};
        --bg-2: #${colors.base0C}90;
        --bg-3:  #${colors.base01};
        --bg-4: #${colors.base00};
        --hover: #${colors.base0C}20;
        --active: #${colors.base0C}60;
        --message-hover: #${colors.base05}20;
        --text-0: var(--text-3)
        --list-item-transition: 0.2s ease;
        --unread-bar-transition: 0.2s ease;
        --moon-spin-transition: 0.4s ease;
        --icon-spin-transition: 1s ease;
        --roundness-xl: 22px;
        --roundness-l: 20px;
        --roundness-m: 16px;
        --roundness-s: 12px;
        --roundness-xs: 10px;
        --roundness-xxs: 8px;
        --discord-icon: none;
        --moon-icon: block;
        --moon-icon-url: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 27 27' width='24' height='24'%3E%3Cpath fill='currentColor' d='M0 0h7v1H6v1H5v1H4v1H3v1H2v1h5v1H0V6h1V5h1V4h1V3h1V2h1V1H0m13 2h5v1h-1v1h-1v1h-1v1h3v1h-5V7h1V6h1V5h1V4h-3m8 5h1v5h1v-1h1v1h-1v1h1v-1h1v1h-1v3h-1v1h-2v1h-1v1h1v-1h2v-1h1v2h-1v1h-2v1h-1v-1h-1v1h-6v-1h-1v-1h-1v-2h1v1h2v1h3v1h1v-1h-1v-1h-3v-1h-4v-4h1v-2h1v-1h1v-1h1v2h1v1h1v-1h1v1h-1v1h2v-2h1v-2h1v-1h1M8 14h2v1H9v4h1v2h1v1h1v1h1v1h4v1h-6v-1H5v-1H4v-5h1v-1h1v-2h2m17 3h1v3h-1v1h-1v1h-1v2h-2v-2h2v-1h1v-1h1m1 0h1v3h-1v1h-2v-1h1v-1h1'%3E%3C/path%3E%3C/svg%3E");
        --moon-icon-size: cover;
        --login-bg-filter: none;
        --green-to-accent-3-filter: none;
        --blurple-to-accent-3-filter: none;
      }
      .wrapper__8436d:hover > .childWrapper_a6ce15::before,
      .wrapper__8436d.selected_ae80f7 > .childWrapper_a6ce15::before {
          background: var(--text-0);
          transform: rotate(-0deg) scale(0.8);
      }
    '';

  xdg.configFile."vesktop/settings/settings.json".text = builtins.toJSON {
    notifyAboutUpdates = false;
    autoUpdate = false;
    autoUpdateNotification = false;
    useQuickCss = true;
    enabledThemes = [];
    enableReactDevtools = false;
    frameless = false;
    transparent = false;
    winCtrlQ = false;
    disableMinSize = false;
    winNativeTitleBar = false;
    plugins = {
      BadgeAPI = {
        enabled = true;
      };
      CommandsAPI = {
        enabled = true;
      };
      ContextMenuAPI = {
        enabled = true;
      };
      MemberListDecoratorsAPI = {
        enabled = false;
      };
      MessageAccessoriesAPI = {
        enabled = true;
      };
      MessageDecorationsAPI = {
        enabled = false;
      };
      MessageEventsAPI = {
        enabled = true;
      };
      MessagePopoverAPI = {
        enabled = true;
      };
      NoticesAPI = {
        enabled = true;
      };
      ShikiCodeblocks = {
        enabled = true;
        theme = "https://raw.githubusercontent.com/sainnhe/gruvbox-material-vscode/master/themes/gruvbox-material-dark.json";
        tryHljs = "SECONDARY";
        useDevIcon = "COLOR";
        bgOpacity = 100;
      };
      ServerListAPI = {
        enabled = false;
      };
      SettingsStoreAPI = {
        enabled = false;
      };
      NoTrack = {
        enabled = true;
      };
      Settings = {
        enabled = true;
        settingsLocation = "aboveActivity";
      };
      AlwaysAnimate = {
        enabled = false;
      };
      AlwaysTrust = {
        enabled = false;
      };
      AnonymiseFileNames = {
        enabled = true;
        method = 2;
        randomisedLength = 7;
        consistent = "image";
      };
      "WebRichPresence (arRPC)" = {
        enabled = true;
      };
      BANger = {
        enabled = false;
      };
      BetterFolders = {
        enabled = false;
      };
      BetterGifAltText = {
        enabled = false;
      };
      BetterNotesBox = {
        enabled = false;
      };
      BetterRoleDot = {
        enabled = false;
      };
      BetterUploadButton = {
        enabled = false;
      };
      BiggerStreamPreview = {
        enabled = false;
      };
      BlurNSFW = {
        enabled = false;
      };
      CallTimer = {
        enabled = false;
      };
      ClearURLs = {
        enabled = false;
      };
      ColorSighted = {
        enabled = false;
      };
      ConsoleShortcuts = {
        enabled = false;
      };
      CrashHandler = {
        enabled = true;
      };
      CustomRPC = {
        enabled = false;
        type = 0;
        timestampMode = 0;
      };
      DisableDMCallIdle = {
        enabled = false;
      };
      EmoteCloner = {
        enabled = false;
      };
      Experiments = {
        enabled = false;
      };
      F8Break = {
        enabled = false;
      };
      FakeNitro = {
        enabled = true;
        enableEmojiBypass = true;
        enableStickerBypass = true;
        enableStreamQualityBypass = true;
        transformStickers = true;
        transformEmojis = true;
        transformCompoundSentence = false;
        stickerSize = 160;
      };
      FakeProfileThemes = {
        enabled = false;
      };
      FavoriteEmojiFirst = {
        enabled = false;
      };
      FixInbox = {
        enabled = false;
      };
      ForceOwnerCrown = {
        enabled = false;
      };
      FriendInvites = {
        enabled = false;
      };
      GameActivityToggle = {
        enabled = false;
      };
      GifPaste = {
        enabled = false;
      };
      HideAttachments = {
        enabled = false;
      };
      iLoveSpam = {
        enabled = false;
      };
      IgnoreActivities = {
        enabled = false;
      };
      ImageZoom = {
        enabled = false;
      };
      InvisibleChat = {
        enabled = false;
      };
      KeepCurrentChannel = {
        enabled = false;
      };
      LastFMRichPresence = {
        enabled = false;
      };
      LoadingQuotes = {
        enabled = false;
      };
      MemberCount = {
        enabled = true;
      };
      MessageClickActions = {
        enabled = false;
      };
      MessageLinkEmbeds = {
        enabled = true;
        automodEmbeds = "never";
        listMode = "blacklist";
        idList = "";
      };
      MessageLogger = {
        enabled = true;
        deleteStyle = "text";
        ignoreBots = true;
        ignoreSelf = true;
        ignoreUsers = "";
        ignoreChannels = "";
        ignoreGuilds = "";
      };
      MessageTags = {
        enabled = false;
      };
      MoreCommands = {
        enabled = false;
      };
      MoreKaomoji = {
        enabled = true;
      };
      MoreUserTags = {
        enabled = false;
      };
      Moyai = {
        enabled = false;
      };
      MuteNewGuild = {
        enabled = false;
      };
      MutualGroupDMs = {
        enabled = false;
      };
      NoBlockedMessages = {
        enabled = true;
        ignoreBlockedMessages = true;
      };
      NoDevtoolsWarning = {
        enabled = false;
      };
      NoF1 = {
        enabled = false;
      };
      NoPendingCount = {
        enabled = false;
      };
      NoProfileThemes = {
        enabled = false;
      };
      NoReplyMention = {
        enabled = true;
        userList = "728263018567434281";
        shouldPingListed = true;
        inverseShiftReply = false;
      };
      NoScreensharePreview = {
        enabled = false;
      };
      NoUnblockToJump = {
        enabled = false;
      };
      NSFWGateBypass = {
        enabled = false;
      };
      oneko = {
        enabled = false;
      };
      OpenInApp = {
        enabled = false;
      };
      "Party mode ud83cudf89" = {
        enabled = false;
      };
      PermissionsViewer = {
        enabled = false;
      };
      petpet = {
        enabled = false;
      };
      PinDMs = {
        enabled = false;
      };
      PlainFolderIcon = {
        enabled = false;
      };
      PlatformIndicators = {
        enabled = false;
      };
      PronounDB = {
        enabled = false;
      };
      QuickMention = {
        enabled = false;
      };
      QuickReply = {
        enabled = false;
      };
      ReactErrorDecoder = {
        enabled = false;
      };
      ReadAllNotificationsButton = {
        enabled = false;
      };
      RelationshipNotifier = {
        enabled = false;
      };
      RevealAllSpoilers = {
        enabled = false;
      };
      ReverseImageSearch = {
        enabled = false;
      };
      ReviewDB = {
        enabled = false;
      };
      RoleColorEverywhere = {
        enabled = false;
      };
      SearchReply = {
        enabled = false;
      };
      SendTimestamps = {
        enabled = false;
      };
      ServerListIndicators = {
        enabled = false;
      };
      ShowAllMessageButtons = {
        enabled = false;
      };
      ShowConnections = {
        enabled = true;
      };
      ShowHiddenChannels = {
        enabled = false;
      };
      ShowMeYourName = {
        enabled = false;
      };
      SilentMessageToggle = {
        enabled = false;
      };
      SilentTyping = {
        enabled = false;
      };
      SortFriendRequests = {
        enabled = false;
      };
      SpotifyControls = {
        enabled = false;
      };
      SpotifyCrack = {
        enabled = true;
      };
      SpotifyShareCommands = {
        enabled = false;
      };
      StartupTimings = {
        enabled = false;
      };
      SupportHelper = {
        enabled = true;
      };
      TextReplace = {
        enabled = false;
      };
      TimeBarAllActivities = {
        enabled = false;
      };
      Translate = {
        enabled = true;
        autoTranslate = false;
      };
      TypingIndicator = {
        enabled = false;
      };
      TypingTweaks = {
        enabled = false;
      };
      Unindent = {
        enabled = true;
      };
      UnsuppressEmbeds = {
        enabled = false;
      };
      UrbanDictionary = {
        enabled = false;
      };
      UserVoiceShow = {
        enabled = false;
      };
      USRBG = {
        enabled = false;
      };
      UwUifier = {
        enabled = false;
      };
      ValidUser = {
        enabled = true;
      };
      VoiceChatDoubleClick = {
        enabled = true;
      };
      VcNarrator = {
        enabled = false;
      };
      VencordToolbox = {
        enabled = false;
      };
      ViewIcons = {
        enabled = false;
      };
      ViewRaw = {
        enabled = false;
      };
      WebContextMenus = {
        enabled = false;
        addBack = true;
      };
      GreetStickerPicker = {
        enabled = false;
      };
      WhoReacted = {
        enabled = false;
      };
      Wikisearch = {
        enabled = false;
      };
      FavoriteGifSearch = {
        enabled = false;
      };
      SecretRingToneEnabler = {
        enabled = false;
      };
      VoiceMessages = {
        enabled = true;
        noiseSuppression = true;
        echoCancellation = true;
      };
      "AI Noise Suppression" = {
        enabled = true;
        isEnabled = true;
      };
      CopyUserURLs = {
        enabled = false;
      };
      NormalizeMessageLinks = {
        enabled = false;
      };
      PreviewMessage = {
        enabled = false;
      };
      Dearrow = {
        enabled = false;
      };
      FixSpotifyEmbeds = {
        enabled = false;
      };
      OnePingPerDM = {
        enabled = false;
      };
      PermissionFreeWill = {
        enabled = false;
      };
      PictureInPicture = {
        enabled = false;
      };
      ServerProfile = {
        enabled = false;
      };
      ShowTimeouts = {
        enabled = false;
      };
      ThemeAttributes = {
        enabled = false;
      };
      WebKeybinds = {
        enabled = true;
      };
      NoMosaic = {
        enabled = false;
      };
      NoTypingAnimation = {
        enabled = false;
      };
      ClientTheme = {
        enabled = false;
      };
      FixImagesQuality = {
        enabled = false;
      };
      SuperReactionTweaks = {
        enabled = false;
      };
    };
    notifications = {
      timeout = 5000;
      position = "bottom-right";
      useNative = "always";
      logLimit = 50;
    };
    cloud = {
      authenticated = false;
      url = "https://api.vencord.dev/";
      settingsSync = false;
      settingsSyncVersion = 1682768329526;
    };
    macosTranslucency = false;
  };
}
