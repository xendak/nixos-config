{ pkgs, ... }:
{
  xdg.configFile."vesktop/settings/quickCss.css".source = pkgs.writeText "quickCss.css" ''
    /**
     * @name Rose Pine
     * @description A dark, rounded discord theme.
     * @author refact0r, anubis, PsychotherapistSam
     * @version 1.6.2
     * @invite nz87hXyvcy
     * @website https://github.com/refact0r/midnight-discord
     * @source https://github.com/refact0r/midnight-discord/blob/master/flavors/midnight-rose-pine.theme.css
     * @authorId 508863359777505290
     * @authorLink https://www.refact0r.dev
    */

    /* IMPORTANT: make sure to enable dark mode in discord settings for the theme to apply properly!!! */

    /* @import url('https://refact0r.github.io/midnight-discord/midnight.css'); */
    @import url('https://refact0r.github.io/midnight-discord/build/midnight.css');

    /* customize things here */
    body {
    	/* font, change to 'gg sans' for default discord font */
    	--font: 'Sofia Pro';

      /* sizes */
      --gap: 12px; /* spacing between panels */
      --divider-thickness: 4px; /* thickness of unread messages divider and highlighted message borders */

      /* animation/transition options */
      --animations: on; /* turn off to disable all midnight animations/transitions */
      --list-item-transition: 0.2s ease; /* transition for list items */
      --dms-icon-svg-transition: 0.4s ease; /* transition for the dms icon */

      /* top bar options */
      --move-top-bar-buttons: on; /* turn on to move inbox button to the server list (recommend setting top bar height to 24px) */
      --custom-app-top-bar-height: 12px; /* height of the titlebar/top bar (default is 36px)*/

      /* window controls */
      --custom-window-controls: on; /* turn off to use discord default window controls */
      --window-control-size: 36px; /* size of custom window controls */

      /* dms button icon options */
      --dms-icon: on; /* set to default to use discord icon, on to use custom icon, off to disable completely */
      --dms-icon-svg-url: url('https://upload.wikimedia.org/wikipedia/commons/c/c4/Font_Awesome_5_solid_moon.svg'); /* icon svg url. MUST BE A SVG. */
      --dms-icon-svg-size: 90%; /* size of the svg (css mask-size) */
      --dms-icon-color-before: var(--icon-secondary); /* normal icon color */
      --dms-icon-color-after: var(--white); /* icon color when button is hovered/selected */

      /* dms button background options */
      --dms-background: off; /* off to disable, image to use a background image, color to use a custom color/gradient */
      --dms-background-image-url: url(""); /* url of the background image */
      --dms-background-image-size: cover; /* size of the background image (css background-size) */
      --dms-background-color: linear-gradient(70deg, var(--blue-2), var(--purple-2), var(--red-2)); /* fixed color/gradient (css background) */

      /* background image options */
      --background-image: off; /* turn on to use a background image */
      --background-image-url: url(""); /* url of the background image */

      /* transparency/blur options */
      /* NOTE: TO USE TRANSPARENCY/BLUR, YOU MUST HAVE TRANSPARENT BG COLORS. FOR EXAMPLE: --bg-4: hsla(220, 15%, 10%, 0.7); */
      --transparency-tweaks: off; /* turn on to remove some elements for better transparency */
      --remove-bg-layer: off; /* turn on to remove the base --bg-3 layer for use with window transparency (WILL OVERRIDE BACKGROUND IMAGE) */
      --panel-blur: off; /* turn on to blur the background of panels */
      --blur-amount: 12px; /* amount of blur */
      --bg-floating: var(--bg-3); /* you can set this to a more opaque color if floating panels look too transparent */

      /* other options */
      --small-user-panel: off; /* turn on to make the user panel smaller like in old discord */
    }

    :root {

    	/* text colors */
    	--text-0: var(--bg-4); /* text on colored elements */
    	--text-1: hsl(29, 50%, 91%); /* other normally white text */
    	--text-2: hsl(31, 22%, 62%); /* headings and important text */
    	--text-3: hsl(29, 51%, 91%); /* normal text */
    	--text-4: hsl(29, 15%, 61%); /* icon buttons and channels */
    	--text-5: hsl(29, 12%, 47%); /* muted channels/chats and timestamps */

    	/* background and dark colors */
    	--bg-1: #242424; /* dark buttons when clicked */
    	--bg-2: #282828; /* dark buttons */
    	--bg-3: #222222; /* spacing, secondary elements */
    	--bg-4: #282828; /* main background color */
    	--hover: #242424; /* buttons when hovered */
    	--active: #222222; /* channels and buttons when clicked or selected */
    	--message-hover: #202020; /* messages when hovered */

    /* accent colors */
      --accent-1: var(--blue-1); /* links and other accent text */
      --accent-2: var(--blue-2); /* small accent elements */
      --accent-3: var(--blue-3); /* accent buttons */
      --accent-4: var(--blue-4); /* accent buttons when hovered */
      --accent-5: var(--blue-5); /* accent buttons when clicked */
      --accent-new: var(--accent-2); /* stuff that's normally red like mute/deafen buttons */

      --mention: linear-gradient(to right, color-mix(in hsl, var(--blue-2), transparent 90%) 40%, transparent); /* background of messages that mention you */
      --mention-hover: linear-gradient(to right, color-mix(in hsl, var(--blue-2), transparent 95%) 40%, transparent); /* background of messages that mention you when hovered */
      --reply: linear-gradient(to right, color-mix(in hsl, var(--text-3), transparent 90%) 40%, transparent); /* background of messages that reply to you */
      --reply-hover: linear-gradient(to right, color-mix(in hsl, var(--text-3), transparent 95%) 40%, transparent); /* background of messages that reply to you when hovered */

      /* status indicator colors */
      --online: var(--green-2); /* change to #43a25a for default */
      --dnd: var(--red-2); /* change to #d83a42 for default */
      --idle: var(--yellow-2); /* change to #ca9654 for default */
      --streaming: var(--purple-2); /* change to #593695 for default */
      --offline: var(--text-4); /* change to #83838b for default offline color */

      /* border colors */
      --border-light: hsl(230, 20%, 40%, 0.1); /* light border color */
      --border: hsl(230, 20%, 40%, 0.2); /* normal border color */
      --button-border: hsl(0, 0%, 100%, 0.1); /* neutral border color of buttons */

      /* base colors */
      --red-1: oklch(76% 0.12 0);
      --red-2: oklch(70% 0.12 0);
      --red-3: oklch(64% 0.12 0);
      --red-4: oklch(58% 0.12 0);
      --red-5: oklch(52% 0.12 0);

      --green-1: oklch(76% 0.11 170);
      --green-2: oklch(70% 0.11 170);
      --green-3: oklch(64% 0.11 170);
      --green-4: oklch(58% 0.11 170);
      --green-5: oklch(52% 0.11 160);

    	--blue-11: oklch(from hsl(216, 45%, 69%) calc(l -0.1) c h);
    	--blue-12: oklch(from hsl(313, 100%, 50%) calc(l -0.1) c h);
    	--blue-13: oklch(from hsl(52, 12%, 64%) calc(l -0.1) c h);
    	--blue-14: oklch(from hsl(4, 15%, 62%) calc(l -0.1) c h);
    	--blue-15: oklch(from hsl(216, 45%, 69%) calc(l -0.1) c h);


    	--blue-1: oklch(0.7 0.1 248);
      --blue-2: oklch(0.92 0.06 75);
      --blue-3: oklch(0.85 0.05 75);
      --blue-4: oklch(0.87 0.06 75);
      --blue-5: oklch(0.63 0.23 340); 


      --yellow-1: oklch(80% 0.11 90);
      --yellow-2: oklch(74% 0.11 90);
      --yellow-3: oklch(68% 0.11 90);
      --yellow-4: oklch(62% 0.11 90);
      --yellow-5: oklch(56% 0.11 90);

      --purple-1: oklch(76% 0.11 310);
      --purple-2: oklch(70% 0.11 310);
      --purple-3: oklch(64% 0.11 310);
      --purple-4: oklch(58% 0.11 310);
      --purple-5: oklch(52% 0.11 310);

    }

    .title_c38106 {
    	opacity: 0 !important;
    }

  '';
}
