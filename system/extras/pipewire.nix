{
  services.pipewire.wireplumber.enable = true;
  hardware.pulseaudio.enable = false;
  services.pipewire = {
    #media-session.enable = true;
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };
}
