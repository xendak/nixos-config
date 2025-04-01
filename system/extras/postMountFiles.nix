{lib, ...}: {
  boot.initrd.postMountCommands = lib.mkBefore ''
    ln -snfT /persist/etc/machine-id /etc/machine-id
  '';
}
