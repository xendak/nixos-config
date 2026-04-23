{ pkgs, ... }:
{
  boot.initrd.systemd.services.persist-machine-id = {
    description = "persist machine-id";
    path = [ pkgs.coreutils ];
    wantedBy = [ "initrd.target" ];
    after = [ "sysroot.mount" ];
    before = [ "initrd-switch-root.target" ];
    unitConfig.DefaultDependencies = "no";
    serviceConfig.Type = "oneshot";
    script = ''
      mkdir -p /sysroot/etc
      ln -snfT /persist/etc/machine-id /sysroot/etc/machine-id
    '';
  };
}
