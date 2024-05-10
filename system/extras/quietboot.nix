{
  pkgs,
  config,
  ...
}: {
  boot = {
    loader.timeout = 5;
    kernelParams = [
      "quiet"
      "splash"
      "boot.shell_on_fail"
      "i915.fastboot=1"
      "loglevel=3"
    ];
    consoleLogLevel = 0;
    initrd.verbose = false;
  };
}
