# get correct partition first :)
mkfs.btrfs -L FLAKE /dev/sda2
mount /dev/sda2 /mnt

btrfs subvolume create /mnt/root
btrfs subvolume create /mnt/nix
btrfs subvolume create /mnt/persist
btrfs subvolume create /mnt/log

btrfs subvolume snapshot -r /mnt/root /mnt/root-blank

umount /mnt
mount -o subvol=root,compress=zstd,noatime /dev/sda2 /mnt
mkdir -p /mnt/{nix,persist,var/log,boot}
mount -o subvol=nix,compress=zstd,noatime /dev/sda2 /mnt/nix
mount -o subvol=persist,compress=zstd,noatime /dev/sda2 /mnt/persist
mount -o subvol=log,compress=zstd,noatime /dev/sda2 /mnt/var/log

mkfs.vfat -n SNOW /dev/sda1
mount /dev/disk/by-label/SNOW /mnt/boot

