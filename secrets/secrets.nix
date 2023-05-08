let
  flakes = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJgDWOC2pl8MY/RZJDTsJv1V6H4SjXt6x+UCu4hQFSbs";
  users = [ flakes ];
in
  {
    "flakes.age".publicKeys = [ flakes ];
  }
