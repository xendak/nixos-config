let
  flakes = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJgDWOC2pl8MY/RZJDTsJv1V6H4SjXt6x+UCu4hQFSbs";
  drops = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINJ3y03wBlzSp0tooMOILl7hGxIYqxT6IjOZWLzDpZHP";
  users = [ flakes drops ];

  snow = "";
  dew = "";
  systems = [ snow dew ];
in
  {
    "dewdrops.age".publicKeys = [ dew drops ];
    "snowflakes.age".publicKeys = [ snow flakes ];
  }
