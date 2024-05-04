let
  # snow = "";
  dew = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOeSiZ3xn1RH1JURg3zmq2qEOspQOewGIwfQ8QGGETdY root@Dew";
  # recovery = "";
  systems = [ dew ];
in
{
  "pw.age".publicKeys = systems;
}
