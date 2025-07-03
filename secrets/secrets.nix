let
  snow = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFbcunaQdK1AAI/Yre6HIKOtLQcJRzzuEINSkpH1eCj/ root@Snow";
  dew = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOeSiZ3xn1RH1JURg3zmq2qEOspQOewGIwfQ8QGGETdY root@Dew";
  # recovery = "";
  systems = [
    snow
    dew
  ];
in
{
  "pw.age".publicKeys = systems;
  "gemini-api-key.age".publicKeys = systems;
}
