let
  snow = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFbcunaQdK1AAI/Yre6HIKOtLQcJRzzuEINSkpH1eCj/ root@Snow";
  dew = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOeSiZ3xn1RH1JURg3zmq2qEOspQOewGIwfQ8QGGETdY root@Dew";

  user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINWQXNYT4TSaRMLFfc+V99dEC0DeoW6ed1e/cfi5g3Th xendak@nixos";
  # recovery = "";
  systems = [
    snow
    dew
  ];
  users = [
    user
  ];
  all = users ++ systems;
in
{
  "pw.age".publicKeys = systems;
  "github-token.age".publicKeys = all;
  "steamgriddb.age".publicKeys = all;
  "gemini-api-key.age".publicKeys = all;
}

# ssh-keyscan localhost
# add the new system key
# agenix --rekey
# commit

# add a line for the new secret file
# agenix -e new-secret.age
# save the file

# edit key with agenix
# agenix -e file.age
