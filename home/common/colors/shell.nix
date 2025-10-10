{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  # buildInputs are the packages available in your shell
  buildInputs = [
    # The Python 3 interpreter
    pkgs.python3

    # The Pillow library, accessed through the python3 package set
    pkgs.python3.pkgs.pillow

    # Bonus: A font package, so your script can easily find a .ttf file
    pkgs.liberation_ttf
  ];
}
