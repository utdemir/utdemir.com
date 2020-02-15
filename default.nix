let
sources = import ./nix/sources.nix;
pkgs = import sources.nixpkgs {};
in
pkgs.stdenv.mkDerivation {
  name = "blog";
  buildInputs = with pkgs; [ bash pandoc jq findutils rsync linkchecker ];
  src = pkgs.nix-gitignore.gitignoreSource [ "publish.sh" ] ./.;
  phases = "unpackPhase buildPhase";
  buildPhase = ''
    bash ./generate.sh
    mv _out "$out"
  '';
}
