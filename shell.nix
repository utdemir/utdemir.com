let
sources = import ./nix/sources.nix;
pkgs = import sources.nixpkgs {};
in
pkgs.mkShell {
  buildInputs = [ (import ./default.nix).buildInputs pkgs.nodePackages.prettier pkgs.nodePackages.netlify-cli ];
  shellHook = ''
    export PYTHONPATH=
  '';
}

