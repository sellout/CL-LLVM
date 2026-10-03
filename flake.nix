{
  description = "Common Lisp LLVM bindings development environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "aarch64-darwin" "x86_64-darwin" "aarch64-linux" "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in {
      devShells = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
          llvm = pkgs.llvmPackages_latest.llvm;
          lispDependencies = pkgs.lib.closePropagation (with pkgs.sbclPackages; [
            cffi
            cffi-grovel
            trivial-features
            cl-ppcre
            split-sequence
            trivial-shell
          ]);
          lispSourceRegistry = pkgs.lib.concatStringsSep ":"
            (map (package: "${package}//") lispDependencies);
          llvmLib = pkgs.lib.getLib llvm;
        in {
          default = pkgs.mkShell {
            packages = [
              pkgs.sbcl
              llvm
              pkgs.autoconf
              pkgs.automake
              pkgs.libtool
            ] ++ lispDependencies;

            shellHook = ''
              export CL_SOURCE_REGISTRY="${lispSourceRegistry}"
              export LLVM_CONFIG="${llvm}/bin/llvm-config"
              export LD_LIBRARY_PATH="${llvmLib}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              export DYLD_FALLBACK_LIBRARY_PATH="${llvmLib}/lib''${DYLD_FALLBACK_LIBRARY_PATH:+:$DYLD_FALLBACK_LIBRARY_PATH}"
            '';
          };
        });
    };
}
