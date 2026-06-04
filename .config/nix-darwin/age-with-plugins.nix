# age wrapped with plugins on PATH
#
# age discovers plugins by looking for executables named `age-plugin-*` in PATH.
# This wrapper ensures age-plugin-se is available when running age commands.

{ pkgs }:

pkgs.symlinkJoin {
  name = "age-with-plugins";
  paths = [ pkgs.age ];
  nativeBuildInputs = [ pkgs.makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/age \
      --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.age-plugin-se ]}
    wrapProgram $out/bin/age-keygen \
      --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.age-plugin-se ]}
  '';
  meta = pkgs.age.meta // {
    description = "age encryption tool with plugins (age-plugin-se)";
  };
}
