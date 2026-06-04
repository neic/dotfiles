# browserpass with passage (age encryption) support
# Based on https://github.com/wlritchi/env/commit/1262f3229b55f5baaa3f726c75585b61df7e05cf

{ pkgs, age-with-plugins }:

let
  ageBin = "${age-with-plugins}/bin/age";
in

pkgs.browserpass.overrideAttrs (oldAttrs: {
  postPatch = (oldAttrs.postPatch or "") + ''
    substituteInPlace helpers/helpers.go \
      --replace-fail \
      'func GpgEncryptFile' \
      'func AgeDecryptFile(filePath string, identityPath string) (string, error) {
    passwordFile, err := os.Open(filePath)
    if err != nil {
      return "", err
    }
    defer passwordFile.Close()
    var stdout, stderr bytes.Buffer
    ageOptions := []string{"--decrypt", "--identity", identityPath}
    cmd := exec.Command("${ageBin}", ageOptions...)
    cmd.Stdin = passwordFile
    cmd.Stdout = &stdout
    cmd.Stderr = &stderr
    if err := cmd.Run(); err != nil {
      return "", fmt.Errorf("Error: %s, Stderr: %s", err.Error(), stderr.String())
    }
    return stdout.String(), nil
    }
    func GpgEncryptFile'

    substituteInPlace request/fetch.go \
      --replace-fail \
      'HasSuffix(request.File, ".gpg")' \
      'HasSuffix(request.File, ".age")'

    substituteInPlace request/fetch.go \
      --replace-fail \
      "does not have the expected '.gpg' extension" \
      "does not have the expected '.age' extension"

    substituteInPlace request/fetch.go \
      --replace-fail \
      'responseData.Contents, err = helpers.GpgDecryptFile(filepath.Join(store.Path, request.File), gpgPath)' \
      'passwordFilePath := filepath.Join(store.Path, "store", request.File)
    identityFilePath := filepath.Join(store.Path, "identities")
    responseData.Contents, err = helpers.AgeDecryptFile(passwordFilePath, identityFilePath)'

    substituteInPlace request/list.go \
      --replace-fail \
      'filepath.Join(store.Path, "/**/*.gpg")' \
      'filepath.Join(store.Path, "store", "/**/*.age")'

    substituteInPlace request/list.go \
      --replace-fail \
      'relativePath, err := filepath.Rel(store.Path, file)' \
      'relativePath, err := filepath.Rel(filepath.Join(store.Path, "store"), file)'
  '';
})
