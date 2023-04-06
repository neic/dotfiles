# Adds Homebrew to PATH, MANPATH, and INFOPATH.
UNAME_MACHINE="$(uname -sm)"
if [[ "${UNAME_MACHINE}" == "Darwin arm64" ]]
then
    # On ARM macOS
    eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ "${UNAME_MACHINE}" == "Darwin x86_64" ]]
then
    # On Intel macOS
    eval "$(/usr/local/bin/brew shellenv)"
fi
