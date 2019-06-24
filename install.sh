#! /bin/bash
set -e

INSTALL_FROM="${HOME}/environment"
INSTALL_TO="${HOME}"

COMMON_PKGS="zsh screen tmux vim"
XMONAD_PKGS="xmonad suckless-tools xscreensaver xmobar scrot rxvt-unicode pavucontrol alsa-utils"

function unsupportedOS {
    echo "Unsupported OS: ${1}"
    exit 1
}

function ensureDir {
    if [ ! -d "${1}" ]; then
        mkdir -p ${1}
    fi
}

function installFile {
    local path="${1}"
    local dir="${2}"

    # directory means we do something else
    if [ -n "${dir}" ]; then
        ensureDir "${INSTALL_TO}/${dir}"
        path="${dir}${path}"
    fi

    if [ ! -f "${INSTALL_FROM}/${path}" ]; then
        echo "Well that's embarrassing: ${INSTALL_FROM}/${path} doesn't exist"
        exit 1
    fi

    # remove old symlink
    test -L "${INSTALL_TO}/${path}" && rm "${INSTALL_TO}/${path}"
    
    # Check for modifications
    if [ ! -f "${INSTALL_TO}/${path}" -o -n "$(diff ${INSTALL_TO}/${path} ${INSTALL_FROM}/${path})" ]; then

        
        # move old symlink
        test -f "${INSTALL_TO}/${path}" && mv "${INSTALL_TO}/${path}" "${INSTALL_TO}/${path}-old"

        # make new symlink
        ln -s "${INSTALL_FROM}/${path}" "${INSTALL_TO}/${path}"
    fi
}

# install packages and handle OS specific functions and flags
case "$OSTYPE" in
    linux-gnu)
        SUPPORTS_XMONAD=true
        # TODO:  validate package manager
        sudo apt install ${COMMON_PKGS} ${XMONAD_PKGS}
        ;;
    darwin*)
        # Mac OSX
        # TODO:  install homebrew
        brew install ${COMMON_PKGS}
        ;;
    cygwin) 
        unsupportedOS "${OSTYPE}" ;;
    msys) 
        unsupportedOS "${OSTYPE}" ;;
    win32) 
        unsupportedOS "${OSTYPE}" ;;
    freebsd*) 
        unsupportedOS "${OSTYPE}" ;;
    *) 
        unsupportedOS "${OSTYPE}" ;;
esac

# ensure INSTALL_TO exists
ensureDir "${INSTALL_TO}"

# git clone repo (if it doesn't exist, otherwise update)
if [ -d "${INSTALL_FROM}" ]; then
    pwd=$(pwd)
    cd "${INSTALL_FROM}" 
    git checkout 
    cd "${pwd}"
else 
    git clone https://github.com/jrymal/environment.git "${INSTALL_FROM}"
fi

# install files
installFile ".zshrc"
installFile ".vimrc"

if [[ -n "${SUPPORTS_XMONAD}" ]]; then
    installFile ".mobarrc" ".xmonad/"
    installFile "xmonad.hs" ".xmonad/"
fi
