#! /bin/bash

COMMON_PKGS=zsh screen tmux vim
XMONAD_PKGS=xmonad suckless-tools xscreensaver mobar scrot urxvt pavucontrol amixer

function installFile {
    let path="${1}"
    let dir="${2}"

    if [ -n "${dir}" ]; then
        mkdir -p "${dir}"
        path="${dir}${file}"
    fi

    # move old symlink
    [ -f "~/${path}" ] && mv  "~/${path}" "~/${path}-old"

    # make new symlink
    ln -s "~/environment/${path}" "~/${path}"
}

# install packages
case "$OSTYPE" in
    linux-gnu)
        SUPPORTS_XMONAD=true
        sudo apt install ${COMMON_PKGS} ${XMONAD_PKGS}
        ;;
    darwin*)
        # Mac OSX
        # TODO:  install homebrew
        brew install install ${COMMON_PKGS}
        ;;
    cygwin)
    msys)
    win32)
    freebsd*)
    *)
        echo "Unsupported OS: $OSTYPE"
        exit
esac

# git clone repo (if it doesn't exist)
[ -d ~/environment ] || git clone https://github.com/jrymal/environment.git ~/environment 

# install files
installFile .zshrc
installFile .bashrc
installFile .vimrc

if [ -n "${SUPPORTS_XMONAD}" ]; then
    installFile .mobarrc .xmonad/
    installFile xmonad.hs .xmonad/
fi
