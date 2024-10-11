export QT_QPA_PLATFORMTHEME=qt5ct

# Source my bashrc.
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME"/.bashrc ]; then
        source "$HOME"/.bashrc
    fi
fi

# Set PATH so it includes user's private executables.
if [ -d "$HOME"/.bin ]; then
    PATH=$PATH:"$HOME"/.bin
fi

# This is another possible location for user-specific binaries.
if [ -d "$HOME"/.local/bin ]; then
    PATH=$PATH:"$HOME"/.local/bin
fi
