#!/bin/zsh

# Define the source directory for your dotfiles
DOTFILES_DIR="$HOME/Repos/dot-files"

# Define the target directory for ~/.config files
CONFIG_DIR="$HOME/.config"

# --- Files to be linked directly to ~ ---
SHELL_DOTFILES=(
    ".bashrc"
    ".bash_profile"
    ".inputrc"
    ".profile"
    ".zshrc"
    ".gitconfig"
    ".Xresources"
)

# --- Directories/Files to be linked to ~/.config ---
CONFIG_DOTFILES=(
    "alacritty"
    "btop"
    "codeblocks"
    "dunst"
    "emacs"
    "flameshot"
    "font-manager"
    "fontconfig"
    "geany"
    "gtk-2.0"
    "gtk-3.0"
    "gtk-4.0"
    "htop"
    "i3"
    "latexindent"
    "mpv"
    "neofetch"
    "nitrogen"
    "nnn"
    "nvim"
    "picom"
    "polybar"
    "qt5ct"
    "qt6ct"
    "ranger"
    "rofi"
    "smplayer"
    "tex-fmt"
    "texstudio"
    "Thunar"
    "vlc"
    "zathura"
    "starship"
)

echo "Starting dotfile linking process (Zsh version)..."

# Create ~/.config if it doesn't exist
if [[ ! -d "$CONFIG_DIR" ]]; then
    echo "Creating $CONFIG_DIR..."
    mkdir -p "$CONFIG_DIR"
fi

# Link shell dotfiles to ~
echo -e "\nLinking shell configuration files to $HOME/..."
for file in "${SHELL_DOTFILES[@]}"; do
    SOURCE_PATH="$DOTFILES_DIR/$file"
    TARGET_PATH="$HOME/$file"
    if [[ -e "$SOURCE_PATH" ]]; then
        if [[ -L "$TARGET_PATH" ]]; then
            echo "Removing existing symlink: $TARGET_PATH"
            rm "$TARGET_PATH"
        elif [[ -e "$TARGET_PATH" ]]; then
            echo "Backing up existing file: $TARGET_PATH to ${TARGET_PATH}.bak"
            mv "$TARGET_PATH" "${TARGET_PATH}.bak"
        fi
        echo "Linking $SOURCE_PATH to $TARGET_PATH"
        ln -sf "$SOURCE_PATH" "$TARGET_PATH"
    else
        echo "Warning: Source file not found: $SOURCE_PATH"
    fi
done

# Link config dotfiles to ~/.config
echo -e "\nLinking application configuration files to $CONFIG_DIR/..."
for dir_or_file in "${CONFIG_DOTFILES[@]}"; do
    SOURCE_PATH="$DOTFILES_DIR/$dir_or_file"
    TARGET_PATH="$CONFIG_DIR/$dir_or_file"
    if [[ -e "$SOURCE_PATH" ]]; then
        if [[ -L "$TARGET_PATH" ]]; then
            echo "Removing existing symlink: $TARGET_PATH"
            rm "$TARGET_PATH"
        elif [[ -e "$TARGET_PATH" ]]; then
            echo "Backing up existing: $TARGET_PATH to ${TARGET_PATH}.bak"
            mv "$TARGET_PATH" "${TARGET_PATH}.bak"
        fi
        echo "Linking $SOURCE_PATH to $TARGET_PATH"
        ln -sf "$SOURCE_PATH" "$TARGET_PATH"
    else
        echo "Warning: Source not found for $dir_or_file: $SOURCE_PATH"
    fi
done

echo -e "\nDotfile linking process complete."
