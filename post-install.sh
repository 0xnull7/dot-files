#!/bin/bash

# ==============================================================================
# Arch Linux Post-Installation Script V2 for i3wm & SDDM
#
# This script automates essential and advanced post-installation tasks on a
# fresh Arch Linux system, specifically tailored for i3wm with SDDM.
# It prioritizes robustness, idempotency, and modularity.
#
# Assumes:
# - A fresh Arch Linux.
# - i3wm and sddm are already installed.
# - The script is run by a user with sudo privileges.
#
# Features:
# - Robust error handling with immediate exit on failure.
# - Graceful cleanup on script termination or error.
# - Installation of paru (AUR helper).
# - Installation of packages from official repositories and AUR.
# - Dotfiles management using a bare Git repository.
# - Automated system maintenance (paccache, reflector).
# - ZRAM setup for performance optimization (disabling zswap).
# - UFW firewall configuration.
# - Comprehensive font installation and configuration.
# - Optional Zsh default shell setup.
# - Enhanced logging with timestamps and log levels
# - Progress indicators for long-running operations
# - Configuration file support for customization
# - Modular functions for better maintainability
# ==============================================================================

# ==============================================================================
# 1. Script Configuration and Robustness Settings
# ==============================================================================

# Strict mode: Exit immediately if a command exits with a non-zero status (-e).
# Treat unset variables as an error (-u).
# The exit status of a pipeline is the status of the last command to exit with a non-zero status (-o pipefail).
set -euo pipefail

# Define script variables
readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly CONFIG_FILE="${SCRIPT_DIR}/post-install.conf"
readonly LOG_FILE="/var/log/arch_post_install.log"
readonly TEMP_DIR=$(mktemp -d) # Create a temporary directory for building AUR packages
readonly DOTFILES_REPO_URL="https://github.com/ismailmajeeb/dot-files.git"
readonly DOTFILES_BARE_DIR="~/Repos/dot-files"
readonly DOTFILES_LOCAL_SETUP_SCRIPT="~/Repos/dot-files/Linker-Bash.sh"

# Default configuration values
DOTFILES_ENABLED=true
AUR_ENABLED=true
ZRAM_ENABLED=true
UFW_ENABLED=true
ZSH_ENABLED=true

# Check if config file exists and source it
if [[ -f "$CONFIG_FILE" ]]; then
    log "INFO" "Loading configuration from $CONFIG_FILE"
    source "$CONFIG_FILE"
fi

# Define package lists (customize these arrays to your needs)
# Packages from official Arch Linux repositories
PACMAN_PACKAGES=(
    "acpi"
    "adobe-source-code-pro-fonts"
    "adobe-source-han-sans-cn-fonts"
    "adobe-source-han-sans-jp-fonts"
    "adobe-source-han-sans-kr-fonts"
    "adobe-source-han-sans-tw-fonts"
    "adobe-source-han-serif-jp-fonts"
    "alacritty"
    "alsa-firmware"
    "alsa-utils"
    "arandr"
    "aspnet-runtime"
    "aspnet-targeting-pack"
    "atool"
    "autorandr"
    "awesome-terminal-fonts"
    "bash-completion"
    "bat"
    "bc"
    "blueman"
    "bluez"
    "bluez-utils"
    "boost"
    "brightnessctl"
    "btop"
    "chafa"
    "clang"
    "cmake"
    "cmatrix"
    "codeblocks"
    "dmenu"
    "dosfstools"
    "dotnet-sdk"
    "dunst"
    "efibootmgr"
    "emacs"
    "exa"
    "fastfetch"
    "fd"
    "feh"
    "ffmpegthumbnailer"
    "figlet"
    "flameshot"
    "font-manager"
    "fzf"
    "gcc"
    "geany"
    "geany-plugins"
    "git"
    "glfw"
    "gnome-keyring"
    "gnome-multi-writer"
    "gnome-themes-extra"
    "go"
    "gparted"
    "gst-plugin-pipewire"
    "gthumb"
    "gvfs"
    "highlight"
    "htop"
    "i3-wm"
    "i3blocks"
    "i3status"
    "imagemagick"
    "intel-media-driver"
    "intel-ucode"
    "iwd"
    "jdk21-openjdk"
    "jq"
    "kitty"
    "lazygit"
    "lib32-mesa"
    "libcaca"
    "libpulse"
    "libva-intel-driver"
    "libxcrypt-compat"
    "linux"
    "linux-firmware"
    "lolcat"
    "lttng-ust2.12"
    "lxappearance-gtk3"
    "make"
    "man-db"
    "markdownlint"
    "mediainfo"
    "mission-center"
    "mpd"
    "nano"
    "nano-syntax-highlighting"
    "ncdu"
    "neovim"
    "network-manager-applet"
    "networkmanager"
    "nitrogen"
    "nnn"
    "nodejs-lts-jod"
    "noto-fonts"
    "noto-fonts-cjk"
    "noto-fonts-emoji"
    "npm"
    "ntfs-3g"
    "numlockx"
    "nvidia"
    "nvidia-utils"
    "obs-studio"
    "obsidian"
    "p7zip"
    "pacman-contrib"
    "pamixer"
    "pavucontrol"
    "perl-file-homedir"
    "perl-unicode-linebreak"
    "perl-yaml-tiny"
    "picom"
    "pipewire"
    "pipewire-alsa"
    "pipewire-jack"
    "pipewire-pulse"
    "polybar"
    "python"
    "python-beautifulsoup4"
    "python-matplotlib"
    "python-numpy"
    "python-opencv"
    "python-pillow"
    "python-pip"
    "python-pygments"
    "python-pyqt5"
    "python-scipy"
    "qbittorrent"
    "racket"
    "ranger"
    "reflector"
    "resources"
    "ripgrep"
    "rofi"
    "rust"
    "rust-analyzer"
    "scrot"
    "sddm"
    "smartmontools"
    "smplayer"
    "smplayer-skins"
    "smplayer-themes"
    "sof-firmware"
    "speedtest-cli"
    "starship"
    "telegram-desktop"
    "termdown"
    "texlive-basic"
    "texlive-bibtexextra"
    "texlive-binextra"
    "texlive-context"
    "texlive-fontsrecommended"
    "texlive-fontutils"
    "texlive-formatsextra"
    "texlive-games"
    "texlive-humanities"
    "texlive-latex"
    "texlive-latexextra"
    "texlive-latexrecommended"
    "texlive-luatex"
    "texlive-mathscience"
    "texlive-pictures"
    "texlive-plaingeneric"
    "texlive-pstricks"
    "texlive-publishers"
    "texlive-xetex"
    "texstudio"
    "thunar"
    "tidy"
    "tk"
    "tmux"
    "ttf-dejavu"
    "ttf-fira-code"
    "ttf-hack"
    "ttf-jetbrains-mono"
    "ttf-liberation"
    "ttf-roboto"
    "udiskie"
    "ufw"
    "unzip"
    "vim"
    "virtualbox"
    "vlc"
    "vulkan-intel"
    "vulkan-radeon"
    "w3m"
    "wget"
    "wireless_tools"
    "wireplumber"
    "xclip"
    "xdg-utils"
    "xf86-video-ati"
    "xf86-video-intel"
    "xfce4-power-manager"
    "xmlstarlet"
    "xorg-xbacklight"
    "xorg-xinit"
    "xorg-xinput"
    "xorg-xkbutils"
    "xorg-xkill"
    "xorg-xrdb"
    "xterm"
    "yazi"
    "yt-dlp"
    "zathura"
    "zathura-cb"
    "zathura-djvu"
    "zathura-pdf-mupdf"
    "zip"
    "zram-generator"
    "zsh"
    "zsh-completions"
    "zstd"
)

# Packages from the Arch User Repository (AUR)
AUR_PACKAGES=(
    "brave-bin"
    "anydesk-bin"
    "nerd-fonts-complete"
)

# ==============================================================================
# 2. Logging and Error Handling Functions
# ==============================================================================

# Function to log messages to console and file
log() {
    local level="$1"
    local message="$2"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    # Ensure log directory exists
    sudo mkdir -p "$(dirname "$LOG_FILE")"
    
    case "$level" in
    INFO) 
        printf "\e[32m[%s] INFO: %s\e[0m\n" "$timestamp" "$message" | tee -a "$LOG_FILE" 
        ;;
    WARN) 
        printf "\e[33m[%s] WARN: %s\e[0m\n" "$timestamp" "$message" | tee -a "$LOG_FILE" >&2 
        ;;
    ERROR) 
        printf "\e[31m[%s] ERROR: %s\e[0m\n" "$timestamp" "$message" | tee -a "$LOG_FILE" >&2 
        ;;
    DEBUG) 
        if [[ "${DEBUG:-false}" == "true" ]]; then
            printf "\e[36m[%s] DEBUG: %s\e[0m\n" "$timestamp" "$message" | tee -a "$LOG_FILE" 
        fi
        ;;
    *) 
        printf "[%s] %s: %s\n" "$timestamp" "$level" "$message" | tee -a "$LOG_FILE" 
        ;;
    esac
}

# Function to show progress for long-running operations
show_progress() {
    local current=$1
    local total=$2
    local desc=$3
    local percent=$((current * 100 / total))
    local filled=$((percent / 2))
    local empty=$((50 - filled))
    
    printf "\r\e[34m%s: [" "$desc"
    printf "%*s" $filled | tr ' ' '='
    printf "%*s" $empty | tr ' ' '-'
    printf "] %d%% (%d/%d)\e[0m" $percent $current $total
}

# Function to handle script exit and cleanup
cleanup() {
    local exit_code=$?
    log "INFO" "Cleaning up temporary directory: $TEMP_DIR"
    rm -rf "$TEMP_DIR"
    
    if [ "$exit_code" -ne 0 ]; then
        log "ERROR" "Script terminated with errors. Exit code: $exit_code. Check $LOG_FILE for details."
    else
        log "INFO" "Script completed successfully."
    fi
    
    exit "$exit_code" # Exit with the original exit code
}

# Set up traps for graceful exit and error handling
trap cleanup EXIT ERR SIGINT SIGTERM

# ==============================================================================
# 3. Core System Setup Functions
# ==============================================================================

# Function to check if running as root or with sudo
check_privileges() {
    if [ "$EUID" -ne 0 ] && [ -z "$SUDO_USER" ]; then
        log "ERROR" "This script must be run with sudo. Please run 'sudo ./post-install.sh'."
        exit 1
    fi
    
    # Store the original user if running with sudo
    if [ -n "$SUDO_USER" ]; then
        ORIGINAL_USER="$SUDO_USER"
        ORIGINAL_HOME=$(eval echo "~$SUDO_USER")
    else
        ORIGINAL_USER="$USER"
        ORIGINAL_HOME="$HOME"
    fi
    
    log "INFO" "Running with privileges for user: $ORIGINAL_USER"
}

# Function to update system and install essential tools
update_system() {
    log "INFO" "Updating system and installing essential build tools."
    
    # Update system
    log "INFO" "Updating package databases and system packages..."
    sudo pacman -Syu --noconfirm || {
        log "ERROR" "Failed to update system."
        return 1
    }
    
    # Install essential tools
    log "INFO" "Installing essential build tools..."
    sudo pacman -S --needed --noconfirm base-devel git || {
        log "ERROR" "Failed to install base-devel and git."
        return 1
    }
    
    log "INFO" "System updated and essential tools installed."
}

# Function to install paru (AUR helper)
install_paru() {
    if [[ "$AUR_ENABLED" != "true" ]]; then
        log "INFO" "AUR support is disabled. Skipping paru installation."
        return 0
    fi
    
    log "INFO" "Checking for existing paru installation..."
    if command -v paru &>/dev/null; then
        log "INFO" "paru is already installed. Skipping paru installation."
        return 0
    fi

    log "INFO" "Installing paru (AUR helper)..."
    cd "$TEMP_DIR"
    
    # Clone paru repository
    log "INFO" "Cloning paru repository..."
    git clone https://aur.archlinux.org/paru-bin.git || {
        log "ERROR" "Failed to clone paru repository."
        return 1
    }
    
    # Build and install paru
    log "INFO" "Building and installing paru..."
    cd paru-bin
    makepkg -si --noconfirm || {
        log "ERROR" "Failed to build and install paru."
        return 1
    }
    
    log "INFO" "paru installed successfully."
}

# Function to install packages from official repositories
install_official_packages() {
    if [ ${#PACMAN_PACKAGES[@]} -eq 0 ]; then
        log "INFO" "No official packages specified for installation."
        return 0
    fi
    
    log "INFO" "Installing official packages: ${PACMAN_PACKAGES[*]}"
    
    local total=${#PACMAN_PACKAGES[@]}
    local current=0
    
    for package in "${PACMAN_PACKAGES[@]}"; do
        current=$((current + 1))
        show_progress $current $total "Installing packages"
        
        # Check if package is already installed
        if pacman -Qi "$package" &>/dev/null; then
            log "DEBUG" "Package $package is already installed. Skipping."
            continue
        fi
        
        # Install package
        sudo pacman -S --needed --noconfirm "$package" || {
            log "ERROR" "Failed to install package: $package"
            return 1
        }
    done
    
    printf "\n" # New line after progress bar
    log "INFO" "Official packages installed successfully."
}

# Function to install packages from AUR using paru
install_aur_packages() {
    if [[ "$AUR_ENABLED" != "true" ]]; then
        log "INFO" "AUR support is disabled. Skipping AUR package installation."
        return 0
    fi
    
    if [ ${#AUR_PACKAGES[@]} -eq 0 ]; then
        log "INFO" "No AUR packages specified for installation."
        return 0
    fi
    
    log "INFO" "Installing AUR packages using paru: ${AUR_PACKAGES[*]}"
    
    local total=${#AUR_PACKAGES[@]}
    local current=0
    
    for package in "${AUR_PACKAGES[@]}"; do
        current=$((current + 1))
        show_progress $current $total "Installing AUR packages"
        
        # Check if package is already installed
        if paru -Qi "$package" &>/dev/null; then
            log "DEBUG" "AUR package $package is already installed. Skipping."
            continue
        fi
        
        # Install package
        paru -S --noconfirm "$package" || {
            log "ERROR" "Failed to install AUR package: $package"
            return 1
        }
    done
    
    printf "\n" # New line after progress bar
    log "INFO" "AUR packages installed successfully."
}

# Function to manage dotfiles using a bare Git repository
manage_dotfiles() {
    if [[ "$DOTFILES_ENABLED" != "true" ]]; then
        log "INFO" "Dotfiles management is disabled. Skipping."
        return 0
    fi
    
    log "INFO" "Starting dotfiles management."
    
    # Expand tilde to actual home directory
    local expanded_dotfiles_dir="${DOTFILES_BARE_DIR/#\~/$ORIGINAL_HOME}"
    local expanded_setup_script="${DOTFILES_LOCAL_SETUP_SCRIPT/#\~/$ORIGINAL_HOME}"

    if [ -d "$expanded_dotfiles_dir" ]; then
        log "INFO" "Bare dotfiles repository already exists at $expanded_dotfiles_dir. Skipping clone."
    else
        log "INFO" "Cloning dotfiles repository as bare Git repo to $expanded_dotfiles_dir..."
        mkdir -p "$(dirname "$expanded_dotfiles_dir")"
        git clone --bare "$DOTFILES_REPO_URL" "$expanded_dotfiles_dir" || {
            log "ERROR" "Failed to clone dotfiles repository."
            return 1
        }
        log "INFO" "Dotfiles repository cloned successfully."
    fi

    # Execute custom setup script if it exists within the dotfiles
    if [ -f "$expanded_setup_script" ]; then
        log "INFO" "Executing custom dotfiles setup script: $expanded_setup_script"
        sudo -u "$ORIGINAL_USER" bash "$expanded_setup_script" || {
            log "ERROR" "Custom dotfiles setup script failed."
            return 1
        }
        log "INFO" "Custom dotfiles setup script completed."
    else
        log "INFO" "No custom dotfiles setup script found at $expanded_setup_script. Skipping."
    fi
}

# ==============================================================================
# 4. Advanced Post-Installation Enhancements Functions
# ==============================================================================

# Function for system maintenance automation (paccache and reflector)
setup_system_maintenance() {
    log "INFO" "Setting up automated system maintenance with paccache and reflector."

    # Paccache setup
    log "INFO" "Configuring paccache for automated package cache cleaning."
    # paccache is installed as part of PACMAN_PACKAGES
    sudo systemctl enable --now paccache.timer || {
        log "ERROR" "Failed to enable paccache.timer."
        return 1
    }
    log "INFO" "paccache.timer enabled for weekly cache cleaning."

    # Reflector setup
    log "INFO" "Configuring reflector for automated mirrorlist optimization."
    # reflector is installed as part of PACMAN_PACKAGES

    # Create reflector configuration file (customize parameters as needed)
    sudo mkdir -p /etc/xdg/reflector
    sudo tee /etc/xdg/reflector/reflector.conf >/dev/null <<EOF
--latest 10
--sort rate
--protocol https
--save /etc/pacman.d/mirrorlist
EOF
    log "INFO" "Reflector configuration written to /etc/xdg/reflector/reflector.conf."

    sudo systemctl enable --now reflector.timer || {
        log "ERROR" "Failed to enable reflector.timer."
        return 1
    }
    log "INFO" "reflector.timer enabled for weekly mirrorlist updates."
}

# Function for performance optimization (ZRAM)
setup_zram() {
    if [[ "$ZRAM_ENABLED" != "true" ]]; then
        log "INFO" "ZRAM setup is disabled. Skipping."
        return 0
    fi
    
    log "INFO" "Setting up ZRAM for enhanced swap performance."
    # zram-generator and zstd are installed as part of PACMAN_PACKAGES

    # Disable zswap permanently to avoid conflicts with ZRAM
    log "INFO" "Disabling zswap to ensure optimal ZRAM performance."
    if grep -q "zswap.enabled=0" /etc/default/grub; then
        log "INFO" "zswap already disabled in GRUB configuration. Skipping."
    else
        log "INFO" "Adding zswap.enabled=0 to GRUB_CMDLINE_LINUX_DEFAULT."
        sudo sed -i 's/GRUB_CMDLINE_LINUX_DEFAULT="\(.*\)"/GRUB_CMDLINE_LINUX_DEFAULT="\1 zswap.enabled=0"/' /etc/default/grub || {
            log "ERROR" "Failed to modify GRUB config for zswap."
            return 1
        }
        sudo grub-mkconfig -o /boot/grub/grub.cfg || {
            log "ERROR" "Failed to update GRUB configuration."
            return 1
        }
        log "INFO" "GRUB updated. zswap will be disabled on next reboot."
    fi

    # Configure zram-generator
    log "INFO" "Creating /etc/systemd/zram-generator.conf for ZRAM setup."
    sudo tee /etc/systemd/zram-generator.conf >/dev/null <<EOF
[zram0]
zram-size = ram * 2 # Set ZRAM size to twice the physical RAM
compression-algorithm = zstd
swap-priority = 100
fs-type = swap
EOF
    log "INFO" "ZRAM configuration written to /etc/systemd/zram-generator.conf."
    log "INFO" "ZRAM will be initialized automatically by systemd-zram-setup@.service on boot."
}

# Function for security baseline (UFW)
setup_ufw_firewall() {
    if [[ "$UFW_ENABLED" != "true" ]]; then
        log "INFO" "UFW firewall setup is disabled. Skipping."
        return 0
    fi
    
    log "INFO" "Configuring UFW firewall."
    # ufw is installed as part of PACMAN_PACKAGES

    log "INFO" "Setting default UFW policies: deny incoming, allow outgoing."
    sudo ufw default deny incoming || {
        log "ERROR" "Failed to set UFW default incoming policy."
        return 1
    }
    sudo ufw default allow outgoing || {
        log "ERROR" "Failed to set UFW default outgoing policy."
        return 1
    }

    log "INFO" "Allowing SSH connections (port 22/tcp)."
    sudo ufw allow ssh/tcp || {
        log "ERROR" "Failed to allow SSH in UFW."
        return 1
    }

    log "INFO" "Enabling and starting UFW service."
    sudo systemctl enable --now ufw.service || {
        log "ERROR" "Failed to enable UFW service."
        return 1
    }
    log "INFO" "UFW firewall enabled and active."
    log "INFO" "Current UFW status:"
    sudo ufw status verbose | tee -a "$LOG_FILE"
}

# Function to set Zsh as default shell
set_zsh_default() {
    if [[ "$ZSH_ENABLED" != "true" ]]; then
        log "INFO" "Zsh setup is disabled. Skipping."
        return 0
    fi
    
    log "INFO" "Setting Zsh as default shell (optional)."
    # zsh is installed as part of PACMAN_PACKAGES

    # Check if zsh is in /etc/shells
    if grep -q "$(command -v zsh)" /etc/shells; then
        log "INFO" "Zsh is already listed in /etc/shells."
    else
        log "INFO" "Adding Zsh to /etc/shells."
        command -v zsh | sudo tee -a /etc/shells || {
            log "ERROR" "Failed to add Zsh to /etc/shells."
            return 1
        }
        log "INFO" "Zsh path added to /etc/shells."
    fi

    log "INFO" "Changing default shell for user $ORIGINAL_USER to Zsh."
    sudo chsh -s "$(command -v zsh)" "$ORIGINAL_USER" || {
        log "ERROR" "Failed to change default shell to Zsh."
        return 1
    }
    log "INFO" "Default shell changed to Zsh for user $ORIGINAL_USER. Please log out and log back in for changes to take effect."
}

# Function to create a sample configuration file
create_sample_config() {
    if [[ ! -f "$CONFIG_FILE" ]]; then
        log "INFO" "Creating sample configuration file at $CONFIG_FILE"
        cat > "$CONFIG_FILE" << EOF
# Arch Linux Post-Installation Configuration
# Toggle features on/off by setting to true/false

# Enable/disable dotfiles management
DOTFILES_ENABLED=true

# Enable/disable AUR support
AUR_ENABLED=true

# Enable/disable ZRAM setup
ZRAM_ENABLED=true

# Enable/disable UFW firewall setup
UFW_ENABLED=true

# Enable/disable Zsh as default shell
ZSH_ENABLED=true

# Enable debug logging
DEBUG=false
EOF
        log "INFO" "Sample configuration file created. Edit $CONFIG_FILE to customize your installation."
    fi
}

# ==============================================================================
# 5. Main Script Execution Flow
# ==============================================================================

main() {
    log "INFO" "Starting Arch Linux Post-Installation Script V2."
    log "INFO" "Log file: $LOG_FILE"
    log "INFO" "Temporary directory: $TEMP_DIR"
    
    # Create sample config if it doesn't exist
    create_sample_config
    
    # Check privileges
    check_privileges
    
    # Update system and install essential tools
    update_system
    
    # Install AUR helper if needed
    install_paru
    
    # Install packages
    install_official_packages
    install_aur_packages
    
    # Setup dotfiles
    manage_dotfiles
    
    # Setup system maintenance
    setup_system_maintenance
    
    # Setup performance optimizations
    setup_zram
    
    # Setup security
    setup_ufw_firewall
    
    # Set default shell
    set_zsh_default

    log "INFO" "All post-installation tasks attempted. Please review the log file for any warnings or errors."
    log "INFO" "A system reboot is recommended to apply all changes, especially for ZRAM and Zsh shell."
}

# Execute the main function
main "$@"