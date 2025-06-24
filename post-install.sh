#!/bin/bash

# ==============================================================================
# Arch Linux Post-Installation Script for i3wm & SDDM
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
# ==============================================================================

# ==============================================================================
# 1. Script Configuration and Robustness Settings
# ==============================================================================

# Strict mode: Exit immediately if a command exits with a non-zero status (-e).
# Treat unset variables as an error (-u).
# The exit status of a pipeline is the status of the last command to exit with a non-zero status (-o pipefail).
set -euo pipefail

# Define script variables
LOG_FILE="/var/log/arch_post_install.log"
TEMP_DIR=$(mktemp -d) # Create a temporary directory for building AUR packages
DOTFILES_REPO_URL="https://github.com/ismailmajeeb/dot-files.git"
DOTFILES_BARE_DIR="$HOME/Repos/dot-files"
DOTFILES_LOCAL_SETUP_SCRIPT="$HOME/Repos/dot-files/Linker-Bash.sh"

# Define package lists (customize these arrays to your needs)
# Packages from official Arch Linux repositories
PACMAN_PACKAGES=(
    "htop" "btop" "fastfetch" "ranger" "feh" "rofi" "dunst" "picom" "polybar"
    "brightnessctl" "pavucontrol" "alsa-utils" "networkmanager" "bluez" "bluez-utils"
    "udiskie" "gvfs" "unzip" "p7zip" "gnome-keyring" "xclip" "bat" "fzf"
    "exa" "ripgrep" "fd" "lazygit" "tmux" "vim" "neovim" "zsh" "kitty" "alacritty"
    "gcc" "make" "cmake" "python" "python-pip" "npm" "go" "rust"
    "firefox" "chromium"
    "pacman-contrib"        # For paccache
    "reflector"             # For mirrorlist optimization
    "ufw"                   # Uncomplicated Firewall
    "zram-generator" "zstd" # For ZRAM performance optimization
    "noto-fonts" "noto-fonts-cjk" "noto-fonts-emoji" "ttf-liberation" "ttf-dejavu" "ttf-roboto"
    "ttf-jetbrains-mono" "ttf-fira-code" "ttf-hack" "adobe-source-code-pro-fonts" # Fonts
    "xorg-xrdb"                                                                   # For X11 font settings
)

# Packages from the Arch User Repository (AUR)
AUR_PACKAGES=(
    "brave-bin"
    "ttf-ms-fonts"
    "nerd-fonts-complete"
    "ttf-symbola" # Additional symbols for fonts
)

# ==============================================================================
# 2. Logging and Error Handling Functions
# ==============================================================================

# Function to log messages to console and file
log() {
    local level="$1"
    local message="$2"
    timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    case "$level" in
    INFO) printf "\e[32m[%s] INFO: %s\e[0m\n" "$timestamp" "$message" | tee -a "$LOG_FILE" ;;
    WARN) printf "\e[33m[%s] WARN: %s\e[0m\n" "$timestamp" "$message" | tee -a "$LOG_FILE" >&2 ;;
    ERROR) printf "\e[31m[%s] ERROR: %s\e[0m\n" "$timestamp" "$message" | tee -a "$LOG_FILE" >&2 ;;
    *) printf "[%s] %s: %s\n" "$timestamp" "$level" "$message" | tee -a "$LOG_FILE" ;;
    esac
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

# Function to install paru (AUR helper)
install_paru() {
    log "INFO" "Checking for existing paru installation..."
    if command -v paru &>/dev/null; then
        log "INFO" "paru is already installed. Skipping paru installation."
        return 0
    fi

    log "INFO" "Installing base-devel and git for paru build..."
    sudo pacman -S --needed --noconfirm base-devel git || {
        log "ERROR" "Failed to install base-devel and git."
        return 1
    }

    log "INFO" "Cloning paru repository to $TEMP_DIR/paru..."
    git clone https://aur.archlinux.org/paru.git "$TEMP_DIR/paru" || {
        log "ERROR" "Failed to clone paru repository."
        return 1
    }

    log "INFO" "Building and installing paru. This may take a moment and prompt for sudo password."
    (cd "$TEMP_DIR/paru" && makepkg -si --noconfirm) || {
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
    sudo pacman -S --needed --noconfirm "${PACMAN_PACKAGES[@]}" || {
        log "ERROR" "Failed to install official packages."
        return 1
    }
    log "INFO" "Official packages installed successfully."
}

# Function to install packages from AUR using paru
install_aur_packages() {
    if [ ${#AUR_PACKAGES[@]} -eq 0 ]; then
        log "INFO" "No AUR packages specified for installation."
        return 0
    fi
    log "INFO" "Installing AUR packages using paru: ${AUR_PACKAGES[*]}"
    paru -S --noconfirm "${AUR_PACKAGES[@]}" || {
        log "ERROR" "Failed to install AUR packages."
        return 1
    }
    log "INFO" "AUR packages installed successfully."
}

# Function to manage dotfiles using a bare Git repository
manage_dotfiles() {
    log "INFO" "Starting dotfiles management."

    if [ -d "$DOTFILES_BARE_DIR" ]; then
        log "INFO" "Bare dotfiles repository already exists at $DOTFILES_BARE_DIR. Skipping clone."
    else
        log "INFO" "Cloning dotfiles repository as bare Git repo to $DOTFILES_BARE_DIR..."
        git clone --bare "$DOTFILES_REPO_URL" "$DOTFILES_BARE_DIR" || {
            log "ERROR" "Failed to clone dotfiles repository."
            return 1
        }
        log "INFO" "Dotfiles repository cloned successfully."
    fi

    # Execute custom setup script if it exists within the dotfiles
    if [ -f "$DOTFILES_LOCAL_SETUP_SCRIPT" ]; then
        log "INFO" "Executing custom dotfiles setup script: $DOTFILES_LOCAL_SETUP_SCRIPT"
        bash "$DOTFILES_LOCAL_SETUP_SCRIPT" || {
            log "ERROR" "Custom dotfiles setup script failed."
            return 1
        }
        log "INFO" "Custom dotfiles setup script completed."
    else
        log "INFO" "No custom dotfiles setup script found at $DOTFILES_LOCAL_SETUP_SCRIPT. Skipping."
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
# setup_zram() {
#     log "INFO" "Setting up ZRAM for enhanced swap performance."
#     # zram-generator and zstd are installed as part of PACMAN_PACKAGES
#
#     # Disable zswap permanently to avoid conflicts with ZRAM
#     log "INFO" "Disabling zswap to ensure optimal ZRAM performance."
#     if grep -q "zswap.enabled=0" /etc/default/grub; then
#         log "INFO" "zswap already disabled in GRUB configuration. Skipping."
#     else
#         log "INFO" "Adding zswap.enabled=0 to GRUB_CMDLINE_LINUX_DEFAULT."
#         sudo sed -i 's/GRUB_CMDLINE_LINUX_DEFAULT="\(.*\)"/GRUB_CMDLINE_LINUX_DEFAULT="\1 zswap.enabled=0"/' /etc/default/grub || {
#             log "ERROR" "Failed to modify GRUB config for zswap."
#             return 1
#         }
#         sudo grub-mkconfig -o /boot/grub/grub.cfg || {
#             log "ERROR" "Failed to update GRUB configuration."
#             return 1
#         }
#         log "INFO" "GRUB updated. zswap will be disabled on next reboot."
#     fi
#
#     # Configure zram-generator
#     log "INFO" "Creating /etc/systemd/zram-generator.conf for ZRAM setup."
#     sudo tee /etc/systemd/zram-generator.conf >/dev/null <<EOF
# [zram0]
# zram-size = ram * 2 # Set ZRAM size to twice the physical RAM
# compression-algorithm = zstd
# swap-priority = 100
# fs-type = swap
# EOF
#     log "INFO" "ZRAM configuration written to /etc/systemd/zram-generator.conf."
#     log "INFO" "ZRAM will be initialized automatically by systemd-zram-setup@.service on boot."
# }

# Function for security baseline (UFW)
# setup_ufw_firewall() {
#     log "INFO" "Configuring UFW firewall."
#     # ufw is installed as part of PACMAN_PACKAGES
#
#     log "INFO" "Setting default UFW policies: deny incoming, allow outgoing."
#     sudo ufw default deny incoming || {
#         log "ERROR" "Failed to set UFW default incoming policy."
#         return 1
#     }
#     sudo ufw default allow outgoing || {
#         log "ERROR" "Failed to set UFW default outgoing policy."
#         return 1
#     }
#
#     log "INFO" "Allowing SSH connections (port 22/tcp)."
#     sudo ufw allow ssh/tcp || {
#         log "ERROR" "Failed to allow SSH in UFW."
#         return 1
#     }
#
#     log "INFO" "Enabling and starting UFW service."
#     sudo systemctl enable --now ufw.service || {
#         log "ERROR" "Failed to enable UFW service."
#         return 1
#     }
#     log "INFO" "UFW firewall enabled and active."
#     log "INFO" "Current UFW status:"
#     sudo ufw status verbose | tee -a "$LOG_FILE"
# }

# Function to set Zsh as default shell
set_zsh_default() {
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

    log "INFO" "Changing default shell for user $USER to Zsh."
    sudo chsh -s "$(command -v zsh)" "$USER" || {
        log "ERROR" "Failed to change default shell to Zsh."
        return 1
    }
    log "INFO" "Default shell changed to Zsh for user $USER. Please log out and log back in for changes to take effect."
}

# ==============================================================================
# 5. Main Script Execution Flow
# ==============================================================================

main() {
    log "INFO" "Starting Arch Linux Post-Installation Script."
    log "INFO" "Log file: $LOG_FILE"
    log "INFO" "Temporary directory: $TEMP_DIR"

    # Ensure script is run with sudo (even if user is already root)
    if [ "$EUID" -ne 0 ] || [ -z "$SUDO_USER" ]; then
        log "ERROR" "This script must be run with sudo. Please run 'sudo ./post-install.sh'."
        exit 1
    fi

    # Drop root privileges for user-specific tasks where possible, or ensure sudo is used.
    # For now, we'll assume sudo is used for privileged commands within functions.

    log "INFO" "Updating system and installing essential build tools."
    sudo pacman -Syu --noconfirm || {
        log "ERROR" "Failed to update system."
        return 1
    }
    sudo pacman -S --needed --noconfirm base-devel git || {
        log "ERROR" "Failed to install base-devel and git."
        return 1
    }
    log "INFO" "System updated and essential tools installed."

    install_paru
    install_official_packages
    install_aur_packages
    manage_dotfiles
    setup_system_maintenance
    # setup_zram
    # setup_ufw_firewall
    set_zsh_default

    log "INFO" "All post-installation tasks attempted. Please review the log file for any warnings or errors."
    log "INFO" "A system reboot is recommended to apply all changes, especially for ZRAM and Zsh shell."
}

# Execute the main function
main "$@"
