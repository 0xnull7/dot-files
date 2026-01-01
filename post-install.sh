#!/bin/bash

# ==============================================================================
# Arch Linux Post-Installation Script V3
# ==============================================================================

set -euo pipefail

# --- Configuration ---
readonly LOG_FILE="$HOME/arch_install_$(date +%Y%m%d).log"
readonly TEMP_DIR=$(mktemp -d)
readonly DOTFILES_REPO="https://github.com/0xnull7/dot-files.git"
readonly DOTFILES_DIR="$HOME/Repos/dot-files"
readonly DOTFILES_LOCAL_SETUP_SCRIPT="$HOME/Repos/dot-files/Linker-Bash.sh"

# Default Toggles
ENABLE_DOTFILES=true
ENABLE_ZRAM=true
ENABLE_UFW=true

# --- Package Lists ---
PACMAN_PACKAGES=(
  # System & Dev Core
  "base-devel" "git" "wget" "unzip" "7zip" "rsync" "sudo" "acpi" "brightnessctl"
  "pacman-contrib" "reflector" "cmake" "clang" "gcc" "python-pip" "nodejs-lts-jod"

  # GUI & Window Manager
  "i3-wm" "i3blocks" "i3status" "sddm" "rofi" "dunst" "picom" "feh" "nitrogen"
  "arandr" "lxappearance" "xclip" "numlockx" "xss-lock" "gtk2" "gtk3" "gtk4"

  # Terminal / CLI
  "alacritty" "kitty" "tmux" "zsh" "zsh-completions" "starship" "fastfetch"
  "bat" "eza" "fzf" "ripgrep" "fd" "htop" "btop" "yazi" "ncdu" "lazygit"

  # Virtualization & Networking
  "virt-manager" "qemu-desktop" "libvirt" "dnsmasq" "iptables-nft"
  "networkmanager" "network-manager-applet" "bluez" "bluez-utils" "blueman" "ufw"

  # Audio & Media Tools
  "pipewire" "pipewire-pulse" "wireplumber" "pavucontrol" "pamixer"
  "vlc" "mpv" "obs-studio" "yt-dlp"

  # Fonts
  "ttf-jetbrains-mono" "ttf-fira-code" "ttf-nerd-fonts-symbols"
  "noto-fonts" "noto-fonts-cjk" "noto-fonts-emoji"
)

AUR_PACKAGES=(
  # AUR Helper
  "yay-bin"

  # Drivers (Nvidia 580xx Series)
  "nvidia-580xx-dkms"
  "nvidia-580xx-utils"
  "nvidia-580xx-settings"
  "libxnvctrl-580xx"

  # Security & Browsers
  "brave-bin"
  "mullvad-browser-bin"
  "anydesk-bin"
  "antigravity-bin-hardened"

  # Tools & Enhancements
  "visual-studio-code-bin"
  "i3lock-color"
  "media-downloader"
  "hakuneko-desktop-bin"
  "aur-check-updates-bin"
  "yp-tools"
)

# --- Functions ---

log() {
  local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
  echo -e "[$timestamp] $1" | tee -a "$LOG_FILE"
}

setup_sudo() {
  log "Authenticating sudo..."
  sudo -v
  while true; do
    sudo -n true
    sleep 60
    kill -0 "$$" || exit
  done 2> /dev/null &
}

interactive_confirm() {
  echo -e "\n--- SUMMARY OF ACTIONS ---"
  echo "1. Update System & Install Base-Devel"
  echo "2. Install yay (AUR Helper)"
  echo "3. Install ${#PACMAN_PACKAGES[@]} Official Packages"
  echo "4. Install ${#AUR_PACKAGES[@]} AUR Packages"
  echo "5. Setup ZRAM, UFW, and Reflector"
  echo "6. Sync Dotfiles from: $DOTFILES_REPO"
  echo "--------------------------"
  read -rp "Proceed with installation? (y/N): " confirm
  [[ "$confirm" == [yY] ]] || exit 0
}

optimize_makepkg() {
  log "INFO: Optimizing makepkg for multi-core compilation..."
  # Set MAKEFLAGS to number of CPU cores
  sudo sed -i "s/-j2/-j$(nproc)/" /etc/makepkg.conf
  # Change compression to use all cores
  sudo sed -i 's/COMPRESSZST=(zstd -c -z -q -)/COMPRESSZST=(zstd -c -z -q - --threads=0)/' /etc/makepkg.conf
}

install_aur_helper() {
  if ! command -v yay &> /dev/null; then
    log "Bootstrapping yay-bin..."
    git clone https://aur.archlinux.org/yay-bin.git "$TEMP_DIR/yay-bin"
    (cd "$TEMP_DIR/yay-bin" && makepkg -si --noconfirm)
  else
    log "INFO: yay is already installed."
  fi
}

setup_services() {
  log "Enabling system services..."
  sudo systemctl enable --now NetworkManager bluetooth sddm ufw
  sudo systemctl enable --now libvirtd

  # Group permissions for Virt-Manager
  sudo usermod -aG libvirt,kvm "$USER"
  log "User added to libvirt and kvm groups."

  if [[ "$ENABLE_UFW" == "true" ]]; then
    sudo ufw default deny incoming
    sudo ufw default allow outgoing
    sudo ufw limit ssh
    sudo ufw enable
  fi

  # Maintenance
  sudo systemctl enable --now paccache.timer

}

setup_zram() {
  log "Configuring ZRAM..."
  sudo tee /etc/systemd/zram-generator.conf << EOF
[zram0]
zram-size = ram / 1
compression-algorithm = zstd
EOF
  sudo systemctl daemon-reload
  sudo systemctl start /dev/zram0
}

manage_dotfiles() {
  log "INFO: Managing Dotfiles..."
  if [ ! -d "$DOTFILES_DIR" ]; then
    git clone --bare "$DOTFILES_REPO" "$DOTFILES_DIR"
    # Optional: Checkout script or symlinking logic here
    if [ -f "$DOTFILES_LOCAL_SETUP_SCRIPT" ]; then
      bash "$DOTFILES_LOCAL_SETUP_SCRIPT"
    fi
  fi
}

# --- Execution ---

main() {
  setup_sudo
  interactive_confirm
  optimize_makepkg

  log "System Update..."
  sudo pacman -Syu --noconfirm

  log "Installing Native Packages..."
  sudo pacman -S --needed --noconfirm "${PACMAN_PACKAGES[@]}"

  install_aur_helper
  log "Installing AUR Packages..."
  yay -S --needed --noconfirm "${AUR_PACKAGES[@]}"

  setup_services
  [[ "$ENABLE_ZRAM" == "true" ]] && setup_zram
  [[ "$ENABLE_DOTFILES" == "true" ]] && manage_dotfiles

  log "Post-install finished. A reboot is highly recommended for the Nvidia drivers."
}

main "$@"

