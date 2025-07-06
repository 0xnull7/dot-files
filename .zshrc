#!/usr/bin/env zsh

# --- ZSH CONFIGURATION ---

# ===== ENVIRONMENT VARIABLES =====
export EDITOR='nvim'
export VISUAL='nvim'
export MANPAGER="nvim +Man!"
export PAGER='less'
export LESS='-R'

# XDG Base Directory Specification
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"

# History settings
HISTFILE=~/.zsh_history
export HISTFILESIZE=100000
export HISTSIZE=100000
export SAVEHIST=100000
export HISTTIMEFORMAT="%F %T"

setopt appendhistory
setopt hist_expire_dups_first # Expire duplicates first
setopt hist_ignore_dups # Ignore repeated commands
setopt hist_ignore_all_dups # Remove older duplicate entries
setopt hist_save_no_dups # Don't write duplicate entries
setopt hist_find_no_dups # Don't display duplicates when searching
setopt hist_reduce_blanks # Remove superfluous blanks
setopt hist_verify # Show command before executing

# Color settings
export CLICOLOR=1
export LS_COLORS='no=00:fi=00:di=00;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:*.xml=00;31:'

# Manpage colors for better readability with 'less'
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# PATH management - more organized and comprehensive
typeset -U PATH path # Ensure no duplicates in PATH
path=(
    $HOME/.local/bin
    $HOME/.cargo/bin
    $HOME/go/bin
    $HOME/.npm-global/bin
    /var/lib/flatpak/exports/bin
    /.local/share/flatpak/exports/bin
    /usr/local/bin
    /usr/local/sbin
    $path
)
export PATH

# ===== ZSH OPTIONS =====
setopt autocd              # cd by typing directory name
setopt correct             # autocorrect commands
setopt correct_all         # autocorrect all arguments
setopt interactivecomments # allow comments in interactive shell
setopt histignorealldups   # remove older duplicate history entries (replaces HISTCONTROL=erasedups:ignoredups)
setopt sharehistory        # share history across terminals
setopt incappendhistory    # write to history immediately
setopt extendedglob        # advanced globbing
setopt no_beep             # no beep on errors (replaces Bash's bind "set bell-style visible")
setopt complete_in_word    # Complete from both ends of a word
setopt always_to_end       # Move cursor to end if word had one match
setopt auto_menu           # Show completion menu on successive tab press
setopt auto_list           # Automatically list choices on ambiguous completion
setopt auto_param_slash    # Add trailing slash when completing directories
setopt extended_glob       # Needed for file modification glob modifiers
setopt multios             # Allow multiple redirections
setopt cdable_vars         # cd to variables as if they were directories
setopt pushd_ignore_dups   # Don't push duplicate directories
setopt pushd_silent        # Silence pushd/popd output
setopt chase_links         # Resolve symlinks to their true path

# ===== KEYBINDINGS =====
bindkey -v                  # vi mode (replaces Bash's set -o vi)
bindkey '^L' clear-screen   # Ctrl+L to clear screen
bindkey '^R' history-incremental-search-backward # Better history search

# Fix keybindings for home/end keys
bindkey "^[[H" beginning-of-line
bindkey "^[[F" end-of-line
bindkey "^[[3~" delete-char

# ===== STARSHIP PROMPT =====
if command -v starship >/dev/null; then
  eval "$(starship init zsh)"
else
  # Fallback prompt if starship is not found
  setopt PROMPT_SUBST
  PROMPT='%F{blue}%~%f %F{yellow}$%f '
  RPROMPT='%F{green}%*%f'
fi

# ===== ZINIT PLUGIN MANAGER =====
# Ensure Zinit is installed
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load important annexes
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

# ===== COMPLETION STYLING =====
zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh/zcompcache
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' '+r:|[._-]=* r:|=*' '+l:|=* r:|=*'
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:messages' format '%F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format '%F{red}-- no matches found --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-separator '-->'
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

# ===== PLUGINS =====
# Essential plugins
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light agkozak/zsh-z           # Fast directory jumping
zinit light hlissner/zsh-autopair   # Auto-close brackets/quotes
zinit light djui/alias-tips         # Show tips when using aliases
zinit light zdharma-continuum/fast-syntax-highlighting

# Additional useful plugins
zinit light MichaelAquilina/zsh-you-should-use # Suggests aliases
#zinit light marlonrichert/zsh-autocomplete   # Enhanced autocomplete
zinit light Tarrasch/zsh-bd                  # Quick directory navigation
zinit light zdharma-continuum/history-search-multi-word # Better history search

# Initialize zoxide (if zsh-z is not used, or if you prefer zoxide directly)
# If using `agkozak/zsh-z` from zinit, this is usually not needed as z-sh implements zoxide-like functionality
# If you want to use the actual zoxide binary and its functions, keep this:
# if command -v zoxide >/dev/null; then
#   eval "$(zoxide init zsh)"
# fi

# ===== FZF CONFIGURATION =====
if command -v fzf >/dev/null; then
  export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border --color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'

  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

  # fzf + fd configuration
  if command -v fd >/dev/null; then
    export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_ALT_C_COMMAND="fd --type d --hidden --follow --exclude .git"
  fi
fi

# ===== ALIASES =====
# General Utilities
alias ls='ls -lAhX --color=always --group-directories-first' # Enhanced ls (Bash default)
alias c='clear'
alias e='exit'
alias da='date "+%Y-%m-%d %A %T %Z"'                                            # Show formatted date
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history 1 | sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"' # Notification for long-running commands (Zsh history syntax adjusted)
alias ezsh='nvim ~/.zshrc'                                                    # Edit this .zshrc
alias reload='source ~/.zshrc && echo "Zsh config reloaded!"'
alias cp='cp -irfv' # Interactive copy
alias mv='mv -iv'   # Interactive move
# alias rm='trash -v' # Use trash-cli for safe removal (ensure trash-cli is installed)
alias mkdir='mkdir -p'  # Create parent directories as needed
alias ps='ps auxf'      # Detailed process listing
alias ping='ping -c 10' # Ping 10 times by default
alias less='less -R'    # Less with raw control characters
alias vi='nvim'         # Alias vi to nvim
alias vim='nvim'        # Alias vim to nvim
alias svi='sudo nvim'   # Sudo edit with nvim
alias snano='sudo nano' # Sudo nano (if installed)
alias xc='xclip -selection clipboard' # Copy to clipboard
alias rr='ranger'
alias ff='fastfetch'

# Grep Aliases (Prioritizing ripgrep if available)
if command -v rg &>/dev/null; then
    alias grep='rg'
else
    alias grep='grep --color=auto'
fi
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# Pacman and Paru (for Arch-based systems)
alias i='paru -S'                                                       # Install standard packages and AUR packages
alias u='paru -Syu'                                                     # Update standard and AUR packages (paru)
alias r='paru -Rn'                                                      # Remove the specified package
alias unlock='sudo rm /var/lib/pacman/db.lck'                           # Remove pacman lock
alias I='paru -Qi'                                                      # List information about a package
alias S='paru -Si'                                                      # Search for package information
alias cleanup='paru -Rns $(paru -Qtdq)'                                 # Remove orphaned packages
alias orphan='paru -Qtdq'
alias pactree='pactree -c'
alias parf="paru -Slq | fzf --multi --preview 'paru -Sii {1}' --preview-window=down:75% | xargs -ro paru -S" # Fuzzy find for paru
# alias yayf="yay -Slq | fzf --multi --preview 'yay -Sii {1}' --preview-window=down:75% | xargs -ro yay -S" # Fuzzy find for yay

# Reflector Aliases (for Arch Linux mirror management)
alias mirror="sudo reflector --latest 50 --number 20 --sort rate --protocol http,https --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

# Git Aliases
alias g='git'
alias ga='git add .'
alias gaa='git add --all'
alias gc='git commit -v'
alias gcm='git commit -m'
alias gca='git commit --amend'
alias gcan='git commit --amend --no-edit'
alias gcb='git checkout -b'
alias gco='git checkout'
alias gcl='git clone --recurse-submodules'
alias gd='git diff'
alias gds='git diff --staged'
alias gl='git log --graph --pretty=format:"%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit'
alias gp='git push'
alias gpf='git push --force-with-lease'
alias gpl='git pull'
alias gst='git status'
alias gsta='git stash push'
alias gstp='git stash pop'
alias gsw='git switch'
alias gswc='git switch -c'
alias grb='git rebase'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias grbi='git rebase -i'
alias grmo='git remote -v'
alias grmoa='git remote add'

# Directory Navigation Aliases
alias home='cd ~'
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# LS Aliases (more comprehensive)
alias lx='ls -lXBh --color=always --group-directories-first'         # sort by extension
alias lk='ls -lSrh --color=always --group-directories-first'         # sort by size, reverse, human readable
alias lc='ls -ltcrh --color=always --group-directories-first'        # sort by change time, reverse, human readable
alias lu='ls -lturh --color=always --group-directories-first'        # sort by access time, reverse, human readable
alias lr='ls -lRh --color=always --group-directories-first'          # recursive ls, human readable
alias lt='ls -ltrh --color=always --group-directories-first'         # sort by date, reverse, human readable
alias lw='ls -xAh --color=always --group-directories-first'          # wide listing format
alias lf="ls -l --color=always | grep -v '^d'" # files only
alias ldir="ls -l --color=always | grep '^d'"  # directories only

# Chmod Aliases
alias mx='chmod a+x'
alias 000='chmod -R 000'
alias 644='chmod -R 644'
alias 666='chmod -R 666'
alias 755='chmod -R 755'
alias 777='chmod -R 777'
alias chmox='chmod +x' # Zsh's already has this, keeping both
alias chowna='sudo chown -R $USER:$USER' # Zsh's already has this, keeping both
alias fixperm='find . -type d -exec chmod 755 {} \; && find . -type f -exec chmod 644 {} \;' # Zsh's already has this, keeping both

# Search Aliases
alias h="history | grep "           # Search command line history (Zsh's 'h' is history, combined)
alias p="ps aux | grep "            # Search running processes
alias topcpu="/bin/ps -eo pcpu,pid,user,args | sort -k 1 -r | head -10" # Top 10 CPU processes
alias f="find . | grep "            # Search files in the current folder

# Disk Space & Directory Information
alias diskspace="du -S | sort -n -r | more"                          # Disk space usage sorted by size
alias folders='du -h --max-depth=1'                                 # Disk usage for current directories
alias folderssort='find . -maxdepth 1 -type d -print0 | xargs -0 du -sk | sort -rn' # Sorted folder sizes
alias tree='tree -CAhF --dirsfirst'                                 # Graphical directory tree with colors and human readable
alias treed='tree -CAFd'                                            # Directory tree for directories only
alias mountedinfo='df -hT'                                          # Human-readable mounted filesystem info
alias df='df -h -x squashfs -x tmpfs -x devtmpfs' # Zsh's default df alias
alias free='free -h' # Zsh's default free alias
alias psmem='ps auxf | sort -nr -k 4 | head -10' # Zsh's default psmem alias
alias pscpu='ps auxf | sort -nr -k 3 | head -10' # Zsh's default pscpu alias


# Archive Aliases
alias mktar='tar -cvf'
alias mkbz2='tar -cvjf'
alias mkgz='tar -cvzf'
alias untar='tar -xvf'
alias unbz2='tar -xvjf'
alias ungz='tar -xvzf'

# Specific Mount Aliases (adjust as needed for your system)
alias mmp='sudo mount /dev/sdc1 /mnt/MP/'
alias me='sudo mount /dev/sda6 /mnt/E/'

# Audio Control Aliases
alias micon='pactl load-module module-loopback latency_msec=1'
alias micoff='pactl unload-module module-loopback'

# Docker
alias d='docker'
alias dc='docker compose'
alias dcu='docker compose up -d'
alias dcd='docker compose down'
alias dcl='docker compose logs -f'
alias dps='docker ps --format "table {{.ID}}\t{{.Names}}\t{{.Status}}\t{{.Ports}}"'
alias dpsa='docker ps -a --format "table {{.ID}}\t{{.Names}}\t{{.Status}}\t{{.Ports}}"'
alias dimg='docker images --format "table {{.ID}}\t{{.Repository}}\t{{.Tag}}\t{{.Size}}"'

# Development
alias py='python'
alias ipy='ipython'
alias serve='python -m http.server'
alias venv='python -m venv .venv && source .venv/bin/activate'

# Network
alias ip='ip -c a' # Zsh's default ip alias
alias iip='curl -s ifconfig.me/ip || curl -s api.ipify.org' # Zsh's default iip alias
alias ports='ss -tulanp' # Zsh's default ports alias
alias listen='ss -tulanp' # Zsh's default listen alias
alias httpdump='sudo tcpdump -i any -A -s 0 port 80'
alias sshgen='ssh-keygen -t ed25519 -a 100'
alias wtr='curl wttr.in' # Zsh's default wtr alias

# Fun
alias cx='cmatrix -B -u 2 | lolcat -p 100 -F 50' # Zsh's default cx alias

# Other Utility Aliases
alias countfiles="for t in files links directories; do echo \`find . -type \${t:0:1} | wc -l\` \$t; done 2> /dev/null"
alias checkcommand="type -t"
alias openports='netstat -nape --inet'
alias rebootsafe='sudo shutdown -r now'
alias rebootforce='sudo shutdown -r -n now'
alias logs="sudo find /var/log -type f -exec file {} \; | grep 'text' | cut -d' ' -f1 | sed -e's/:$//g' | grep -v '[0-9]$' | xargs tail -f"
alias sha1='openssl sha1'
alias please='sudo $(fc -ln -1)' # NOTE: This is clever; 'sudo !!' is a built-in alternative

# ===== FUNCTIONS =====

# Countdown function (requires figlet and lolcat)
cdown() {
    local N=$1
    if ! command -v figlet >/dev/null || ! command -v lolcat >/dev/null; then
        echo "cdown requires 'figlet' and 'lolcat' to be installed." >&2
        return 1
    fi
    while [[ $((--N)) -ge 0 ]]; do
        echo "$N" | figlet -c | lolcat && sleep 1
        if [[ "$N" -eq 0 ]]; then
            echo "Count Down Completed" | figlet -c | lolcat # Or whatever final message
            break
        fi
    done
}

# Universal Archive Extraction (enhanced with command checks)
ex() {
    if [[ -f "$1" ]]; then
        local filename="$1"
        local success=0
        case "$filename" in
            *.tar.bz2) command -v tar >/dev/null && tar xjf "$filename" || { echo "tar not found."; success=1; } ;;
            *.tar.gz)  command -v tar >/dev/null && tar xzf "$filename" || { echo "tar not found."; success=1; } ;;
            *.bz2)     command -v bunzip2 >/dev/null && bunzip2 "$filename" || { echo "bunzip2 not found."; success=1; } ;;
            *.rar)     command -v unrar >/dev/null && unrar x "$filename" || { echo "unrar not found."; success=1; } ;;
            *.gz)      command -v gunzip >/dev/null && gunzip "$filename" || { echo "gunzip not found."; success=1; } ;;
            *.tar)     command -v tar >/dev/null && tar xf "$filename" || { echo "tar not found."; success=1; } ;;
            *.tbz2)    command -v tar >/dev/null && tar xjf "$filename" || { echo "tar not found."; success=1; } ;;
            *.tgz)     command -v tar >/dev/null && tar xzf "$filename" || { echo "tar not found."; success=1; } ;;
            *.zip)     command -v unzip >/dev/null && unzip "$filename" || { echo "unzip not found."; success=1; } ;;
            *.Z)       command -v uncompress >/dev/null && uncompress "$filename" || { echo "uncompress not found."; success=1; } ;;
            *.7z)      command -v 7z >/dev/null && 7z x "$filename" || { echo "7z not found."; success=1; } ;;
            *.deb)     command -v ar >/dev/null && ar x "$filename" || { echo "ar not found."; success=1; } ;;
            *.tar.xz)  command -v tar >/dev/null && tar xf "$filename" || { echo "tar not found."; success=1; } ;;
            *.tar.zst) command -v tar >/dev/null && tar -I zstd -xf "$filename" || { echo "tar with zstd support not found."; success=1; } ;;
            *) echo "'$filename' cannot be extracted via ex()" >&2; success=1 ;;
        esac
        return $success
    else
        echo "'$1' is not a valid file" >&2
        return 1
    fi
}

# Searches for text in all files in the current folder (using grep or rg)
ftext() {
    if command -v rg &>/dev/null; then
        # Use ripgrep if available for speed and features
        rg -iIHrn --color=always "$1" . | less -r
    else
        # Fallback to grep
        grep -iIHrn --color=always "$1" . | less -r
    fi
}

# Copy file with a progress bar (using rsync for robustness)
cpp() {
    if [[ -z "$1" || -z "$2" ]]; then
        echo "Usage: cpp <source> <destination>"
        return 1
    fi
    rsync -WavP --human-readable --progress "$1" "$2"
}

# Copy and go to the directory
cpg() {
    if [ -d "$2" ]; then
        cp "$1" "$2" && cd "$2"
    else
        cp "$1" "$2"
    fi
}

# Move and go to the directory
mvg() {
    if [ -d "$2" ]; then
        mv "$1" "$2" && cd "$2"
    else
        mv "$1" "$2"
    fi
}

# Create and go to the directory (renamed to mkcd for brevity and consistency with Zsh's common practice)
mkcd() {
    mkdir -p "$1" && cd "$1" || return 1
}

# Goes up a specified number of directories  (i.e. up 4)
up() {
    local d=""
    local limit=$1
    if [[ -z "$limit" || "$limit" -lt 1 ]]; then
        limit=1
    fi
    for ((i = 1; i <= limit; i++)); do
        d=$d/..
    done
    d=$(echo $d | sed 's/^\///')
    if [ -z "$d" ]; then
        d=..
    fi
    cd $d
}

# Automatically do an ls after each cd, z, or zoxide
# Zsh has a built-in chpwd hook for this, which is more robust than aliasing cd.
# Add `autoload -Uz chpwd` and `chpwd_functions=(chpwd_ls)` to enable.
# Alternatively, you can override 'cd' function as done in bash:
cd() {
    if [ -n "$1" ]; then
        builtin cd "$@" && ls
    else
        builtin cd ~ && ls
    fi
}

# Show the current distribution
distribution() {
    local dtype="unknown" # Default to unknown

    # Use /etc/os-release for modern distro identification
    if [ -r /etc/os-release ]; then
        source /etc/os-release
        case $ID in
        fedora | rhel | centos)
            dtype="redhat"
            ;;
        sles | opensuse*)
            dtype="suse"
            ;;
        ubuntu | debian)
            dtype="debian"
            ;;
        gentoo)
            dtype="gentoo"
            ;;
        arch | manjaro)
            dtype="arch"
            ;;
        slackware)
            dtype="slackware"
            ;;
        *)
            # Check ID_LIKE only if dtype is still unknown
            if [ -n "$ID_LIKE" ]; then
                case $ID_LIKE in
                *fedora* | *rhel* | *centos*)
                    dtype="redhat"
                    ;;
                *sles* | *opensuse*)
                    dtype="suse"
                    ;;
                *ubuntu* | *debian*)
                    dtype="debian"
                    ;;
                *gentoo*)
                    dtype="gentoo"
                    ;;
                *arch*)
                    dtype="arch"
                    ;;
                *slackware*)
                    dtype="slackware"
                    ;;
                esac
            fi
            ;;
        esac
    fi
    echo $dtype
}

DISTRIBUTION=$(distribution)
if [ "$DISTRIBUTION" = "redhat" ] || [ "$DISTRIBUTION" = "arch" ]; then
    alias cat='bat'
else
    alias cat='batcat'
fi

# Show the current version of the operating system
ver() {
    local dtype
    dtype=$(distribution)

    case $dtype in
    "redhat")
        if [ -s /etc/redhat-release ]; then
            cat /etc/redhat-release
        else
            cat /etc/issue
        fi
        uname -a
        ;;
    "suse")
        cat /etc/SuSE-release
        ;;
    "debian")
        lsb_release -a
        ;;
    "gentoo")
        cat /etc/gentoo-release
        ;;
    "arch")
        cat /etc/os-release
        ;;
    "slackware")
        cat /etc/slackware-version
        ;;
    *)
        if [ -s /etc/issue ]; then
            cat /etc/issue
        else
            echo "Error: Unknown distribution"
            exit 1
        fi
        ;;
    esac
}

# Automatically install the needed support files for this config file
install_zshrc_support() { # Renamed function
    local dtype
    dtype=$(distribution)

    case $dtype in
    "arch")
        paru multitail tree zoxide trash-cli fzf fastfetch figlet lolcat
        ;;
    *)
        echo "Unknown distribution or unsupported for automatic installation."
        ;;
    esac
}

# IP address lookup
alias whatismyip="whatsmyip"
function whatsmyip() {
    # Internal IP Lookup.
    if command -v ip &>/dev/null; then
        echo -n "Internal IP: "
        ip addr show wlan0 | grep "inet " | awk '{print $2}' | cut -d/ -f1
    else
        echo -n "Internal IP: "
        ifconfig wlan0 | grep "inet " | awk '{print $2}'
    fi

    # External IP Lookup
    echo -n "External IP: "
    curl -4 ifconfig.me
}

# Git commit all changes with a message (redundant with 'gcm' but kept for direct migration)
gcom() {
    git add .
    git commit -m "$1"
}

# Git add, commit, and push in one go
lazyg() {
    git add .
    git commit -m "$1"
    git push
}

# Quick cheat sheet
cheat() {
    curl -s "cheat.sh/$1" | less -R
}

# Copy with progress
cpv() {
    rsync -WavP --human-readable --progress "$1" "$2"
}

# Get public IP with more details (already exists in Zsh config, kept the more detailed one)
myip() {
    echo "Public IPv4: $(curl -s ifconfig.me)"
    echo "Public IPv6: $(curl -s ifconfig.me/ipv6)"
    echo "Location: $(curl -s ifconfig.me/city), $(curl -s ifconfig.me/country)"
}

# Enhanced weather (already exists in Zsh config)
weather() {
    curl -s "wttr.in/${1:-}?m" | less -R
}

# Calculator (already exists in Zsh config)
calc() {
    echo "$*" | bc -l
}

# Function to interactively search file contents using ripgrep and fzf,
# then open the selected file in Neovim at the correct line.
# Usage: rgf <search_term> (renamed to fw for consistency with existing zshrc)
fw() {
  local search_term="$*" # Capture all arguments as a single search term
  if [ -z "$search_term" ]; then
    echo "Usage: fw <search_term>"
    return 1
  fi

  rg --line-number --color=always --no-heading "$search_term" | \
    fzf --ansi \
        --preview 'bat --color=always --line-range {2}: {1}' \
        --header "Search results for \"$search_term\"" \
        --exact --query "$search_term" \
    | awk -F: '{print $1 "+" $2}' | xargs -r nvim
}

# Alias for searching file names only (if you need it) (already exists in Zsh config)
# Usage: fdf <filename_pattern>
fdf() {
  local pattern="$*"
  if [ -z "$pattern" ]; then
    fd . | fzf | xargs -r nvim
  else
    fd "$pattern" | fzf --query "$pattern" | xargs -r nvim
  fi
}

# Fastfetch on interactive shell start (if available)
if [ -f /usr/bin/fastfetch ]; then
    fastfetch
fi

# Automatically start X server on tty1 if no DISPLAY is set (for graphical login)
# This is typically handled by display managers or systemd, but kept for direct migration if you manage it this way.
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx
fi

# --- END OF FILE ---
