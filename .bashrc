#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Source global definitions and bash completion
# These should be sourced early to ensure proper function.
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Determine if the shell is interactive for conditional bindings
iatest=$(expr index "$-" i)

### EXPORTS AND ENVIRONMENT VARIABLES

# Disable the bell for a quieter terminal
if [[ $iatest -gt 0 ]]; then bind "set bell-style visible"; fi

# History settings for better command recall
export HISTFILESIZE=10000
export HISTSIZE=500
export HISTTIMEFORMAT="%F %T"
export HISTCONTROL=erasedups:ignoredups:ignorespace

# Append to history instead of overwriting for continuous history across sessions
shopt -s histappend
PROMPT_COMMAND='history -a'

# Check window size after each command to update LINES and COLUMNS
shopt -s checkwinsize

# Set up XDG base directories for cleaner dotfiles
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

# Allow Ctrl-S for history navigation (complementing Ctrl-R)
[[ $- == *i* ]] && stty -ixon

# Auto-completion settings for convenience
if [[ $iatest -gt 0 ]]; then bind "set completion-ignore-case on"; fi
if [[ $iatest -gt 0 ]]; then bind "set show-all-if-ambiguous On"; fi

# Set the default editor to nvim
export EDITOR=nvim
export VISUAL=nvim

# Color settings for better readability in terminal output
export CLICOLOR=1
export LS_COLORS='no=00:fi=00:di=00;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:*.xml=00;31:'

# Manpage colors for better readability with 'less'
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Add common user binary paths to PATH
export PATH=$PATH:"$HOME/.local/bin:$HOME/.cargo/bin:/var/lib/flatpak/exports/bin:/.local/share/flatpak/exports/bin"

# Manpager preference: nvim with Man! plugin, fallback to less
export MANPAGER="nvim +Man!"
# If you prefer less:
# export MANPAGER="less"

# Uncomment to enable vi-like keybindings in bash (default is emacs-like)
# set -o vi
# bind -m vi-command 'Control-l: clear-screen'
# bind -m vi-insert 'Control-l: clear-screen'

### PROMPT AND INITIALIZATION

# Use Starship for a powerful and customizable prompt
eval "$(starship init bash)"
# Alternative classic prompt (uncomment if not using starship)
# PS1='[\u@\h \W]\$ '

# Initialize zoxide for smart directory navigation
eval "$(zoxide init bash)"

# Fastfetch on interactive shell start (if available)
if [ -f /usr/bin/fastfetch ]; then
    fastfetch
fi

# Automatically start X server on tty1 if no DISPLAY is set (for graphical login)
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx
fi

### ALIASES

# General Utilities
alias ls='ls -al --color=always --group-directories-first' # Enhanced ls
alias c='clear'
alias e='exit'
alias da='date "+%Y-%m-%d %A %T %Z"'                                                                                                                                 # Show formatted date
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"' # Notification for long-running commands
alias ebrc='nvim ~/.bashrc'                                                                                                                                          # Edit this .bashrc
alias reload='source ~/.bashrc'
alias cp='cp -i' # Interactive copy
alias mv='mv -i' # Interactive move
# alias rm='trash -v' # Use trash-cli for safe removal
alias mkdir='mkdir -p'  # Create parent directories as needed
alias ps='ps auxf'      # Detailed process listing
alias ping='ping -c 10' # Ping 10 times by default
alias less='less -R'    # Less with raw control characters
alias vi='nvim'         # Alias vi to nvim
alias vim='nvim'        # Alias vim to nvim
alias svi='sudo nvim'   # Sudo edit with nvim
alias snano='sudo nano' # Sudo nano (if installed)

# Grep Aliases (Prioritizing ripgrep if available)
if command -v rg &>/dev/null; then
    alias grep='rg'
else
    alias grep='grep --color=auto'
fi
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# Pacman and Paru (for Arch-based systems)
alias i='paru -S'                                                                                            # Install standard packages and AUR packages (paru)
alias u='paru -Syu'                                                                                          # Update standard and AUR packages (paru)
alias r='paru -Rn'                                                                                           # Remove the specified package
alias unlock='sudo rm /var/lib/pacman/db.lck'                                                                # Remove pacman lock
alias I='paru -Qi'                                                                                           # List information about a package
alias S='paru -Si'                                                                                           # Search for package information
alias cleanup='paru -Rns'                                                                                    # Remove orphaned packages (use with caution!)
alias yayf="yay -Slq | fzf --multi --preview 'yay -Sii {1}' --preview-window=down:75% | xargs -ro yay -S"    # Fuzzy find for yay
alias parf="paru -Slq | fzf --multi --preview 'paru -Sii {1}' --preview-window=down:75% | xargs -ro paru -S" # Fuzzy find for paru

# Reflector Aliases (for Arch Linux mirror management)
alias mirror="sudo reflector --latest 50 --number 20 --sort rate --protocol http,https --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

# Git Aliases
alias addall='git add .'
alias branch='git branch'
alias checkout='git checkout'
alias clone='git clone'
alias commit='git commit -m'
alias fetch='git fetch'
alias pull='git pull origin'
alias push='git push origin'
alias stt='git status' # 'status' is protected, using 'stt'

# Directory Navigation Aliases
alias home='cd ~'
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# LS Aliases (more comprehensive)
alias la='ls -Alh'               # show hidden files with human readable sizes
alias lx='ls -lXBh'              # sort by extension
alias lk='ls -lSrh'              # sort by size, reverse, human readable
alias lc='ls -ltcrh'             # sort by change time, reverse, human readable
alias lu='ls -lturh'             # sort by access time, reverse, human readable
alias lr='ls -lRh'               # recursive ls, human readable
alias lt='ls -ltrh'              # sort by date, reverse, human readable
alias lm='ls -alh |more'         # pipe through 'more'
alias lw='ls -xAh'               # wide listing format
alias ll='ls -Fls'               # long listing format, classification
alias labc='ls -lap'             # alphabetical sort, show hidden, classification
alias lf="ls -l | egrep -v '^d'" # files only
alias ldir="ls -l | egrep '^d'"  # directories only
alias lla='ls -Al'               # List and Hidden Files (no human readable)
alias las='ls -A'                # Hidden Files (no long format)
alias lls='ls -l'                # List (no hidden files, no human readable)

# Chmod Aliases
alias mx='chmod a+x'
alias 000='chmod -R 000'
alias 644='chmod -R 644'
alias 666='chmod -R 666'
alias 755='chmod -R 755'
alias 777='chmod -R 777'

# Search Aliases
alias h="history | grep "                                               # Search command line history
alias p="ps aux | grep "                                                # Search running processes
alias topcpu="/bin/ps -eo pcpu,pid,user,args | sort -k 1 -r | head -10" # Top 10 CPU processes
alias f="find . | grep "                                                # Search files in the current folder

# Disk Space & Directory Information
alias diskspace="du -S | sort -n -r |more"                                          # Disk space usage sorted by size
alias folders='du -h --max-depth=1'                                                 # Disk usage for current directories
alias folderssort='find . -maxdepth 1 -type d -print0 | xargs -0 du -sk | sort -rn' # Sorted folder sizes
alias tree='tree -CAhF --dirsfirst'                                                 # Graphical directory tree with colors and human readable
alias treed='tree -CAFd'                                                            # Directory tree for directories only
alias mountedinfo='df -hT'                                                          # Human-readable mounted filesystem info

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

### FUNCTIONS

# Countdown function (requires figlet and lolcat)
cdown() {
    N=$1
    while [[ $((--N)) -gt 0 ]]; do
        echo "$N" | figlet -c | lolcat && sleep 1
    done
}

# Universal Archive Extraction (your original 'ex' function is more concise)
ex() {
    if [ -f "$1" ]; then
        case $1 in
        *.tar.bz2) tar xjf "$1" ;;
        *.tar.gz) tar xzf "$1" ;;
        *.bz2) bunzip2 "$1" ;;
        *.rar) unrar x "$1" ;;
        *.gz) gunzip "$1" ;;
        *.tar) tar xf "$1" ;;
        *.tbz2) tar xjf "$1" ;;
        *.tgz) tar xzf "$1" ;;
        *.zip) unzip "$1" ;;
        *.Z) uncompress "$1" ;;
        *.7z) 7z x "$1" ;;
        *.deb) ar x "$1" ;;
        *.tar.xz) tar xf "$1" ;;
        *.tar.zst) unzstd "$1" ;;
        *) echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# Searches for text in all files in the current folder
ftext() {
    # -i case-insensitive
    # -I ignore binary files
    # -H causes filename to be printed
    # -r recursive search
    # -n causes line number to be printed
    # optional: -F treat search term as a literal, not a regular expression
    # optional: -l only print filenames and not the matching lines ex. grep -irl "$1" *
    grep -iIHrn --color=always "$1" . | less -r
}

# Copy file with a progress bar
cpp() {
    set -e
    strace -q -ewrite cp -- "${1}" "${2}" 2>&1 |
        awk '{
        count += $NF
        if (count % 10 == 0) {
            percent = count / total_size * 100
            printf "%3d%% [", percent
            for (i=0;i<=percent;i++)
                printf "="
            printf ">"
            for (i=percent;i<100;i++)
                printf " "
            printf "]\r"
        }
    }
    END { print "" }' total_size="$(stat -c '%s' "${1}")" count=0
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

# Create and go to the directory
mkdirg() {
    mkdir -p "$1"
    cd "$1"
}

# Goes up a specified number of directories  (i.e. up 4)
up() {
    local d=""
    limit=$1
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

            # If ID or ID_LIKE is not recognized, keep dtype as unknown
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

# Automatically install the needed support files for this .bashrc file
install_bashrc_support() {
    local dtype
    dtype=$(distribution)

    case $dtype in
    "arch")
        sudo paru multitail tree zoxide trash-cli fzf bash-completion fastfetch
        ;;
    *)
        echo "Unknown distribution"
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

# Git commit all changes with a message
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

# Count all files (recursively) in the current folder
alias countfiles="for t in files links directories; do echo \`find . -type \${t:0:1} | wc -l\` \$t; done 2> /dev/null"

# To see if a command is aliased, a file, or a built-in command
alias checkcommand="type -t"

# Show open ports
alias openports='netstat -nape --inet'

# Alias's for safe and forced reboots
alias rebootsafe='sudo shutdown -r now'
alias rebootforce='sudo shutdown -r -n now'

# Show all logs in /var/log
alias logs="sudo find /var/log -type f -exec file {} \; | grep 'text' | cut -d' ' -f1 | sed -e's/:$//g' | grep -v '[0-9]$' | xargs tail -f"

# SHA1 hash of a file
alias sha1='openssl sha1'

# Bind Ctrl+f to insert 'zi' followed by a newline (for zoxide interactive search)
if [[ $- == *i* ]]; then
    bind '"\C-f":"zi\n"'
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
