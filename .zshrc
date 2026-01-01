#!/usr/bin/env zsh
# ==============================================================================
#   ZSH CONFIGURATION VERSION 2
# ==============================================================================

# --- 1. ENVIRONMENT VARIABLES -------------------------------------------------

# Default Editors
export EDITOR='nvim'
export VISUAL='nvim'
export MANPAGER="nvim +Man!"
export PAGER='less'
export LESS='-R -F -X -K' # -F quit if one screen, -X disable termcap init, -K allow less to quit on ctrl-c

# XDG Base Directory Specification
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_STATE_HOME="${HOME}/.local/state"
export XDG_CACHE_HOME="${HOME}/.cache"

# History Configuration
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

# Better History Options
setopt appendhistory
setopt hist_expire_dups_first # Expire duplicate entries first when trimming history.
setopt hist_ignore_dups       # Don't record an entry that was just recorded again.
setopt hist_ignore_all_dups   # Delete old recorded entry if new entry is a duplicate.
setopt hist_find_no_dups      # Do not display a line previously found.
setopt hist_reduce_blanks     # Remove superfluous blanks before recording entry.
setopt hist_verify            # Show command with history expansion to user before executing it.
setopt inc_append_history     # Write to the history file immediately, not when the shell exits.
setopt share_history          # Share history between all sessions.
setopt extended_history       # Record timestamp in history :<beginning time>:<seconds>:<command>

# Colors
export CLICOLOR=1
export LS_COLORS='no=00:fi=00:di=00;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:*.xml=00;31:'

# Manpage colors
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# PATH Management
typeset -U PATH path
path=(
    $HOME/.local/bin
    $HOME/.cargo/bin
    $HOME/go/bin
    $HOME/.npm-global/bin
    $HOME/.config/v-analyzer/bin
    $HOME/.dotnet/tools
    /var/lib/flatpak/exports/bin
    /var/lib/flatpak/exports/share/bin
    ~/.local/share/flatpak/exports/bin
    /usr/local/bin
    /usr/local/sbin
    $path
)
export PATH

# --- 2. ZSH OPTIONS ----------------------------------------------------------

# Changing Directories
setopt autocd              # Change directory just by typing its name
setopt cdable_vars         # Change directory to a variable path
setopt pushd_ignore_dups   # Don't push duplicate directories
setopt pushd_silent        # Silence pushd/popd output
setopt pushd_to_home       # Push to home directory if no args

# Completion
setopt always_to_end       # Move cursor to the end of a completed word.
setopt auto_list           # Automatically list choices on ambiguous completion.
setopt auto_menu           # Show completion menu on successive tab presses.
setopt auto_param_slash    # If completed parameter is a directory, add a trailing slash.
setopt complete_in_word    # Complete from both ends of a word.
setopt extended_glob       # Needed for file modification glob modifiers (e.g., *, ^, ~)
setopt path_dirs           # Perform path search even on command names with slashes.
setopt list_packed         # Compact completion lists

# Correction (Optional: disable if annoying)
setopt correct             # Correct commands
setopt correct_all         # Correct arguments
# Disable the annoying "correct to..." prompt
# unsetopt correct
# unsetopt correctall

# History
setopt hist_ignore_space   # Don't record commands starting with a space

# Input/Output
setopt multios             # Perform multiple redirections (e.g., >file1 >file2)
setopt interactivecomments # Allow comments in interactive shell

# Job Control
setopt long_list_jobs      # List jobs in the long format by default
setopt notify              # Report status of background jobs immediately

# --- 3. KEYBINDINGS & VI MODE -----------------------------------------------

# Enable Vi Mode
bindkey -v

# Reduce ESC key timeout (better for vi mode)
export KEYTIMEOUT=1

# Use builtin key bindings for Home/End/Delete where possible
# Using `terminfo` is more robust than hardcoded ANSI codes
[[ -n "${terminfo[khome]}" ]] && bindkey "${terminfo[khome]}" beginning-of-line
[[ -n "${terminfo[kend]}" ]]  && bindkey "${terminfo[kend]}" end-of-line
[[ -n "${terminfo[kdch1]}" ]] && bindkey "${terminfo[kdch1]}" delete-char
[[ -n "${terminfo[kich1]}" ]] && bindkey "${terminfo[kich1]}" overwrite-mode

# History Search (Ctrl+R)
bindkey '^R' history-incremental-search-backward

# Vi Mode: Fix Backspace issues in vi mode
bindkey '^?' backward-delete-char
bindkey '^H' backward-delete-char

# Vi Mode: Better history search in vicmd mode
# bindkey -M vicmd 'k' history-substring-search-up
# bindkey -M vicmd 'j' history-substring-search-down

# Change cursor shape based on vi mode
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q' # Block
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q' # Beam (Line)
  fi
}
zle -N zle-keymap-select
zle-line-init() { zle-keymap-select; }
zle -N zle-line-init

# --- 4. PROMPT ---------------------------------------------------------------

if command -v starship >/dev/null; then
  eval "$(starship init zsh)"
else
  # Robust Fallback Prompt
  setopt PROMPT_SUBST
  # User color: Red for root, Blue for user
  local user_color="%(!.%F{red}.%F{blue})"
  PROMPT='${user_color}%n%f@%F{magenta}%m%f:%F{green}%~%f ${user_color}%#%f '
  RPROMPT='%F{yellow}%D{%H:%M:%S}%f'
fi

# --- 5. PLUGIN MANAGER (ZINIT) ----------------------------------------------

# Ensure Zinit is installed
### Added by Zinit's installer
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

# Load a few important annexes, without Turbo
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk

# Load Plugins (Order matters: Completions -> Highlighting -> Autosuggestions)
zinit light zsh-users/zsh-completions

# Syntax Highlighting (Must be loaded last)
zinit light zdharma-continuum/fast-syntax-highlighting

# Autosuggestions (Must be loaded after syntax highlighting usually)
zinit light zsh-users/zsh-autosuggestions

# Load 0 seconds after the prompt appears (Turbo Mode)
zinit wait lucid for \
    agkozak/zsh-z \
    hlissner/zsh-autopair \
    djui/alias-tips \
    MichaelAquilina/zsh-you-should-use \
    Tarrasch/zsh-bd \
    zdharma-continuum/history-search-multi-word

# --- 6. COMPLETION STYLING -------------------------------------------------
autoload -Uz compinit
if [[ -n ${ZDOTDIR:-$HOME}/.zcompdump(#qN.m-1) ]]; then
  compinit -C
else
  compinit
fi

# Tell zsh that 'cf' should complete like 'cat' or 'cp' (files)
compdef '_files' cf

source <(chezmoi completion zsh)

zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh/zcompcache
zstyle ':completion:*' menu select
# Fuzzy matching (case insensitive)
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' '+r:|[._-]=* r:|=*' '+l:|=* r:|=*'

zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:messages' format '%F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format '%F{red}-- no matches found --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' group-order 'directories' 'files' 'commands' 'builtins'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-separator '-->'
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

# --- 7. FUZZY FINDER (FZF) -------------------------------------------------

if command -v fzf >/dev/null; then
  export FZF_DEFAULT_OPTS='--height 40% --layout=reverse --border --color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'

  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

  if command -v fd >/dev/null; then
    export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_ALT_C_COMMAND="fd --type d --hidden --follow --exclude .git"
  fi
fi

# --- 8. ALIASES -------------------------------------------------------------

# === General Utilities ===
alias ls='ls -lhv --group-directories-first --color=auto'
alias c='clear'
alias e='exit'
alias da='date "+%Y-%m-%d %A %T %Z"'
alias zshconfig='${EDITOR} ~/.zshrc'
alias reload='source ~/.zshrc && echo "Zsh config reloaded!"'
alias cp='cp -irv'
alias mv='mv -iv'
alias mkdir='mkdir -pv'
alias ps='ps auxf'
alias ping='ping -c 10'
alias less='less -R'
alias vim='nvim'
alias svim='sudo -E nvim'
alias snano='sudo nano'
alias rr='ranger'
alias ff='fastfetch'
alias dl='aria2c'
alias xc='xclip -selection clipboard'

# === Chezmoi Aliases ===
# The "Standard" workflow: Edit -> Diff -> Apply
alias cz="chezmoi"
alias cze="chezmoi edit"
alias czd="chezmoi diff"
alias cza="chezmoi apply -v"

# The "Review & Sync" workflow: 
alias czu="chezmoi update -v"

# The "Emergency" / Quick-access
# Quickly jump into the source directory to do git operations
alias czcd='cd $(chezmoi source-path)'

alias czs="chezmoi status"

# === Grep ===
if command -v rg &>/dev/null; then
    alias grep='rg --smart-case'
    alias egrep='rg --smart-case'
    alias fgrep='rg --smart-case'
else
    alias grep='grep --color=auto'
    alias egrep='egrep --color=auto'
    alias fgrep='fgrep --color=auto'
fi

# === OS Specific (Arch Linux) ===
if command -v pacman &>/dev/null; then
    # Determine AUR helper preference (yay > paru > pacman)
    if command -v yay >/dev/null; then
        AUR_HELPER="yay"
    elif command -v paru >/dev/null; then
        AUR_HELPER="paru"
    else
        AUR_HELPER="sudo pacman"
    fi

    alias i="$AUR_HELPER -S"
    alias u="$AUR_HELPER -Syu"
    alias r="$AUR_HELPER -Rn"
    alias unlock='sudo rm /var/lib/pacman/db.lck'
    alias I="$AUR_HELPER -Qq | fzf --multi --preview 'pacman -Qil {}' --layout=reverse --preview-window=right:70% --bind 'enter:execute(pacman -Qil {+} | less)'"
    alias S="$AUR_HELPER -Slq | fzf --multi --preview 'pacman -Si {}' --layout=reverse --preview-window=right:70% --bind 'enter:execute(pacman -Si {+} | less)'"
    alias cleanup="$AUR_HELPER -Qtq | fzf --multi --preview 'pacman -Qil {}' --preview-window=right:70% | xargs -ro $AUR_HELPER -Rns"
    alias pkgf="yay -Slq | fzf --multi --preview 'yay -Sii {1}' --preview-window=down:75% | xargs -ro yay -S"
    
    # Reflector
    if command -v reflector >/dev/null; then
        alias mirror="sudo reflector --latest 50 --number 20 --sort rate --protocol http,https --save /etc/pacman.d/mirrorlist"
        alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
        alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
        alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"
    fi
fi

# === Git ===
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

# === Directory Navigation ===
alias home='cd ~'
alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# === LS Variants ===
alias lx='ls -lXBh --group-directories-first'     # sort by extension
alias lk='ls -lSrh --group-directories-first'     # sort by size
alias lt='ls -ltrh --group-directories-first'     # sort by date
alias lw='ls -xAh --group-directories-first'      # wide
alias lf="ls -l | grep -v '^d'"                  # files only
alias ldir="ls -l | grep '^d'"                   # dirs only

# === Permissions ===
alias mx='chmod a+x'
alias 000='chmod -R 000'
alias 644='chmod -R 644'
alias 755='chmod -R 755'
alias chmox='chmod +x'
alias chowna='sudo chown -R $USER:$USER'
alias fixperm='find . -type d -exec chmod 755 {} \; && find . -type f -exec chmod 644 {} \;'

# === Tmux ===
alias t='tmux new -s $(basename "$PWD") || tmux attach -t $(basename "$PWD")'
alias tn='tmux new -s '
alias ta='tmux attach'
alias tls='tmux ls'
alias tat='tmux attach -t '
alias tks='tmux kill-session -t '
alias tkillall='tmux kill-server'

# === Search ===
alias h="history | grep "
alias p="ps aux | grep "
alias topcpu="/bin/ps -eo pcpu,pid,user,args | sort -k 1 -r | head -10"
alias f="find . | grep "

# === Disk Space ===
alias diskspace="du -S | sort -n -r | more"
alias folders='du -h --max-depth=1'
alias folderssort='find . -maxdepth 1 -type d -print0 | xargs -0 du -sk | sort -rn'
alias tree='tree -CAhF --dirsfirst'
alias treed='tree -CAFd'
alias mountedinfo='df -hT'
alias df='df -h -x squashfs -x tmpfs -x devtmpfs'
alias free='free -h'
alias psmem='ps auxf | sort -nr -k 4 | head -10'
alias pscpu='ps auxf | sort -nr -k 3 | head -10'

# === Archives ===
alias mktar='tar -cvf'
alias mkbz2='tar -cvjf'
alias mkgz='tar -cvzf'
alias untar='tar -xvf'
alias unbz2='tar -xvjf'
alias ungz='tar -xvzf'

# === Audio ===
alias micon='pactl load-module module-loopback latency_msec=1'
alias micoff='pactl unload-module module-loopback'

# === Docker ===
alias d='docker'
alias dc='docker compose'
alias dcu='docker compose up -d'
alias dcd='docker compose down'
alias dcl='docker compose logs -f'
alias dps='docker ps --format "table {{.ID}}\t{{.Names}}\t{{.Status}}\t{{.Ports}}"'
alias dpsa='docker ps -a --format "table {{.ID}}\t{{.Names}}\t{{.Status}}\t{{.Ports}}"'
alias dimg='docker images --format "table {{.ID}}\t{{.Repository}}\t{{.Tag}}\t{{.Size}}"'

# === Development ===
alias py='python'
alias ipy='ipython'
alias serve='python -m http.server'
alias venv='python -m venv .venv && source .venv/bin/activate'

# === Network ===
alias ip='ip -c a'
alias iip='curl -s ifconfig.me/ip || curl -s api.ipify.org'
alias ports='ss -tulanp'
alias listen='ss -tulanp'
alias httpdump='sudo tcpdump -i any -A -s 0 port 80'
alias sshgen='ssh-keygen -t ed25519 -a 100'
alias wtr='curl wttr.in'

# === Fun ===
alias cx='cmatrix -B -u 2 | lolcat -p 100 -F 50'

# === Other ===
alias countfiles="for t in files links directories; do echo \`find . -type \${t:0:1} | wc -l\` \$t; done 2> /dev/null"
alias checkcommand="type -t"
alias openports='netstat -nape --inet'
alias rebootsafe='sudo shutdown -r now'
alias rebootforce='sudo shutdown -r -n now'
alias logs="sudo find /var/log -type f -exec file {} \; | grep 'text' | cut -d' ' -f1 | sed -e's/:$//g' | grep -v '[0-9]$' | xargs tail -f"
alias sha1='openssl sha1'
alias please='sudo $(fc -ln -1)'

# --- 9. FUNCTIONS -----------------------------------------------------------

# === Yazi ===
function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	command yazi "$@" --cwd-file="$tmp"
	IFS= read -r -d '' cwd < "$tmp"
	[ -n "$cwd" ] && [ "$cwd" != "$PWD" ] && builtin cd -- "$cwd"
	rm -f -- "$tmp"
}
# === Clipboard ===
if type clipcat-menu >/dev/null 2>&1; then
    alias clipedit='clipcat-menu --finder=builtin edit'
    alias clipdel='clipcat-menu --finder=builtin remove'

    bindkey -s '^\' "^Q clipcat-menu --finder=builtin insert ^J"
    bindkey -s '^]' "^Q clipcat-menu --finder=builtin remove ^J"
fi

# === Countdown ===
cdown() {
    local N=$1
    if ! command -v figlet >/dev/null || ! command -v lolcat >/dev/null; then
        echo "cdown requires 'figlet' and 'lolcat' to be installed." >&2
        return 1
    fi
    while [[ $((--N)) -ge 0 ]]; do
        echo "$N" | figlet -c | lolcat && sleep 1
        if [[ "$N" -eq 0 ]]; then
            echo "Count Down Completed" | figlet -c | lolcat
            break
        fi
    done
}

# === Universal Archive Extraction ===
ex() {
    if [[ -f "$1" ]]; then
        local filename="$1"
        local success=0

        case "$filename" in
            *.tar.bz2|*.tbz2)   tar xjf "$filename" ;;
            *.tar.gz|*.tgz)     tar xzf "$filename" ;;
            *.bz2)              bunzip2 "$filename" ;;
            *.rar)              unrar x "$filename" ;;
            *.gz)               gunzip "$filename" ;;
            *.tar)              tar xf "$filename" ;;
            *.zip)              unzip "$filename" ;;
            *.Z)                uncompress "$filename" ;;
            *.7z)               7z x "$filename" ;;
            *.deb)              ar x "$filename" ;;
            *.tar.xz)           tar xf "$filename" ;;
            *.tar.zst)          tar --use-compress-program=unzstd -xf "$filename" ;;
            *)                  echo "'$filename' cannot be extracted via ex()" >&2; success=1 ;;
        esac
        return $success
    else
        echo "'$1' is not a valid file" >&2
        return 1
    fi
}

# === File Text Search ===
ftext() {
    if command -v rg &>/dev/null; then
        rg -iIHrn --color=always "$1" . | less -r
    else
        grep -iIHrn --color=always "$1" . | less -r
    fi
}

# === Universal File Copy to Clipboard ===
cf() {
    # Check if a file was actually provided
    if [[ ! -f "$1" ]]; then
        echo "Error: '$1' is not a valid file." >&2
        return 1
    fi

    # Determine the clipboard tool based on the session type
    if [[ "$XDG_SESSION_TYPE" == "wayland" ]] && command -v wl-copy >/dev/null 2>&1; then
        wl-copy < "$1"
    elif command -v xclip >/dev/null 2>&1; then
        xclip -selection clipboard < "$1"
    elif command -v xsel >/dev/null 2>&1; then
        xsel --clipboard --input < "$1"
    else
        echo "Error: No clipboard tool found (install wl-clipboard or xclip)." >&2
        return 1
    fi

    echo "Contents of '$1' copied to clipboard."
}

# === Copy with Progress ===
cpp() {
    if [[ -z "$1" || -z "$2" ]]; then
        echo "Usage: cpp <source> <destination>"
        return 1
    fi
    rsync -WavP --human-readable --progress "$1" "$2"
}

# === Copy/Move and Go ===
cpg() {
    if [ -d "$2" ]; then
        cp "$1" "$2" && cd "$2"
    else
        cp "$1" "$2"
    fi
}

mvg() {
    if [ -d "$2" ]; then
        mv "$1" "$2" && cd "$2"
    else
        mv "$1" "$2"
    fi
}

# === Make Directory and Go ===
mkcd() {
    mkdir -p "$1" && cd "$1" || return 1
}

# === Go Up N Directories ===
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

# === Cat/Bat Logic ===
if command -v bat &>/dev/null; then
    alias cat='bat'
elif command -v batcat &>/dev/null; then
    alias cat='batcat'
fi

# === Install Support Tools ===
install_zshrc_support() {
    echo "Installing Arch support packages..."
    yay -S multitail tree zoxide trash-cli fzf fastfetch figlet lolcat
}

# === IP Address Lookup ===
alias whatismyip="whatsmyip"
whatsmyip() {
    local iface
    iface=$(ip route get 1 2>/dev/null | awk '{print $5; exit}')
    
    echo -n "Internal IP: "
    if [ -n "$iface" ]; then
        ip addr show "$iface" | grep "inet " | awk '{print $2}' | cut -d/ -f1
    else
        echo "Could not detect interface."
    fi

    echo -n "External IP: "
    curl -4 ifconfig.me
}

# === Git Utils ===
gcom() {
    git add .
    git commit -m "$1"
}

lazyg() {
    git add .
    git commit -m "$1"
    git push
}

# === Cheat Sheet ===

# === Public IP Details ===
myip() {
    echo "Public IPv4: $(curl -s ifconfig.me)"
    echo "Public IPv6: $(curl -s ifconfig.me/ipv6)"
    echo "Location: $(curl -s ifconfig.me/city), $(curl -s ifconfig.me/country)"
}

# === Weather ===
weather() {
    curl -s "wttr.in/${1:-}?m" | less -R
}

# === Calculator ===
calc() {
    echo "$*" | bc -l
}

# === FZF File Search ===
fw() {
    local search_term="$*"
    if [ -z "$search_term" ]; then
        echo "Usage: fw <search_term>"
        return 1
    fi

    # Check if bat is installed for preview
    local preview_cmd="cat"
    if command -v bat >/dev/null; then
        preview_cmd="bat --color=always --line-range {2}: {1}"
    fi

    rg --line-number --no-heading "$search_term" | \
        fzf --ansi \
            --preview "$preview_cmd" \
            --header "Search results for \"$search_term\"" \
            --exact --query "$search_term" \
        | awk -F: '{print $1 "+" $2}' | xargs -r nvim
}

# === FZF Directory Search ===
fdf() {
    local pattern="$*"
    if command -v fd >/dev/null; then
        if [ -z "$pattern" ]; then
            fd . | fzf | xargs -r nvim
        else
            fd "$pattern" | fzf --query "$pattern" | xargs -r nvim
        fi
    else
        echo "Error: 'fd' command not found."
    fi
}

# --- 10. MISC & HOOKS ------------------------------------------------------

# Auto ls after cd
chpwd() {
    ls
}

# Fastfetch on start (if exists and interactive)
if [ -t 1 ] && command -v fastfetch >/dev/null; then
    fastfetch
fi

# Auto start X on tty1 (Arch specific)
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx
fi

# --- 11. SDKMAN (Must be at end) -------------------------------------------
# export SDKMAN_DIR="$HOME/.sdkman"
# [[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"

