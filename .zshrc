#!/usr/bin/env zsh

# ===== ENVIRONMENT =====
export EDITOR='nvim'
export VISUAL='nvim'
export MANPAGER="nvim +Man!"
export PAGER='less'
export LESS='-R'

# PATH management - more organized and comprehensive
typeset -U PATH path # Ensure no duplicates in PATH
path=(
    $HOME/.local/bin
    $HOME/.cargo/bin
    $HOME/go/bin
    $HOME/.npm-global/bin
    /usr/local/bin
    /usr/local/sbin
    $path
)
export PATH

# ===== ZSH OPTIONS =====
setopt autocd                   # cd by typing directory name
setopt correct                  # autocorrect commands
setopt correct_all              # autocorrect all arguments
setopt interactivecomments      # allow comments in interactive shell
setopt histignorealldups        # remove older duplicate history entries
setopt sharehistory             # share history across terminals
setopt incappendhistory         # write to history immediately
setopt extendedglob             # advanced globbing
setopt no_beep                  # no beep on errors
setopt complete_in_word         # Complete from both ends of a word
setopt always_to_end            # Move cursor to end if word had one match
setopt auto_menu                # Show completion menu on successive tab press
setopt auto_list                # Automatically list choices on ambiguous completion
setopt auto_param_slash         # Add trailing slash when completing directories
setopt extended_glob            # Needed for file modification glob modifiers
setopt multios                  # Allow multiple redirections
setopt cdable_vars              # cd to variables as if they were directories
setopt pushd_ignore_dups        # Don't push duplicate directories
setopt pushd_silent             # Silence pushd/popd output
setopt chase_links              # Resolve symlinks to their true path

# ===== HISTORY =====
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
setopt appendhistory
setopt hist_expire_dups_first   # Expire duplicates first
setopt hist_ignore_dups         # Ignore repeated commands
setopt hist_ignore_all_dups     # Remove older duplicate entries
setopt hist_save_no_dups        # Don't write duplicate entries
setopt hist_find_no_dups        # Don't display duplicates when searching
setopt hist_reduce_blanks       # Remove superfluous blanks
setopt hist_verify              # Show command before executing

# ===== KEYBINDINGS =====
bindkey -v                      # vi mode
bindkey '^L' clear-screen       # Ctrl+L to clear screen
bindkey '^R' history-incremental-search-backward # Better history search

# Fix keybindings for home/end keys
bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line
bindkey  "^[[3~"  delete-char

# ===== STARSHIP PROMPT =====
# NOTE: Ensure Starship is installed. This line initializes it.
if command -v starship >/dev/null; then
  eval "$(starship init zsh)"
else
  # ADDED: Fallback prompt if starship is not found
  setopt PROMPT_SUBST
  PROMPT='%F{blue}%~%f %F{yellow}$%f '
  RPROMPT='%F{green}%*%f'
fi

# ===== ZINIT PLUGIN MANAGER =====
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
zinit light agkozak/zsh-z                   # Fast directory jumping
zinit light hlissner/zsh-autopair           # Auto-close brackets/quotes
zinit light djui/alias-tips                 # Show tips when using aliases
zinit light zdharma-continuum/fast-syntax-highlighting

# Additional useful plugins
zinit light MichaelAquilina/zsh-you-should-use  # Suggests aliases
#zinit light marlonrichert/zsh-autocomplete     # Enhanced autocomplete
zinit light Tarrasch/zsh-bd                   # Quick directory navigation
zinit light zdharma-continuum/history-search-multi-word  # Better history search

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

# ===== FUNCTIONS =====
# Enhanced countdown
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

# Enhanced extract function
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

# Create and cd into directory
mkcd() {
    mkdir -p "$1" && cd "$1" || return 1
}

# Quick cheat sheet
cheat() {
    curl -s "cheat.sh/$1" | less -R
}

# Copy with progress
cpv() {
    rsync -WavP --human-readable --progress "$1" "$2"
}

# Get public IP with more details
myip() {
    echo "Public IPv4: $(curl -s ifconfig.me)"
    echo "Public IPv6: $(curl -s ifconfig.me/ipv6)"
    echo "Location: $(curl -s ifconfig.me/city), $(curl -s ifconfig.me/country)"
}

# Enhanced weather
weather() {
    curl -s "wttr.in/${1:-}?m" | less -R
}

# Calculator
calc() {
    echo "$*" | bc -l
}

# Function to interactively search file contents using ripgrep and fzf,
# then open the selected file in Neovim at the correct line.
# Usage: rgf <search_term>
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

# Alias for searching file names only (if you need it)
# Usage: fdf <filename_pattern>
fdf() {
  local pattern="$*"
  if [ -z "$pattern" ]; then
    fd . | fzf | xargs -r nvim
  else
    fd "$pattern" | fzf --query "$pattern" | xargs -r nvim
  fi
}
# ===== ALIASES =====
# Navigation & Shell
alias ls='ls -Ah --color=always --group-directories-first' 
alias ll='ls -lAh --color=always --group-directories-first'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias c='clear'
alias e='exit'
alias h='history'
alias path='echo -e ${PATH//:/\\n}'
alias reload='source ~/.zshrc && echo "Zsh config reloaded!"'
alias zshconfig="\$EDITOR ~/.zshrc"
alias xc='xclip -selection clipboard'

# Editors & Programs
alias vim='nvim'
alias nv='nvim'
alias sv='sudo nvim'
alias rr='ranger'
alias ff='fastfetch'
alias ffa='fastfetch -c all'
# Sudo & Permissions
alias sudo='sudo ' # Ensure aliases are expanded after sudo
alias please='sudo $(fc -ln -1)' # NOTE: This is clever; 'sudo !!' is a built-in alternative
alias chmox='chmod +x'
alias chowna='sudo chown -R $USER:$USER'
alias fixperm='find . -type d -exec chmod 755 {} \; && find . -type f -exec chmod 644 {} \;'

# System info
alias df='df -h -x squashfs -x tmpfs -x devtmpfs'
alias du='du -h -d1'
alias free='free -h'
alias psmem='ps auxf | sort -nr -k 4 | head -10'
alias pscpu='ps auxf | sort -nr -k 3 | head -10'


# Package management
alias i='paru -S'
alias u='paru -Syu'
alias r='paru -Rn'
alias unlock='sudo rm /var/lib/pacman/db.lck'
alias I='paru -Qi'
alias S='paru -Si'
alias cleanup='paru -Rns $(paru -Qtdq)'
alias orphan='paru -Qtdq'
alias pactree='pactree -c'
alias mirror="sudo reflector --latest 50 --number 20 --sort rate --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

# Git
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
alias gpl='git pull --rebase'
alias gst='git status'
alias gsta='git stash push'
alias gstp='git stash pop'
alias gsw='git switch'
alias gswc='git switch -c'
alias grb='git rebase'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias grbi='git rebase -i'

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
alias venv='python -m venv venv && source venv/bin/activate'

# Network
alias ip='ip -c a'
alias iip='curl -s ifconfig.me/ip || curl -s api.ipify.org'
alias ports='ss -tulanp'
alias listen='ss -tulanp'
alias ping='ping -c 5'
alias httpdump='sudo tcpdump -i any -A -s 0 port 80'
alias sshgen='ssh-keygen -t ed25519 -a 100'
alias wtr='curl wttr.in'

# Fun
alias cx='cmatrix -B -u 2 | lolcat -p 100 -F 50'

# --- END OF FILE ---
