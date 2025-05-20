# ~/.zshrc

# --- ENVIRONMENT ---
export EDITOR='nvim'
export MANPAGER="nvim +Man!"
export PATH="$HOME/.cargo/bin:$PATH"

# --- ZSH OPTIONS ---
setopt autocd             # cd by typing directory name
setopt correct            # autocorrect commands
setopt interactivecomments
setopt histignorealldups  # remove older duplicate history entries
setopt sharehistory       # share history across terminals
setopt incappendhistory   # write to history immediately
setopt extendedglob       # advanced globbing
setopt no_beep            # no beep on errors
setopt COMPLETE_IN_WORD    # Complete from both ends of a word
setopt ALWAYS_TO_END       # Move cursor to end if word had one match
setopt AUTO_MENU          # Show completion menu on successive tab press
setopt AUTO_LIST          # Automatically list choices on ambiguous completion
setopt AUTO_PARAM_SLASH   # If completed parameter is a directory, add a trailing slash
setopt EXTENDED_GLOB      # Needed for file modification glob modifiers with compinit

# --- History ---
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_FIND_NO_DUPS

# --- KEYBINDINGS ---
bindkey -v                # vi mode
bindkey '^L' clear-screen # Ctrl+L to clear screen

# --- STARSHIP PROMPT ---
eval "$(starship init zsh)"

# --- ZINIT PLUGIN MANAGER ---
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
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk

# Completion styling
zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.cache/zsh/zcompcache
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:messages' format '%F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format '%F{red}-- no matches found --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# --- ESSENTIAL PLUGINS ---
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light agkozak/zsh-z            # Fast directory jumping
zinit light hlissner/zsh-autopair    # Auto-close brackets/quotes
zinit light djui/alias-tips          # Show tips when using aliases
zinit light zsh-users/zsh-syntax-highlighting
zinit light zdharma-continuum/fast-syntax-highlighting

# --- FZF (if installed) ---
if command -v fzf >/dev/null; then
  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
fi

# --- MIGRATED FUNCTIONS ---
cdown() {
    N=$1
    while [[ $((--N)) -gt 0 ]]; do
        echo "$N" | figlet -c | lolcat && sleep 1
    done
}

ex() {
    if [[ -f "$1" ]]; then
        case "$1" in
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

# --- ALIASES (IMPROVED & EXTENDED) ---
alias ls='ls -lh --color=always --group-directories-first'
alias lsa='ls -lah --color=always --group-directories-first'
alias c='clear'
alias e='exit'
alias vim='nvim'
alias v='nvim'
alias cx='cmatrix -B -u 2 | lolcat -p 100 -F 50'

# Grep with color
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# Pacman/Paru
alias i='paru -S'
alias u='paru -Syu'
alias r='paru -Rn'
alias unlock='sudo rm /var/lib/pacman/db.lck'
alias I='paru -Qi'
alias S='paru -Si'
alias cleanup='paru -Rns'
alias mirror="sudo reflector --latest 50 --number 20 --sort rate --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

# Git
alias ga='git add .'
alias gc='git commit -m'
alias gcb='git checkout -b'
alias gco='git checkout'
alias gcl='git clone'
alias gd='git diff'
alias gl='git log --oneline --graph --decorate'
alias gp='git push'
alias gpl='git pull'
alias gpop='git stash pop'
alias gs='git status'
alias gsw='git switch'
alias gsta='git stash'
alias gswc='git switch -c'

# General productivity
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias please='sudo $(fc -ln -1)' # rerun last command with sudo
alias ip='ip -c a'
alias myip='curl ifconfig.me'
alias weather='curl wttr.in'
alias zshconfig="$EDITOR ~/.zshrc"
alias reload="source ~/.zshrc"
alias h="history"
alias ports="netstat -tulanp"
alias mmp='sudo mount /dev/sdc1 /mnt/MP/'
alias me='sudo mount /dev/sda6 /mnt/E/'
alias micon='pactl load-module module-loopback latency_msec=1'
alias micoff='pactl unload-module module-loopback'

# Coding/dev
alias py='python'
alias serve='python -m http.server'
alias mkvenv='python -m venv venv && source venv/bin/activate'



# --- END OF FILE ---
