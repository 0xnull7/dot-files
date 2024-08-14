#
# ~/.bashrc
#
# My bash config. Not much to see here; just some pretty standard stuff.

### SET MANPAGER
### Uncomment only one of these!

### "nvim" as manpager
export MANPAGER="nvim +Man!"
export PATH=/home/usb/.cargo/bin:$PATH

### "less" as manpager
# export MANPAGER="less"

### SET VI MODE ###
# Comment this line out to enable default emacs-like bindings
#set -o vi
#bind -m vi-command 'Control-l: clear-screen'
#bind -m vi-insert 'Control-l: clear-screen'

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

### PROMPT
eval "$(starship init bash)"
#PS1='[\u@\h \W]\$ '

### COUNTDOWN

cdown() {
    N=$1
    while [[ $((--N)) -gt 0 ]]; do
        echo "$N" | figlet -c | lolcat && sleep 1
    done
}

### ARCHIVE EXTRACTION
# usage: ex <file>
ex() {
    if [ -f "$1" ]; then
        case $1 in
        *.tar.bz2) tar xjf $1 ;;
        *.tar.gz) tar xzf $1 ;;
        *.bz2) bunzip2 $1 ;;
        *.rar) unrar x $1 ;;
        *.gz) gunzip $1 ;;
        *.tar) tar xf $1 ;;
        *.tbz2) tar xjf $1 ;;
        *.tgz) tar xzf $1 ;;
        *.zip) unzip $1 ;;
        *.Z) uncompress $1 ;;
        *.7z) 7z x $1 ;;
        *.deb) ar x $1 ;;
        *.tar.xz) tar xf $1 ;;
        *.tar.zst) unzstd $1 ;;
        *) echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

### ALIASWS ###
alias ls='ls -al --color=always --group-directories-first'
alias c='clear'
alias e='exit'
alias C='git clone'
alias vim="nvim"
alias mmp='sudo mount /dev/sdc1 /mnt/MP/'
alias me='sudo mount /dev/sda6 /mnt/E/'
alias micon='pactl load-module module-loopback latency_msec=1'
alias micoff='pactl unload-module module-loopback'

# Colorize grep output (good for log files)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# pacman and paru
alias i='paru -S'                             # update standard pkgs and AUR pkgs (paru)
alias u='paru -Syu'                           # update standard pkgs and AUR pkgs (paru)
alias r='paru -Rn'                            # Remove the specified package
alias unlock='sudo rm /var/lib/pacman/db.lck' # remove pacman lock
alias I='paru -Qi'                            # list information about a package
alias S='paru -Si'
alias cleanup='paru -Rns' # remove orphaned packages (DANGEROUS!)

# get fastest mirrors
#alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
alias mirror="sudo reflector --latest 50 --number 20 --sort rate --protocol http,https --save /etc/pacman.d/mirrorlist"
alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

# git
# alias addup='git add -u'
alias addall='git add .'
alias branch='git branch'
alias checkout='git checkout'
alias clone='git clone'
alias commit='git commit -m'
alias fetch='git fetch'
alias pull='git pull origin'
alias push='git push origin'
alias stt='git status' # 'status' is protected name so using 'stat' instead
# alias tag='git tag'
# alias newtag='git tag -a'
