zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
fi

autoload -Uz compinit
compinit

export ZPLUG_HOME=/opt/homebrew/opt/zplug
source $ZPLUG_HOME/init.zsh

zplug "zsh-users/zsh-completions",                defer:0
zplug "zsh-users/zsh-autosuggestions",            defer:1
zplug "zsh-users/zsh-syntax-highlighting",        defer:2
zplug "zsh-users/zsh-history-substring-search",   defer:3

zplug "lukechilds/zsh-better-npm-completion",     defer:0
zplug "chrissicool/zsh-256color"
zplug "changyuheng/fz"
zplug "rupa/z", use:z.sh
zplug "zplug/zplug", hook-build:'zplug --self-manage'


if ! zplug check --verbose; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi

zplug load

bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

eval $(thefuck --alias)

# Aliases
alias zl="source ~/.zshrc"

alias ls="exa"
alias la="exa -a"
alias ll="exa -la"

alias vim="nvim"

alias buku="buku --suggest"
alias upgrade="brew update && brew upgrade && brew cleanup && mas upgrade"

# Dotfiles managment
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# fnm
export PATH=$HOME/.fnm:$PATH
eval "`fnm env`"
eval "$(fnm env --use-on-cd)"

eval "$(starship init zsh)"

# homebrew
export PATH="/usr/local/sbin:$PATH"

export PATH="$HOME/.local/bin:$PATH"

export BAT_THEME="Dracula"

# Syntax highlight man pages with bat
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

# Syntax highlight help messages with bat
alias bathelp='bat --plain --language=help'
help() {
    "$@" --help 2>&1 | bathelp
}
