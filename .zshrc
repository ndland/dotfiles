zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
autoload -Uz compinit
compinit

source ~/.zplug/init.zsh

zplug "zsh-users/zsh-completions",                defer:0
zplug "zsh-users/zsh-autosuggestions",            defer:1
zplug "zsh-users/zsh-syntax-highlighting",        defer:2
zplug "zsh-users/zsh-history-substring-search",   defer:3

zplug "lukechilds/zsh-better-npm-completion",     defer:4
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
