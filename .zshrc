# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false
# set descriptions format to enable group support
zstyle ':completion:*:descriptions' format '[%d]'
# set list-colors to enable filename colorizing
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# preview directory's content with exa when completing cd
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
# switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh-completions:$FPATH
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
  FPATH=/opt/homebrew/share/zsh/site-functions:$FPATH
fi

autoload -Uz compinit
compinit

export HISTFILE=~/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
setopt appendhistory
setopt histignorealldups

export ZPLUG_HOME=$(brew --prefix)/opt/zplug
source $ZPLUG_HOME/init.zsh

zplug "zsh-users/zsh-completions",                defer:0
zplug "zsh-users/zsh-autosuggestions",            defer:1
zplug "zsh-users/zsh-syntax-highlighting",        defer:2

zplug "dracula/zsh"
zplug "Aloxaf/fzf-tab"

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

# bindkey '^[[A' history-substring-search-up
# bindkey '^[[B' history-substring-search-down

eval $(thefuck --alias)

# Aliases
alias zl="source ~/.zshrc"

alias ls="exa"
alias la="exa -a"
alias ll="exa -la"

alias vim="nvim"

alias b="buku --suggest"
alias upgrade="brew update && brew upgrade && brew cleanup && mas upgrade"

# Dotfiles managment
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# fnm
export PATH="$HOME/Library/Application Support/fnm:$PATH"
eval "`fnm env`"
eval "$(fnm env --use-on-cd)"

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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(starship init zsh)"
