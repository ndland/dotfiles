zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
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

ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
[ ! -d $ZINIT_HOME ] && mkdir -p "$(dirname $ZINIT_HOME)"
[ ! -d $ZINIT_HOME/.git ] && git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
source "${ZINIT_HOME}/zinit.zsh"

autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

zinit light zsh-users/zsh-completions
zinit light dracula/zsh
zinit light Aloxaf/fzf-tab
zinit light lukechilds/zsh-better-npm-completion
zinit light chrissicool/zsh-256color
zinit light changyuheng/fz
zinit light rupa/z
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-autosuggestions

# bindkey '^[[A' history-substring-search-up
# bindkey '^[[B' history-substring-search-down

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

alias k=kubectl

# fnm
export PATH="$HOME/Library/Application Support/fnm:$PATH"
eval "`fnm env`"
eval "$(fnm env --use-on-cd)"

export PATH="$HOME/.rd/bin:$PATH"

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

# ZSH auto-completion for kubernetes.
if [ $commands[kubectl] ]; then
     source <(kubectl completion zsh)
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
