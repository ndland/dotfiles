# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Fast Zsh Configuration with Fish Features

# Add Homebrew's bin to PATH
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
  alias update="sudo apt update && sudo apt upgrade && brew update && brew upgrade && brew cleanup"
elif [[ "$OSTYPE" == "darwin"* ]]; then
  export PATH="/opt/homebrew/bin:$PATH"
  alias update="brew update && brew upgrade && brew cleanup"
fi

HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000
SAVEHIST=10000

# Enable Zsh options
setopt SHARE_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_FIND_NO_DUPS
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt AUTO_CD

# Plugin Management with zinit
if [[ ! -d "$HOME/.zinit/bin" ]]; then
  mkdir -p "$HOME/.zinit" && git clone https://github.com/zdharma-continuum/zinit.git "$HOME/.zinit/bin"
fi
source "$HOME/.zinit/bin/zinit.zsh"

# Plugins
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-syntax-highlighting
zinit light ajeetdsouza/zoxide
zinit light zsh-users/zsh-completions
zinit light junegunn/fzf-git.sh
zinit light spwhitt/nix-zsh-completions
zinit ice depth=1; zinit light romkatv/powerlevel10k

# History search key bindings
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# Enable autosuggestions
bindkey '^[[Z' autosuggest-accept

# Aliases
alias vim="nvim"
alias bn="buku --np"
alias bw="buku --np -w"
alias t="task"
alias tw="timew"

# Abbreviations (Fish-like functions)
function mkcd {
  mkdir -p "$1" && cd "$1"
}
function extract {
  case "$1" in
    *.tar.gz) tar -xvzf "$1" ;;
    *.zip) unzip "$1" ;;
    *) echo "Unknown file format: $1" ;;
  esac
}

# Completion Settings
rm -f ~/.zcompdump*  # Remove stale compinit cache

fpath=(~/.zsh/completions/ $fpath)
autoload -Uz compinit
compinit -C
zstyle ':completion:*' menu select
zstyle ':completion:*' descriptions true
zstyle ':completion:*' verbose true
zstyle ':completion:*' matcher-list "m:{a-zA-Z}={A-Za-z}" "r:|=*" "l:|=*"

# Performance Optimizations
zstyle ':completion:*' rehash true
zstyle ':completion:*:descriptions' format '%B%d%b'

# Asynchronous initialization
zinit wait lucid light-mode for \
  zsh-users/zsh-history-substring-search \
  mafredri/zsh-async

# Load asynchronously for faster startup
zinit ice wait"2" lucid
zinit light zsh-users/zsh-completions

# Custom Functions
function edit {
  nvim "$@"
}

# Install Homebrew if not present
if ! command -v brew &> /dev/null; then
  echo "Homebrew not found. Installing Homebrew..."
  if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
  elif [[ "$OSTYPE" == "darwin"* ]]; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/opt/homebrew/bin/brew shellenv)"
  fi
else
  eval "$(brew shellenv)"
fi

# Alias ls to eza with color and additional options
alias ls="eza --git --header --group --group-directories-first --icons --long"
alias la="ls -all"
function lt() {
  ls --all --tree --level=$1
}
# Enable tab completion for the lt function
compdef '_arguments "1:--level"' lt

# Add eza completions manually
if ! command -v eza &> /dev/null; then
  echo "eza not found. Please install it manually for better ls functionality."
fi

# Add zoxide support
if alias zi &> /dev/null; then
  unalias zi
fi

eval "$(zoxide init zsh)"

# Nix shell completion
if command -v nix &> /dev/null; then
  if [[ -f "/etc/profile.d/nix.sh" ]]; then
    . /etc/profile.d/nix.sh
  fi
  autoload -U compinit
  compinit
  zstyle ':completion:*' menu select
fi

export EDITOR="nvim"

alias ghs='gh auth switch && gh auth setup-git'
alias lg='lazygit'

eval "$(fnm env --use-on-cd --shell zsh)"

# fzf shell integration
source <(fzf --zsh)

if [[ "$(hostname)" == "VTMACMKXYVH42WL" ]]; then
  export ZK_NOTEBOOK_DIR="$HOME/code/personal/github.com/ndland/zk/"
else
  export ZK_NOTEBOOK_DIR="$HOME/code/github.com/ndland/zk/"  # Update this to your path
fi

PATH=~/.console-ninja/.bin:$PATH

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
