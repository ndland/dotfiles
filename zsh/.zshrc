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
  alias update="sudo apt update && sudo apt upgrade"
elif [[ "$OSTYPE" == "darwin"* ]]; then
  export PATH="/opt/homebrew/bin:$PATH"
  alias update="brew update && brew upgrade && brew cleanup"
fi

# Enable Zsh options
setopt SHARE_HISTORY
setopt INC_APPEND_HISTORY
setopt HIST_FIND_NO_DUPS
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
compinit
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
unalias zi
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

export EDITOR=nvim

if [[ "$HOST" == "GMMACANCNXYVH42"* ]]; then
  export ZK_NOTEBOOK_DIR="$HOME/code/personal/github.com/ndland/zk/"
  taskCommit='feat: update tasks from work machine'
  zkCommit='doc: update notes from work machine'
else
  export ZK_NOTEBOOK_DIR="$HOME/code/github.com/ndland/zk/"
  taskCommit='feat: update tasks from personal machine'
  zkCommit='doc: update notes from personal machine'
fi

# Helper to automatically update git repositories and push to them
function update_repo {
  local dir=$1
  local commit_msg=$2
  cd $dir
  echo "Pulling changes from remote..."
  git pull
  echo "Done pulling changes."
  if [[ -z $(git status --porcelain) ]]; then
    echo "No changes to commit."
    cd -
    return
  fi
  if [[ -n $(git status --porcelain) ]]; then
    git add .
    git commit -m "$commit_msg"
  fi
  git push
  cd -
}

alias ut='update_repo ~/.task/ "$taskCommit"'
alias uzk='update_repo $ZK_NOTEBOOK_DIR "$zkCommit"'
alias ghs='gh auth switch && gh auth setup-git'
alias lg='lazygit'

eval "$(fnm env --use-on-cd --shell zsh)"

# fzf shell integration
source <(fzf --zsh)

# Install and configure Starship prompt
if ! command -v starship &> /dev/null; then
  echo "Starship not found. Installing..."
  brew install starship
fi
export RPROMPT='$(starship prompt --right)'
export STARSHIP_CONFIG="$HOME/.config/starship.toml"
if [[ "$(hostname)" == "VTMACMKXYVH42WL" ]]; then
  export ZK_NOTEBOOK_DIR="$HOME/code/personal/github.com/ndland/zk-notes/"
else
  export ZK_NOTEBOOK_DIR="$HOME/code/github.com/ndland/zk-notes/"  # Update this to your path
fi

function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}

# eval "$(starship init zsh)"

PATH=~/.console-ninja/.bin:$PATH

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
