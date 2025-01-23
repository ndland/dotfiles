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

# Prompt Configuration
# Using powerlevel10k for a fast and feature-rich prompt
if [[ ! -d "$HOME/powerlevel10k" ]]; then
  git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ~/powerlevel10k
fi
source ~/powerlevel10k/powerlevel10k.zsh-theme

# Avoid zinit and zoxide aliases colliding
alias | grep -q 'zi=' && unalias zi

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

# History search key bindings
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# Enable autosuggestions
bindkey '^[[Z' autosuggest-accept

# Completion Settings
rm -f ~/.zcompdump*  # Remove stale compinit cache
autoload -Uz compinit
compinit -v  # Enable verbose output for debugging
zstyle ':completion:*' menu select
zstyle ':completion:*' descriptions true
zstyle ':completion:*' verbose true
zstyle ':completion:*' matcher-list "m:{a-zA-Z}={A-Za-z}" "r:|=*" "l:|=*"

fpath=(~/.zsh/completion $fpath)

# Aliases
alias gco="git checkout"
alias cls="clear"
alias vim="nvim"

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

# Install eza (ls replacement) if not present
if ! command -v eza &> /dev/null; then
  echo "eza not found. Please install it manually for better ls functionality."
fi

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
alias ls="eza --icons --group-directories-first"
alias ll="eza -lh --icons"
alias la="eza -la --icons"

# Add eza completions manually
if ! command -v eza &> /dev/null; then
  echo "eza not found. Please install it manually for better ls functionality."
else
  # Add completion definitions for eza
  function _eza_completion {
    local -a flags
    flags=(
      '--all[show hidden files]'
      '--long[list in long format]'
      '--icons[show icons]'
      '--group-directories-first[show directories first]'
    )
    _arguments -s $flags
  }
  compdef _eza_completion eza
fi

# Add zoxide support
if ! command -v zoxide &> /dev/null; then
  echo "zoxide not found. Please install it manually for faster navigation."
fi

# Nix shell completion
if command -v nix &> /dev/null; then
  if [[ -f "/etc/profile.d/nix.sh" ]]; then
    . /etc/profile.d/nix.sh
  fi

  autoload -U compinit
  compinit
  zstyle ':completion:*' menu select

  # Nix completions
  zinit light spwhitt/nix-zsh-completions
fi

# Source Powerlevel10k Config
if [[ -f ~/.p10k.zsh ]]; then
  source ~/.p10k.zsh
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

eval "$(fnm env --use-on-cd --shell zsh)"

eval "$(zoxide init zsh)"
