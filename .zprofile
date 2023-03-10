# Set PATH, MANPATH, etc., for Homebrew.
eval "$($(brew --prefix)/bin/brew shellenv)"

eval $(thefuck --alias)

#in ZSH, add to ~/.zprofile
# Where should I put you?
bindkey -s ^f "tmux-sessionizer\n"

export TERM="screen-256color"

export PATH=$HOME/.gem/ruby/2.6.0/bin:$PATH
