# Login zsh PATH mirrors fish/.config/fish/config.fish.
export PATH="$HOME/.local/bin:$PATH"

if [[ -x /opt/homebrew/bin/brew ]]; then
  eval "$(SHELL=/bin/zsh /opt/homebrew/bin/brew shellenv)"
elif [[ -x /usr/local/bin/brew ]]; then
  eval "$(SHELL=/bin/zsh /usr/local/bin/brew shellenv)"
fi

export PATH="$HOME/.local/share/mise/shims:$PATH"

if [[ -x "$HOME/.local/bin/mise" ]]; then
  eval "$("$HOME/.local/bin/mise" activate zsh)"
fi
