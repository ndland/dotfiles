#!/usr/bin/env bash

# This script installs Homebrew formulae and symlinks the nvim directory using GNU Stow.

# Exit immediately if a command exits with a non-zero status.
set -e

# --- Configuration ---
# Set the desired formulae to install
formulae=(
  'neovim'
  'ripgrep'
  'fzf'
)

# Set the source and destination directories for stow
stow_dir="$HOME/dotfiles"
nvim_dir="nvim"

# Function to install Homebrew formulae
install_formulae() {
  echo "Installing Homebrew formulae..."
  for formula in "${formulae[@]}"; do
    if brew list --formula | grep -q "$formula"; then
      echo "  $formula is already installed. Skipping..."
    else
      echo "  Installing $formula..."
      brew install "$formula"
    fi
  done
}

# Function to symlink nvim config with stow
symlink_nvim() {
  if ! command -v stow &> /dev/null; then
    echo "GNU Stow not found. Please install it with 'brew install stow'."
    exit 1
  fi

  if [[ ! -d "$stow_dir/$nvim_dir" ]]; then
    echo "The '$stow_dir/$nvim_dir' directory does not exist. Please ensure your dotfiles are in place."
    exit 1
  fi

  echo "Symlinking $nvim_dir using stow..."
  cd "$stow_dir"
  stow "$nvim_dir" -t $HOME
  echo "Symlinking complete."
}

# --- Execution ---
install_formulae
symlink_nvim

echo "Script finished successfully! 🎉"
