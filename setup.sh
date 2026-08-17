#!/usr/bin/env bash
# Bootstrap this machine from the repo: Homebrew, bundle, stow configs, fish login shell.
#
# New Mac: xcode-select --install, clone this repo, ./setup.sh
# Existing Mac: git pull && ./setup.sh

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BREWFILE="${REPO_ROOT}/.Brewfile"
TARGET="${HOME}"
HOMEBREW_INSTALL_URL='https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh'

brew_executable() {
  if [[ -x /opt/homebrew/bin/brew ]]; then
    echo /opt/homebrew/bin/brew
  elif [[ -x /usr/local/bin/brew ]]; then
    echo /usr/local/bin/brew
  elif command -v brew >/dev/null 2>&1; then
    command -v brew
  fi
}

require_homebrew() {
  local brew_path
  brew_path="$(brew_executable || true)"

  if [[ -z "${brew_path}" ]]; then
    echo "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL "${HOMEBREW_INSTALL_URL}")"
    brew_path="$(brew_executable || true)"
  fi

  if [[ -z "${brew_path}" ]]; then
    echo "Homebrew is not installed. Install it from https://brew.sh then re-run $0." >&2
    exit 1
  fi

  eval "$("${brew_path}" shellenv)"
}

install_formulae() {
  if [[ ! -f "${BREWFILE}" ]]; then
    echo "Missing ${BREWFILE}." >&2
    exit 1
  fi

  echo "Installing Homebrew formulae from ${BREWFILE}..."
  brew bundle --file="${BREWFILE}"
}

stow_packages() {
  if ! command -v stow >/dev/null 2>&1; then
    echo "GNU Stow is not installed. Add it to .Brewfile and re-run $0." >&2
    exit 1
  fi

  echo "Symlinking packages into ${TARGET}..."

  local pkg_dir pkg
  for pkg_dir in "${REPO_ROOT}"/*/; do
    pkg="$(basename "${pkg_dir}")"
    echo "  stow ${pkg}"
    stow --dir="${REPO_ROOT}" --target="${TARGET}" --restow "${pkg}"
  done
}

install_fish_plugins() {
  if ! command -v fish >/dev/null 2>&1; then
    return
  fi

  echo "Installing fish plugins..."
  fish -c "fisher update" || echo "fisher update skipped."
}

set_login_shell() {
  local fish_path
  fish_path="$(command -v fish || true)"

  if [[ -z "${fish_path}" ]]; then
    echo "fish is not installed. Add it to .Brewfile and re-run $0." >&2
    exit 1
  fi

  local current=""
  current="$(dscl . -read "${HOME}" UserShell 2>/dev/null | awk '{print $2}' || true)"

  if [[ "${current}" == "${fish_path}" || "${SHELL}" == "${fish_path}" ]]; then
    echo "Login shell is already ${fish_path}."
    return
  fi

  echo "Setting login shell to ${fish_path}..."
  chsh -s "${fish_path}"
}

require_homebrew
install_formulae
stow_packages
install_fish_plugins
set_login_shell

echo "Done."
