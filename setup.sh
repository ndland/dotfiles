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

  echo "Symlinking tracked dotfile packages into ${TARGET}..."

  local pkg

  while IFS= read -r pkg; do
    [[ -n "${pkg}" ]] || continue

    # Only directories containing tracked files are eligible packages.
    # This prevents unrelated/untracked directories from being stowed.
    if [[ ! -d "${REPO_ROOT}/${pkg}" ]]; then
      continue
    fi

    echo "  stow ${pkg}"

    stow \
      --no-folding \
      --dir="${REPO_ROOT}" \
      --target="${TARGET}" \
      --restow \
      "${pkg}"
  done < <(
    git -C "${REPO_ROOT}" ls-files \
      | awk -F/ 'NF > 1 { print $1 }' \
      | sort -u
  )
}


install_mise_toolchain() {
  local mise_path=""

  if command -v mise >/dev/null 2>&1; then
    mise_path="$(command -v mise)"
  elif [[ -x "${HOME}/.local/bin/mise" ]]; then
    mise_path="${HOME}/.local/bin/mise"
  fi

  if [[ -z "${mise_path}" ]]; then
    echo "mise was expected from Homebrew but is unavailable." >&2
    exit 1
  fi

  echo "Installing portable mise toolchain..."
  "${mise_path}" install --yes
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
install_mise_toolchain

if [[ -x "${HOME}/.local/bin/configure-git-platform" ]]; then
  echo "Configuring platform-specific Git/1Password integration..."
  "${HOME}/.local/bin/configure-git-platform"
fi

set_login_shell

echo "Done."
