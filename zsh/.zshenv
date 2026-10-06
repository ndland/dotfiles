# mise shims before Homebrew for non-login zsh (e.g. Cursor agent `zsh -c`).
export PATH="$HOME/.local/share/mise/shims:$PATH"

# Cursor agent shells export TMPDIR='' after startup files run, so an export
# here would be overwritten. Git treats an empty TMPDIR as the filesystem root,
# and SSH commit signing then fails with "Read-only file system".
git() { TMPDIR=${TMPDIR:-$(getconf DARWIN_USER_TEMP_DIR)} command git "$@"; }
