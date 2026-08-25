# Dotfiles

Fish, WezTerm, nvim, and git configs, plus a Homebrew Bundle file.
GNU Stow links the packages into `$HOME`.

## New Mac

1. Install Xcode Command Line Tools (needed for `git` and Homebrew):

   ```sh
   xcode-select --install
   ```

2. Clone this repo (any path is fine):

   ```sh
   git clone git@github.com:ndland/dotfiles.git ~/code/personal/github.com/ndland/dotfiles
   cd ~/code/personal/github.com/ndland/dotfiles
   ```

3. Bootstrap Homebrew, packages, symlinks, and fish as the login shell:

   ```sh
   ./setup.sh
   ```

4. Sign in to the things a script cannot do:

   - 1Password (SSH git signing)
   - `gh auth login` for the personal and work GitHub accounts
   - Create `~/.config/dev/agent` with one line: `cursor-agent` on the work
     Mac, `opencode` on the personal machine. `dev` and the `agent` fish
     function both read this file. There is no repo default.
   - On the work Mac, log the Cursor CLI into your work account
     (`cursor-agent login` or `agent login`)

On a machine that already has this repo, the same script is the update path:

```sh
git pull && ./setup.sh
```

`brew bundle` only installs what is missing. It does not uninstall extra packages.
