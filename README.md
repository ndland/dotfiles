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

## Git identities

Keep personal repositories under `~/code/personal/` and work repositories under
`~/code/work/`. Git selects the email, signing key, and SSH key from
`~/.gitconfig-personal` or `~/.gitconfig-work`. The existing
`~/code/learning/` and `~/code/github.com/ndland/` layouts also select personal.
There is no default email outside these trees (`user.useConfigOnly = true`).

The personal profile is managed in this repository. Keep the work profile outside
it at `~/.config/git/identities/work.gitconfig`, and link it as `~/.gitconfig-work`.
Each profile contains its email, SSH signing public key, and native SSH command.
Run `configure-git-platform` after changing a signing key; it exports public-key
files to `~/.config/git/ssh/` while private keys stay in 1Password.

Use canonical SSH remotes rather than account-specific host aliases:

```sh
git remote set-url origin git@github.com:OWNER/REPO.git
```

The initial clone needs an explicit identity before directory routing applies:

```sh
git -c core.sshCommand='ssh -o IdentitiesOnly=yes -i ~/.config/git/ssh/work.pub' \
    clone git@github.com:ORG/REPO.git ~/code/work/REPO
```

Use `personal.pub` and a personal destination for a personal clone. WSL continues
to use Windows OpenSSH and the profile selected by the wrapper.

For a repository-specific email exception:

```sh
git config --local user.email "required@example.com"
```

That changes commit attribution only, not SSH authentication or the signing key.
`gh auth switch` selects the GitHub CLI/API account; it does not change Git's
author identity or SSH account.

## Local development-session settings

`~/.config/dev/agent` and `~/.config/dev/aliases` are machine-local and are
intentionally ignored by this repository. Set `agent` to the local CLI command
(for example, `cursor-agent` or `opencode`). Optional aliases use one `NAME=PATH`
entry per line; they can point `dev` at projects outside `~/code`.
