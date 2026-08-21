# ===============================================================
# Cross-platform development shell
# ===============================================================

fish_add_path -g "$HOME/.local/bin"

# ---------------------------------------------------------------
# Homebrew compatibility
# ---------------------------------------------------------------

set -l brew_path ""

if test -x /opt/homebrew/bin/brew
    set brew_path /opt/homebrew/bin/brew
else if test -x /usr/local/bin/brew
    set brew_path /usr/local/bin/brew
else if test -x /home/linuxbrew/.linuxbrew/bin/brew
    set brew_path /home/linuxbrew/.linuxbrew/bin/brew
end

if test -n "$brew_path"
    eval ($brew_path shellenv)
end

set -gx EDITOR nvim
set -gx VISUAL nvim

# Explicit default; replaces Fish's generated 4.3 migration file.
set -g fish_key_bindings fish_default_key_bindings

# ---------------------------------------------------------------
# Runtime manager
#
# mise is preferred.
# fnm remains the compatibility fallback for existing Macs.
# ---------------------------------------------------------------

if test -x "$HOME/.local/bin/mise"
    if test -d "$HOME/.local/share/mise/shims"
        fish_add_path -g "$HOME/.local/share/mise/shims"
    end

    # WSL uses mise shims only to avoid expensive shell-hook startup.
    # macOS/Linux retain full interactive activation.
    if status is-interactive; and not set -q WSL_DISTRO_NAME
        "$HOME/.local/bin/mise" activate fish | source
    end
else if command -q mise
    if test -d "$HOME/.local/share/mise/shims"
        fish_add_path -g "$HOME/.local/share/mise/shims"
    end

    if status is-interactive; and not set -q WSL_DISTRO_NAME
        mise activate fish | source
    end
else if command -q fnm
    fnm env --use-on-cd --shell fish | source
end

if status is-interactive
    # -----------------------------------------------------------
    # WSL uses Windows OpenSSH -> 1Password.
    # macOS continues to use native OpenSSH -> 1Password.
    # -----------------------------------------------------------

    if set -q WSL_DISTRO_NAME
        if command -q ssh.exe
            function ssh --wraps=ssh.exe \
                --description "Windows OpenSSH / 1Password agent"
                command ssh.exe $argv
            end
        else if test -x /mnt/c/Windows/System32/OpenSSH/ssh.exe
            function ssh \
                --description "Windows OpenSSH / 1Password agent"
                /mnt/c/Windows/System32/OpenSSH/ssh.exe $argv
            end
        end

        if command -q ssh-add.exe
            function ssh-add --wraps=ssh-add.exe \
                --description "Windows 1Password SSH identities"
                command ssh-add.exe $argv
            end
        else if test -x /mnt/c/Windows/System32/OpenSSH/ssh-add.exe
            function ssh-add \
                --description "Windows 1Password SSH identities"
                /mnt/c/Windows/System32/OpenSSH/ssh-add.exe $argv
            end
        end
    end

    # No gh/auth subprocess is run during shell startup.

    if command -q zoxide
        zoxide init fish | source
    end

    if command -q direnv
        direnv hook fish | source
    end

    if command -q eza
        abbr -a ll \
            'eza -lah --group-directories-first --git'

        abbr -a tree \
            'eza --tree --group-directories-first'
    end

    if command -q lazygit
        abbr -a lg lazygit
    end

    if command -q yazi
        function y
            set -l tmp (mktemp -t "yazi-cwd.XXXXXX")

            yazi $argv --cwd-file="$tmp"

            if read -z cwd < "$tmp"
                if test -n "$cwd"; and test "$cwd" != "$PWD"
                    builtin cd -- "$cwd"
                end
            end

            rm -f -- "$tmp"
        end
    end

    if command -q oh-my-posh
        if test -f "$HOME/.config/fish/my_theme.yml"
            oh-my-posh init fish \
                --config "$HOME/.config/fish/my_theme.yml" |
                source
        end
    end
end

# Existing monorepo test completion retained.
function __fish_yarn_test_unit_scopes
    set -l current_dir (pwd)
    set -l workspace_root ""

    while test "$current_dir" != "/"
        if test -f "$current_dir/package.json"
            set workspace_root "$current_dir"
            break
        end

        set current_dir (dirname "$current_dir")
    end

    if test -z "$workspace_root"
        return
    end

    if test -d "$workspace_root/packages"
        for dir in $workspace_root/packages/*/
            set -l package_json "$dir/package.json"

            if test -f "$package_json"; and command -q jq
                jq -r '.name' "$package_json" 2>/dev/null
            end
        end
    end
end

complete \
    -c yarn \
    -n '__fish_seen_subcommand_from test:unit' \
    -l scope \
    -r \
    -a '(__fish_yarn_test_unit_scopes)' \
    -d 'Package scope'
