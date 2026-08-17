if status is-interactive
  # Initialize Homebrew (check common paths for macOS and Linux)
  set -l brew_path ""
  if test -f /opt/homebrew/bin/brew
    set brew_path /opt/homebrew/bin/brew
  else if test -f /usr/local/bin/brew
    set brew_path /usr/local/bin/brew
  else if test -f /home/linuxbrew/.linuxbrew/bin/brew
    set brew_path /home/linuxbrew/.linuxbrew/bin/brew
  end
  if test -n "$brew_path"
    eval ($brew_path shellenv)
  end

  function __refresh_posh_github_user
    if not command -q gh
      set -e POSH_GITHUB_USER
      return
    end

    set -l login (command gh auth status --active --hostname github.com --json hosts --jq '.hosts["github.com"][] | select(.active) | .login' 2>/dev/null)

    if test -n "$login"
      set -gx POSH_GITHUB_USER "$login"
    else
      set -e POSH_GITHUB_USER
    end
  end

  __refresh_posh_github_user

  # Wrapper function to keep POSH_GITHUB_USER in sync with the active gh account.
  function gh --wraps=gh
    command gh $argv
    set -l gh_status $status

    if test "$argv[1]" = "auth"; and contains -- "$argv[2]" switch login logout
      __refresh_posh_github_user
    end

    return $gh_status
  end

  function y
    set tmp (mktemp -t "yazi-cwd.XXXXXX")
    yazi $argv --cwd-file="$tmp"
    if read -z cwd < "$tmp"; and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
      builtin cd -- "$cwd"
    end
    rm -f -- "$tmp"
  end
end

# Completion for yarn test:unit --scope
function __fish_yarn_test_unit_scopes
    # Try to find the workspace root
    set -l current_dir (pwd)
    set -l workspace_root ""
    
    # Walk up the directory tree to find package.json with workspaces
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
    
    # Extract package names from each package.json
    if test -d "$workspace_root/packages"
        for dir in $workspace_root/packages/*/
            set -l package_json "$dir/package.json"
            if test -f "$package_json"
                # Try using jq first (most reliable)
                if command -v jq >/dev/null 2>&1
                    jq -r '.name' "$package_json" 2>/dev/null
                # Fallback: use grep/sed (less reliable but works without jq)
                else
                    grep -o '"name"\s*:\s*"[^"]*"' "$package_json" 2>/dev/null | sed 's/.*"name"\s*:\s*"\([^"]*\)".*/\1/' | head -1
                end
            end
        end
    end
end

# WezTerm-specific aliases
if set -q WEZTERM_EXECUTABLE
  # WezTerm-aware cd (updates pane title)
  function wez-cd
    cd $argv
    wezterm cli set-notes -- pane-id (wezterm cli list --format json | jq -r '.[] | select(.foreground_process_name=="fish") | .pane_id')
  end
end

# Neovim as default editor
set -gx EDITOR nvim
set -gx VISUAL nvim

fnm env --use-on-cd | source

# Complete --scope flag for yarn test:unit
complete -c yarn -n '__fish_seen_subcommand_from test:unit' -l scope -r -a '(__fish_yarn_test_unit_scopes)' -d 'Package scope'

export PATH="$HOME/.local/bin:$PATH"

oh-my-posh init fish --config ~/.config/fish/my_theme.yml | source

if functions -q _omp_get_prompt
  function _omp_space_key_handler
    commandline --function expand-abbr
    commandline --insert ' '

    set -l tooltip_command (commandline --current-buffer | string trim -l | string split --allow-empty -f1 ' ' | string collect)
    set -l tooltip_prompt (_omp_get_prompt tooltip --command=$tooltip_command | string join '')

    if test -z "$tooltip_prompt"
      set --global _omp_tooltip_command ''
      set --global _omp_current_rprompt (_omp_get_prompt right | string join '')
      set --global _omp_new_prompt 0
      commandline --function repaint
      return
    end

    set --global _omp_tooltip_command "$tooltip_command"
    set --global _omp_current_rprompt "$tooltip_prompt"
    set --global _omp_new_prompt 0
    commandline --function repaint
  end

  function _omp_backspace_key_handler
    commandline --function backward-delete-char

    set -l current_command (commandline --current-buffer | string trim -l | string split --allow-empty -f1 ' ' | string collect)
    set --global _omp_tooltip_command "$current_command"
    set --global _omp_current_rprompt (_omp_get_prompt tooltip --command=$current_command | string join '')
    set --global _omp_new_prompt 0
    commandline --function repaint
  end

  bind \x20 _omp_space_key_handler -M default
  bind \x20 _omp_space_key_handler -M insert
  bind \x7f _omp_backspace_key_handler -M default
  bind \x7f _omp_backspace_key_handler -M insert
end
direnv hook fish | source
