if status is-interactive
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"

  # Commands to run in interactive sessions can go here
  # Initialize oh-my-posh with custom theme
  oh-my-posh init fish --config ~/.config/fish/my_theme.yml | source

  # Wrapper function to update POSH_GITHUB_USER when gh auth switch is run
  function gh --wraps=gh
    command gh $argv
    if test "$argv[1]" = "auth" -a "$argv[2]" = "switch"
      set -gx POSH_GITHUB_USER (command gh api user --jq .login)
    end
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

source "$HOME/.config/fish/conf.d/fzf-git.fish"

# Complete --scope flag for yarn test:unit
complete -c yarn -n '__fish_seen_subcommand_from test:unit' -l scope -r -a '(__fish_yarn_test_unit_scopes)' -d 'Package scope'
