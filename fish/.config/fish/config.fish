if status is-interactive
    # Set POSH_GITHUB_USER environment variable
    set -gx POSH_GITHUB_USER (gh api user --jq .login 2>/dev/null; or echo "unknown")

    # Commands to run in interactive sessions can go here
    # Initialize oh-my-posh with custom theme
    oh-my-posh init fish --config ~/.config/fish/my_theme.yml | source
end

# Cache the last update time to avoid excessive API calls
set -g __github_user_last_update 0
set -g __github_user_cache_ttl 300  # 5 minutes

# Get GitHub user with error handling
function __get_github_user
    gh api user --jq .login 2>/dev/null; or echo "unknown"
end

# Update GitHub user, but only if cache is stale
function __update_github_user_cached
    set current_time (date +%s)
    set time_since_update (math "$current_time - $__github_user_last_update")
    
    if test $time_since_update -gt $__github_user_cache_ttl
        set new_user (__get_github_user)
        # Only update cache timestamp if we got a valid response
        if test "$new_user" != "unknown"
            set -gx POSH_GITHUB_USER $new_user
            set -g __github_user_last_update $current_time
        end
    end
end

# Hook to update POSH_GITHUB_USER before each prompt render (with caching)
function __update_github_user --on-event fish_prompt
    __update_github_user_cached
end

# Wrapper function to update POSH_GITHUB_USER when gh auth commands are run
function gh --wraps=gh
    command gh $argv
    # Update POSH_GITHUB_USER after any auth command (switch, login, logout, refresh, etc.)
    if test "$argv[1]" = "auth"
        set new_user (command gh api user --jq .login 2>/dev/null; or echo "unknown")
        if test "$new_user" != "unknown"
            set -gx POSH_GITHUB_USER $new_user
            set -g __github_user_last_update (date +%s)
        end
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

# Complete --scope flag for yarn test:unit
complete -c yarn -n '__fish_seen_subcommand_from test:unit' -l scope -r -a '(__fish_yarn_test_unit_scopes)' -d 'Package scope'
