if status is-interactive
    # Set POSH_GITHUB_USER environment variable
    set -gx POSH_GITHUB_USER (gh api user --jq .login)

    # Commands to run in interactive sessions can go here
    # Initialize oh-my-posh with custom theme
    oh-my-posh init fish --config ~/.config/fish/my_theme.yml | source
end

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
