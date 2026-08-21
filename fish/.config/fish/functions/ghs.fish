function ghs \
    --description 'Switch active GitHub CLI account'

    command gh auth switch $argv
    set -l switch_status $status

    if test $switch_status -eq 0
        # Only refresh prompt identity when the user explicitly switches.
        set -l login (
            command gh auth status \
                --active \
                --hostname github.com \
                --json hosts \
                --jq '.hosts["github.com"][] | select(.active) | .login' \
                2>/dev/null
        )

        if test -n "$login"
            set -gx POSH_GITHUB_USER "$login"
        else
            set -e POSH_GITHUB_USER
        end
    end

    return $switch_status
end
