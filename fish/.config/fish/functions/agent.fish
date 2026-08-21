function agent \
    --description 'Launch preferred terminal coding agent'

    if command -q opencode
        opencode $argv
    else if command -q cursor-agent
        cursor-agent $argv
    else
        echo "No terminal coding agent is installed." >&2
        return 127
    end
end
