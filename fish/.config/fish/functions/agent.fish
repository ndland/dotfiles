function agent --description 'Launch machine-local terminal coding agent'
    set -l resolved (resolve-dev-agent)
    or return $status

    $resolved $argv
end
