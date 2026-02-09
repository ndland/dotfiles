# Completions for t (test runner)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Walk up from cwd to find the nearest package.json (any node project).
# Echoes the directory path.
function __t_project_root
    set -l dir (pwd)
    while test "$dir" != "/"
        if test -f "$dir/package.json"
            echo "$dir"
            return 0
        end
        set dir (dirname "$dir")
    end
    return 1
end

# Check whether the project root has a workspaces field.
function __t_is_workspace
    set -l root $argv[1]
    if command -v jq >/dev/null 2>&1
        jq -e '.workspaces' "$root/package.json" >/dev/null 2>&1
        return $status
    else
        grep -q '"workspaces"' "$root/package.json" 2>/dev/null
        return $status
    end
end

# Read the "name" field from a package.json.
function __t_pkg_name
    set -l pjson $argv[1]
    if command -v jq >/dev/null 2>&1
        jq -r '.name // empty' "$pjson" 2>/dev/null
    else
        grep -o '"name"[[:space:]]*:[[:space:]]*"[^"]*"' "$pjson" 2>/dev/null \
            | string replace -r '.*"name"[^"]*"([^"]*)".*' '$1' \
            | head -1
    end
end

# Given a workspace root, resolve the workspace globs and emit each package
# directory that contains a package.json.
#
# Handles both formats:
#   "workspaces": ["packages/*", "apps/*"]
#   "workspaces": { "packages": ["packages/*"] }
function __t_workspace_package_dirs
    set -l root $argv[1]
    if not command -v jq >/dev/null 2>&1; return; end

    set -l globs (jq -r '
        .workspaces // empty |
        if type == "array" then .[]
        elif type == "object" then (.packages // [])[]
        else empty end
    ' "$root/package.json" 2>/dev/null)

    for g in $globs
        # Strip trailing /* or /** to get the base directory
        set -l base (string replace -r '/\*\*?$' '' "$g")
        if test -d "$root/$base"
            for dir in $root/$base/*/
                if test -f "$dir/package.json"
                    echo $dir
                end
            end
        end
    end
end

# ---------------------------------------------------------------------------
# Completion providers
# ---------------------------------------------------------------------------

# Emit package names for the first argument.
# - Workspace repo:     list every workspace package name
# - Single-package repo: list the root package name
function __t_list_packages
    set -l root (__t_project_root)
    if test -z "$root"; return; end

    if __t_is_workspace "$root"
        for pkg_dir in (__t_workspace_package_dirs "$root")
            __t_pkg_name "$pkg_dir/package.json"
        end
    else
        __t_pkg_name "$root/package.json"
    end
end

# Emit test file basenames for the second argument, scoped to the package
# chosen in the first argument.
function __t_list_test_files
    set -l root (__t_project_root)
    if test -z "$root"; return; end

    set -l tokens (commandline -opc)
    if test (count $tokens) -lt 2; return; end
    set -l package_name $tokens[2]

    # Determine the directory to search for test files
    set -l search_dir ""

    if __t_is_workspace "$root"
        for pkg_dir in (__t_workspace_package_dirs "$root")
            set -l name (__t_pkg_name "$pkg_dir/package.json")
            if test "$name" = "$package_name"
                set search_dir "$pkg_dir"
                break
            end
        end
    else
        # Single-package repo — search from the project root
        set search_dir "$root"
    end

    if test -z "$search_dir"; return; end

    find "$search_dir" -type f \
        \( -name "*.test.jsx" -o -name "*.test.js" -o -name "*.test.ts" -o -name "*.test.tsx" \) \
        -not -path "*/node_modules/*" \
        -not -path "*/.next/*" \
        -not -path "*/dist/*" \
        -not -path "*/build/*" \
        2>/dev/null | while read -l file
        basename "$file"
    end
end

# ---------------------------------------------------------------------------
# Register completions
# ---------------------------------------------------------------------------

# Disable default file completions
complete -c t -f

# 1st arg — package name
complete -c t -n "test (count (commandline -opc)) -eq 1" -a '(__t_list_packages)' -d 'Package'

# 2nd arg — test file
complete -c t -n "test (count (commandline -opc)) -eq 2" -a '(__t_list_test_files)' -d 'Test file'
