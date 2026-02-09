function t --description "Run unit tests for a package"
    if test (count $argv) -lt 1
        echo "Usage: t <package> [test-file]"
        return 1
    end

    set -l package_name $argv[1]

    # ── Find project root ──────────────────────────────────────────────
    set -l root ""
    set -l dir (pwd)
    while test "$dir" != "/"
        if test -f "$dir/package.json"
            set root "$dir"
            break
        end
        set dir (dirname "$dir")
    end
    if test -z "$root"
        echo "t: no package.json found"
        return 1
    end

    # ── Detect workspace ───────────────────────────────────────────────
    set -l is_workspace false
    if command -v jq >/dev/null 2>&1
        if jq -e '.workspaces' "$root/package.json" >/dev/null 2>&1
            set is_workspace true
        end
    else if grep -q '"workspaces"' "$root/package.json" 2>/dev/null
        set is_workspace true
    end

    # ── Locate the target package.json ─────────────────────────────────
    set -l pkg_json "$root/package.json"
    if test "$is_workspace" = true
        if command -v jq >/dev/null 2>&1
            set -l globs (jq -r '
                .workspaces // empty |
                if type == "array" then .[]
                elif type == "object" then (.packages // [])[]
                else empty end
            ' "$root/package.json" 2>/dev/null)

            for g in $globs
                set -l base (string replace -r '/\*\*?$' '' "$g")
                if test -d "$root/$base"
                    for d in $root/$base/*/
                        if test -f "$d/package.json"
                            set -l name (jq -r '.name // empty' "$d/package.json" 2>/dev/null)
                            if test "$name" = "$package_name"
                                set pkg_json "$d/package.json"
                                break
                            end
                        end
                    end
                end
            end
        end
    end

    # ── Detect test script ─────────────────────────────────────────────
    # Prefer test:unit, fall back to test
    set -l test_script ""
    if command -v jq >/dev/null 2>&1
        if jq -e '.scripts["test:unit"]' "$pkg_json" >/dev/null 2>&1
            set test_script "test:unit"
        else if jq -e '.scripts.test' "$pkg_json" >/dev/null 2>&1
            set test_script test
        end
    else
        if grep -q '"test:unit"' "$pkg_json" 2>/dev/null
            set test_script "test:unit"
        else if grep -q '"test"' "$pkg_json" 2>/dev/null
            set test_script test
        end
    end

    if test -z "$test_script"
        echo "t: no test script (test:unit or test) found in $pkg_json"
        return 1
    end

    # ── Detect test runner (vitest vs jest) ────────────────────────────
    set -l runner jest
    if command -v jq >/dev/null 2>&1
        set -l script_cmd (jq -r ".scripts[\"$test_script\"]" "$pkg_json" 2>/dev/null)
        if string match -q '*vitest*' "$script_cmd"
            set runner vitest
        end
    else if grep -q 'vitest' "$pkg_json" 2>/dev/null
        set runner vitest
    end

    # ── Build file filter arg ──────────────────────────────────────────
    set -l file_args
    if test (count $argv) -ge 2
        if test "$runner" = vitest
            # Vitest accepts file filters as positional args
            set file_args $argv[2]
        else
            # Jest needs --testPathPattern
            set file_args --testPathPattern="$argv[2]"
        end
    end

    # ── Execute ────────────────────────────────────────────────────────
    if test "$is_workspace" = true
        yarn workspace $package_name $test_script $file_args
    else
        if test (count $file_args) -gt 0
            if test "$runner" = vitest
                yarn $test_script $file_args
            else
                yarn $test_script -- $file_args
            end
        else
            yarn $test_script
        end
    end
end
