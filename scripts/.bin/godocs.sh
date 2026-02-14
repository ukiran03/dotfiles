#!/usr/bin/env bash

# Kill existing processes on these ports to prevent "address already in use"
fuser -k 6060/tcp 6061/tcp > /dev/null 2>&1

echo "Starting Go documentation servers..."

# 1. Modern pkgsite (The Go team's replacement for godoc)
# It automatically finds modules in your current directory and GOPATH.
pkgsite -http=:6061 -gorepo=$(go env GOPATH) -cache > /dev/null 2>&1 &
echo "  - pkgsite: http://localhost:6061"

# 2. Classic godoc (If you still need it for standard library exploration)
godoc -http=localhost:6060 > /dev/null 2>&1 &
echo "  - godoc:   http://localhost:6060"

echo "Servers are running in the background. Use 'pkill pkgsite' to stop them."

### Not in woods yet
# dufs --allow-search ~/prog/doc/go.dev/ &
