#!/usr/bin/env bash
set -e

tapestry="$(cd "$(dirname "$0")" && pwd)"
src="$(dirname "$tapestry")"
mkdir -p $tapestry
mkdir -p $tapestry/checkout
mkdir -p $tapestry/tokens
mkdir -p $tapestry/lib
mkdir -p $tapestry/include
mkdir -p $tapestry/bin
export TAPESTRY="$tapestry"
BASH_FILE=""

# select profile file
if   [ -f "$HOME/.bash_profile" ]; then
    BASH_FILE="$HOME/.bash_profile"
elif [ -f "$HOME/.bashrc"       ]; then
    BASH_FILE="$HOME/.bashrc"
else
    BASH_FILE="$HOME/.bash_profile"
    touch "$BASH_FILE"
fi

# find instances of $TAPESTRY [used in PATH] and TAPESTRY= [env]
r_import='^[[:space:]]*export[[:space:]]+TAPESTRY='
r_dbg='^[[:space:]]*export[[:space:]]+DBG='
r_path='\$TAPESTRY'
f="$(mktemp)"
path_set=0
dbg_set=0
importer_exporter=0

while IFS= read -r line; do
    if   [[ "$line" =~ $r_path   ]]; then
        path_set=1
    elif [[ "$line" =~ $r_import ]]; then
        importer_exporter=1
        # lets read what its set to, then we can compare
    elif [[ "$line" =~ $r_dbg ]]; then
        dbg_set=1
        line="export DBG=\"$DBG\""
        echo "tapestry: updated DBG in $BASH_FILE"
    fi
    echo "$line" >> "$f"
done < "$BASH_FILE"

if [ "$importer_exporter" -eq 0 ]; then
    echo >> "$f"
    echo "export TAPESTRY=\"$TAPESTRY\"" >> "$f"
    echo "tapestry: added TAPESTRY in $BASH_FILE"
fi

# 3. append IMPORT if it wasn't found
if [ "$path_set" -eq 0 ]; then
    echo >> "$f"
    echo "export PATH=\"\$TAPESTRY/bin:\$PATH\"" >> "$f"
    echo "tapestry: updated PATH in $BASH_FILE"
fi

# insert DBG if not set
if [ "$dbg_set" -eq 0 ]; then
    echo >> "$f"
    echo "export DBG=\"$DBG\"" >> "$f"
    echo "tapestry: added DBG=\"$DBG\" in $BASH_FILE"
fi

echo "tapestry: environment set"

# 4. move updated file back
mv "$f" "$BASH_FILE"

make
