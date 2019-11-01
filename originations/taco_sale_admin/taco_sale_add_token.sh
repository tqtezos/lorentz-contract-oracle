#!/usr/bin/env bash

set -euo pipefail

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

($DIR/taco_sale.rb add_sale $1 | sed 's/"\(\$.*\)"/\1/' | sed 's/TokenToken/Token/g') > "$1.yaml"
$DIR/../../lorentz-contract-originate.rb "$1.yaml"
echo "new sale contract:"
echo ""
$DIR/taco_sale.rb parse_sale "$1.originated.yaml"

