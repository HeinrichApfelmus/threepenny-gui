#!/bin/sh
mhs -z -b64 -i../src Example.hs -oExample.comb
cat >Example.js <<EOF
var combexpr = \`$(cat Example.comb)\`;
EOF
