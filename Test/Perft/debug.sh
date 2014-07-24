#!/usr/bin/env bash
###############################################################
# A simple debug script for perft using crafty as a reference #
###############################################################

# $1 : the FEN to debug
# $2 : the depth to debug at

PERFT=$(dirname $0)/../../dist/build/perft/perft
CRAFTY=crafty

if [[ "$2" -lt  1 ]]; then
    exit
fi

getCraftyResult()
{
   ($CRAFTY |& grep "total moves" | sed "s/.*total moves=\([0-9]*\).*/\1/") <<EOF
ponder off
setboard $1
perft $2 0 0
EOF
}


crafty_result=`getCraftyResult "$1" "$2"`
perft_result=`$PERFT perft "$1" "$2"`

if [[ "$crafty_result" != "$perft_result" ]]; then
    echo "debugging : $1 @ $2 $crafty_result != $perft_result" 
    $PERFT siblings "$1" | while read newfen; do
	$0 "$newfen" "$((${2} - 1))"
    done
fi
