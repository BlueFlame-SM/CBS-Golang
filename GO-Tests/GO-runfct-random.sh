#!/bin/bash

# Check if the filename argument is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <filename> [integer]"
  exit 1
fi

filename="$1"

# Check if the integer argument is provided
if [ -z "$2" ]; then
  integer=$(od -An -N2 -tu2 /dev/random | awk '{print $1}')
else
  integer="$2"
fi

# Run the command with the provided or generated values
# Disable `pattern-matching` non-derterminism flag, seems to be broken...
command="GO-runfct $filename --format-string-outputs --non-deterministic value-operations,rules,interleaving-of-args --seed $integer"
echo "Seed:"
echo "$integer"
echo ""
eval "$command"

