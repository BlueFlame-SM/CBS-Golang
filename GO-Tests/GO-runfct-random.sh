#!/bin/bash

# Check if the filename argument is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <filename> [integer]"
  exit 1
fi

filename="$1"

# Check if the integer argument is provided
if [ -z "$2" ]; then
  # Generate a random integer between 1 and 1000 (inclusive)
  integer=$(od -An -N2 -tu2 /dev/random | awk '{print $1}')
else
  integer="$2"
fi

# Run the command with the provided or generated values
command="GO-runfct $filename --non-deterministic value-operations,rules,pattern-matching,interleaving-of-args --seed $integer"
echo "Seed:"
echo "$integer"
echo ""
eval "$command"

