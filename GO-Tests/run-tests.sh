#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -lt 3 ] || [ $(($# % 2)) -ne 1 ]; then
  echo "Usage: $0 <N> <command1> <output1> <command2> <output2> ..."
  exit 1
fi

# Get the number of iterations
N=$1
shift

# Run the commands N times and append their output to the respective files
for ((i=1; i<=$#; i+=2)); do
  command="${!i}"
  output="${@:$((i+1)):1}"
  echo "Running command: $command"
  echo "Output file: $output"

  # Create the output file if it doesn't exist
  touch "$output"

  # Execute the command N times and append output to the file
  for ((j=1; j<=N; j++)); do
    echo "--/--" >> "$output"
    eval "$command" >> "$output"
  done
done

echo "Script execution completed!"

