import sys
import json
import subprocess
import os

# Read configuration from JSON file
def read_configuration(file_path):
    with open(file_path) as file:
        return json.load(file)

# Execute commands based on configuration
def execute_commands(configuration, config_directory):
    for item in configuration:
        command = item["command"]
        output = item["output"]
        repetitions = item.get("repetitions", 1)

        print(f"Running command: {command}")
        print(f"Output file: {output}")
        print(f"Repetitions: {repetitions}\n")

        output_path = os.path.join(config_directory, output)
        with open(output_path, "a") as output_file:
            for i in range(repetitions):
                output_file.write(f"START OUTPUT\n")
                output_file.flush()  # Flush buffer to ensure immediate writing
                subprocess.run(command, shell=True, stdout=output_file, stderr=subprocess.STDOUT)
                output_file.write("\nEND OUTPUT\n")

    print("Script execution completed!")

# Main function
def main():
    if len(sys.argv) < 2:
        print("Usage: python script.py <config_file>")
        return

    config_file = sys.argv[1]
    config_directory = os.path.dirname(os.path.abspath(config_file))

    configuration = read_configuration(config_file)
    execute_commands(configuration, config_directory)

if __name__ == "__main__":
    main()

