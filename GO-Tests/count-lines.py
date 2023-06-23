import sys
from collections import Counter

def count_line_occurrences(file_path, min_count=0):
    try:
        with open(file_path, 'r') as file:
            lines = file.readlines()
            line_count = Counter(lines)
            max_line_length = max(len(line.strip()) for line in line_count.keys())
            max_count_length = max(len(str(count)) for count in line_count.values())
            
            for line, count in line_count.items():
                if count >= min_count:
                    line = line.strip()
                    count_str = str(count)
                    line_padding = ' ' * (max_line_length - len(line))
                    count_padding = ' ' * (max_count_length - len(count_str))
                    print(f"Line: {line}{line_padding} | Count: {count_padding}{count_str}")
    
    except FileNotFoundError:
        print("File not found.")
    except IOError:
        print("An error occurred while reading the file.")

def main():
    # Check if file name and min_count were provided as command line arguments
    if len(sys.argv) < 3:
        print("Please provide a file name and minimum count as command line arguments.")
    else:
        file_name = sys.argv[1]
        min_count = int(sys.argv[2])
        count_line_occurrences(file_name, min_count)

if __name__ == '__main__':
    main()

