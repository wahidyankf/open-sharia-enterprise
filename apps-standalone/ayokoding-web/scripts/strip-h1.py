import os
import re

# Function to strip H1 headers from markdown files
def strip_h1_from_file(file_path):
    with open(file_path, 'r') as file:
        content = file.readlines()

    # Remove H1 headers
    content = [line for line in content if not line.startswith('# ')]

    with open(file_path, 'w') as file:
        file.writelines(content)

# Function to recursively strip H1 headers from all markdown files in a directory
def strip_h1_from_directory(directory):
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith('.md'):
                strip_h1_from_file(os.path.join(root, file))

# Main execution
if __name__ == '__main__':
    directory_to_strip = '../content'  # Change this to the target directory
    strip_h1_from_directory(directory_to_strip)
