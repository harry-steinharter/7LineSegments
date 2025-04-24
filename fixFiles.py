import os

# Set the root folder of your project
project_root = '/Users/harrysteinharter/Documents/MSc/Timo Internship/7LineSegments/'  # ← Change this

# Define what to replace
old_string = 'psychopyFiles.psychopy'
new_string = 'psychopy'

# Walk through the directory and edit each .py file
for dirpath, _, filenames in os.walk(project_root):
    for filename in filenames:
        if filename.endswith('.py'):
            filepath = os.path.join(dirpath, filename)
            with open(filepath, 'r', encoding='utf-8') as file:
                content = file.read()

            if old_string in content:
                new_content = content.replace(old_string, new_string)
                with open(filepath, 'w', encoding='utf-8') as file:
                    file.write(new_content)
                print(f'Updated: {filepath}')
