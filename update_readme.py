#!/usr/bin/env python3

import re
import datetime
import sys

def update_version(version):
    with open("README.md", "r") as f:
        content = f.read()
    
    version_pattern = r"- Version: (\d+\.\d+\.\d+)"
    content = re.sub(version_pattern, f"- Version: {version}", content)

    table_version_pattern = r"\| (Python|Go|VSCode Extension)\s*\|.*\|.*\| (\d+\.\d+\.\d+) \|"
    content = re.sub(table_version_pattern, r"| \1   | Stable | Yes  | " + version + " |", content)
    
    with open("README.md", "w") as f:
        f.write(content)
    print("README.md version updated successfully!")

def update_date():
    today = datetime.date.today().strftime("%Y-%m-%d")
    
    with open("README.md", "r") as f:
        content = f.read()
    
    last_update_pattern = r"- Last Update: (\d{4}-\d{2}-\d{2})"
    content = re.sub(last_update_pattern, f"- Last Update: {today}", content)
    
    with open("README.md", "w") as f:
        f.write(content)
    print("README.md last update date updated successfully!")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: update_readme.py [version <new_version>|date]")
        sys.exit(1)
    
    action = sys.argv[1]
    
    if action == "version":
        if len(sys.argv) != 3:
            print("Usage: update_readme.py version <new_version>")
            sys.exit(1)
        version = sys.argv[2]
        update_version(version)
    elif action == "date":
        update_date()
    else:
        print("Invalid action. Use 'version' or 'date'.")
        sys.exit(1)