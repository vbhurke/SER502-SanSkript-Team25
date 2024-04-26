# Author - Milind Deshpande, Siddharth Sharma
# Purpose - To act as an entrypoint to run the SanSkript programs
# Version - Version_2 Final Project
# Date - 26 April 2024

import os
import sys
from _path import ROOT_DIRECTORY
from src.compiler.analyzer import tokenize, process_prolog
import subprocess

def main():
    try:
        if len(sys.argv) < 2:
            print("Usage: python runskript.py <filename>")
            return

        filename = sys.argv[1]
        file_path = ROOT_DIRECTORY + "/data/" + filename

        if not os.path.isfile(file_path):
            print(f"Error: File {filename} does not exist in the data directory.")
            return

        # Tokenization and processing
        tokens = tokenize(file_path)
        tmp_file = sys.argv[1]
        parse_file = os.path.splitext(tmp_file)[0]
        prolog_code = process_prolog(tokens, parse_file)

        # Running Prolog or other script using subprocess if needed
        command = [
            "swipl",
            "-f", "src/compiler/grammar.pl",
            "-g", f"program(M, {prolog_code}, []), eval_program(M, [], F).",
            "-t" , "halt"
        ]
        result = subprocess.run(command, capture_output=True, text=True, check=True)
        print(result.stdout)
        # command2 = ["swipl", "-f", "src/compiler/parse_tokens.pl", "-g", f'convert("{parse_file}")']
        # result2 = subprocess.run(command2, capture_output=True, text=True, check=True)

    except subprocess.CalledProcessError as e:
        print("An error occurred while running the command:")
        print(e)
    except Exception as e:
        print(f"An unexpected error occurred: {e}")
    finally:
        print("Execution completed.")

if __name__ == "__main__":
    main()