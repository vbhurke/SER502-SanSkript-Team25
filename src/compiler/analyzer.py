# Author - Milind Deshpande, Siddharth Sharma
# Purpose - To tokenize the input program file for further processing by the grammar and the evaluator
# Version - Version_2 Final Project
# Date - 26 April 2024

import re
import os
# Token specifications
token_specification = [
    ('SINGLE_QUOTE_STRING', r"'[^']*'"), # Single quoted string
    ('INCREMENT',  r'\+\+'),          # Increment operator
    ('DECREMENT',  r'--'),            # Decrement operator
    ('ID',         r'[A-Za-z]+'),     # Identifiers
    ('GTE',        r'>='),            # Greater than or equal to
    ('LTE',        r'<='),            # Less than or equal to
    ('ARROW',      r'->'),            # Arrow operator
    ('QUESTION',   r'\?'),            # Question mark for conditional operations
    ('GT',         r'>'),             # Greater than
    ('LT',         r'<'),             # Less than
    ('EQ',         r'=='),            # Equal to
    ('NEQ',        r'=/='),           # Not equal to
    ('STRING',     r'"[^"]*"'),       # Double quoted string
    ('NUMBER',     r'\d+(\.\d*)?'),   # Integer or decimal number
    ('ASSIGN',     r'='),             # Assignment operator
    ('END',        r'\|\|'),          # Statement terminator '||'
    ('OP',         r'[+\-*/]'),       # Arithmetic operators
    ('BOOL_OP',    r'and|or|not'),    # Boolean operators
    ('BOOL',       r'true|false'),    # Boolean literals
    ('LPAREN',     r'\('),            # Left parenthesis
    ('RPAREN',     r'\)'),            # Right parenthesis
    ('LANG_START', r'aarambh'),       # Language start
    ('LANG_END',   r'antah'),         # Language end
    ('PRINT',      r'likhyam'),       # Print keyword
    ('COLON',      r':'),             # Colon, commonly used in ternary operations or other constructs
    ('SKIP',       r'[ \t]+'),        # Skip over spaces and tabs
    ('NEWLINE',    r'\n'),            # Line endings
    ('MISMATCH',   r'.'),             # Any other character
]

# Building the regex
tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)

def tokenize(file_path):
    """
    Tokenizes the input file based on the predefined token specifications and writes the output to a file.
    """
    tokenized_output = []  # List to collect tokens
    with open(file_path, 'r') as file, open('tokens_output.txt', 'w') as token_file:
        code = file.read()
        line_num = 1
        for mo in re.finditer(tok_regex, code):
            kind = mo.lastgroup
            value = mo.group()
            if kind == 'NEWLINE' or kind == 'SKIP':
                line_num += 1 if kind == 'NEWLINE' else 0
                continue
            if kind == 'MISMATCH':
                raise RuntimeError(f'{value!r} unexpected on line {line_num}')
            # Only put specific tokens in single quotes
            if kind == 'END':
                tokenized_output.append("'||'")
            elif kind == 'LPAREN':
                tokenized_output.append("'('")
            elif kind == 'RPAREN':
                tokenized_output.append("')'")
            elif kind == 'QUESTION':
                tokenized_output.append("'?'")
            elif kind == 'COLON':
                tokenized_output.append("':'")
            elif kind == 'SINGLE_QUOTE_STRING':
                tokenized_output.append("'")
            else:
                tokenized_output.append(value)  
            token_file.write(f'{value}\n')  

    return tokenized_output

def process_prolog(tokens, parse_file):
    """
    Converts token list to a format suitable for Prolog processing or further manipulation.
    """
    prolog_code = "[" + ", ".join(map(str, tokens)) + "]"
    temp_directory = os.path.join(os.getcwd(), 'src/runtime/temp')
    
    if not os.path.exists(temp_directory):
        os.makedirs(temp_directory)
    
    output_file_path = os.path.join(temp_directory, f"{parse_file}.sktc")
    
    with open(output_file_path, 'w') as output_file:
        output_file.write(prolog_code)
        
    return prolog_code