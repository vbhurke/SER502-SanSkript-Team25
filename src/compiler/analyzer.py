import re

# Code to tokenize sample programs 

# Token specifications
token_specification = [
    ('NUMBER',   r'\d+(\.\d*)?'),  # Integer or decimal number
    ('ASSIGN',   r'='),            # Assignment operator
    ('END',      r';'),            # Statement terminator
    ('ID',       r'[A-Za-z]+'),    # Identifiers
    ('OP',       r'[+\-*/]'),      # Arithmetic operators
    ('BOOL_OP',  r'and|or|not'),   # Boolean operators
    ('BOOL',     r'true|false'),   # Boolean literals
    ('LPAREN',   r'\('),           # Left parenthesis
    ('RPAREN',   r'\)'),           # Right parenthesis
    ('LANG_START', r'Aarambh'),    # Language start
    ('LANG_END', r'Antah'),        # Language end
    ('PRINT',    r'Likhyam'),      # Print keyword
    ('SKIP',     r'[ \t]+'),       # Skip over spaces and tabs
    ('NEWLINE',  r'\n'),           # Line endings
    ('MISMATCH', r'.'),            # Any other character
]

# Building the regex
tok_regex = '|'.join('(?P<%s>%s)' % pair for pair in token_specification)

def tokenize(code):
    tokens = []
    line_num = 1
    for mo in re.finditer(tok_regex, code):
        kind = mo.lastgroup
        value = mo.group()
        if kind in ['NEWLINE', 'SKIP']:  # Ignore newlines and whitespace
            continue
        if kind == 'MISMATCH':
            raise RuntimeError(f'{value!r} unexpected on line {line_num}')
        if kind not in ['MISMATCH', 'SKIP', 'NEWLINE']:  # Add only valid tokens
            tokens.append(value)
    return tokens

# Writing the tokens to a file I/O
def write_tokens_to_file(tokens, filename):
    with open(filename, 'w') as file:
        for token in tokens:
            file.write(token + '\n')

# Example usage for a sample code according to our language design.
code = """
Aarambh
int num = 5;
Likhyam(num);
Antah
"""

# Tokenize the code
tokens = tokenize(code)

# Writing tokens to a file
write_tokens_to_file(tokens, 'tokens.txt')

