import os

__location__ = os.path.realpath(os.path.join(os.getcwd(), os.path.dirname(__file__)))
f = open(os.path.join(__location__, 'Cobol.g4'))
lines = f.readlines()

def permute(s):
    result = [[s]]
    for i in range(1, len(s)):
        first = [s[:i]]
        rest = s[i:]
        for p in permute(rest):
            result.append(first + p)
    return result

perms = {}
keywords = set()
skip = False
# Find key words in the COBOL grammar and calculate the permutations of each word.
for index, line in enumerate(lines):
    stripped = line.strip()
    if stripped != "" and not stripped.startswith("*") and not stripped.startswith("//") and not stripped.startswith("/*"):
        # Skip grammar rules.
        if skip == False and stripped[0].islower():
            skip = True
        else:
            if skip == True:
                if stripped.startswith(';'):
                    skip = False
            else:
                # Get keyword.
                keyword = stripped[0:stripped.index(':')].strip()
                keywords.add(keyword)
                # Find all permutations for the keyword.
                perms[keyword] = permute(keyword)

# Print each key and permutations if each substring is a COBOL keyword.
# Key
for key in perms:
    perm = perms[key]
    for p in perm:
        if set(p).issubset(keywords) and len(p) != 1:
            print("Keyword: " + key, end = " ")
            print(", Permutations:", end = " ")
            print(p, end = "\n")
