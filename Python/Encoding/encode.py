import re

def encode(words):
    A = 65
    offset = 0
    new_words = ""
    words = re.sub('[^a-zA-Z]+', '', words.upper())
    
    length = len(words)
    
    for char in words:
        new_words += str((ord(char) - A) + 1 + (offset % 26))
        offset += 1
    
    return new_words

print(encode("Testing!@#$%^423"))