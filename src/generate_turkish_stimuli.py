# This script generates all possible Turkish CVCVC words
import csv
import pandas as pd

consonants = [
    'm',
    'n',
    'p',
    't',
    'k',
    'b',
    'd',
    'g',
    'ɟ',
    't͡ʃ',
    'd͡ʒ',
    'f',
    's',
    'ʃ',
    'h',
    'v',
    'z',
    'ʒ',
    'l',
    'ɫ',
    'j',
    'ɾ',
    'c',
]

vowels = [
    'i',
    'y',
    'ɯ',
    'u',
    'e',
    'œ',
    'a',
    'o'
]

real_words = pd.read_csv('data/real_turkish_words.csv')
real_words = set(real_words.word)
results = []

for c1 in consonants:
    for v1 in vowels:
        for c2 in consonants:
            for v2 in vowels:
                for c3 in consonants:
                    word = "{} {} {} {} {}".format(
                        c1, v1, c2, v2, c3
                    )
                    if not word in real_words:
                        results.append([word])

with open('data/turkish_stimuli_candidates.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerows(results)
