# This script generates all possible Turkish CVCVC words
import csv
import pandas

consonants_1 = [
    'm',
    'n',
    'p',
    't',
    'k',
    'b',
    'd',
    'g',
    't͡ʃ',
    'd͡ʒ',
    'f',
    's',
    'ʃ',
    'h',
    'z',
    'ʒ',
    'l',
    'j',
    'ɾ',
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

results = []

for c1 in consonants:
    for v1 in vowels:
        for c2 in consonants:
            for v2 in vowels:
                for c3 in consonants:
                    word = "'{}{}{}{}{}".format(
                        c1, v1, c2, v2, c3
                    )

                    results.append(word)

with open('turkish_stimuli_candidates.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerows(results)
