# This script generates all possible Turkish CVCVC words
import csv
import pandas as pd

front_consonants = [
    'c', # k
    'ɟ', # g
    'l'  
]

back_consonants = [
    'k',
    'g',
    'ɫ'  # l
]

labials = [
    'p',
    'b',
    'f',
    'v',
    'm'
]

voiced_stops = [
    'ɟ',
    'g',
    'b',
    'd',
    'd͡ʒ',
]

consonants = [
    'n',
    't',
    'd',
    't͡ʃ', # ç
    'd͡ʒ', # c
    's',
    'ʃ', # ş
    'h',
    'z',
    'ʒ', # j
    'j', # y
    'ɾ'  # r
] + labials + front_consonants + back_consonants

front_vowels = [
    'i',
    'y', # ü
    'e',
    'ø', # ö
]

back_vowels = [
    'ɯ', # ı
    'u',
    'a',
    'o'
]

vowels = front_vowels + back_vowels

real_words = pd.read_csv('data/real_turkish_words.csv')
real_words = set(real_words.word)
results = []

real_word_count = 0
for c1 in consonants:
    for v1 in vowels:
        for c2 in consonants:
            for v2 in vowels:
                for c3 in consonants:
                    if not (c1 == 'ɾ' or
                            c1 in front_consonants and v1 in back_vowels or 
                            c1 in back_consonants and v1 in front_vowels or
                            c2 in front_consonants and v2 in back_vowels or
                            c2 in back_consonants and v2 in front_vowels or
                            c3 in front_consonants and v2 in back_vowels or
                            c3 in back_consonants and v2 in front_vowels or
                            c3 in voiced_stops):
                        word = "{} {} {} {} {}".format(
                            c1, v1, c2, v2, c3
                        )
                        if not word in real_words:
                            results.append([word])
                        else:
                            real_word_count += 1

with open('data/turkish_stimuli_candidates.csv', 'w') as f:
    writer = csv.writer(f)
    writer.writerows(results)
