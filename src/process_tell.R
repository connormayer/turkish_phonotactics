library(tidyverse)

setwd("C:/Users/conno/git_repos/turkish_phonotactics")
#setwd("E:/git_repos/turkish_phonotactics")

########################################################
# TASK 1: Get a list of all the suffixed forms in TELL #
########################################################
elicit_1 <- read_tsv('telldata/ELICIT.db.txt')
elicit_2 <- read_tsv('telldata/ELICIT2.db.txt')

# Get two elicitation sets and pivot them into useful format
words_1 <- elicit_1 %>%
  select(-knows, -uses, -ulcase, -lexeme) %>%
  pivot_longer(everything(), names_to = 'type', values_to = 'word')

words_2 <-  elicit_2 %>%
  select(-knows, -uses, -ulcase, -lexeme) %>%
  pivot_longer(everything(), names_to = 'type', values_to = 'word')

# Filter out weird characters, inconsistent notation
words <- rbind(words_1, words_2) %>%
  filter(!is.na(word)) %>%
  mutate(word=str_replace_all(word, ":", "")) %>%
  mutate(word=str_replace_all(word, ';', '')) %>%
  mutate(word=str_replace_all(word, '\002', '')) %>%
  mutate(word=str_replace_all(word, '3/4', '')) %>%
  mutate(word=str_replace_all(word, '\\[puɫɫ 9\\]', '')) %>%
  mutate(word = map_chr(
    str_split(word, pattern = ""),
    str_flatten, collapse=" "
  )) %>%
  mutate(word=str_replace_all(word, "t ʃ", "t͡ʃ")) %>%
  mutate(word=str_replace_all(word, "d ʒ", "d͡ʒ")) %>%
  mutate(word=str_replace_all(word, "ø", "œ")) %>%
  mutate(word=str_replace_all(word, "- ", "")) %>%
  mutate(word=str_replace_all(word, "@ ?", '')) %>%
  mutate(word=str_replace_all(word, "_ ", '')) %>%
  mutate(word=strsplit(as.character(word), '\\s*[~\\?>#]\\s*')) %>%
  unnest(word) %>%
  mutate(word=str_replace_all(word, "a s t ɾ a g a n d͡ʒ �", "a s t ɾ a g a n d͡ʒ ɯ")) %>%
  mutate(word=str_replace_all(word, "ɾ e f ɾ a k t œ ɾ d͡ʒ �", "ɾ e f ɾ a k t œ ɾ d͡ʒ y")) %>%
  mutate(word=str_replace_all(word, "s y ɾ y m d͡ʒ �", "s y ɾ y m d͡ʒ y")) %>%
  mutate(word=str_replace_all(word, "t a b a n d͡ʒ �", "t a b a n d͡ʒ ɯ")) %>%
  mutate(word=str_replace_all(word, "t a b a n d͡ʒ �", "t a b a n d͡ʒ ɯ")) %>%
  mutate(word=str_replace_all(word, "b i n k ɯ ɫ ɯ d͡ʒ ɯ �", "b i n k ɯ ɫ ɯ d͡ʒ ɯ")) %>%
  mutate(word=str_replace_all(word, "g y t͡ʃ �", "g y t͡ʃ y")) %>%
  mutate(word=str_replace_all(word, "h i s a ɾ d͡ʒ �", "h i s a ɾ d͡ʒ ɯ")) %>%
  filter(!str_detect(word, 'x')) %>%
  distinct() %>%
  mutate(word=str_trim(word))

# Check that all the characters in the data are what we expect
paste0(unique(unlist(strsplit(words$word, ''))), collapse = " ")

# Save output to file
words %>%
  select(-type) %>%
  write_csv('data/real_turkish_words.csv')

#################################################################
# TASK 2: Check for height/back/round harmony in suffixed forms #
#################################################################

high_vowels <- "[iyɯu]"
low_vowels <- "[eøao]"

front_vowels <- "[iyeø]"
back_vowels <- "[ɯuao]"

round_vowels <- "[yuøo]"
unround_vowels <- "[iɯea]"

# First check in suffixed forms
vowel_only_suffixed <- words %>%
  mutate(vowels = str_trim(str_replace_all(word, "[^iyeœɯuao]", ''))) %>%
  mutate(vowels = map_chr(
    str_split(vowels, pattern = ""),
    str_flatten, collapse=" "
  )) %>%
  mutate(vowels=str_replace_all(vowels, 'œ', 'ø')) %>%
  mutate(height_harmony=!(str_detect(vowels, high_vowels) & str_detect(vowels, low_vowels)),
         back_harmony=!(str_detect(vowels, back_vowels) & str_detect(vowels, front_vowels)),
         round_harmony=!(str_detect(vowels, round_vowels) & str_detect(vowels, unround_vowels)))

vowel_props_suffixed <- vowel_only_suffixed %>%
  summarize(prop_height_harmony = mean(height_harmony),
            prob_back_harmony = mean(back_harmony),
            prop_round_harmony = mean(round_harmony))

# Now check in citation forms only
# TODO: This is a coarse way of removing the infinitival suffix,
# probably captures some nouns too
vowel_only_citation <- words %>%
  filter(type == 'citation' & !is.na(word)) %>%
  mutate(word = ifelse(str_sub(word, -5, -1) %in% c('m a k', 'm e k'),
                       str_sub(word, 1, -6),
                       word)) %>%
  mutate(vowels = str_trim(str_replace_all(word, "[^iyeœɯuao]", ''))) %>%
  mutate(vowels = map_chr(
    str_split(vowels, pattern = ""),
    str_flatten, collapse=" "
  )) %>%
  mutate(vowels=str_replace_all(vowels, 'œ', 'ø')) %>%
  mutate(height_harmony=!(str_detect(vowels, high_vowels) & str_detect(vowels, low_vowels)),
         back_harmony=!(str_detect(vowels, back_vowels) & str_detect(vowels, front_vowels)),
         round_harmony=!(str_detect(vowels, round_vowels) & str_detect(vowels, unround_vowels)))

vowel_props_citation <- vowel_only_citation %>%
  summarize(prop_height_harmony = mean(height_harmony),
            prob_back_harmony = mean(back_harmony),
            prop_round_harmony = mean(round_harmony))

# Now check in roots
