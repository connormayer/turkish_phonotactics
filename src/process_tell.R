library(tidyverse)

#setwd("C:/Users/conno/git_repos/turkish_phonotactics")
setwd("E:/git_repos/turkish_phonotactics")

########################################################
# TASK 1: Get a list of all the suffixed forms in TELL #
########################################################
elicit_1 <- read_tsv('telldata/ELICIT.db.txt')
elicit_2 <- read_tsv('telldata/ELICIT2.db.txt')

# Get two elicitation sets and pivot them into useful format
words_1 <- elicit_1 %>%
  select(-knows, -uses, -ulcase) %>%
  pivot_longer(-lexeme, names_to = 'type', values_to = 'word')

words_2 <-  elicit_2 %>%
  select(-knows, -uses, -ulcase) %>%
  pivot_longer(-lexeme, names_to = 'type', values_to = 'word')

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
  mutate(word=str_replace_all(word, "- ", "")) %>%
  mutate(word=str_replace_all(word, "@ ?", '')) %>%
  mutate(word=str_replace_all(word, "_ ", '')) %>%
  mutate(word=strsplit(as.character(word), '\\s*[~\\?>#]\\s*')) %>%
  unnest(word) %>%
  mutate(word=str_replace_all(word, "a s t ɾ a g a n d͡ʒ �", "a s t ɾ a g a n d͡ʒ ɯ")) %>%
  mutate(word=str_replace_all(word, "ɾ e f ɾ a k t ø ɾ d͡ʒ �", "ɾ e f ɾ a k t ø ɾ d͡ʒ y")) %>%
  mutate(word=str_replace_all(word, "s y ɾ y m d͡ʒ �", "s y ɾ y m d͡ʒ y")) %>%
  mutate(word=str_replace_all(word, "t a b a n d͡ʒ �", "t a b a n d͡ʒ ɯ")) %>%
  mutate(word=str_replace_all(word, "t a b a n d͡ʒ �", "t a b a n d͡ʒ ɯ")) %>%
  mutate(word=str_replace_all(word, "b i n k ɯ ɫ ɯ d͡ʒ ɯ �", "b i n k ɯ ɫ ɯ d͡ʒ ɯ")) %>%
  mutate(word=str_replace_all(word, "g y t͡ʃ �", "g y t͡ʃ y")) %>%
  mutate(word=str_replace_all(word, "h i s a ɾ d͡ʒ �", "h i s a ɾ d͡ʒ ɯ")) %>%
  filter(!str_detect(word, 'x')) %>%
  filter(word != 'n') %>%
  filter(word != 'd͡ʒ') %>%
  filter(word != 'j') %>%
  mutate(word=str_trim(word)) %>%
  distinct() %>%
  filter(word != '')

# Check that all the characters in the data are what we expect
paste0(unique(unlist(strsplit(words$word, ''))), collapse = " ")

# Save output to file
words %>%
  select(-type, -lexeme) %>%
  write_csv('data/suffixed.csv', col_names = FALSE)

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
  mutate(vowels = str_trim(str_replace_all(word, "[^iyeøɯuao]", ''))) %>%
  mutate(vowels = map_chr(
    str_split(vowels, pattern = ""),
    str_flatten, collapse=" "
  )) %>%
  mutate(height_harmony=!(str_detect(vowels, high_vowels) & str_detect(vowels, low_vowels)),
         back_harmony=!(str_detect(vowels, back_vowels) & str_detect(vowels, front_vowels)),
         round_harmony=!(str_detect(vowels, round_vowels) & str_detect(vowels, unround_vowels)))

paste0(unique(unlist(strsplit(vowel_only_suffixed$vowels, ''))), collapse = " ")

vowel_props_suffixed <- vowel_only_suffixed %>%
  summarize(prop_height_harmony = mean(height_harmony),
            prob_back_harmony = mean(back_harmony),
            prop_round_harmony = mean(round_harmony))
vowel_props_suffixed

# Save output to file
vowel_only_suffixed %>%
  select(vowels) %>%
  write_csv('data/vowel_only_suffixed.csv', col_names=FALSE)

# Now check in citation forms only
# Coarse way of removing infinitival suffix
verb_check <- rbind(elicit_1, elicit_2) %>%
  mutate(mek_noun=str_sub(lexeme, -3, -1) %in% c('mak', 'mek') & !is.na(accusative) & is.na(aorist)) %>%
  filter(mek_noun) %>%
  distinct(lexeme)

citation <- words %>%
  mutate(mek_noun = lexeme %in% verb_check$lexeme) %>%
  filter(type == 'citation' & !is.na(word)) %>%
  mutate(word = ifelse(!mek_noun & str_sub(word, -5, -1) %in% c('m a k', 'm e k'),
                       str_sub(word, 1, -6),
                       word)) 

citation %>%
  select(word) %>%
  write_csv('data/citation.csv', col_names=FALSE)

vowel_only_citation <- citation %>%
  mutate(vowels = str_trim(str_replace_all(word, "[^iyeøɯuao]", ''))) %>%
  mutate(vowels = map_chr(
    str_split(vowels, pattern = ""),
    str_flatten, collapse=" "
  )) %>%
  mutate(height_harmony=!(str_detect(vowels, high_vowels) & str_detect(vowels, low_vowels)),
         back_harmony=!(str_detect(vowels, back_vowels) & str_detect(vowels, front_vowels)),
         round_harmony=!(str_detect(vowels, round_vowels) & str_detect(vowels, unround_vowels)))

paste0(unique(unlist(strsplit(vowel_only_citation$vowels, ''))), collapse = " ")

vowel_props_citation <- vowel_only_citation %>%
  summarize(prop_height_harmony = mean(height_harmony),
            prob_back_harmony = mean(back_harmony),
            prop_round_harmony = mean(round_harmony))
vowel_props_citation

vowel_only_citation %>%
  select(vowels) %>%
  write_csv('data/vowel_only_citation.csv', col_names=FALSE)

# Now check in roots
# We can't use roots for training data because they don't distinguish
# light and dark l :(, but we can use them to check harmony
root_df <- read_tsv('telldata/ROOTS.db.txt') %>%
  select(root) %>%
  mutate(root = map_chr(
    str_split(root, pattern = ""),
    str_flatten, collapse=" "
  )) %>%
  mutate(root=str_replace_all(root, 'c @', 't͡ʃ')) %>%
  mutate(root=str_replace_all(root, 'c', 'd͡ʒ')) %>%
  mutate(root=str_replace_all(root, 'g @', 'ɟ')) %>%
  mutate(root=str_replace_all(root, 'i @', 'ɯ')) %>%
  mutate(root=str_replace_all(root, 'j', 'ʒ')) %>%
  mutate(root=str_replace_all(root, 'k @', 'c')) %>%
  mutate(root=str_replace_all(root, 'j', 'ʒ')) %>%
  mutate(root=str_replace_all(root, 'o @', 'ø')) %>%
  mutate(root=str_replace_all(root, 's @', 'ʃ')) %>%
  mutate(root=str_replace_all(root, 'r', 'ɾ')) %>%
  mutate(root=str_replace_all(root, 'y', 'j')) %>%
  mutate(root=str_replace_all(root, 'u @', 'y')) %>%
  mutate(height_harmony=!(str_detect(root, high_vowels) & str_detect(root, low_vowels)),
         back_harmony=!(str_detect(root, back_vowels) & str_detect(root, front_vowels)),
         round_harmony=!(str_detect(root, round_vowels) & str_detect(root, unround_vowels)))
  
  vowel_props_root <- root_df %>%
    summarize(prop_height_harmony = mean(height_harmony),
              prob_back_harmony = mean(back_harmony),
              prop_round_harmony = mean(round_harmony))
  vowel_props_root
