library(tidyverse)

setwd("C:/Users/conno/git_repos/turkish_phonotactics")
#setwd("E:/git_repos/turkish_phonotactics")

elicit_1 <- read_tsv('telldata/ELICIT.db.txt')
elicit_2 <- read_tsv('telldata/ELICIT2.db.txt')

words_1 <- elicit_1 %>%
  select(-knows, -uses, -ulcase, -lexeme) %>%
  pivot_longer(everything(), names_to = 'type', values_to = 'word')

words_2 <-  elicit_2 %>%
  select(-knows, -uses, -ulcase, -lexeme) %>%
  pivot_longer(everything(), names_to = 'type', values_to = 'word')

words <- rbind(words_1, words_2) %>%
  select(-type) %>%
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
  write_csv('data/real_turkish_words.csv')

paste0(unique(unlist(strsplit(words$word, ''))), collapse = " ")
