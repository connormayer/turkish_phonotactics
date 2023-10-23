library(tidyverse)

setwd("E:/git_repos/turkish_phonotactics")

elicit_1 <- read_tsv('telldata/ELICIT.db.txt')
elicit_2 <- read_tsv('telldata/ELICIT2.db.txt')

roots_1 <- elicit_1 %>% 
  transmute(words = )

words_1 <- elicit_1 %>%
  select(-knows, -uses, -ulcase, -lexeme) %>%
  pivot_longer(everything(), names_to = 'type', values_to = 'word') %>%
  filter(!is.na(word))

words_2 <-  elicit_2 %>%
  select(-knows, -uses, -ulcase, -lexeme) %>%
  pivot_longer(everything(), names_to = 'type', values_to = 'word') %>%
  filter(!is.na(word))
