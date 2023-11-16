library(tidyverse)

citation_height_df <- read_csv('data/agl_stimuli/trained_on_citation_forms/height_citation_dataframe.csv')
citation_height_df %>% 
  group_by(condition, harmonic) %>%
  summarize_if(is.numeric, mean) 

suffix_height_df <- read_csv('data/agl_stimuli/trained_on_suffixed_forms/height_suffix_dataframe.csv')
suffix_height_df %>% 
  group_by(condition, harmonic) %>%
  summarize_if(is.numeric, mean) 

citation_back_df <- read_csv('data/agl_stimuli/trained_on_citation_forms/back_citation_dataframe.csv')
citation_back_df %>% 
  group_by(condition, harmonic) %>%
  summarize_if(is.numeric, mean) 

suffix_back_df <- read_csv('data/agl_stimuli/trained_on_suffixed_forms/back_suffix_dataframe.csv')
suffix_back_df %>% 
  group_by(condition, harmonic) %>%
  summarize_if(is.numeric, mean) 
