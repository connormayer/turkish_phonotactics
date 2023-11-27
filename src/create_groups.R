library(tidyverse)


scores_df <- read_csv("data/turkish_phonotactic_judgments/stimuli_candidates_scored.csv") %>%
  select(word, uni_prob, bi_prob, bi_prob_smoothed, pos_uni_score, pos_bi_score, 
         pos_uni_score_smoothed, pos_bi_score_smoothed)

df <- read_csv("data/turkish_phonotactic_judgments/candidates_ortho_v4.csv") %>%
  mutate(vowels=str_replace_all(word, "[^ieouayɯø]", "")) %>%
  inner_join(scores_df, by='word')

# Verify we have the right amounts, should be 36 in each
df %>%
  group_by(vowels) %>%
  count() %>%
  filter(n != 9)

num_groups <- 6

# Set seed for reproducibility
set.seed(123456789)

# Randomly re-order rows within buckets
df_randomized <- df %>% 
  group_by(vowels) %>%
  slice(sample(1:n())) %>%
  ungroup()

# Assign block number
grouped_df <- df_randomized %>%
  mutate(group = rep(seq_len(num_groups), length.out=n()))

grouped_df %>% 
  group_by(group) %>%
  summarize(mean_uni = mean(uni_prob),
         mean_bi = mean(bi_prob_smoothed))

grouped_df %>%
  write_excel_csv('data/grouped_stimuli.csv')
