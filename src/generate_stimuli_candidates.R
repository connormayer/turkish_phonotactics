library(RANN)
library(tidyverse)

setwd("E:/git_repos/turkish_phonotactics")

full_candidates <- read_csv("data/turkish_phonotactic_judgments/stimuli_candidates_scored.csv") %>%
  mutate(vowels = str_trim(str_replace_all(word, "[^iyeøɯuao]", ''))) %>%
  mutate(vowels = map_chr(
    str_split(vowels, pattern = ""),
    str_flatten, collapse=" "
  )) %>%
  select(-contains("weighted"))

unique(full_candidates$vowels)

tokens <- data.frame()
for (vowel_pair in unique(full_candidates$vowels)) {
  print(vowel_pair)
  temp_df <- full_candidates %>%
    filter(vowels == vowel_pair) %>%
    select(word, uni_prob, bi_prob_smoothed)
  
  uni_range <- range(temp_df$uni_prob)
  bi_range <- range(temp_df$bi_prob_smoothed)
  
  uni_range_start <- uni_range[1]
  uni_range_end <- uni_range[2]
  
  bi_range_start <- bi_range[1]
  bi_range_end <- bi_range[2]
  
  uni_seq <- seq(
    uni_range_start,
    uni_range_end, 
    by=(uni_range_end - uni_range_start) / 2
  )
  bi_seq <- seq(
    bi_range_start, 
    bi_range_end, 
    by=(bi_range_end - bi_range_start) / 2
  )
  for (i in uni_seq) {
    for (j in bi_seq) {
      point = c(i, j)
      closest_idx <- nn2(
        temp_df %>% select(uni_prob, bi_prob_smoothed), 
        as.data.frame(t(point)), k=100)$nn.idx
      found_point <- FALSE
      closest_i <- 1
      while (!found_point) {
        closest <- temp_df[closest_idx[closest_i],]
        if (nrow(tokens) > 0 && 
            nrow(anti_join(closest, tokens, by="word")) == 0) {
          # Closest point is already in our sample
          closest_i = closest_i + 1
        } 
        else {
          found_point <- TRUE
        }
      }
      tokens <- rbind(tokens, closest)
    }
  }
}

full_tokens <- inner_join(full_candidates, tokens)

# Check that tokens are roughly evenly distributed in unigram/bigram space
full_tokens %>%
  ggplot() +
  geom_point(aes(x=uni_prob, y=bi_prob_smoothed)) +
  facet_wrap(~vowels)

convert_to_ortho <- function(df) {
  df %>%
    select(word) %>%
    mutate(ortho = str_replace_all(word, 'y', 'ü')) %>%
    mutate(ortho = str_replace_all(ortho,'ø', 'ö')) %>%
    mutate(ortho = str_replace_all(ortho,'ɯ', 'ı')) %>%
    mutate(ortho = str_replace_all(ortho,'c', 'k')) %>%
    mutate(ortho = str_replace_all(ortho,'t͡ʃ', 'ç')) %>%
    mutate(ortho = str_replace_all(ortho,'d͡ʒ', 'c')) %>%
    mutate(ortho = str_replace_all(ortho,'ʃ', 'ş')) %>%
    mutate(ortho = str_replace_all(ortho,'j', 'y')) %>%
    mutate(ortho = str_replace_all(ortho,'ʒ', 'j')) %>%
    mutate(ortho = str_replace_all(ortho,'ɾ', 'r')) %>%
    mutate(ortho = str_replace_all(ortho,'ɟ', 'g')) %>%
    mutate(ortho = str_replace_all(ortho,'ɫ', 'l')) %>%
    mutate(ortho = str_replace_all(ortho, ' ', ''))
}

full_tokens_ortho <- convert_to_ortho(full_tokens) %>% 
  write_csv('data/turkish_phonotactic_judgments/candidates_ortho.csv')

# Find poik equivalent
poik_df <- full_candidates %>%
  filter(!(word %in% full_tokens$word)) %>%
  select(
    word, uni_prob, bi_prob_smoothed, pos_uni_score_smoothed, 
    pos_bi_score_smoothed
  ) %>%
  mutate(
    uni_prob = cume_dist(uni_prob),
    bi_prob_smoothed = cume_dist(bi_prob_smoothed),
    pos_uni_score_smoothed = cume_dist(pos_uni_score_smoothed),
    pos_bi_score_smoothed = cume_dist(pos_bi_score_smoothed)
  )

# Find kip equivalents
poik_df %>%
  arrange(-bi_prob_smoothed, -uni_prob) %>%
  select(word) %>%
  convert_to_ortho() %>%
  slice_head(n=20) %>%
  write_csv('data/turkish_phonotactic_judgments/kip_candidates.csv')

# Find poik equivalents
closest_idx_poik <- nn2(
  poik_df %>% 
    select(uni_prob, bi_prob_smoothed, pos_uni_score_smoothed, 
           pos_bi_score_smoothed), 
  as.data.frame(t(c(0.484, 0.676, 0.834, 0.296))), k=10)$nn.idx

poik_df[as.vector(closest_idx_poik),] %>%
  select(word) %>%
  convert_to_ortho() %>%
  slice_head(n=20) %>%
  write_csv('data/turkish_phonotactic_judgments/poik_candidates.csv')

# Find lvag equivalents
poik_df %>%
  arrange(bi_prob_smoothed, uni_prob) %>%
  select(word) %>%
  convert_to_ortho() %>%
  slice_head(n=20) %>%
  write_csv('data/turkish_phonotactic_judgments/lvag_candidates.csv')
