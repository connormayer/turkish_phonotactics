library(tidyverse)
library(tidytext)
library(viridis)

# Read in experimental results
file_dir <- "results/real_results"
#Read read in the filenames of all files from the naming task
filenames <- list.files(file_dir)

# Create tibbles to hold experimental data
task <- tibble()
guided_test_run <- tibble()
test_run <- tibble()
consent <- tibble()
background <- tibble()
audio_check <- tibble()

# Read in each experimental data file, force reaction time to be numeric,
# and add it to our task tibble
for (filename in filenames) {
  result <- read_csv(paste(file_dir, filename, sep="/"), show_col_types = FALSE) %>%
    filter(`Event Index` != "END OF FILE")
  if (str_detect(filename, 'ibmm')) {
    consent <- rbind(consent, result)
  } else if (str_detect(filename, '16tk')) {
    # Did not consent, discard
  } else if (str_detect(filename, 'ir4a')) {
    background <- rbind(background, result)
  } else if (str_detect(filename, 'axcc')) {
    audio_check <- rbind(audio_check, result)
  } else if (str_detect(filename, 'vpa3')) {
    guided_test_run <- rbind(guided_test_run, result)
  } else if (str_detect(filename, 'umle')) {
    test_run <- rbind(test_run, result)
  } else if (str_detect(filename, 'yepb')) {
    # Final screen file, don't need
  } else{
    task <- rbind(task, result) 
  }
}

# Clean up column names
task <- task %>% 
  rename(ID = `Participant Private ID`,
         timestamp = `UTC Timestamp`,
         zone = `Zone Type`,
         response = Response,
         RT = `Reaction Time`,
         trial = `Trial Number`,
         orthography = ortho,
         filename = filename,
         word = word,
         vowels = vowels,
         uni_prob = uni_prob,
         bi_prob_smoothed = bi_prob_smoothed,
         group = group)

task_responses <- task %>%
  filter(zone == "response_slider_endValue") %>%
  mutate(response = as.numeric(response))

front_vowels <- "[iyeø]" 
back_vowels <- "[ɯuao]"
high_vowels <- "[iyɯu]"
low_vowels <- "[eøao]"
round_vowels <- "[yøuo]"
unround_vowels <- "[ieɯa]"

task_responses <- task_responses %>%
  mutate(# First vowel
         v1 = substr(vowels, 1, 1),
         # Second vowel
         v2 = substr(vowels, 2, 2),
         # Is root back harmonic?
         back_harmonic = !(str_detect(vowels, front_vowels) & str_detect(vowels, back_vowels)),
         # Is root height harmonic?
         height_harmonic = !(str_detect(vowels, high_vowels) & str_detect(vowels, low_vowels)),
         # Does root begin with o or ø?
         oO_violation = v1 == 'o' | v1 == 'ø',
         # Is root round harmonic?
         round_harmonic = !(
            (str_detect(v1, round_vowels) & str_detect(v2, unround_vowels) & str_detect(v2, high_vowels)) |
            (str_detect(v1, unround_vowels) & str_detect(v2, round_vowels) & str_detect(v2, high_vowels))),
         # Is root both back and round harmonic?
         back_and_round_harmonic = back_harmonic & round_harmonic,
         # Gradient score for backness/rounding violations
         back_round_gradient = (1 - back_harmonic) + (1 - round_harmonic),
         # Including initial o/ø constraint
         back_round_o_harmonic = back_harmonic & round_harmonic & oO_violation,
         back_round_o_gradient = (1 - back_harmonic) & (1 - round_harmonic) &  (1 - oO_violation),
         dai_values = vowels %in% (c('ii', 'ie', 'ei', 'ee', 'ye', 'yy', 'ɯɯ',
                                     'ɯa', 'aɯ', 'aa', 'ua', 'uu', 'oa', 'ou')))

# Add unigram and bigram scores to results tibble
vowel_scores <- read_csv("data/vowel_scores_citation.csv") %>%
  mutate(vowels = str_replace(word, ' ', ''),
         v_uni_prob = uni_prob,
         v_bi_prob_smoothed = bi_prob_smoothed,
         v_pos_uni_score_smoothed = pos_uni_score_smoothed,
         v_pos_bi_score_smoothed = pos_bi_score_smoothed) %>%
  select(vowels, v_uni_prob, v_bi_prob_smoothed, v_pos_uni_score_smoothed, v_pos_bi_score_smoothed)

task_responses <- task_responses %>%
  inner_join(vowel_scores, by='vowels')

# TODO: Neighborhood density
# Scale both unigram and bigram probability
stats_data <- task_responses %>%
  mutate(z_v_bi_prob_smoothed = as.vector(scale(v_bi_prob_smoothed)),
         z_v_uni_prob = scale(v_uni_prob))

cor_data <- stats_data %>% 
  # Scale responses within participant
  group_by(ID) %>% 
  mutate(z_response = as.vector(scale(response))) %>%
  ungroup() %>%
  # Take the mean response across participants for each token
  group_by(vowels, v_bi_prob_smoothed, z_v_bi_prob_smoothed, back_harmonic, round_harmonic, 
           back_and_round_harmonic, back_round_gradient, back_round_o_gradient, 
           back_round_o_harmonic, dai_values) %>%
  summarize(z_mean_response = mean(z_response),
            mean_response = mean(response))

# Add in threshold-based constraints
citation_corpus <- read_csv('data/vowel_only_citation.csv', col_names = 'word') %>%
  mutate(word = str_c('x ', word, ' x'))

citation_bigrams <- citation_corpus %>%
  unnest_tokens(bigram, word, token = 'ngrams', n = 2) %>%
  filter(!is.na(bigram)) %>%
  mutate(bigram = str_replace_all(bigram, 'x', '#'))

counts <- citation_bigrams %>%
  group_by(bigram) %>%
  summarize(count = n()) %>%
  separate(bigram, into = c("x1", "x2"), sep = " ", remove = FALSE) %>%
  group_by(x1) %>%
  mutate(c_prob = count / sum(count),
         attested = c_prob > 0) %>%
  ungroup() %>%
  mutate(j_prob = count / sum(count),
         x1 = fct_relevel(x1, c('#', 'ɯ', 'u', 'a', 'o', 'i', 'y', 'e', 'ø')),
         x2 = fct_relevel(x2, c('#', 'ɯ', 'u', 'a', 'o', 'i', 'y', 'e', 'ø')),
         quantile = ntile(c_prob, 100),
         q_10 = quantile < 10,
         q_20 = quantile < 20,
         q_30 = quantile < 30,
         q_40 = quantile < 40,
         q_50 = quantile < 50,
         q_60 = quantile < 60,
         q_70 = quantile < 70,
         q_80 = quantile < 80,
         q_90 = quantile < 90,
         back_harmonic = !(str_detect(bigram, front_vowels) & str_detect(bigram, back_vowels)),
         # Is root height harmonic?
         height_harmonic = !(str_detect(bigram, high_vowels) & str_detect(bigram, low_vowels)),
         # Is root round harmonic?
         round_harmonic = !(
           (str_detect(x1, round_vowels) & str_detect(x2, unround_vowels) & str_detect(x2, high_vowels)) |
             (str_detect(x1, unround_vowels) & str_detect(x2, round_vowels) & str_detect(x2, high_vowels))),
         # Is root both back and round harmonic?
         back_and_round_harmonic = back_harmonic & round_harmonic,
         # Gradient score for backness/rounding violations
         back_round_gradient = (1 - back_harmonic) + (1 - round_harmonic), 
         dai_values = bigram %in% (c('i i', 'i e', 'e i', 'e e', 'y e', 'y y', 'ɯ ɯ',
                                     'ɯ a', 'a ɯ', 'a a', 'u a', 'u u', 'o a', 'o u')) | str_detect(bigram, '#'))

add_threshold_violations <- function(counts, cor_data) {
  new_df <- tibble()
  
  for (row_idx in 1:nrow(cor_data)) {
    print(row_idx)
    row <- cor_data[row_idx,]
    padded_row <- row %>%
      mutate(vowels = str_c('x ', str_replace(vowels, '(.)(.)', '\\1 \\2'), ' x')) %>% 
      ungroup()
    
    token_bigrams <- padded_row %>%
      unnest_tokens(bigram, vowels, token = 'ngrams', n = 2) %>%
      mutate(bigram = str_replace_all(bigram, 'x', '#')) %>%
      select(bigram)
    
    boolean_scores <- rep(TRUE, 9)
    gradient_scores <- rep(0, 9)
    
    for (t_bigram in token_bigrams$bigram) {
      violation_row <- counts %>%
        filter(bigram == t_bigram)
      if (violation_row$q_10) {
        boolean_scores[1] <- FALSE
        gradient_scores[1] <- gradient_scores[1] + 1
      }
      if (violation_row$q_20) {
        boolean_scores[2] <- FALSE
        gradient_scores[2] <- gradient_scores[2] + 1
      }
      if (violation_row$q_30) {
        boolean_scores[3] <- FALSE
        gradient_scores[3] <- gradient_scores[3] + 1
      }
      if (violation_row$q_40) {
        boolean_scores[4] <- FALSE
        gradient_scores[4] <- gradient_scores[4] + 1
      }
      if (violation_row$q_50) {
        boolean_scores[5] <- FALSE
        gradient_scores[5] <- gradient_scores[5] + 1
      }
      if (violation_row$q_60) {
        boolean_scores[6] <- FALSE
        gradient_scores[6] <- gradient_scores[6] + 1
      }
      if (violation_row$q_70) {
        boolean_scores[7] <- FALSE
        gradient_scores[7] <- gradient_scores[7] + 1
      }
      if (violation_row$q_80) {
        boolean_scores[8] <- FALSE
        gradient_scores[8] <- gradient_scores[8] + 1
      }
      if (violation_row$q_90) {
        boolean_scores[9] <- FALSE
        gradient_scores[9] <- gradient_scores[9] + 1
      }
    }
    new_row <- row %>%
      mutate(q_10_harmonic = boolean_scores[1],
             q_10_gradient = gradient_scores[1],
             q_20_harmonic = boolean_scores[2],
             q_20_gradient = gradient_scores[2],
             q_30_harmonic = boolean_scores[3],
             q_30_gradient = gradient_scores[3],
             q_40_harmonic = boolean_scores[4],
             q_40_gradient = gradient_scores[4],
             q_50_harmonic = boolean_scores[5],
             q_50_gradient = gradient_scores[5],
             q_60_harmonic = boolean_scores[6],
             q_60_gradient = gradient_scores[6],
             q_70_harmonic = boolean_scores[7],
             q_70_gradient = gradient_scores[7],
             q_80_harmonic = boolean_scores[8],
             q_80_gradient = gradient_scores[8],
             q_90_harmonic = boolean_scores[9],
             q_90_gradient = gradient_scores[9])
    new_df <- rbind(new_df, new_row)
  }
  return(new_df)
}

cor_data <- add_threshold_violations(counts, cor_data)

hw_data <- read_tsv('UCLAPhonotacticLearner/turkish_test/output/blickTestResults.txt', col_names = FALSE) %>%
  select(X1, X2) %>%
  transmute(vowels = str_replace(X1, ' ', ''),
            hw_score = as.numeric(X2))

cor_data_hw <- inner_join(cor_data, hw_data, by='vowels')

cor(cor_data_hw$z_mean_response, -cor_data_hw$hw_score, method='pearson')
cor(cor_data_hw$z_mean_response, -cor_data_hw$hw_score, method='kendall')
cor(cor_data_hw$z_mean_response, -cor_data_hw$hw_score, method='spearman')

cor(cor_data$z_mean_response, cor_data$z_v_bi_prob_smoothed, method='pearson')
cor(cor_data$z_mean_response, cor_data$z_v_bi_prob_smoothed, method='kendall')
cor(cor_data$z_mean_response, cor_data$z_v_bi_prob_smoothed, method='spearman')

cor(cor_data$z_mean_response, cor_data$back_and_round_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$back_and_round_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$back_and_round_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$back_round_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$back_round_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$back_round_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$back_round_o_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$back_round_o_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$back_round_o_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$back_round_o_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$back_round_o_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$back_round_o_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_10_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_10_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_10_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_10_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_10_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_10_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_20_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_20_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_20_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_20_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_20_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_20_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_30_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_30_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_30_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_30_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_30_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_30_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_40_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_40_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_40_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_40_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_40_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_40_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_50_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_50_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_50_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_50_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_50_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_50_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_60_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_60_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_60_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_60_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_60_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_60_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_70_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_70_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_70_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_70_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_70_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_70_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_80_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_80_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_80_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_80_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_80_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_80_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_90_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_90_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_90_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$q_90_gradient, method='pearson')
cor(cor_data$z_mean_response, cor_data$q_90_gradient, method='kendall')
cor(cor_data$z_mean_response, cor_data$q_90_gradient, method='spearman')

cor(cor_data$z_mean_response, cor_data$dai_values, method='pearson')
cor(cor_data$z_mean_response, cor_data$dai_values, method='kendall')
cor(cor_data$z_mean_response, cor_data$dai_values, method='spearman')

cor(cor_data$z_v_bi_prob_smoothed, cor_data$q_40_harmonic, method='pearson')
cor(cor_data$z_v_bi_prob_smoothed, cor_data$q_40_harmonic, method='kendall')
cor(cor_data$z_v_bi_prob_smoothed, cor_data$q_40_harmonic, method='spearman')

cor(cor_data$z_v_bi_prob_smoothed, cor_data$back_and_round_harmonic, method='pearson')
cor(cor_data$z_v_bi_prob_smoothed, cor_data$back_and_round_harmonic, method='kendall')
cor(cor_data$z_v_bi_prob_smoothed, cor_data$back_and_round_harmonic, method='spearman')

#########
# PLOTS #
#########

stats_data %>%
  ggplot(aes(x=back_round_gradient, y=response, group=back_round_gradient)) + 
  geom_boxplot()

stats_data %>%
  ggplot(aes(x=back_and_round_harmonic, y=response, group=back_and_round_harmonic)) + 
  geom_boxplot()

stats_data %>%
  ggplot(aes(x=back_harmonic, y=response, group=back_harmonic)) + 
  geom_boxplot()

stats_data %>%
  ggplot(aes(x=round_harmonic, y=response, group=round_harmonic)) + 
  geom_boxplot()

plot_data <- cor_data %>% 
  group_by(vowels, z_v_bi_prob_smoothed) %>%
  summarize(mean_response = mean(z_response),
            mean_v_pred = mean(model_v_pred),
            mean_harmonic_pred = mean(model_harmonic_pred))

plot_data %>%
  ggplot(aes(x=z_v_bi_prob_smoothed, y=mean_response, label=vowels)) + 
  geom_point() + 
  geom_label()

plot_data %>%
  ggplot(aes(x=mean_response, y=mean_harmonic_pred)) + 
  geom_point()


task_responses <- task_responses %>%
  mutate(v_group = case_when(v_bi_prob_smoothed > mean(v_bi_prob_smoothed) + sd(v_bi_prob_smoothed) ~ 'high',
                             v_bi_prob_smoothed < mean(v_bi_prob_smoothed) - sd(v_bi_prob_smoothed) ~ 'low',
                             TRUE ~ 'average'))

task_responses %>%
  arrange(-response) %>%
  ggplot(aes(x=fct_reorder(vowels, response), y=response, fill=back_harmonic)) +
  geom_boxplot()

task_responses %>%
  arrange(-response) %>%
  ggplot(aes(x=back_harmonic, y=response, fill=back_harmonic)) +
  geom_boxplot()

task_responses %>%
  ggplot(aes(x=uni_prob, y=response)) +
  geom_point(aes(color=back_harmonic, size=3)) +
  geom_smooth(method='lm')

task_responses %>%
  ggplot(aes(x=bi_prob_smoothed, y=response)) +
  geom_point(aes(color=back_harmonic), size=3) +
  geom_smooth(method='lm')

task_responses %>%
  arrange(-response) %>%
  ggplot(aes(x=height_harmonic, y=response, fill=height_harmonic)) +
  geom_boxplot()

task_responses %>%
  arrange(-response) %>%
  ggplot(aes(x=round_harmonic, y=response, fill=round_harmonic)) +
  geom_boxplot()

task_responses %>%
  ggplot(aes(x=v_uni_prob, y=response)) +
  geom_point(aes(color=back_harmonic, size=3)) +
  geom_smooth(method='lm')

task_responses %>%
  ggplot(aes(x=v_bi_prob_smoothed, y=response)) +
  geom_point(aes(color=back_harmonic), size=3) +
  geom_smooth(method='lm')


task_responses %>% 
  group_by(orthography, uni_prob, bi_prob_smoothed, harmonic) %>%
  summarize(response = mean(response)) %>%
  ggplot() +
  aes(x = uni_prob, y = response, group = harmonic, color = harmonic) +
  geom_point(color = "grey", alpha = .7) +
  geom_smooth(method = "lm")

task_responses %>% 
  group_by(orthography, uni_prob, bi_prob_smoothed, harmonic) %>%
  summarize(response = mean(response)) %>%
  ggplot() +
  aes(x = bi_prob_smoothed, y = response, group = harmonic, color = harmonic) +
  geom_point(color = "grey", alpha = .7) +
  geom_smooth(method = "lm")

cor_data %>%
  ggplot(aes(x=z_v_bi_prob_smoothed, y=z_mean_response)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_label(aes(label=vowels)) +   
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  xlab("Bigram probability (z-score)") +
  ylab("Mean response (z-score)")
ggsave("figs/bigram_prob_plot.png")

cor_data %>%
  ggplot(aes(x=back_and_round_harmonic, y=z_mean_response, fill=back_and_round_harmonic)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  #geom_smooth(method = 'lm') +
  #geom_label(aes(label=vowels)) +   
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  xlab("Accepted?") +
  ylab("Mean response (z-score)") +
  guides(fill="none") 
ggsave("figs/categorical_prob_plot.png")

cor_data %>%
  ggplot(aes(x=q_40_harmonic, y=z_mean_response, fill=q_40_harmonic)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  #geom_smooth(method = 'lm') +
  #geom_label(aes(label=vowels)) +   
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  xlab("Accepted?") +
  ylab("Mean response (z-score)") +
  guides(fill="none") 
ggsave("figs/threshold_prob_plot.png")

cor_data %>%
  ggplot(aes(x=dai_values, y=z_mean_response, fill=dai_values)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  #geom_smooth(method = 'lm') +
  #geom_label(aes(label=vowels)) +   
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  xlab("Accepted?") +
  ylab("Mean response (z-score)") +
  guides(fill="none") 
ggsave("figs/dai_prob_plot.png")


cor_data %>%
  ggplot(aes(x=as.factor(back_round_gradient), y=z_mean_response, fill=back_and_round_harmonic)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  #geom_smooth(method = 'lm') +
  #geom_label(aes(label=vowels)) +   
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20)) +
  xlab("Cost") +
  ylab("Mean response (z-score)") +
  guides(fill="none") 
ggsave("figs/cost_prob_plot.png", units = 'in', width=6, height=4)

counts %>%
  ggplot(aes(x=x1, y=x2)) +
  geom_tile(aes(fill = !q_40)) +
  geom_text(aes(label = !q_40)) +
  xlab("First segment") +
  ylab("Second segment") +
  scale_fill_viridis(option = "D", name = "Legal bigram?", discrete = TRUE) +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20))
ggsave("figs/boolean_threshold_constraints.png", units = 'in', width=8, height=4)

counts %>%
  ggplot(aes(x=x1, y=x2)) +
  geom_tile(aes(fill = back_and_round_harmonic)) +
  geom_text(aes(label = back_and_round_harmonic)) +
  xlab("First segment") +
  ylab("Second segment") +
  scale_fill_viridis(option = "D", name = "Legal bigram?", discrete = TRUE) +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20))
ggsave("figs/boolean_harmonic_constraints.png", units = 'in', width=8, height=4)

counts %>%
  ggplot(aes(x=x1, y=x2)) +
  geom_tile(aes(fill = dai_values)) +
  geom_text(aes(label = dai_values)) +
  xlab("First segment") +
  ylab("Second segment") +
  scale_fill_viridis(option = "D", name = "Legal bigram?", discrete = TRUE) +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20))
ggsave("figs/dai_constraints.png", units = 'in', width=8, height=4)


counts %>%
  ggplot(aes(x=x1, y=x2)) +
  geom_tile(aes(fill = c_prob)) +
  geom_text(aes(label = round(c_prob, 3))) +
  xlab("First segment") +
  ylab("Second segment") +
  scale_fill_viridis(option = "D", name = "P(x2|x1)") +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20))
ggsave("figs/probability_constraints.png", units = 'in', width=8, height=4)

counts %>%
  ggplot(aes(x=x1, y=x2)) +
  geom_tile(aes(fill = as.factor(back_round_gradient))) +
  geom_text(aes(label = back_round_gradient)) +
  xlab("First segment") +
  ylab("Second segment") +
  scale_fill_viridis(option = "D", name = "Constraint value", discrete = TRUE) +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20))
ggsave("figs/gradient_harmonic_constraints.png")


counts %>%
  ggplot(aes(x=x1, y=x2)) +
  geom_tile(aes(fill = as.factor(back_round_gradient))) +
  geom_text(aes(label = back_round_gradient)) +
  xlab("First segment") +
  ylab("Second segment") +
  scale_fill_viridis(option = "D", name = "Constraint value", discrete = TRUE) +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20))
ggsave("figs/gradient_harmonic_constraints.png")

counts %>%
  ggplot(aes(x=x1, y=x2)) +
  geom_tile(aes(fill = as.factor(back_round_gradient))) +
  geom_text(aes(label = back_round_gradient)) +
  xlab("First segment") +
  ylab("Second segment") +
  scale_fill_viridis(option = "D", name = "Constraint value", discrete = TRUE) +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20))
ggsave("figs/gradient_harmonic_constraints.png")
