library(lme4)
library(tidyverse)

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

# model_s <- lmer(response ~ scale(uni_prob) * scale(bi_prob_smoothed), data=task_responses)

front_vowels <- "[iyeø]" 
back_vowels <- "[ɯuao]"
high_vowels <- "[iyɯu]"
low_vowels <- "[eøao]"
round_vowels <- "[yøuo]"
unround_vowels <- "[ieɯa]"

# TODO: Add restriction to initial O/o
# TODO: Add labial attraction
task_responses <- task_responses %>%
  mutate(v1 = substr(vowels, 1, 1),
         v2 = substr(vowels, 2, 2),
         back_harmonic = !(str_detect(vowels, front_vowels) & str_detect(vowels, back_vowels)),
         height_harmonic = !(str_detect(vowels, high_vowels) & str_detect(vowels, low_vowels)),
         oO_violation = v1 == 'o' | v1 == 'ø',
         round_harmonic = !(
            (str_detect(v1, round_vowels) & str_detect(v2, unround_vowels) & str_detect(v2, high_vowels)) |
            (str_detect(v1, unround_vowels) & str_detect(v2, round_vowels) & str_detect(v2, high_vowels))),
         la_harmonic_local_final = !(v2 == 'ɯ' & str_detect(substr(orthography, 3, 3), "[vfpmb]")),
         la_harmonic_local_total = !str_detect(orthography, "[vfpmb][yu]"),
         la_harmonic_classic = !(v1 == 'a' & v2 == 'ɯ' & str_detect(substr(orthography, 3, 3), "[vfpmb]")),
         has_j = str_detect(orthography, 'j'),
         back_and_round_harmonic = back_harmonic & round_harmonic,
         back_round_gradient = (1 - back_harmonic) + (1 - round_harmonic),
         back_round_o_harmonic = back_harmonic & round_harmonic & oO_violation,
         back_round_o_gradient = (1 -back_harmonic) & (1 - round_harmonic) &  (1 - oO_violation),
         back_round_la_harmonic = back_harmonic & round_harmonic & la_harmonic_classic,
        back_round_la_gradient = (1 -back_harmonic) & (1 - round_harmonic) &  (1 - la_harmonic_classic))

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

vowel_scores <- read_csv("data/vowel_scores_citation.csv") %>%
  mutate(vowels = str_replace(word, ' ', ''),
         v_uni_prob = uni_prob,
         v_bi_prob_smoothed = bi_prob_smoothed,
         v_pos_uni_score_smoothed = pos_uni_score_smoothed,
         v_pos_bi_score_smoothed = pos_bi_score_smoothed) %>%
  select(vowels, v_uni_prob, v_bi_prob_smoothed, v_pos_uni_score_smoothed, v_pos_bi_score_smoothed)

task_responses <- task_responses %>%
  inner_join(vowel_scores, by='vowels')

task_responses %>%
  ggplot(aes(x=v_uni_prob, y=response)) +
  geom_point(aes(color=back_harmonic, size=3)) +
  geom_smooth(method='lm')

task_responses %>%
  ggplot(aes(x=v_bi_prob_smoothed, y=response)) +
  geom_point(aes(color=back_harmonic), size=3) +
  geom_smooth(method='lm')

# TODO: Neighborhood density
stats_data <- task_responses %>%
  mutate(z_v_bi_prob_smoothed = as.vector(scale(v_bi_prob_smoothed)),
         z_v_uni_prob = scale(v_uni_prob))
# 
# model_v_bigram <- lmer(
#   response ~ z_v_bi_prob_smoothed + (1 + z_v_bi_prob_smoothed|ID) + (1|orthography), 
#   data=stats_data,
#   control=lmerControl(optimizer="bobyqa")
#   )
# summary(model_v_bigram)
# 
# model_baseline <- lmer(
#   response ~ 1 + (1|ID) + (1|orthography), 
#   data=stats_data,
#   control=lmerControl(optimizer="bobyqa"))
# summary(model_baseline)
# 
# model_harmonic <- lmer(
#   response ~ back_harmonic + height_harmonic + round_harmonic + la_harmonic_local_final + (1 + back_harmonic + round_harmonic|ID) + (1|orthography), 
#   data=stats_data,
#   control=lmerControl(optimizer="bobyqa"))
# summary(model_harmonic)
# 
# model_harmonic_2 <- lmer(
#   response ~ back_and_round_harmonic + (1 + back_and_round_harmonic|ID) + (1|orthography), 
#   data=stats_data,
#   control=lmerControl(optimizer="bobyqa"))
# summary(model_harmonic_2)
# 
# model_harmonic_3 <- lmer(
#   response ~ back_round_gradient + (1 + back_round_gradient|ID) + (1|orthography), 
#   data=stats_data,
#   control=lmerControl(optimizer="bobyqa"))
# summary(model_harmonic_3)

# model_harmonic_2 <- lmer(
#   response ~ back_harmonic + height_harmonic + round_harmonic + la_harmonic_local_total + (1 + back_harmonic + round_harmonic|ID) + (1|orthography), 
#   data=stats_data,
#   control=lmerControl(optimizer="bobyqa"))
# summary(model_harmonic_2)
# 
# model_harmonic_3 <- lmer(
#   response ~ back_harmonic + height_harmonic + round_harmonic + la_harmonic_classic + (1 + back_harmonic + round_harmonic|ID) + (1|orthography), 
#   data=stats_data,
#   control=lmerControl(optimizer="bobyqa"))
# summary(model_harmonic_3)

# model_both <- lmer(
#   response ~ z_v_bi_prob_smoothed + back_harmonic + height_harmonic + round_harmonic + (1 + back_harmonic + round_harmonic + z_v_bi_prob_smoothed|ID) + (1|orthography), 
#   data=stats_data,
#   control=lmerControl(optimizer="bobyqa"))
# summary(model_both)
# 
# model_both_2 <- lmer(
#   response ~ z_v_bi_prob_smoothed + back_and_round_harmonic + (1 + back_and_round_harmonic + z_v_bi_prob_smoothed|ID) + (1|orthography), 
#   data=stats_data,
#   control=lmerControl(optimizer="bobyqa"))
# summary(model_both_2)
# 
# model_both_3 <- lmer(
#   response ~ z_v_bi_prob_smoothed + back_round_gradient + (1 + back_round_gradient + z_v_bi_prob_smoothed|ID) + (1|orthography), 
#   data=stats_data,
#   control=lmerControl(optimizer="bobyqa"))
# summary(model_both_3)
# 
# anova(model_v_bigram, model_both)
# anova(model_harmonic, model_both)
# 
# anova(model_v_bigram, model_both_2)
# anova(model_harmonic_2, model_both_2)
# 
# anova(model_v_bigram, model_both_3)
# anova(model_harmonic_3, model_both_3)

cor_data <- stats_data %>% 
  group_by(ID) %>% 
  mutate(z_response = as.vector(scale(response))) %>%
  ungroup() %>%
  group_by(vowels, z_v_bi_prob_smoothed, back_harmonic, round_harmonic, 
           back_and_round_harmonic, back_round_gradient, back_round_o_gradient, 
           back_round_o_harmonic) %>%
  summarize(z_mean_response = mean(z_response),
            mean_response = mean(response))

cor(cor_data$z_mean_response, cor_data$z_v_bi_prob_smoothed, method='pearson')
cor(cor_data$z_mean_response, cor_data$z_v_bi_prob_smoothed, method='kendall')
cor(cor_data$z_mean_response, cor_data$z_v_bi_prob_smoothed, method='spearman')

cor(cor_data$z_mean_response, cor_data$back_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$back_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$back_harmonic, method='spearman')

cor(cor_data$z_mean_response, cor_data$round_harmonic, method='pearson')
cor(cor_data$z_mean_response, cor_data$round_harmonic, method='kendall')
cor(cor_data$z_mean_response, cor_data$round_harmonic, method='spearman')

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

stats_data$model_v_pred <- fitted(model_v_bigram)
stats_data$model_harmonic_pred <- fitted(model_harmonic_2)

stats_data %>%
  ggplot(aes(x=response, y=model_v_pred)) + 
           geom_point()

stats_data %>%
  ggplot(aes(x=response, y=model_harmonic_pred)) + 
  geom_point()

stats_data %>%
  ggplot(aes(x=response, y=model_v_pred)) + 
  geom_point()

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
ggsave("figs/cost_prob_plot.png")
