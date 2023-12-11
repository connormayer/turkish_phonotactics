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

task_responses <- task_responses %>%
  mutate(harmonic = !(str_detect(vowels, front_vowels) & str_detect(vowels, back_vowels)),
         has_j = str_detect(orthography, 'j'))

task_responses %>%
  arrange(-response) %>%
  ggplot(aes(x=fct_reorder(vowels, response), y=response, fill=harmonic)) +
  geom_boxplot()

task_responses %>%
  arrange(-response) %>%
  ggplot(aes(x=harmonic, y=response, fill=harmonic)) +
  geom_boxplot()

task_responses %>%
  ggplot(aes(x=uni_prob, y=response)) +
  geom_point(aes(color=harmonic, size=3)) +
  geom_smooth(method='lm')

task_responses %>%
  ggplot(aes(x=bi_prob_smoothed, y=response)) +
  geom_point(aes(color=harmonic), size=3) +
  geom_smooth(method='lm')

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
  geom_point(aes(color=harmonic, size=3)) +
  geom_smooth(method='lm')

task_responses %>%
  ggplot(aes(x=v_bi_prob_smoothed, y=response)) +
  geom_point(aes(color=harmonic), size=3) +
  geom_smooth(method='lm')

model_v_bigram <- lmer(response ~ scale(v_bi_prob_smoothed) + (1|ID) + (1|orthography), data=task_responses)
summary(model_v_bigram)

model_harmonic <- lmer(response ~ harmonic + (1|ID) + (1|orthography), data=task_responses)
summary(model_harmonic)

model_both <- lmer(response ~ harmonic * scale(v_bi_prob_smoothed) + (1|ID) + (1|orthography), data=task_responses)
summary(model_both)

anova(model_v_bigram, model_harmonic, model_both)

model_full <- lmer(response ~ harmonic * scale(uni_prob) * scale(bi_prob_smoothed) + (1|ID) + (1|orthography), data=task_responses)
summary(model_full)


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
