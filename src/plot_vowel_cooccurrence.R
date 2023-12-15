library(tidyverse)

citation_corpus <- read_csv('data/vowel_only_citation.csv', col_names = 'word')
citation_bigrams <- citation_corpus %>%
  unnest_tokens(bigram, word, token = 'ngrams', n = 2) %>%
  filter(!is.na(bigram))

counts <- citation_bigrams %>%
  group_by(bigram) %>%
  summarize(count = n()) %>%
  separate(bigram, into = c("v1", "v2"), sep = " ", remove = FALSE) %>%
  group_by(v1) %>%
  mutate(c_prob = count / sum(count),
         attested = c_prob > 0) %>%
  ungroup() %>%
  mutate(j_prob = count / sum(count),
         v1 = fct_relevel(v1, c('ɯ', 'u', 'a', 'o', 'i', 'y', 'e', 'ø')),
         v2 = fct_relevel(v2, c('ɯ', 'u', 'a', 'o', 'i', 'y', 'e', 'ø')))

counts %>%
  ggplot(aes(x=v1, y=v2)) +
  geom_tile(aes(fill = attested)) +
  geom_text(aes(label = attested)) +
  xlab("First vowel") +
  ylab("Second vowel") +
  scale_fill_viridis(option = "D", name = "Attested?", 
                     discrete=TRUE, direction=-1) +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20))
  
ggsave('figs/turkish_attested.png')

counts %>%
  ggplot(aes(x=v1, y=v2)) +
  geom_tile(aes(fill = j_prob)) +
  geom_text(aes(label = round(j_prob, 3))) +
  xlab("First vowel") +
  ylab("Second vowel") +
  scale_fill_viridis(option = "D", name = "p(V1, V2)") +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20))

ggsave('figs/turkish_joint_probs.png')

counts %>%
  ggplot(aes(x=v1, y=v2)) +
  geom_tile(aes(fill = c_prob)) +
  geom_text(aes(label = round(c_prob, 3))) +
  xlab("First vowel") +
  ylab("Second vowel") +
  scale_fill_viridis(option = "D", name = "p(V2|V1)") +
  theme_minimal() +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=20),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20))
  
ggsave("figs/turkish_cond_probs.png")
