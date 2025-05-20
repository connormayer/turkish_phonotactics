library(tidyverse)

# This script does some spreadsheet formatting to set groups up properly
# in Gorilla.

# Run this after synthesizing speech
df <- read_csv('data/stimuli_with_filenames.csv')
df$display <- 'Task'
df$randomise_trials <- 1

for (g in unique(df$group)) {
  group_df <- df %>%
    select(-...1) %>%
    filter(group == g) %>%
    add_row(display='Prompt', .before=1) %>%
    write_excel_csv(str_glue('data/stimuli_group_{g}.csv'))
}