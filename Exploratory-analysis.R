library(tidyverse)
library(readxl)


# First getting data from Excel sheet
df <- read_excel("all_combined.xlsx")


# Converting language to lifestyle factor
df$language <- factor(df$language)


# Listing lifestyle variables
luq_vars <- c("exp_score", "conf_score", "self_pre_score", "self_post_score")


# Correlation across Spanish and English combined

for (v in luq_vars) {
  cat("\n--- WAB vs", v, "---\n")
  print(cor.test(df$seq_com, df[[v]], method = "pearson"))
}


# Now splitting up Spanish and English

# English rows
eng <- df %>% filter(language == "English")


# Spanish rows
spa <- df %>% filter(language == "Spanish")

# English-only correlations

for (v in luq_vars) {
  cat("\n--- English WAB vs", v, "---\n")
  print(cor.test(eng$seq_com, eng[[v]], method = "pearson"))
}


# Spanish-only correlations


for (v in luq_vars) {
  cat("\n--- Spanish WAB vs", v, "---\n")
  print(cor.test(spa$seq_com, spa[[v]], method = "pearson"))
}
