df <- read.csv(file.choose(), header=T)


library(tidyverse)
library(psych)


# Define columns
green_cols <- c("seq_com_eng", "seq_com_span")


yellow_cols <- c(
  "exp_eng", "exp_spa",
  "conf_eng", "conf_spa",
  "self_english_pre", "self_spanish_pre",
  "self_english_post", "self_spanish_post"
)


# Run correlation test
corr_results <- corr.test(df[green_cols], df[yellow_cols], method = "pearson")


# Extract correlation and p-value matrices
r_mat <- corr_results$r
p_mat <- corr_results$p


# Build output table
tidy_corr <- r_mat %>%
  as.data.frame() %>%
  rownames_to_column("green_var") %>%
  pivot_longer(-green_var, names_to = "yellow_var", values_to = "r") %>%
  mutate(
    p = as.vector(p_mat)
  ) %>%
  arrange(green_var, yellow_var)


tidy_corr


library(tidyverse)


# Ensure factors keep the preferred order
tidy_corr <- tidy_corr %>%
  mutate(
    yellow_var = factor(yellow_var, levels = unique(yellow_var)),
    green_var  = factor(green_var,  levels = unique(green_var))
  )


library(tidyverse)


# Desired order for Y axis (Spanish block, then English block)
yellow_order <- c(
  "exp_spa",
  "conf_spa",
  "self_spanish_post",
  "self_spanish_pre",
  "exp_eng",
  "conf_eng",
  "self_english_post",
  "self_english_pre"
)


tidy_corr <- tidy_corr %>%
  mutate(
    yellow_var = factor(yellow_var, levels = yellow_order),
    green_var  = factor(green_var, levels = unique(green_var))
  )


# Heatmap with reordered Y axis


library(stringr)
ggplot(tidy_corr, aes(x = green_var, y = yellow_var, fill = r)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    name = "Pearson's r",
    limits = c(-1, 1)
  ) +
  scale_x_discrete(labels = c(
    seq_com_eng = "English",
    seq_com_span = "Spanish"
  )) +
  scale_y_discrete(
    labels = function(labs) str_wrap(c(
      exp_spa = "Exposure (Spa)",
      conf_spa = "Overall confidence (Spa)",
      self_spanish_pre = "Pre dx listening confidence (Spa)",
      self_spanish_post = "Post dx listening confidence (Spa)",
      exp_eng = "Exposure (Eng)",
      conf_eng = "Overall confidence (Eng)",
      self_english_pre = "Pre dx listening confidence (Eng)",
      self_english_post = "Post dx listening confidence (Eng)"
    )[labs], width = 18)
  ) +
  labs(
    x = "Command Scores",
    y = "Language Measures"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(lineheight = 0.9),
    panel.grid = element_blank()
  )
