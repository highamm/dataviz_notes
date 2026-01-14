library(tidyverse)
popularity_df <- read_csv("https://raw.githubusercontent.com/highamm/ds234_quarto/main/data_online/geom_popularity.csv")

popularity_top25 <- popularity_df |>
  arrange(desc(github_hits)) |>
  slice(1:25) |>
  mutate(geom_name = fct_reorder(geom_name, github_hits))

ggplot(data = popularity_top25, aes(x = github_hits, y = geom_name)) +
  geom_segment(aes(x = 0, xend = github_hits, yend = geom_name),
               colour = "skyblue1") +
  geom_point(colour = "skyblue4") +
  theme_minimal()
