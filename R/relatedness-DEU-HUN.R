here::i_am("R/relatedness-DEU-HUN.R")
library(pacman)
p_load(here)
p_load(data.table)
p_load(dplyr)
p_load(tidyr)
p_load(ggplot2)
p_load(stringr)
p_load(haven)

hun_data_raw <- fread(
  file = here("data/SR_yearly.csv"), colClasses = c(
    "year"="integer",
    "Target08"="character",
    "Source08"="character",
    "lab_flo_ind"="integer",
    "Fi"="double",
    "Fj"="double",
    "Ftotal"="integer",
    "Rij"="double",
    "SR"="double")
) %>% 
  mutate(
    Target08 = str_pad(string = Target08, width = 4, side = "left", pad = "0"),
    Source08 = str_pad(string = Source08, width = 4, side = "left", pad = "0")
  )

hun_data <- hun_data_raw %>% 
  filter(year == 2008) %>% 
  rename(
    SRt_hun = SR,
    destination = Target08,
    origin = Source08
  ) %>% 
  select(c("destination", "origin", "SRt_hun"))

deu_data <- read_dta(here("data/wz08.dta")) %>% 
  mutate(
    origin = str_pad(string = wz08_1, width = 4, side = "left", pad = "0"),
    destination = str_pad(string = wz08_2, width = 4, side = "left", pad = "0")
  ) %>% 
  select(-c("wz08_1", "wz08_2")) %>% 
  rename(SRt_deu = SRt)

hun_deu <- full_join(
  x = deu_data, y = hun_data, 
  by = c("origin", "destination")
)

# check cases for incomplete data:
only_hun_data <- hun_deu %>% 
  filter(
    is.na(SRt_deu), !is.na(SRt_hun)
  )

only_deu_data <- hun_deu %>% 
  filter(
    !is.na(SRt_deu), is.na(SRt_hun)
  )

both_data <- inner_join(
  x = deu_data, y = hun_data, 
  by = c("origin", "destination")
)

set.seed(123)
p_rel <- seq(-1, 1, length.out=nrow(both_data))
p_rel_y <- p_rel + rnorm(n = length(p_rel), mean = 0, sd = 0.15)
p_rel_y[p_rel_y < (-1)] <- -1
p_rel_y[p_rel_y > 1] <- 1

perfect_relationship_plot <- ggplot(
    data = tibble(p_rel_y, p_rel), 
    mapping = aes(x=p_rel, y=p_rel_y)
  ) +
  geom_point(alpha = 0.25, color = "black", fill="grey", shape = 21) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  labs(
    x = "German skill relatedness", 
    y = "Hungarian skill relatedness", 
    title = "If DEU and HUN were similar..."
  ) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

actual_relationship <- ggplot(data = both_data, aes(x = SRt_deu, y = SRt_hun)) +
  geom_point(alpha = 0.25, color = "black", fill="grey", shape = 21) +
  geom_abline(slope = 1, intercept = 0, color = "blue") +
  labs(
    x = "German skill relatedness", 
    y = "Hungarian skill relatedness", 
    title = "This is how it actually looks like"
  ) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  plot = perfect_relationship_plot, 
  filename = here("output/artificial_relationship.png"), 
  width = 5, height = 2)

ggsave(
  plot = actual_relationship, 
  filename = here("output/actual_relationship.png"), 
  width = 5, height = 2)
