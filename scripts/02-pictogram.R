list_of_packages <-
  c("waffle", "tidyverse", "here", "janitor", "extrafont", "ggtext")
new_packages <-
  list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(new_packages))
  install.packages(new_packages)

library(waffle)
library(tidyverse)
library(here)
library(janitor)
library(extrafont)
library(ggtext)

library(tidytuesdayR)

# devtools::install_github("thebioengineer/tidytuesdayR")

beer <- tidytuesdayR::tt_load(2020, week = 14)

brewer_n <-
  beer$brewer_size %>%
  dplyr::filter(brewer_size %in% c("Under 1 Barrel")) %>%
  group_by(year) %>%
  summarise(n = sum(n_of_brewers))

micro_brewer <-
  beer$brewer_size %>%
  mutate(is_micro = if_else(brewer_size == "Under 1 Barrel", "Yes", "No")) %>%
  group_by(year, is_micro) %>%
  summarise(n = sum(n_of_brewers)) %>%
  filter(year == 2019) %>%
  ungroup() %>%
  mutate(n_2 = c(8, 2))

lager <- "#EF9D06"
dark <- "#331D06"

extrafont::loadfonts(quiet = TRUE)


micro_brewer %>%
  ggplot(aes(values = n, label = is_micro)) +
  geom_pictogram(
    n_rows = 10,
    aes(color = is_micro),
    flip = TRUE,
    make_proportional = TRUE,
    size = 9
  ) +
  scale_label_pictogram(values = "beer", name = "Produces less than a barrel?") +
  scale_color_manual(values = c(lager, dark), name = "Produces less than a barrel?") +
  labs(title = "Micro In Production And In Number",
       subtitle = "In 2019, two out of 100 brewers had small productions in the U.S",
       caption = "Vizualisation by Francisko de Moraes Rezende (@francisko_r) | Data: Alcohol and Tobacco Tax and Trade Bureau (TTB)") +
  theme_enhance_waffle() +
  theme_void() +
  theme(
    text = element_text(family = "Roboto Slab", size = 16),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.title = element_text(family = "BERNIER distressed"),
    plot.subtitle = element_text(size = 14),
    plot.background = element_rect(fill = "#D7D1DC"),
    legend.position = "bottom",
    plot.caption = element_text(size = 9)
  )

ggsave(
  here::here("plots", "day2.png"),
  width = 9.34,
  height = 5.75,
  units = "in"
)