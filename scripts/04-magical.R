list_of_packages <- c("tidyverse", "here", "janitor", "extrafont", "ggtext", "ggimage")
new_packages <-
  list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(new_packages))
  install.packages(new_packages)

library(tidyverse)
library(here)
library(janitor)
# devtools::install_git("https://git.rud.is/hrbrmstr/waffle.git")
library(waffle)
library(extrafont)
library(ggtext) 
library(ggimage)


hp <-
  readr::read_csv2(here::here("data", "Characters.csv")) %>%
  janitor::clean_names() %>%
  dplyr::count(house) %>%
  dplyr::arrange(n) %>%
  tidyr::drop_na() %>% 
  dplyr::filter(!(house %in% c("Durmstrang Institute", "Beauxbatons Academy of Magic"))) %>% 
  dplyr::mutate(house = factor(house),
                house = forcats::fct_reorder(house, n))
  
extrafont::font_import()

# extrafont::loadfonts(quiet = TRUE)

house_cols <- 
  c(
      Hufflepuff = "#e9b102",               
      Ravenclaw = "#355188",
      Slytherin = "#19672b",
      Gryffindor = "#8e0000"
  )

hp %>%
  ggplot(aes(
    values = n,
    fill = house
  )) +
  geom_waffle(
    n_rows = 5,
    flip = TRUE,
    make_proportional = FALSE,
    color = "black",
    size = 0.8
  ) +
  scale_y_continuous(
    labels = function(x)
      x * 5,
    expand = c(0, 0),
    name = "Number of characters"
  ) +
  facet_grid(. ~ house) +
  scale_fill_manual(values = house_cols) +
  theme_enhance_waffle() +
  theme_minimal() +
  labs(
    title = "Houses by their numbers",
    subtitle = "The number of Harry Potter  characters that belong to <span style='color: #A27A01'>Hufflepuff</span>, <span style='color: #355188'>Ravenclaw</span>, <span style='color: #19672b'>Slytherin</span>, and <span style='color: #8e0000'>Gryffindor</span>",
    caption = "plot by Francisko de Moraes Rezende (@francisko_r) | Data: Harry Potter dataset uploaded to Kaggle by Gulsah Demiryurek"
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "none",
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.y = element_line(size = 0.3),
    text = element_text(family = "Aquiline",
                        size = 25),
    plot.title = element_text(family = "Inkpot"),
    plot.subtitle = element_markdown(),
    plot.caption = element_text(size = 13),
    strip.text = element_blank()
    ) -> gg

ggbackground(gg, background = here::here("img", "parchment_bg.jpg")) 
  
ggsave(here::here("plots", "day4.png"), width = 15.99, height = 7.99, units = "in")
