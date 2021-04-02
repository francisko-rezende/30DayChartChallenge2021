list_of_packages <-
  c("tidyverse", "here", "janitor", "showtext", "ggtext")
new_packages <-
  list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(new_packages))
  install.packages(new_packages)

library(tidyverse)
library(here)
library(janitor)
library(showtext)
library(ggtext)

water <-
  read_csv(here::here("data", "SYB63_315_202009_Water and Sanitation Services.csv"),
           skip = 1) %>%
  janitor::clean_names() %>%
  rename(area = x2) %>%
  filter(
    str_detect(area, "Total"),
    series == "Safely managed drinking water sources, total (Proportion of population with access)"
  ) %>%
  mutate(complement = 100 - value) %>%
  pivot_longer(
    cols = c(value, complement),
    names_to = "vars",
    values_to = "values"
  ) %>%
  mutate(vars = factor(vars),
         labels = str_c(round(values, 2)), "%", sep = " ")

labels <- water %>% 
  select(year, vars, values) %>% 
  filter(vars == "value") %>% 
  mutate(labels = str_c(round(values, 2), "%", sep = " "),
         year = factor(year),
         values = values / 100)

font_add_google("Merriweather Sans", "Merriweather Sans")
font_add_google("Merriweather", "Merriweather")

showtext_auto()

levels(water$vars)

blue_ncs <- "#2E86AB"


ggplot(water, aes(x = factor(year), y = values, fill = vars)) +
  geom_col(position = "fill",
           color = "black",
           alpha = 0.5) +
  geom_text(data = labels, aes(y = values,label = labels), nudge_y = 0.05, size = 15, family = "Merriweather", color = "#4D4D4D") +
  labs(
    title = "The Glass is (more than) Half Full",
    subtitle = "The proportion of the population <span style = 'color:#2E86AB;'> with access </span> to safely managed drinking<br>water sources has been increasing since 2005",
    x = "",
    y = "",
    caption = "Francisko de Moraes Rezende (@francisko_r) | Data: World Health Organization (WHO) and United Nations Children's Fund (UNICEF)"
  ) +
  scale_fill_manual(values = c(NA, blue_ncs)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 60, family = "Merriweather"),
    plot.title = element_markdown(family = "Merriweather Sans", face = "bold"),
    plot.subtitle = element_markdown(
      family = "Merriweather",
      size = 50,
      lineheight = 0.3
    ),
    plot.background = element_rect(fill = "#FBFADA"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.caption = element_markdown(color = "#4D4D4D",
                                    size = 25)
  )

ggsave(here::here("plots", "day1.png"), width = 9.34, height = 5.75, units = "in")
