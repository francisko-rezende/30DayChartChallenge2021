list_of_packages <- c("tidyverse", "here", "scales", "extrafont")
new_packages <-
  list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(new_packages))
  install.packages(new_packages)


library(tidyverse)
library(here)
library(scales)
library(extrafont)


covid <-
  read_csv(here::here("data", "owid-covid-data.csv")) %>%
  filter(!(location %in% c("International", "World")))


covid <-
  covid %>%
  mutate(iso_length = str_length(iso_code)) %>%
  select(iso_length, everything()) %>%
  filter(iso_length == 3)

covid <-
  covid %>%
  mutate(is_brazil = if_else(location == "Brazil", "y", "n")) # %>%
# filter(date > "2021-01-01")

bra <-
  covid %>%
  filter(is_brazil == "y")

cols_theme <-
  c(bkg = "#000c1a",
    txt = "#fafafa",
    line = "#94C9A9")

# extrafont::font_import(paths = here::here("fonts"))

# extrafont::fonts()
# 
# extrafont::loadfonts()

bra %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  group_by(year, month) %>%
  summarise(total_deaths_month = sum(new_deaths)) %>%
  mutate(
    day = 1,
    date = str_c(year, month, day, sep = "-"),
    date = lubridate::ymd(date)
  ) %>%
  filter(month != 4,
         date >= "2020-04-01") %>%
  ggplot(aes(x = date,
             y = total_deaths_month)) +
  labs(title = str_to_title("A steep toll"),
       subtitle = "The number of deaths due to COVID-19 has increased drastically in Brazil, reaching\nmore than 65 000 in March 2021. That is twice as many as in February.",
       y = stringr::str_to_title("number of Deaths"),
       caption = "Plot by Francisko de Moraes Rezende (@francisko_r) | Data: Our World in Data") +
  scale_x_date(breaks = scales::breaks_width("1 month"),
               labels = scales::label_date_short()) +
  scale_y_continuous(labels =  scales::label_number()) +
  geom_line(color = cols_theme["line"]) +
  geom_point(color = cols_theme["line"]) +
  geom_text(aes(label = total_deaths_month, fontface = "bold"), color = cols_theme["line"], nudge_y = 2500) +
  theme_minimal(base_family = "Open Sans", base_size = 16) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(vjust = 2.5),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = cols_theme["bkg"]),
    text = element_text(color = cols_theme["txt"]),
    plot.caption = element_text(size = 9),
    axis.text = element_text(color = cols_theme["txt"]),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = .25, color = cols_theme["txt"])
  )

ggsave(here::here("plots", "day5.png"), width = 10.99, height = 5.67, units = "in")
