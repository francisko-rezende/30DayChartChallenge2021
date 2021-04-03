library(tidyverse)
library(here)
library(showtext)

temp <-
  read.table(here::here("data/HadCRUT.4.6.0.0.annual_ns_avg.txt")) %>% 
  dplyr::rename(date = 1, temp_median = 2) %>% 
  tibble::as_tibble() %>% 
  tibble::rownames_to_column() %>% 
  dplyr::mutate(x = "x", y = 1) 

cols <- c(
  "#124e91",
  "#2876b6",
  "#559ccb",
  "#bcd6ea",
  "#f5dfd7",
  "#fbb69f",
  "#fc8f70",
  "#ed4733",
  "#a20b16",
  "#6a020f"
)

font_add_google("Merriweather Sans", "Merriweather Sans")

showtext_auto()

ggplot(temp, aes(x = x, y = y, fill = temp_median)) +
  geom_col() +
  scale_fill_gradientn(colours = cols) +
  coord_flip() +
  theme_void() +
  labs(
    caption = "Vizualisation by Francisko de Moraes Rezende (@francisko_r) | Data: Hadley Centre (HadCRUT4)"
  ) +
  theme(
    legend.position = "none",
    plot.caption = element_text(size = 25, family = "Merriweather Sans"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")
  )

ggsave(
  here::here("plots", "day3.png"),
  width = 9.34,
  height = 5.75,
  units = "in"
)

# additional attempts -----------------------------------------------------
dark_blue <- "#08306b"
dark_red <- "#67000d"
  
ggplot2::ggplot(temp, aes(x = x, y = y, fill = temp_median)) +
  geom_col() +
  scale_fill_gradient2(low = dark_blue, high = dark_red) +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none"
  )

ggplot(temp, aes(x = x, y = y, fill = temp_median)) +
  geom_col() +
  scale_fill_viridis_c(option = "plasma") +
  coord_flip() +
  theme_void() +
  theme(
    legend.position = "none"
  )
