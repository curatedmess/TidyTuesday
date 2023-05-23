# #TidyTuesday | 2023-05-23 | Squirrel
# Data Source is 2018 Central Park Squirrel Census

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(janitor)

# add font ----------------------------------------------------------------
font_add_google(name = "Oswald", family = "Oswald")
font_add_google(name = "Open Sans", family = "Open Sans")
font_t <- "Oswald"
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# # load data ---------------------------------------------------------------
squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')

# wrangle data and create df ----------------------------------------------
df <- squirrel_data %>% 
  clean_names() %>%
  select(unique_squirrel_id, primary_fur_color, running, chasing, climbing, eating, foraging) %>% 
  pivot_longer(!c(unique_squirrel_id, primary_fur_color), names_to = "status", values_to = "count") %>% 
  na.omit() %>% 
  filter(!count == "FALSE") %>% 
  mutate(status = str_to_upper(status)) %>% 
  group_by(status) %>% 
  tally()

# get squirrel image ------------------------------------------------------
svg_url <- 'https://www.svgrepo.com/download/102314/squirrel.svg'
svg_txt <- paste(readLines(svg_url), collapse = "\n")

# create plot -------------------------------------------------------------
df %>% 
  arrange(n) %>%
  mutate(status = factor(status, unique(status))) %>%
  ggplot(aes(x = n, y = reorder(status, n))) +
  geom_segment(aes(x = 0, xend = n, y = status, yend = status)) +
  geom_point_svg(aes(x = n, y = status), svg = svg_txt, size = 13) +
  geom_text(aes(label = n), family = font_t, size = 2.5, vjust = 4.5) +
  scale_x_continuous(limits = c(NA, 1525)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 32, hjust = 0.5, face = "bold", color = "#000000", margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.5, color = "#000000", lineheight = 1.2, margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 25)),
        legend.position = "none",
        axis.text.y = element_text(size = 10, family = font_t, color = "#000000", hjust = 0.5, margin = margin(l = 5)),
        # axis.text.x = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 10)),
        # panel.grid.major.x = element_line(color = "#E0E0E0", linewidth = 0.4),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "What are the squirrels up to?",
       subtitle = "Observational counts of squirrel behaviors by volunteers in NYC's Central Park.",
       caption = "#TidyTuesday | Data: 2018 Central Park Squirrel Census | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("nyc_squirrels_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

  
  