# #TidyTuesday | 2023-05-02 | The Portal Project
# Data Source is Portal Project

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Oswald", family = "Oswald")
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font_t <- "Oswald"
font <- "Source Sans Pro"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 18)

plots <- tuesdata$plots
species <- tuesdata$species
surveys <- tuesdata$surveys

# wrangle and create df ---------------------------------------------------
df <- surveys %>% 
  left_join(species, by = "species") %>% 
  group_by(year, commonname) %>% 
  summarise(n = n())

# create plot -------------------------------------------------------------
df %>%
  ggplot(aes(x = year, y = commonname)) +
  geom_line(linewidth = 0.3) +
  geom_point(aes(size = n), alpha = 0.5) + 
  scale_size_continuous(breaks = c(min(df$n), 300, max(df$n))) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(breaks = seq(1978, 2022, by = 22)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 40, hjust = 0.5, face = "bold", color = "#000000", margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.5, color = "#000000", margin = margin(b = 40)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5),
        legend.position = c(0.25, 1.1),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(family = font, hjust = 0.5, size = 7, color = "#000000"),
        axis.text.y = element_text(size = 9, family = font, color = "#000000", hjust = 0),
        axis.text.x = element_text(size = 9, family = font, color = "#000000", margin = margin(t = 20)),
        #axis.line.x = element_line(linewidth = 0.2, color = "#000000"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Portal Project",
       subtitle = "Count of each rodent type surveyed across all 24 study plots by year from 1978 to 2022",
       caption = "\n\n#TidyTuesday | Data: Portal Project | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("portal_project_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)





