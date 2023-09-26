# #TidyTuesday | 2023-09-26 | Roy Kent F**k count
# Data Source comes from richmondway R package by Deepsha Menghani

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)


# add font ----------------------------------------------------------------
font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_t <- "Roboto Slab"

font_add_google(name = "Roboto Mono", family = "Roboto Mono")
font <- "Roboto Mono"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 39)
richmondway <- tuesdata$richmondway

# wrangle data and create df ----------------------------------------------
df <- richmondway %>%
  group_by(Season) %>% 
  mutate(midpoint = mean(c(min(Episode), max(Episode)))) %>% # used to find the midpoint for labels
  mutate(avg = round(mean(F_count_RK), digits = 0))

# create list for facet labels --------------------------------------------
label <- c("1" = "Season 1", "2" = "Season 2", "3" = "Season 3")

# create data frame ------------------------------------------------------
df %>% 
  ggplot(aes(x = Episode, y = F_count_RK)) +
  geom_segment(aes(x = Episode, xend = Episode, y = -3, yend = 3, linewidth = F_count_RK)) +
  geom_richtext(aes(x = midpoint, y = -2.8, label = paste0("AVG ", avg, " PER EPISODE")), hjust = 0.5, family = font, size = 2.35, label.r = unit(0, "lines"), label.padding = unit(0.25, "lines"), color = "#FFFFFF", text.color = "#000000", fill = "#FFFFFF", fontface = "bold") +
  facet_wrap(~Season, scales = "free_y", ncol = 1, labeller = as_labeller(label)) +
  theme_void() +
  theme(text = element_text(size = 9, family = font, color = "#000000"),
        plot.title = element_text(family = font_t, size = 36, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 8.25, hjust = 0.5, margin = margin(t = 5, b = 25)),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 35)),
        strip.text = element_text(family = font, size = 8.5, margin = margin(b = 3), hjust = 0, face = "bold", color = "#D3D3D3"), 
        panel.spacing = unit(1.25, "lines"),
        legend.position = "none",
        plot.margin = unit(c(1, 2.5, 1, 2.5), "cm"),
        plot.background = element_rect(fill = "#FFFFFF")) +
  labs(title = "F-words spoken",
       subtitle = "by Roy Kent per episode of Ted Lasso range from 2 to 23.",
       caption = "#TidyTuesday | Data: {richmondway} | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("kent_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 5, height = 7)
