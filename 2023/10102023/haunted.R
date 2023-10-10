# #TidyTuesday | 2023-10-10 | Haunted Places in the United States
# Data Source comes from Tim Renner, using The Shadowlands Haunted Places Index, and shared on data.world.

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(maps)
library(ggfx)


# add font ----------------------------------------------------------------
font_add_google(name = "Special Elite", family = "Special Elite")
font <- "Special Elite"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 41)
haunted_places <- tuesdata$haunted_places

# wrangle data and create df ----------------------------------------------
df <- haunted_places %>%
  filter(!state_abbrev %in% c("AK", "HI")) %>% 
  filter(grepl("motel|hotel", location, ignore.case = TRUE)) %>% 
  select(1, 4, 9:10) %>% 
  na.omit()

# create data frame ------------------------------------------------------
df %>% 
  ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), size = 0.33, color = "#dcdcdc") +
  with_outer_glow(geom_point(aes(x = city_longitude, y = city_latitude), size = 0.5, color = "#9cff90"),
                  colour = "#09ff00", sigma = 20, expand = 10) +
  coord_map(clip = "off") +
  theme_void() +
  theme(text = element_text(size = 9.5, family = font, color = "#292929"),
        plot.title = element_text(family = font, size = 24, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "#292929", family = font, size = 10, hjust = 0.5, margin = margin(t = 5, b = 40)),
        plot.caption = element_text(hjust = 0.5, margin = margin(t = 40)),
        legend.position = "none",
        plot.margin = unit(c(1, 0.5, 1, 0.5), "cm"),
        plot.background = element_rect(fill = "#dcdcdc", color = NA)) +
  labs(title = "Haunted Hotels and Motels",
       subtitle = "285 places to stay for your next five star spooky getaway",
       caption = "#TidyTuesday | Data: Tim Renner on data.world | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("haunted_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


