# TidyTuesday | September 13, 2022 | Week 37
# Data source is BFRO by way of data.world


# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(imager)
library(ggtext)


# add font ----------------------------------------------------------------
font_add_google(name = "Bitter", family = "Bitter")
font_add_google(name = "Open Sans", family = "Open Sans")

# turn on showtext --------------------------------------------------------
showtext_auto()

font <- "Bitter"
font2 <- "Open Sans"

# load data ---------------------------------------------------------------
# unable to pull file from #tidytuesday, so pulled file from data.world
bigfoot <- readr::read_csv('bfro_reports_geocoded.csv')

pic_points <- readr::read_csv('bigfoot_points.csv')


# wrangle data and create data frame ------------------------------------------------------------
bigfoot_df <- bigfoot %>% 
  filter(!season == "Unknown") %>%
  select(season, state, number) %>%
  arrange(season)
  
df <- cbind(bigfoot_df, pic_points)


# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = ifelse(season == "Winter", "Winter", "Other Seasons")), size = 0.1) +
  scale_x_continuous(expand = c(0, 0), limits = c(NA, 1.5)) +
  scale_color_manual(values = c("#BCAAA4", "#448AFF")) +
  annotate(geom = "text", y = 0.85, x = 0.9, label = "BIG FOOT", hjust = "left", family = font, fontface = "bold", size = 10, color = "#000000") +
  annotate(geom = "richtext", y = 0.74, x = 0.89, label = "<span style='color: #448AFF;'>Winter</span> accounts for the least<br>number of big foot sightings<br>in the U.S. at 15% of the total.", hjust = "left", family = font2, size = 3, color = "#000000", fill = NA, label.color = NA) +
  annotate(geom = "text", y = 0.25, x = 1, label = "Each point represents 4,929 sightings\nwhere the season detail is available.", hjust = "left", family = font2, size = 2, color = "#000000") +
  annotate(geom = "curve", x = 1, y = 0.29, xend = 0.90, yend = 0.32,curvature = 0.25, size = 0.3,  arrow = arrow(length = unit(1.5, "mm")), color = "#000000") +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0.5, size = 7, family = font2, color = "#000000"),
        legend.position = "none",
        legend.title = element_blank(),
        plot.margin = unit(c(1.75, 1.75, 1.75, 1.75), "cm"),
        plot.background = element_rect(color = "#F2F2F2", fill = "#F2F2F2")) +
  labs(caption = "\n#TidyTuesday | Data: BFRO by way of data.world | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("bigfoot_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)




