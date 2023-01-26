# TidyTuesday | January 24, 2023 | Alone
# Data Source is {Alone} data package by @danoehm

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggpubr)
library(magick)
library(ggshadow)


# add font ----------------------------------------------------------------
font_add_google(name = "Special Elite", family = "Special Elite")
font_add_google(name = "Roboto", family = "Roboto")
font_t <- "Special Elite"
font <- "Roboto"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load background ---------------------------------------------------------
# tree images created using canva
background <- image_read("background_forest-3.png")

# load data ---------------------------------------------------------------
survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')
# loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
# episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv')
# seasons <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/seasons.csv')

# wrangle data ------------------------------------------------------------
df <- survivalists %>% 
  filter(grepl("Lonely|Missed|Family|family", reason_tapped_out))

# create df for axis text to go inside plot -------------------------------
y_axis <- data.frame(season_num = c(1, 2, 3, 4, 5, 6, 7, 8, 9))
x_axis <- data.frame(days = c(5, 10, 25, 30, 35, 50))

# create plot -------------------------------------------------------------
ggplot() +
  # credit for gradient code to @tanya_shapiro
  # https://twitter.com/tanya_shapiro/status/1592328864176693248?s=20&t=vpCboM081dcuOqHkpwx1PQ
  ggpattern::geom_rect_pattern(data = df, aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), pattern = "gradient", pattern_fill = "#242f3a", pattern_fill2 = "#4b5f74", color = NA) +
  geom_glowpoint(data = df, aes(x = days_lasted, y = season), shape = 8, color = "#FFFFFF", size = 0.2, alpha = 0.9, shadowsize = 0.4, shadowalpha = 0.025, shadowcolor = "#FFECB3") +
  background_image(background) +  
  geom_text(data = y_axis, aes(x = 0, y = season_num, label = season_num), color = "#84848c", family = font, size = 2) +
  geom_text(data = x_axis, aes(x = days, y = 0, label = days), color = "#84848c", family = font, size = 2) +
  annotate(geom = "text", x = 30.5, y = 11.5, label = "ALONE", hjust = "center", family = font_t, size = 11, color = "#FFFFFF") +
  annotate(geom = "text", x = 30.5, y = 10.75, label = "Each star represents a contestant from the survivalist show who tapped out because of loneliness or missing family.", hjust = "center", vjust = "top", family = font, size = 2.5, color = "#FFFFFF") +
  annotate(geom = "text", x = 50, y = -2, label = "#TidyTuesday | Data: {alone} | Design: Ryan Hart", hjust = "center", vjust = "top", family = font, size = 2.5, color = "#F2F2F2") +
  # top/righ annotation
  annotate(geom = "text", x = 50, y = 7.35, label = "Teimojin Tan lasted 63 days\nfinishing 3rd on season 9", hjust = "left", vjust = "top", family = font, size = 2, color = "#FFFFFF") +
  annotate(geom = "curve", x = 56, y = 7.6, xend = 60, yend = 8.65, linewidth = 0.3,  curvature = -0.25, arrow = arrow(length = unit(1.25, "mm")), color = "#FFFFFF") +
  # bottom/left annotation
  annotate(geom = "text", x = 3, y = 5.75, label = "Jim Shields lasted 2 days\nfinishing last on season 3", hjust = "left", vjust = "top", family = font, size = 2, color = "#FFFFFF") +
  annotate(geom = "curve", x = 4, y = 5, xend = 1.8, yend = 3.5, linewidth = 0.3,  curvature = 0.20, arrow = arrow(length = unit(1.25, "mm")), color = "#FFFFFF") +
  annotate(geom = "text", x = -2, y = 0, label = "Seasons >", hjust = "left", vjust = "top", family = font, size = 2, color = "#84848c", angle = 90) +
  annotate(geom = "text", x = 1, y = -0.5, label = "Days Lasted >", hjust = "left", vjust = "top", family = font, size = 2, color = "#84848c") +
  scale_y_continuous(limits = c(-2, 12)) +
  scale_x_continuous(limits = c(-2, 65)) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.background = element_rect(color = "#28282B", fill = "#28282B"))

# save plot ---------------------------------------------------------------
ggsave(paste0("alone_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

