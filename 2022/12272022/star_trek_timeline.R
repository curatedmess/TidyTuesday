# TidyTuesday | December 27, 2022 | Star Trek
# Data Source is {rtrek}

# load libraries ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(geomtextpath)
#library(ggtext)
library(trekcolors)

# add font ----------------------------------------------------------------
font_add_google(name = "Antonio", family = "Antonio")
font <- "Antonio"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get data ----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 52)

tlBooks <- tuesdata$tlBooks
#tlFootnotes <- tuesdata$tlFootnotes

# wrangle and clean up the data -------------------------------------------
df <- tlBooks %>% 
  filter(format == "episode") %>% 
  # group_by(year, series) %>% 
  # count() %>% 
  # # ungroup() %>%
  dplyr::select(year, series) %>% 
  unique() %>% 
  arrange(year) %>% 
  mutate(minute = ifelse(year == year[1], 1, year - year[1] + 1))


# create df for hour labels -----------------------------------------------
big_hours <- data.frame(hours = seq(0, 1380, by = 240))
# small_hours <- data.frame(hours = seq(0, 23, by = 1))

# create clock plot -------------------------------------------------------------
ggplot() +
  annotate("segment", x = 0, xend = 1440, y = 0.97, yend = 0.97, size = 1.5, color = "#F2F2F2") +
  annotate("segment", x = 0, xend = 227, y = 1, yend = 1, color = "#F2F2F2", alpha = 0.6, size = 8) +
  geom_point(aes(x = 797, y = 0.95), shape = 1, size = 15, color = "#F2F2F2", alpha = 0.6) +
  geom_segment(data = big_hours, aes(x = hours, xend = hours, y = 0.965, yend = 0.92), color = "#F2F2F2") +
  geom_segment(data = df, aes(x = minute, xend = minute, y = 0.965, yend = 0.93, color = series), alpha = 0.9) +
  scale_color_manual(values = c("#FF80AB", "#80D8FF", "#FFFF8D", "#EA80FC"), labels = c("Enterprise", "The Next Generation", "The Original Series", "Voyager")) +
  scale_x_continuous(breaks = c(0, 240, 480, 720, 960, 1200), labels = c("24", "4", "8", "12", "16", "20")) +
  ylim(c(0.5, 1)) +
  annotate(geom = "text", x = 0.5, y = 0.5, size = 5, hjust = 0.5, label = "Start at midnight\nwith the earliest year\nand the timeline\nends before 4 A.M.\n•••\nExcept for one\nfinal year that\noccurs much later\nafter 1 P.M.", family = font, color = "#F2F2F2") +
  coord_curvedpolar(clip = "off") +
  theme_void() +
  theme(plot.title = element_markdown(family = font, size = 30, hjust = 0.5, color = "#F2F2F2"),
        plot.subtitle = element_text(family = font, size = 15, hjust = 0.5, color = "#F2F2F2"),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 10, color = "#F2F2F2"),
        plot.caption.position = "plot",
        axis.text.x = element_text(size = 12, family = font, color = "#F2F2F2", vjust = 6.5),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = font, color = "#F2F2F2"),
        plot.margin = margin(1, 0, 1, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#28282B"),
        panel.background = element_rect(color = NA, fill = "#28282B")) +
  labs(title = "STAR TREK TIMELINE",
       subtitle = "\nImagine if EVERY YEAR featured in the TV series timeline was a MINUTE in a 24-hour day.\n",
       caption = "#TidyTuesday | Data: {rtrek} | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("star_trek_timeline", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 8)

