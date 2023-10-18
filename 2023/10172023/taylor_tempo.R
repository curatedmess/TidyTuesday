# #TidyTuesday | 2023-10-17 | Taylor Swift
# Data Source comes from taylor R package from W. Jake Thompson

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)
library(ggbeeswarm)
library(scico)

# add font ----------------------------------------------------------------
font_add_google(name = "IM Fell DW Pica", family = "IM Fell DW Pica")
font <- "IM Fell DW Pica"

font_add_google(name = "Lato", family = "Lato")
font_d <- "Lato"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 42)
taylor_all_songs <- tuesdata$taylor_all_songs

# wrangle data and create df ----------------------------------------------
df <- taylor_all_songs %>% 
  filter(!grepl("Taylor's Version", album_name, ignore.case = TRUE)) %>% 
  filter(!album_name == "The Taylor Swift Holiday Collection") %>% 
  select(album_name, track_name, album_release, tempo) %>% 
  filter(!is.na(tempo)) %>% 
  mutate(catalog_median = median(tempo)) %>%
  mutate(quant_25 = quantile(tempo, probs = 0.25)) %>% 
  mutate(quant_75 = quantile(tempo, probs = 0.75)) %>% 
  group_by(album_name) %>% 
  mutate(album_median = median(tempo)) %>% 
  mutate(album_mad = mad(tempo)) %>% 
  ungroup()

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = tempo, y = reorder(album_name, album_release, decreasing = TRUE))) +
  geom_rect(aes(xmin = quant_25, xmax = quant_75, ymin = "Midnights", ymax = "Taylor Swift"), fill = "#fbe8e9", color = "#fbe8e9") +
  geom_line(linewidth = 0.5, color = "#000000") +
  geom_text(aes(x = album_median, label = round(album_median, 0)), color = "#000000", family = font_d, size = 2.25, vjust = -1.9) +
  geom_point(aes(x = album_median), color = "#000000", shape = "|", size = 4) +
  geom_beeswarm(shape = 21, color = "#fefefe", fill = "#000000", size = 2) +
  annotate("text", x = 225, y = 11, label = "Variability", size = 2.5, family = font_d, color = "#000000", vjust = "top") + 
  geom_richtext(aes(x = 225, label = paste0(round(album_mad, 0), " BPM"), fill = album_mad), family = font_d, size = 2.1, vjust = 0.5, label.colour = NA, color = "#FFFFFF", label.padding = unit(0.4, "lines"), label.r = unit(0, "lines")) +
  annotate("text", x = 160, y = 12.75, label = "50% of songs\nin this range", size = 2.5, family = font_d, color = "#000000", vjust = "top", hjust = "left") + 
  annotate("curve", x = 158, y = 12.6, xend = 130, yend = 11.25, linewidth = 0.3,  curvature = 0.3, arrow = arrow(length = unit(1.25, "mm")), color = "#000000") +
  geom_richtext(aes(x = quant_25, y = 11, label = round(quant_25, 0)), size = 2.1, family = font_d, color = "#000000", label.colour = NA, fill = "#fbe8e9", label.padding = unit(0.4, "lines"), label.r = unit(0, "lines")) + 
  geom_richtext(aes(x = quant_75, y = 11, label = round(quant_75, 0)), size = 2.1, family = font_d, color = "#000000", label.colour = NA, fill = "#fbe8e9", label.padding = unit(0.4, "lines"), label.r = unit(0, "lines")) + 
  geom_segment(aes(x = 60, xend = 210, y = 0.5, yend = 0.5), linewidth = 0.3) +
  scale_fill_scico(palette = "grayC", begin = 0.3) +
  scale_color_scico(palette = "grayC", begin = 0.3) +
  scale_x_continuous(breaks = c(75, 100, 125, 150, 175, 200)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(text = element_text(size = 9.5, family = font, color = "#000000"),
        plot.title = element_text(family = font, size = 28, color = "#A02B48", hjust = 0, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(color = "#000000", family = font_d, size = 9, hjust = 0, margin = margin(t = 3, b = 5), lineheight = 1.2),
        plot.caption.position = "plot",
        plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 25)),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 7, family = font_d, margin = margin(t = 3)),
        axis.title.x = element_text(family = font_d, size = 8, hjust = 0.5, margin = margin(t = 15)),
        panel.grid = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Swiftly Shifting Tempos",
       subtitle = "A look at the tempo (speed) of Taylor Swift's songs along with the <b>median</b>, the <b>range</b> and <b>variability</b><br>of song tempos within each of her albums.",
       x = "Tempo (measured in BPM)",
       caption = "#TidyTuesday | Data: <span style='color: #A02B48;'><b>{taylor}</b></span> R package | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("taylor_tempo_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

