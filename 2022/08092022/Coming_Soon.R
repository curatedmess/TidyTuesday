# TidyTuesday | August 9, 2022 - Ferris Wheels
# Data source is {ferriswheels}

# load libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(scales)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()


# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2022-08-09')
wheels <- tuesdata$wheels


# wrangle data and create data frame ------------------------------------------------------------
df <- wheels %>% 
  filter(country == "USA") %>% 
  filter(status %in% c("Operating", "Planned", "In development", "Under Construction")) %>%
  select("country", "height", "status") %>%
  mutate(new_status = ifelse(status == "Operating", "In Operation", "Coming Soon")) %>%
  group_by(new_status) %>%
  tally(height) %>% 
  #https://r-charts.com/part-whole/pie-chart-labels-outside-ggplot2/
  mutate(csum = rev(cumsum(rev(n))), 
         pos = n/2 + lead(csum, 1),
         pos = if_else(is.na(pos), n/2, pos))


# create plot -------------------------------------------------------------
df %>%
  ggplot(aes(x = 5, y = n, fill = new_status, label = n)) +
  geom_col(width = 1, color = "#f2f2f2") +
  geom_textpath(aes(x = 5, y = pos, label = paste(n, " feet")), text_only = TRUE, angle = 90, size = 3, color = "#f2f2f2") +
  xlim(c(1.5, 5.5)) +
  coord_polar("y", start = 0, clip = "off") +
  scale_fill_manual(values = c("#682957", "#F0B54D")) +
  annotate(geom='richtext', x = 1.5, y = 0, size = 3, label = "<b>Cumulative Height</b><br>4,902 feet", family = font, fill = NA, label.color = NA) +
  theme_void() +
  theme(text = element_text(size = 9, family = font, color = "#000000"),
        plot.title = element_text(family = font, size = 18, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 9, hjust = 0.5, lineheight = 1.1),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "top",
        legend.title = element_blank(),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.background = element_rect(color = "#f2f2f2", fill = "#f2f2f2")) +
  labs(title = "New Ferris Wheels Coming Soon",
       subtitle = "As of 2013, the USA has 45 operational Ferris wheels with six new\nplanned, in development, or under construction. Once complete,\nthe cumulative height for all 51 Ferris wheels will explode by 158%!\n",
       caption = "\n#TidyTuesday | Data: @Emil_Hvitfeldt | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("Coming_Soon_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


