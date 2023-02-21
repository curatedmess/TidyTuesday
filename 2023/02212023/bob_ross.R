# TidyTuesday | February 21, 2023 | Bob Ross Paintings
# Data Source is Bob Ross Paintings via @frankiethull

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 8)
bob_ross <- tuesdata$bob_ross

# wrangle and create df ---------------------------------------------------

# get list of distinct hexcode colors -------------------------------------
hex_df <- bob_ross %>%  
  mutate(color_hex = gsub("\\[|\\]|\\'", "", color_hex)) %>% 
  separate_rows(color_hex, sep = ",") %>% 
  select(color_hex) %>% 
  mutate(color_hex = gsub("[[:space:]]", "", color_hex)) %>% 
  distinct()

# ordering colors using Lab Color Space -----------------------------------
# code adopted from this link ---------------------------------------------
# https://stackoverflow.com/questions/61193516/how-to-sort-colours --------

rgb_df <- col2rgb(hex_df$color_hex) 

lab_df <- convertColor(t(rgb_df), 'sRGB', 'Lab')
df <- data.frame(color = hex_df$color_hex[order(lab_df[, 'L'])])

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = color, y = 1, fill = color)) +
  geom_tile() +
  scale_fill_identity() + 
  theme_void() +
  theme(plot.title = element_text(family = font, size = 28, hjust = 0.5, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.5, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
        axis.title.x = element_text(size = 7, family = font, color = "#000000"),
        plot.margin = unit(c(1.75, 1.75, 1.75, 1.75), "cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2")) +
labs(title = "The Joy of Painting",
       subtitle = "Distinct paint colors from 13 seasons of the show\n",
       caption = "\n\n\n#TidyTuesday | Data: Bob Ross Colors data package | Design: Ryan Hart",
       x = "Colors ordered by lightness using the Lab color space")

# save plot ---------------------------------------------------------------
ggsave(paste0("bob_ross_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


