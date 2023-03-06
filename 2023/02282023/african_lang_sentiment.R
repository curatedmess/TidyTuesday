# TidyTuesday | February 28, 2023 | African Language Sentiment
# Data Source is AfriSenti via @shmuhammad2004

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 9)

afrisenti <- tuesdata$afrisenti
languages <- tuesdata$languages
# language_scripts <- tuesdata$language_scripts
# language_countries <- tuesdata$language_countries
# country_regions <- tuesdata$country_regions
  
# wrangle and create df ---------------------------------------------------
df <- afrisenti %>% 
  left_join(languages) %>% 
  # filter(intended_use == "test") %>%
  group_by(language) %>%
  mutate(n = n()) %>%
  #filter(n >= 5000) %>% 
  ungroup() %>% 
  group_by(language, label) %>%
  summarize(value = n()) %>% 
  mutate(percent = value/sum(value)) %>% 
  group_by(language) %>%
  mutate(total_tweets = sum(value))

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = percent, y = fct_reorder(language, total_tweets, .desc = FALSE), fill = label)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  geom_text(aes(label = paste(language, " - ", total_tweets, "tweets"), x = 0.50), nudge_y = 0.5,   size = 3, color = "#000000", family = font) +
  geom_text(aes(label = if_else(percent > 0.05, paste0(scales::percent(percent, accuracy = 0.1L)), NULL)), position = position_fill(vjust = 0.5), size = 3, color = "#000000", family = font) +
  scale_fill_manual(values = c("#cb4f46", "#dfbd53", "#557a67")) +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 26.5, hjust = 0.5, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = font, size = 10, hjust = 0.5, color = "#000000", lineheight = 1.1),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "AFRICAN LANGUAGE TWEETS",
       subtitle = "Percent of tweets by <span style='color:#557a67;'><b>positive</b></span>, <span style='color:#dfbd53;'><b>neutral</b></span> and <span style='color:#cb4f46;'><b>negative</b></span> sentiment<br>",
       caption = "\n#TidyTuesday | Data: AfriSenti via @shmuhammad2004 | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("african_lang_sentiment", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 9)


