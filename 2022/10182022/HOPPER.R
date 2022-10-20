# TidyTuesday | October 18, 2022 | Week 42
# Data source is http://8flix.com - prepped by Dan Fellowes & Jonathan Kitt

# load libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(stringr)
library(scales)
library(tidytext)
library(imager)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()

# load data ---------------------------------------------------------------
dialogue_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')

# load image and change to grayscale --------------------------------------
hopper_pic <- load.image("hopper_final.png")
hopper_gray <- grayscale(rm.alpha(hopper_pic))

# reduce the number of points ---------------------------------------------
hopper_pic_df <- hopper_gray %>% 
  as.data.frame() %>%
  filter(value > 0) %>% 
  mutate(x = cut(x, round(dim(hopper_gray)[1]/20, 0), labels = FALSE),
         y = cut(y, round(dim(hopper_gray)[2]/20, 0), labels = FALSE)) %>%
  group_by(x, y) %>%
  summarise(value = mean(value)) %>% 
  arrange(y,x)

# create data frame -------------------------------------------------------
words_df <- dialogue_raw %>% 
  filter(grepl("Hopper", stage_direction)) %>% 
  select(season, stage_direction, dialogue) %>% 
  mutate(text = sub('.*Hopper(.*?)].*', '\\1', stage_direction)) %>% 
  na_if("") %>%
  na.omit() %>% 
  unnest_tokens(word, text) %>% 
  distinct(word)


word_string <- data.frame(string = paste0(words_df$word, collapse = ''))

word_letters <- str_split(word_string$string, "") 
word_df <- data.frame(letters = Reduce(rbind, word_letters)) %>% 
  bind_rows(replicate(60, word_df, simplify = FALSE)) %>% 
  slice(1:4229)

df <- cbind(hopper_pic_df, word_df)
  

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_text(aes(label = letters, color = value, size = value), family = font) +
  scale_color_continuous(low =  "#000000", high = "#c5baa4") +
  scale_x_continuous(expand = c(0,0), limits = c(-20, 100)) +
  scale_size_continuous(range = c(3.5, 1)) +
  scale_y_continuous(expand = c(0,0), limits = c(60, 0), trans=scales::reverse_trans()) +
  geom_text(aes(y = 35, x = -8, label = "H\nO\nP\nP\nE\nR"), lineheight = 0.6, family = font, fontface = "bold", size = 24, color = "#f01a22") +
  theme_void() +
  theme(plot.caption.position = "plot",
      plot.caption = element_markdown(hjust = 0.5, size = 11, family = font),
      plot.margin = unit(c(0, 0, 0.25, 0), "in"),
      legend.position = "none",
      plot.background = element_rect(fill = "#f2f2f2", color = NA),
      panel.background = element_rect(fill = "#f2f2f2", color = NA)) +
  labs(caption = "<br>Stage direction prompts for Jim Hopper on the <b><span style=color:#f01a22;'>Stranger Things</span></b> television series.<br><br><span style=font-size:9.0pt;'>#TidyTuesday | Data: 8flix.com | Design: Ryan Hart</span>")

# save plot ---------------------------------------------------------------
ggsave(paste0("Hopper_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6, units = "in")


  
  