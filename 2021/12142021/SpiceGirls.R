# tidytuesday Week 12-14-2021 | Spice Girls
# Data: Genius.com by way of Jacquie Tran

# libraries
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(tidytext)

# add font
font_add_google(name = "Jost", family = "Jost")

# turn on showtext
showtext_auto()

# load lyric data
lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv')

#view data
view(lyrics)

# tokenize and wrangle the data
words <- lyrics %>%
  unnest_tokens(word, line) %>%
  anti_join(stop_words) %>%
  group_by(album_name) %>%
  count(word, sort = TRUE) %>%
  ungroup

# plot data
words %>%
  group_by(album_name) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder_within(word, n, album_name)) %>%
  ggplot(aes(word, n)) +
  geom_col(show.legend = FALSE, fill = "#000000") +
  geom_text(aes(label = n, hjust = 1.3), color="white", size=3, family = "Jost") +
  ylim(NA, 70) +
  facet_wrap(~album_name, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  theme_void() +
  theme(text = element_text(family = "Jost", color = "#000000"),
        plot.title = element_text(size=20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(0,0,20,0)),
        plot.caption = element_text(size = 8, hjust = 1.0, margin=margin(20,0,0,0)),
        plot.margin = unit(c(1.25,2,2,1.5), "cm"),
        strip.text.x = element_text(size = 11, color = "#000000"),
        axis.text.y = element_text(size = 10, color ="#000000", hjust = 1, margin = margin(0,2,0,0)),
        axis.text.x = NULL,
        axis.line.y = element_line(color = "#000000"),
        plot.background = element_rect(color = "white", fill = "white")) +
  labs(title = "Love, Love, La and the Spice Girls",
       subtitle = "Top 10 most used words in song lyrics by Album",
       caption = "\n#tidytuesday | Data: Genius.com by way of Jacquie Tran | Design: Ryan Hart")

# save plot
ggsave(paste0("SpiceGirls_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)

