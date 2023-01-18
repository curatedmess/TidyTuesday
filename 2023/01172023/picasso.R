# TidyTuesday | January 17, 2023 | Art History
# Data Source is arthistory data package

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

# wrangle data ------------------------------------------------------------
df <- artists %>% 
  select(edition_number, book, artist_name, space_ratio_per_page_total) %>% 
  group_by(edition_number, book) %>% 
  mutate(avg = mean(space_ratio_per_page_total, na.rm = TRUE)) %>% 
  filter(artist_name == "Pablo Picasso") %>% 
  mutate(delta = space_ratio_per_page_total - avg) %>% 
  pivot_longer(cols = c(space_ratio_per_page_total, avg)) %>% 
  mutate(name = recode(name, space_ratio_per_page_total = "Picasso", avg = "Average")) %>% 
  filter(book == "Janson")

# required to conditionally adjust the hjust for geom_text labels ---------
align <- ifelse(df$name == "Average", 0, 1)

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(y = as.character(edition_number), x = value)) +
  geom_line(aes(group = edition_number), size = 3, color = "#022658") +
  geom_text(aes(label = name), nudge_y = 0.3, size = 2, color = "#888888", hjust = align, family = font) +
  scale_x_continuous() +
  theme_minimal() +
  theme(plot.title = element_text(family = font, size = 28, hjust = 0.5, color = "#022658"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 9, hjust = 0.5, lineheight = 1.1, color = "#888888" ),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "888888", hjust = 0.5),
        legend.position = "none",
        axis.title = element_text(size = 9, family = font, color = "#888888", hjust = 0.5),
        axis.text = element_text(size = 8, family = font, color = "#888888", hjust = 0.5),
        panel.grid = element_line(linewidth = 0.35, linetype = "dotted", color = "#888888"),
        plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm"),
        plot.background = element_rect(color = "#FFFFFF", fill = "#FFFFFF")) +
  labs(title = "Pablo Picasso",
       subtitle = "The space allocated to the artist Pablo Picasso in every edition of\nJanson's History of Art books relative to the average for all artists.\n",
       y = "Book Editions\n",
       x = "\nSpace per Page Ratio",
       caption = "\n#TidyTuesday | Data: {arthistory} | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("picasso_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

