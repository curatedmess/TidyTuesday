# TidyTuesday | September 27, 2022 | Week 39
# Data source is arts.gov by way of Data is Plural

# load libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(scales)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2022-09-27')
artists <- tuesdata$artists

# create df ---------------------------------------------------------------
df <- artists %>% 
  mutate(race2 = if_else(race == "Hispanic", "Hispanic", "Non-Hispanic")) %>% 
  select(race2, type, artists_n) %>% 
  group_by(type, race2) %>% 
  summarise(n = sum(artists_n, na.rm = TRUE)) %>%
  mutate(perc = round(n / sum(n), 3))

# create plot -------------------------------------------------------------
df %>%
  ggplot(aes(y = reorder(type, desc(type)), x = perc, fill = race2)) +
  geom_col(position="dodge2") +
  geom_text(aes(label = paste(round(perc*100 , digits = 1),"%",sep="")), color = "#000000", size = 2.5, family = font, position = position_dodge2(width = 0.9), hjust = 1.10) +
  geom_vline(xintercept = 0.18, linetype = "dotted", size = 0.8, color = "#000000") +
  annotate("text", x = 0.19, y = "Writers And Authors", label = "\nHispanics are 18% of total U.S. population", color = "#000000", size = 3, family = font, fontface = "bold", hjust = "left", vjust = "top") +
  scale_fill_manual(values = c("#f0932b", "#95afc0")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(labels = wrap_format(30)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(text = element_text(size = 9, family = font, color = "#000000"),
        plot.title = element_text(family = font, size = 27.5, hjust = 0, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 8.5, family = font, color = "#000000"),
        axis.text.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(color = "#000000", size = 0.5),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = "#F2F2F2", fill = "#F2F2F2")) +
  labs(title = "Hispanic Diversity in the Arts",
       subtitle = "The Hispanic community is mostly under-represented as artists compared to Hispanic\nrepresentation in the U.S. population.\n",
       caption = "\n\n#TidyTuesday | Data: American Community Survey (2015-2019) | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("hispanic_arts_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 8)



  
  
  