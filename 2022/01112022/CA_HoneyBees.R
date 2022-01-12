# TidyTuesday | January 11 - Bee Colonies
# Data source is USDA


# libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(scales)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Red Hat Display", family = "Red Hat Display")
font_add_google(name = "Red Hat Text", family = "Red Hat Text")

# turn on showtext --------------------------------------------------------
showtext_auto()

# get data ----------------------------------------------------------------
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')

# view(colony)

# wrangle data for plot ---------------------------------------------------
df <- colony %>%
  group_by(year) %>% 
  filter(state == "California") %>%
  na.omit(colony_n) %>% 
  summarise(year, mean = mean(colony_n)) %>%
  unique()

# view(df)

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(y = mean, x = as.factor(year), fill = as.factor(year))) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::comma(mean)), vjust = 1.75, color="white", size=3, family = "Red Hat Text") +  
  scale_fill_manual(values = c("#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#ffb101")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1300000)) +
  theme_minimal() +
  theme(text = element_text(family = "Red Hat Text", color = "#000000"),
        plot.title = element_text(family = "Red Hat Display", size = 20, hjust = 0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.subtitle = element_textbox_simple(hjust = 0.5, halign = 0.5, lineheight = 1.3, padding = margin(10, 10, 10, 10)),
        plot.caption = element_text(size = 9, hjust = 0.5, margin = margin(5,0,5,0)),
        axis.text.x = element_text(family = "Red Hat Text", color = "#000000"),
        axis.title.x = element_text(size = 8, family = "Red Hat Text", color = "#000000"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.line.x.bottom = element_line(color = "#000000"),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.background = element_rect(color = "#ffffff", fill = "#ffffff")) +
  labs(title = "California Honey Bee Colonies",
       subtitle = "The yearly average of honey bee colonies <span style = 'color:#ffb101;'><b>surpassed one million in 2021</b></span>.",
       caption = "\n#TidyTuesday | Data: USDA | Design: Ryan Hart",
       x = "\nUSDA collects survey data quarterly, with the second quarter of 2019 data omitted because of survey suspension.")

# save plot
ggsave(paste0("CA_HoneyBees_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)
