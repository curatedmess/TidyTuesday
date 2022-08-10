# TidyTuesday | August 2, 2022 - Oregon Spotted Frog
# Data source is usgs.gov

# libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(scales)
library(scico)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()

# color palatte
palette <- "bamako"

# get data ----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2022-08-02')
frogs <- tuesdata$frogs

# create plot -------------------------------------------------------------
frogs %>% 
  ggplot(aes(x = HabType, fill = as.character(Female))) +
  geom_bar(position="fill") +
  scale_y_continuous(expand = c(0,0), labels = label_percent()) +
  scale_fill_scico_d(palette = palette, labels=c('Male', 'Female')) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = font, color = "#000000"),
        plot.title = element_text(family = font, size = 32, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.5, lineheight = 1.1),
        plot.caption = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 8, family = font, color = "#000000"),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#000000", size = 0.25),
        axis.line.x.bottom = element_line(color = "#000000", size = 0.5),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.background = element_rect(color = "#f2f2f2", fill = "#f2f2f2")) +
  labs(title = "Oregon Spotted Frog",
       subtitle = "Gender by habitat at the Crane Prairie Reservoir in Oregon, USA. Data\ncollected by USGS between September 2018 through November 2018.",
       caption = "\n#TidyTuesday | Data: usgs.gov | Design: Ryan Hart",
       x = "Habitat\n")

# save plot ---------------------------------------------------------------
ggsave(paste0("Spotted_Frog_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


