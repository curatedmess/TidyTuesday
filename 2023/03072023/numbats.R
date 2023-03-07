# TidyTuesday | March 7, 2023 | Numbats in Australia
# Data Source is Atlas of Living Australia

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(scico)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 10)
numbats <- tuesdata$numbats
  
# wrangle and create df ---------------------------------------------------
df <- numbats %>% 
  filter(!is.na(year)) %>% 
  group_by(year, dataResourceName) %>%
  summarise(total = n()) %>% 
  group_by(year) %>% 
  mutate(percent = total/sum(total) * 100) %>% 
  ungroup() %>% 
  filter(year >= 2000)

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = year, y = total, fill = case_when(dataResourceName == "SA Fauna (BDBSA)" ~ "SA Fauna (BDBSA)", 
                                                   dataResourceName == "NSW BioNet Atlas" ~ "NSW BioNet Atlas", 
                                                   TRUE ~ "The Other Five Partners"))) +
  geom_bar(stat = "identity", position = "stack") +
  annotate("text", x = 2001, y = 80, size = 5, color = "#F2F2F2", hjust = "left", label = "82.9%") +
  annotate("segment", x = 2001, xend = 2005, y = 74, yend = 74, size = 0.4, color = "#F2F2F2") +
  annotate("text", x = 2001, y = 66, size = 2.5, color = "#F2F2F2", hjust = "left", label = "Combined contribution\nby two data partners") +
  scale_fill_scico_d(palette = "hawaii") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(breaks = c(2000, 2013, 2023)) +
  theme_minimal() +
  theme(plot.title = element_text(family = font, size = 18, hjust = 0, face = "bold", color = "#F2F2F2"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 11, hjust = 0, color = "#F2F2F2", lineheight = 1.1),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#F2F2F2", hjust = 0.5),
        axis.text = element_text(size = 8, family = font, color = "#F2F2F2"),
        axis.line = element_line(linewidth = 0.2, color = "#F2F2F2"),
        axis.ticks.x = element_line(linewidth = 0.9, color = "#F2F2F2"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = font, color = "#F2F2F2"),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
        plot.background = element_rect(color = NA, fill = "#28282B")) +
  labs(title = "Recording the Whereabouts of Australia's Numbat",
       subtitle = "Number of contributions by data partners since 2000\n",
       caption = "\n\n#TidyTuesday | Data: Atlas of Living Australia | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("numbats_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)


