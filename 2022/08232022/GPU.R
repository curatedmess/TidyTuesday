# TidyTuesday | August 23, 2022 | Week 34
# Data source is The CHIP Dataset


# load libraries ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)
library(showtext)
library(scales)


# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()

# load data ---------------------------------------------------------------
full <- readr::read_csv("chip_dataset.csv")


# wrangle data and create data frame ------------------------------------------------------------
df <- full %>% 
  clean_names() %>% 
  filter(!grepl('NaT', release_date)) %>% 
  mutate(date = ymd(release_date),
         date = as.Date(date)) %>% 
  select(date, type, vendor, freq_m_hz) %>%
  filter(!grepl('ATI', vendor)) %>% 
  filter(!grepl('Other', vendor)) %>%
  mutate(year = year(date),
         year = make_date(year)) %>% 
  filter(year >= "2010-01-01") %>%
  filter(type == "GPU") %>% 
  mutate(vendor = toupper(vendor)) %>% 
  group_by(year, vendor) %>%
  summarise(max = max(freq_m_hz))


# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = year(year), y = max, color = vendor, label = max)) + 
  geom_line(size = 1.3) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(from = 2010, to = 2021, by = 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 2500), labels = comma) +
  scale_color_manual(values = c("#FD7272", "#B33771", "#D6A2E8")) +
  coord_cartesian(clip = 'off') +
  theme_minimal() +
  theme(text = element_text(size = 9, family = font, color = "#000000"),
        plot.title = element_text(family = font, size = 24, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 8, family = font, color = "#000000"),
        axis.title.y = element_text(size = 8, family = font, color = "#000000"),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#000000", size = 0.25, linetype = "dotted"),
        axis.line.x.bottom = element_line(color = "#000000", size = 0.4),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
        plot.background = element_rect(color = "#F2F2F2", fill = "#F2F2F2")) +
  labs(title = "GRAPHICS PROCESSORS",
       subtitle = "Fastest GPUs released by year\n",
       caption = "\n#TidyTuesday | Data: The CHIP Dataset | Design: Ryan Hart",
       y = "Clock Speed (MHz)\n")


# save plot ---------------------------------------------------------------
ggsave(paste0("GPU_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


  
  
  


