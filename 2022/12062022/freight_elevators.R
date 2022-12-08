# TidyTuesday | December 6, 2022 | Elevators
# Data Source is the {Elevators} package by @Emil_Hvitfeldt

# load libraries ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(scales)
library(janitor)
library(ggchicklet)
library(MetBrewer)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2022-12-06')
elevators <- tuesdata$elevators

# wrangle and clean up the data -------------------------------------------
df <- elevators %>% 
  clean_names() %>% 
  filter(device_type == "Freight (F)") %>% 
  filter(dv_device_status_description == "ACTIVE") %>% 
  dplyr::select(dv_capacity_lbs, borough) %>% 
  na.omit() %>% 
  filter(dv_capacity_lbs >= 14000) %>% 
  mutate(max_capacity = case_when(dv_capacity_lbs >= 14000 & dv_capacity_lbs < 28000  ~ "One", 
                           dv_capacity_lbs > 28000 & dv_capacity_lbs < 42000  ~ "Two",
                           dv_capacity_lbs > 42000 & dv_capacity_lbs < 56000  ~ "Three",
                           dv_capacity_lbs > 56000 & dv_capacity_lbs < 70000  ~ "Four",
                           dv_capacity_lbs > 70000 & dv_capacity_lbs < 84000  ~ "Five")) %>% 
  group_by(borough, max_capacity) %>% 
  summarise(total = n())
  
# change factors to correct ordering --------------------------------------
df$max_capacity <- factor(df$max_capacity, levels = c("One", "Two", "Three", "Four", "Five"))
df$borough <- factor(df$borough, levels = c("Bronx", "Staten Island", "Queens", "Brooklyn", "Manhattan"))

# create plot -------------------------------------------------------------
ggplot() +
  geom_chicklet(data = df, aes(x = borough, y = total, fill = max_capacity), color = "#F2F2F2") +
  coord_flip() +
  scale_fill_manual(values = met.brewer("Kandinsky"), name = "Max number of elephants based\non elevator's weight capacity", guide = guide_legend(title.position = "top")) +
  theme_minimal() +
  theme(plot.title = element_text(family = font, size = 24, hjust = 0.5, color = "#28282B"),
        plot.subtitle = element_text(family = font, size = 9, hjust = 0.5, color = "#28282B"),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 8, color = "#28282B"),
        plot.caption.position = "plot",
        legend.position = "top",
        legend.text = element_text(size = 6, family = font, color = "#28282B"),
        legend.title = element_text(size = 7, family = font, color = "#28282B", hjust = 0.5),
        panel.grid.major = element_line(color = "#28282B", size = 0.2, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8, family = font, color = "#28282B"),
        axis.text = element_text(size = 8, family = font, color = "#28282B"),
        plot.margin = margin(1, 1, 1, 1,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
    labs(title = "Did you know?",
         subtitle = "There are 137 freight elevators in New York City with a max weight\ncapacity capable of lifting the equivalent of 14K pound elephant(s).\n", 
         y = "\nTotal Number of Elevators by NYC Borough",
         caption = "\n\n#TidyTuesday| Data: {Elevators} | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("freight_elevators_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
  

