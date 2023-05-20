# #TidyTuesday | 2023-05-16 | Tornados
# Data Source is NOAA's National Weather Service Storm Prediction Center

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggbeeswarm)
library(ggridges)

# add font ----------------------------------------------------------------
font_add_google(name = "Cabin", family = "Cabin")
font <- "Cabin"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 20)

tornados <- tuesdata$tornados

# wrangle data and create df ----------------------------------------------
df <- tornados %>% 
  select(om, yr, st, time) %>% 
  mutate(state = state.name[match(st, state.abb)]) %>% 
  filter(yr >= 2013) %>% 
  group_by(yr, time) %>%
  mutate(time = as.POSIXct(strptime(time, format='%H:%M', tz = "America/Chicago"))) %>%
  mutate(count = n()) %>% 
  ungroup() %>% 
  group_by(state) %>%
  mutate(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n > 500) %>%
  ungroup() 

# create plot -------------------------------------------------------------
df %>%
  ggplot(aes(x = time, y = state)) + 
  geom_density_ridges2(scale = 2, color = "#FFFFFF", fill = "#6D214F") +
  # geom_vline(xintercept = as.POSIXct(strptime(c("17:00"), format = "%H:%M", tz = "GMT"))) +
  scale_x_datetime(expand = c(0, 0), breaks = as.POSIXct(strptime(c("0:00", "3:00", "6:00", "9:00", "12:00", "15:00", "18:00", "21:00","23:59"), format = "%H:%M", tz = "America/Chicago")), date_labels = ("%I %p")) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 30, hjust = 0.5, face = "bold", color = "#000000", margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.5, color = "#000000", margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 30)),
        legend.position = "none",
        axis.text.y = element_text(size = 8, family = font, color = "#000000", hjust = 1),
        axis.text.x = element_text(size = 7, family = font, color = "#000000", hjust = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 15)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "When do Tornados Strike?",
       subtitle = "States with more than 500 tornado events recorded between 2013 and 2022",
       x = "Time of Day (Central Time)",
       caption = "#TidyTuesday | Data: NOAA | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("tornados_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
