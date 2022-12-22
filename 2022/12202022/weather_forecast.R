# TidyTuesday | December 20, 2022 | Weather Forecasts
# Data Source is USA National Weather Service

# load libraries ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(lubridate)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2022, week = 51)

weather_forecasts <- tuesdata$weather_forecasts
cities <- tuesdata$cities
outlook_meanings <- tuesdata$outlook_meanings

# wrangle and clean up the data -------------------------------------------
df <- weather_forecasts %>% 
  filter(state == "NC") %>% 
  filter(possible_error == "none") %>% 
  filter(forecast_hours_before == 12) %>% 
  filter(date >= as_date("2021-06-01"),
         date <= as_date("2022-05-31")) %>% 
  mutate(location = paste(city, state, sep = ", ")) %>% 
  mutate(delta = abs(observed_temp - forecast_temp)) %>% 
  #filter(!delta == 0) %>% 
  na.omit() %>% 
  select(date, location, high_or_low, delta) %>% 
  group_by(location, high_or_low, delta) %>% 
  count()

# Remove _ from Raleigh Durham --------------------------------------------
df[df == "RALEIGH_DURHAM, NC"] <- "RALEIGH DURHAM, NC"
  

# create plot -------------------------------------------------------------
df %>%
  ggplot(aes(x = as.factor(delta), y = n, fill = high_or_low), alpha = 0.8) +
  geom_bar(stat = "identity" , position = "dodge") +
  scale_fill_manual(values = c("#ffb142", "#34ace0"), labels = c("High", "Low")) +
  facet_wrap(~ location, scales = "free") +
  theme_minimal() +
  theme(plot.title = element_markdown(family = font, size = 24, hjust = 0.5, color = "#28282B"),
       plot.subtitle = element_text(family = font, size = 10, hjust = 0.5, color = "#28282B"),
       plot.title.position = "plot",
       plot.caption = element_text(family = font, hjust = 0.5, size = 8, color = "#28282B"),
       plot.caption.position = "plot",
       legend.position = "top",
       legend.title = element_blank(),
       legend.text = element_text(size = 8, family = font, color = "#28282B"),
       strip.text = element_text(family = font, hjust = 0.5, size = 9, color = "#28282B"),
       panel.spacing = unit(1, "lines"),
       axis.title = element_text(size = 8, family = font, color = "#28282B"),
       axis.text = element_text(size = 8, family = font, color = "#28282B"),
       panel.grid.major = element_line(color = "#636e72", linewidth = 0.2, linetype = "dotted"),
       panel.grid.minor = element_blank(),
       plot.margin = margin(1, 0.5, 1, 0.5,"cm"),
       plot.background = element_rect(color = NA, fill = "#F2F2F2"),
       panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "FORECASTING THE WEATHER",
      #title = "THE <span style='color:#cf6a87;'>HIGHS</span> AND <span style='color:#546de5;'>LOWS</span>",
       subtitle = "How often are the 12-hour predicted and observed temperatures\ndifferent, and by how many degrees? (6/1/2021 to 5/31/2022)\n",
       x = "\nDifference between Predicted and Observed Temperature (°F)",
       y = "Number of Observations\n",
       caption = "\n\n#TidyTuesday| Data: USA National Weather Service | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("weather_forecast_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)



