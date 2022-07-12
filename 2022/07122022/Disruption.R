# TidyTuesday | July 12, 2022 - Disruption in Air Travel
# Data source is Eurocontrol


# libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)
library(scales)
library(lubridate)

# add font ----------------------------------------------------------------
font_add_google(name = "IBM Plex Mono", family = "IBM Plex Mono")

# turn on showtext --------------------------------------------------------
showtext_auto()


# get data
tuesdata <- tidytuesdayR::tt_load('2022-07-12')

flights <- tuesdata$flights 


# create df and wrangle data
df <- flights %>%
  mutate(Date = make_date(YEAR,MONTH_NUM)) %>%
  group_by(Date, STATE_NAME) %>%
  summarise(total = sum(FLT_TOT_1))

# create plot
df %>%
ggplot(aes(x = Date, y = total, group = STATE_NAME)) + 
  geom_line() +
  scale_y_continuous(expand = c(0,0), limits = c(NA, 230000)) +
  scale_x_date(expand= c(2016-01-01, NA)) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "IBM Plex Mono", color = "#000000"),
        plot.title = element_text(family = "IBM Plex Mono", size = 31.5, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "IBM Plex Mono", size = 10, hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 8, family = "IBM Plex Mono", color = "#000000"),
        axis.title.y = element_text(size = 8, family = "IBM Plex Mono", color = "#000000", angle=90),
        legend.position = "NULL",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#000000", size = 0.25, linetype = "dotted"),
        axis.line.x.bottom = element_line(color = "#000000", size = 1),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = "#f2f2f2", fill = "#f2f2f2")) +
  annotate(geom = "curve", x = as.Date("2021-01-01"), y = 170000, xend = as.Date("2020-04-01"), yend = 120000,
           curvature = -0.2, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", y = 180000, x = as.Date("2021-01-01"), label = "COVID Cliff", hjust = "center", family = "IBM Plex Mono", fontface = "bold", size = 3) +
  labs(title = "Disruption in Air Travel",
       subtitle = "Total Number of Monthly Flights by Country in Europe from Jan 2016 to May 2022",
       caption = "\n#TidyTuesday | Data: Eurocontrol | Design: Ryan Hart",
       y = "Total # of Flights (Arrivals + Departures)\n")


# save plot
ggsave(paste0("Disruption_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)
