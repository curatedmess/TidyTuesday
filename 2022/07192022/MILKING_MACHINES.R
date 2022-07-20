# TidyTuesday | July 19, 2022 - Technology Development | MILKING MACHINES
# Data source is data.nber.org


# libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)
library(scales)
library(geomtextpath)


# add font ----------------------------------------------------------------
font_add_google(name = "IBM Plex Mono", family = "IBM Plex Mono")


# turn on showtext --------------------------------------------------------
showtext_auto()


# get data ----------------------------------------------------------------
technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')


# create df and wrangle data (isolate New Zealand and Turkey and c --------
df <- technology %>%
  filter(label == "Milking machines in use") %>%
  group_by(iso3c) %>%
  filter(iso3c %in% c("TUR", "NZL")) %>%
  mutate(name = if_else(iso3c == "TUR", "Turkey", "New Zealand")) %>%
  filter(between(year,1970, 2000)) %>%
  mutate(percent_change = (last(value) - first(value))/ first(value) * 100)


# create plot -------------------------------------------------------------
df %>%
  ggplot(aes(y = value, x = year, group = iso3c, label = name)) +
  geom_textline(family = "IBM Plex Mono", size = 2.74, color = "#ffffff") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 110000), labels = comma) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "IBM Plex Mono", color = "#ffffff"),
        plot.title = element_text(family = "IBM Plex Mono", size = 30, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "IBM Plex Mono", size = 9, hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 8, family = "IBM Plex Mono", color = "#ffffff"),
        axis.title.y = element_text(size = 7, family = "IBM Plex Mono", color = "#ffffff"),
        legend.position = "NULL",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#ffffff", size = 0.25, linetype = "dotted"),
        axis.line.x.bottom = element_line(color = "#ffffff", size = 0.4),
        plot.margin = unit(c(1.3, 1.3, 1.3, 1.3), "cm"),
        plot.background = element_rect(color = "#121212", fill = "#121212")) +
  annotate(geom = "curve", x = 1990, y = 73000, xend = 1998, yend = 88000,curvature = -0.2, size = 0.5,  arrow = arrow(length = unit(2, "mm")), color = "#ffffff") +
  annotate(geom = "text", y = 65000, x = 1990, label = "A whopping 59,830%\nincrease for Turkey", hjust = "center", family = "IBM Plex Mono", fontface = "bold", size = 3, color = "#ffffff") +
  labs(title = "MILKING MACHINES",
       subtitle = "A tale of two approaches to production technology as evidenced by the use of milking machines\nin the production of milk by two top 10 milk-producing countries globally between 1970 to 2000.",
       caption = "\n\n#TidyTuesday | Data: data.nber.org | Design: Ryan Hart",
       y = "milking machines in use per ha of arable land\n")


# save plot ---------------------------------------------------------------
ggsave(paste0("MILKING_MACHINES_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)
