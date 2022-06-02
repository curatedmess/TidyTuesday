# TidyTuesday | May 31 2022 - T-Mobile a Notch Above the Rest
# Data source is 2022 Axios Harris Poll 100


# libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(scico)

# add font ----------------------------------------------------------------
font_add_google(name = "IBM Plex Mono", family = "IBM Plex Mono")

# turn on showtext --------------------------------------------------------
showtext_auto()

# get data ----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2022-05-31')
poll <- tuesdata$poll
reputation <- tuesdata$reputation

# wrangle data for plot ---------------------------------------------------
df <- reputation %>%
  filter(industry == "Telecom")

# color palatte
palette <- "batlow"

df %>% 
ggplot(aes(fill = company, y = score, x = name)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(expand = c(0,0), limits = c(0, 90)) +
  scale_fill_scico_d(palette = palette) +
  #scale_fill_manual(values = met.brewer("Thomas")) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "IBM Plex Mono", color = "#000000"),
        plot.title = element_text(family = "IBM Plex Mono", size = 24, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "IBM Plex Mono", size = 10, hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 8, family = "IBM Plex Mono", color = "#000000"),
        axis.title.y = element_text(size = 8, family = "IBM Plex Mono", color = "#000000", angle=90),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#000000", size = 0.25, linetype = "dotted"),
        axis.line.x.bottom = element_line(color = "#000000", size = 0.5),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.background = element_rect(color = "#f2f2f2", fill = "#f2f2f2")) +
  labs(title = "T-Mobile a Notch Above the Rest",
       subtitle = "How did Americans rate Telecoms across seven key dimensions of reputation?",
       caption = "\n#TidyTuesday | Data: 2022 Axios Harris Poll 100 | Design: Ryan Hart",
       y = "Score\n",
       x = "Poll Category\n")


# save plot
ggsave(paste0("Telecom_AxiosHarrisPoll_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)

