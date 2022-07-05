# TidyTuesday | July 5, 2022 = San Francisco Rents
# Data source is Kate Pennington's Bay Area Craigslist Rental Housing Posts, 2000-2018


# libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)
library(ggridges)

# add font ----------------------------------------------------------------
font_add_google(name = "Bitter", family = "Bitter")

# turn on showtext --------------------------------------------------------
showtext_auto()

# get data ----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2022-07-05')
rent <- tuesdata$rent

# wrangle data for plot ---------------------------------------------------
df <- rent %>%
  filter(county == "san francisco")
  
# create plot
df %>%
  ggplot(aes(x = price, y = as.factor(year))) + 
  geom_density_ridges2(fill = "#d3d3d3", scale = 3, jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0), point_size = 0.4, alpha = 0.9) +
  scale_x_continuous(expand = c(0,0), labels=scales::dollar_format()) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(text = element_text(size = 8, family = "Bitter", color = "#000000"),
        plot.title = element_text(family = "Bitter", size = 24, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = "Bitter", hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 7, family = "Bitter", color = "#000000"),
        axis.text = element_text(size = 7, family = "Bitter", color = "#000000"),
        axis.title.y = element_blank(),
        legend.position = "Null",
        panel.grid = element_blank(),
        plot.margin = unit(c(1.3, 1.3, 1.3, 1.3), "cm"),
        plot.background = element_rect(color = "#f2f2f2", fill = "#f2f2f2")) +
  labs(title = "San Francisco",
       subtitle = "<span style = 'font-size:10pt;'>Rental Prices for Housing<br></span>",
       caption = "\nData: Kate Pennington's Bay Area Craigslist Rental Housing Posts\n\n#TidyTuesday | Design: Ryan Hart",
       x = "\nRental Price (USD)")


# save plot
ggsave(paste0("SF_Rents_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

