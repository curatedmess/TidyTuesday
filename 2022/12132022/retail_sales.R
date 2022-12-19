# TidyTuesday | December 13, 2022 | Retail Sales
# Data Source is US Census Bureau

# load libraries ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(scales)
library(ggtext)
library(lubridate)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2022-12-13')
tuesdata <- tidytuesdayR::tt_load(2022, week = 50)

state_retail <- tuesdata$state_retail
coverage_codes <- tuesdata$coverage_codes

# wrangle and clean up the data -------------------------------------------
df_NC <- state_retail %>% 
  filter(state_abbr == "NC") %>% 
  filter(year == 2019) %>% 
  filter(!change_yoy == "S" ) %>% 
  mutate(month = month(month, label = TRUE))

df_NC[df_NC == 'total'] <- 'Total Retail'

df_REST <- state_retail %>% 
  filter(!state_abbr == "NC") %>% 
  filter(!state_abbr == "USA") %>% 
  filter(year == 2019) %>% 
  filter(!change_yoy == "S" ) %>% 
  mutate(month = month(month, label = TRUE))

df_REST[df_REST == 'total'] <- 'Total Retail'

test <- df_REST %>% distinct(state_abbr)


# create plot -------------------------------------------------------------
ggplot() +
  geom_line(data = df_REST, aes(x = month, y = as.numeric(change_yoy), group = state_abbr), color = "grey", alpha = 0.8) +
  geom_line(data = df_NC, aes(x = month, y = as.numeric(change_yoy), group = "NC"), color = "#0652DD", size = 1.1) +
  facet_wrap(~ subsector, ncol = 3) +
  theme_minimal() +
    theme(plot.title = element_text(family = font, size = 20, hjust = 0.5, color = "#28282B"),
          plot.subtitle = element_markdown(family = font, size = 14, hjust = 0.5, color = "#28282B"),
          plot.title.position = "plot",
          plot.caption = element_text(family = font, hjust = 0.5, size = 12, color = "#28282B"),
          plot.caption.position = "plot",
          legend.position = "none",
          panel.grid.major = element_line(color = "#28282B", size = 0.2, linetype = "dotted"),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.title.y = element_text(size = 11, family = font, color = "#28282B"),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 8, family = font, color = "#28282B"),
          plot.margin = margin(1, 1, 1, 1,"cm"),
          plot.background = element_rect(color = NA, fill = "#F2F2F2"),
          panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "U.S. Monthly State-Level Retail Sales for 2019",
       subtitle = "<br><span style='color:#0652DD;'><b>North Carolina</b></span> compared to the other 49 states, plus D.C.<br>",
       y = "YoY Percent Change\n",
       caption = "\n\n#TidyTuesday| Data: US Census Bureau | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("retail_sales_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 10)





