# #TidyTuesday | 2023-10-03 | US Government Grant Opportunities
# Data Source comes from grants.gov

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Roboto Mono", family = "Roboto Mono")
font <- "Roboto Mono"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# load data --------------------------------------------------------------
grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv')
grant_opportunity_details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grant_opportunity_details.csv')

# wrangle data and create df ----------------------------------------------
df <- grants %>% 
  filter(grepl("HHS", agency_code)) %>% 
  mutate(year = year(posted_date)) %>% 
  filter(year < 2023, year > 2012) %>% 
  filter(!is.na(estimated_funding)) %>% 
  group_by(year) %>% 
  summarise(total = sum(estimated_funding)) 

df2 <- df %>%
  slice(which.min(total), which.max(total))

# create data frame ------------------------------------------------------
df %>% 
  ggplot(aes(x = year, y = total)) +
  geom_step() +
  geom_point(aes(x = year, y = total, fill = ifelse(year %in% c(2017, 2021), "#000000", "#FFFFFF")), shape = 21, color = "#000000") +
  geom_text(data = df2, aes(x = year -0.25, y = total, label = unit_format(prefix = "$", unit = "B", scale = 1e-9)(total)), family = font, size = 3, hjust = 0.95) +
  scale_fill_identity() +
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9, prefix = "$")) +
  scale_x_continuous(breaks = seq(2013, 2022, by = 1)) +
  theme_minimal() +
  theme(text = element_text(size = 9, family = font, color = "#000000"),
        plot.title = element_text(family = font, size = 18, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 9, hjust = 0.5, margin = margin(t = 10, b = 25), lineheight = 1.2),
        plot.caption = element_text(family = font, size = 8, hjust = 0.5, margin = margin(t = 25)),
        axis.title = element_blank(),
        axis.text.x = element_text(margin = margin(t = 10)),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(fill = "#FFFFFF")) +
  labs(title = "Health and Human Services Grants",
       subtitle = "The ups and downs of grant funding amounts from HHS and its\naffiliated agencies by year posted over a 10-year period.",
       caption = "#TidyTuesday | Data: grants.gov | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("hhs_grants_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
