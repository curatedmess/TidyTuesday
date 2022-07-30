# TidyTuesday| July 26, 2022 | Week 30 - BYOD - % Change in Movie Ticket Prices
# Data source is https://www.the-numbers.com/market/

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(scales)
library(rvest)
#library(stringr)

# add font ----------------------------------------------------------------
font_add_google(name = "Ultra", family = "Ultra")

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")

# turn on showtext --------------------------------------------------------
showtext_auto()

font <- "Ultra"
font2 <- "Open Sans"

# WEB SCRAPE -----------------------------------------------------

# get data using rvest for screen scraping html ---------------------------

# url
url <- "https://www.the-numbers.com/market/"

web_data <- read_html(url)

# get data and create df --------------------------------------------------
movie_data <- web_data %>%
  html_nodes(xpath = '//*[@id="page_filling_chart"]/center[2]/table') %>%
  html_table()

df_temp <- data.frame(movie_data) %>%
  clean_names()

# wrangle data for plot ---------------------------------------------------
df <- df_temp %>%
  clean_names() %>% 
  select(year, average_ticket_price)
  
df$average_ticket_price <- gsub("[$]", "", as.character(df$average_ticket_price)) 

df_final <- data.frame(df) %>% 
  mutate(price = as.numeric(average_ticket_price)) %>% 
  mutate(percent_change = (price/lead(price) - 1) *100) %>%
  mutate(across(where(is.numeric), round, 2)) %>% 
  filter(year >= 2002 & year <= 2022) 

# create plot -------------------------------------------------------------
df_final %>% 
  ggplot(aes(x = year, y = 1, fill = percent_change)) +
  geom_tile() +
  scale_fill_gradient(high = "#3C362A", low = "#C9D6EA", labels = label_percent(scale = 1), breaks = c(1, 3, 5)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(from = 2002, to = 2022, by = 2)) +
  theme_minimal() +
  theme(text = element_text(size = 8, family = font2, color = "#000000"),
        plot.title = element_text(family = font, size = 24, hjust = 0, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font2, size = 10, hjust = 0),
        plot.caption = element_text(hjust = 0.5, size = 8),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 8, family = font2, color = "#000000"),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.key.height = unit(0.5, 'cm'),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        plot.background = element_rect(color = "#ffffff", fill = "#ffffff")) +
  labs(title = "MOVIE TICKET PRICES",
       subtitle = "Yearly percent change for the national average of movie ticket prices in\nthe United States from 2002 to 2022.",
       caption = "\n#TidyTuesday | Data: www.the-numbers.com | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("MovieTickets_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
