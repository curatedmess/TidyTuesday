# TidyTuesday | February 7, 2023 | Big Tech Stock Prices
# Data Source is Yahoo Finance via Kaggle (by Evan Gower)

# inspiration for this week's plot
# https://www.cnbc.com/2020/06/29/tesla-stock-up-4125percent-since --------


# load libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(tidyquant)
library(ggrepel)

# add fonts ---------------------------------------------------------------
font_add_google(name = "News Cycle", family = "News Cycle")
font <- "News Cycle"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

# wrangle data  -----------------------------------------------------------


# used this blog post to learn how to calculate returns using {tid --------
# https://www.codingfinance.com/post/2018-04-03-calc-returns/

# calculate daily returns -------------------------------------------------
daily_returns <- big_tech_stock_prices %>%
  filter(stock_symbol %in% c("NFLX", "TSLA", "AMZN")) %>% 
  #filter(date >= as.Date("2010-06-29")) %>% 
  filter(between(date, as.Date("2010-06-29"), as.Date("2022-06-30"))) %>% 
  group_by(stock_symbol) %>% 
  tq_transmute(select = adj_close, mutate_fun = periodReturn, period = "monthly", col_rename = "returns")

cumulative_returns <- daily_returns %>% 
  mutate(cumulative = cumprod(1 + returns)) %>%
  mutate(cumulative_returns = (cumulative - 1) * 100)
  
# create plot -------------------------------------------------------------
cumulative_returns %>% 
  ggplot(aes(x = date, y = cumulative_returns)) +
  geom_hline(yintercept = c(5000, 10000, 15000, 20000, 25000), linewidth = 0.2, color = "#ebebeb") +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "#000000") +
  geom_line(aes(color = stock_symbol), linewidth = 1) +
  geom_text(aes(label = stock_symbol, color = stock_symbol), data = . %>% filter(date == max(date)), size = 2.5, hjust = 0, nudge_x = 0.5, family = font) +
  scale_color_manual(values = c("#9caec5", "#32a1c9", "#b53f3c",  "#153f6a")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, big.mark = ","), breaks = seq(0, 25000, by = 5000)) +
  scale_x_date(breaks = seq(as.Date("2010-01-01"), as.Date("2022-12-31"), by = "24 months"), date_labels = "%Y") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(plot.title = element_text(family = font, size = 20, hjust = 0, face = "bold", color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, family = font, color = "#000000", hjust = 0),
        axis.title.x = element_text(size = 8, family = font, color = "#b2b5bf"),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, family = font, color = "#000000"),
        axis.line.x = element_line(linewidth = 0.4, color = "#ebebeb"),
        axis.ticks.x = element_line(linewidth = 0.4, color = "#ebebeb"),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Tesla's wild ride",
       subtitle = "Stock performance of Tesla's stock since June 30, 2010\n",
       caption = "\n\n\n\n#TidyTuesday | Data: Yahoo Finance via Kaggle (by Evan Gower) | Design: Ryan Hart",
       x = "\nData shows cumulative return on a monthly basis through market close on June 30, 2022.")

# save plot ---------------------------------------------------------------
ggsave(paste0("tesla_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
