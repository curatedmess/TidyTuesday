# #TidyTuesday | 2023-04-25 | London Marathon
# Data Source is London Marathon R Package

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(stringr)
library(tsibble)
library(ggtext)
library(ggforce)
library(bsts)
library(brms)
library(tidybayes)

# add font ----------------------------------------------------------------
font_add_google(name = "Oswald", family = "Oswald")
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font_t <- "Oswald"
font <- "Source Sans Pro"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-04-25')

winners <- tuesdata$winners

# wrangle data and create data frame --------------------------------------
df <- winners %>% 
  select(Category, Year, Time) %>% 
  filter(Year >= 1990) %>% 
  mutate(Seconds = period_to_seconds(hms(Time))) %>% 
  filter(Category %in% c("Men")) %>% 
  select(Year, Category, Seconds) 

# reference for following model and forecast code
# https://github.com/mjskay/uncertainty-examples/blob/master/arima.md

# create model ------------------------------------------------------------
set.seed(74)

model <- with(df, bsts(Seconds, state.specification = AddLocalLinearTrend(list(), Seconds), niter = 5000))

# set time horizon for forecast -------------------------------------------
horizon <- 20

df_forecast <- data.frame(Year = max(df$Year) + 1:horizon) %>% 
  add_draws(predict(model, horizon = horizon)$distribution) %>% 
  sample_draws(200)

# first <- df_forecast %>% 
#   filter(.value < 7200) %>% 
#   arrange(Year)

# calculate y-axis labels -------------------------------------------------
axis_df <- data.frame(Seconds = c(7000, 8000, 9000, 7196)) %>% 
  mutate(Duration = seconds_to_period(Seconds)) %>% 
  mutate(Duration = sprintf("%02d:%02d:%02d", Duration@hour, minute(Duration), second(Duration)))

# create plot -------------------------------------------------------------
ggplot() +
  geom_line(data = df, aes(x = Year, y = Seconds), linewidth = 0.7) +
  geom_line(data = df_forecast, aes(x = Year, y = .value, group = .draw), color = "#E040FB", alpha = 0.1) +
  scale_x_continuous(breaks = seq(1990, 2040, by = 5)) +
  scale_y_continuous(breaks = seq(7000, 9000, by = 1000), labels = c("01:56:40", "02:13:20", "02:30:00")) +
  geom_mark_circle(aes(x = 2025, y = 7196.435),  linetype = "dashed", expand = unit(3, "mm")) +
  annotate("segment", x = 2025, xend = 2025, y = 7450, yend = 7900, linetype = "dashed") +
  annotate("text", x = 2025, y = 7950, label = "Maybe in 2025 with\na winning time of\n01:59:56", family = font_t, color = "#000000", size = 3, vjust = "bottom") +
  theme_minimal() +
  theme(plot.title = element_text(family = font_t, size = 30, hjust = 0.5, color = "#000000"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = font, size = 20, hjust = 0.5, color = "#000000", margin = margin(b = 30)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 30)),
        legend.position = "none",
        axis.text = element_text(size = 8, family = font, color = "#000000"),
        axis.title.y = element_text(size = 8, family = font, color = "#000000"),
        axis.title.x = element_markdown(size = 8, family = font, color = "#000000", margin = margin(t = 20)),
        axis.line = element_line(color = "#000000", linewidth = 0.3),
        panel.grid = element_line(color = "#BDBDBD", linewidth = 0.2, linetype = "dotted"),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "LONDON MARATHON WINNING TIMES",
       subtitle = "When will the men run sub 2 hours?",
       y = "Winner's Finishing Time\n",
       x = "\n<b>Historical: 1990 to 2022</b> | <span style='color: #E040FB;'><b>Forecast: 2023 to 2042 (Bayesian Structural Time Series)</b></span>",
       caption = "#TidyTuesday | Data: London Marathon R package | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("sub_two_v2", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 5)

