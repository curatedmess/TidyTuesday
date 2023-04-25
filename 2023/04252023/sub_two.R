# #TidyTuesday | 2023-04-25 | London Marathon
# #30DayChartChallenge | April 2023 - Day 25 | global change  
# Data Source is London Marathon R Package

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(forecast)
library(fable)
library(tsibble)
library(feasts)
library(ggtext)
library(ggforce)

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
# london_marathon <- tuesdata$london_marathon

# wrangle data and create data frame --------------------------------------
df <- winners %>% 
  select(Category, Year, Time) %>% 
  # add_row(Category = "Women", Year = 2023, Time = "Time(2:01:25)") %>% 
  # # add_row(Category = "Men", Year = 2023, Time = 2:1:25) %>% 
  filter(Year >= 1990) %>% 
  mutate(Seconds = period_to_seconds(hms(Time))) %>% 
  filter(Category %in% c("Men")) %>% 
  select(Year, Category, Seconds) %>% 
  mutate(Status = "Historical")

# create tsibble ----------------------------------------------------------
df_ts <- df %>% 
  as_tsibble(key = Category, index = Year)

# create model and forecast -----------------------------------------------
fit <- df_ts %>% 
  model(arima = ARIMA(Seconds)) %>% 
  mutate(average = (arima))

fc <- fit %>% 
  forecast(h = "25 years")

# create data frame of forecasted values ----------------------------------
forecast <- data.frame(fc) %>% 
  select(1, 3, 5) %>% 
  rename(Seconds = .mean) %>% 
  mutate(Status = "Forecast")

# new data frame for plotting ---------------------------------------------
df_final <- rbind(df, forecast) %>% mutate(Duration = seconds_to_period(round(Seconds, 0))) %>% 
  mutate(Duration = sprintf("%02d:%02d:%02d", Duration@hour, minute(Duration), second(Duration)))

# calculate y-axis labels -------------------------------------------------
axis_df <- data.frame(Seconds = c(7200, 7300, 7400, 7500, 7600, 7700, 7800)) %>% 
  mutate(Duration = seconds_to_period(Seconds)) %>% 
  mutate(Duration = sprintf("%02d:%02d:%02d", Duration@hour, minute(Duration), second(Duration)))

# create plot -------------------------------------------------------------
# fc %>%
#   autoplot(df_ts, level = NULL) +
df_final %>% 
  ggplot(aes(x = Year, y = Seconds)) +
  geom_line(aes(group = Category, color = Status), linewidth = 0.7) +
  scale_color_manual(values = c("#E040FB", "#000000")) +
  scale_x_continuous(breaks = seq(1990, 2045, by = 5)) +
  scale_y_continuous(breaks = seq(7200, 7800, by = 100), labels = c("02:00:00", "02:01:40", "02:03:20", "02:05:00", "02:06:40", "02:08:20", "02:10:00")) +
  geom_mark_circle(aes(x = 2043, y = 7193.938),  linetype = "dashed", expand = unit(3, "mm")) +
  annotate("segment", x = 2043, xend = 2043, y = 7250, yend = 7350, linetype = "dashed") +
  annotate("text", x = 2043, y = 7370, label = "Maybe in 2043 with\na winning time of\n01:59:54", family = font_t, color = "#000000", size = 3, vjust = "bottom") +
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
       x = "\n<b>Historical: 1990 to 2022</b> | <span style='color: #E040FB;'><b>Forecast: 2023 to 2047 (ARIMA model)</b></span>",
       caption = "#TidyTuesday & #30DayChartChallenge | Data: London Marathon R package | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("sub_two_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 5)

