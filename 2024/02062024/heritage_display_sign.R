# #TidyTuesday | 2024-02-06 | World Heritage Sites
# Data source comes from the from  UNESCO World Heritage Sites

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(tidytext)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Bebas Neue", family = "Bebas Neue")
font <- "Bebas Neue"

font_add_google(name = "Kalam", family = "Kalam")
font2 <- "Kalam"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2024-02-06')
heritage <- tuesdata$heritage

# wrangle and format data -------------------------------------------------
# country -----------------------------------------------------------------
long_data <- heritage %>%
  select(country) %>% 
  mutate(letter = strsplit(as.character(country), "")) %>%
  unnest(letter) %>%
  group_by(country) %>% 
  mutate(x = row_number()) %>% 
  mutate(y = case_when(country == "Sweden" ~ 3.5,
                   country == "Denmark" ~ 2.5,
                   country == "Norway" ~ 1.5))

# year --------------------------------------------------------------------
year_data <- heritage %>%
  pivot_longer(cols = c('2004', '2022'),
               names_to = 'year',
               values_to = 'number') %>%
  mutate(number = sprintf("%02d", number))

# 2004 --------------------------------------------------------------------
year_2004_data <- year_data %>% 
  filter(year == 2004) %>% 
  mutate(number = strsplit(as.character(number), "")) %>%
  unnest(number) %>%
  group_by(country) %>% 
  mutate(x = row_number() + 11) %>% 
  mutate(y = case_when(country == "Sweden" ~ 3.5,
                       country == "Denmark" ~ 2.5,
                       country == "Norway" ~ 1.5))

# 2022 --------------------------------------------------------------------
year_2022_data <- year_data %>% 
  filter(year == 2022) %>% 
  mutate(number = strsplit(as.character(number), "")) %>%
  unnest(number) %>%
  group_by(country) %>% 
  mutate(x = row_number() + 14) %>% 
  mutate(y = case_when(country == "Sweden" ~ 3.5,
                       country == "Denmark" ~ 2.5,
                       country == "Norway" ~ 1.5))

# percent change ----------------------------------------------------------
perc_data <- year_data %>% 
  group_by(country) %>% 
  mutate(perc_change = round((as.numeric(number)/lag(as.numeric(number)) - 1), 2)) %>% 
  mutate(y = case_when(country == "Sweden" ~ 3.5,
                       country == "Denmark" ~ 2.5,
                       country == "Norway" ~ 1.5)) %>% 
  filter(year == 2022)

# create country squares --------------------------------------------------
rect_country_data <- expand.grid(x = 0.5:9.5, y = 1:3)
rect_country_data$xmax <- rect_country_data$x + 1
rect_country_data$ymax <- rect_country_data$y + 1

# create 2004 squares -----------------------------------------------------
rect_2004_data <- expand.grid(x = 11.5:12.5, y = 1:3)
rect_2004_data$xmax <- rect_2004_data$x + 1
rect_2004_data$ymax <- rect_2004_data$y + 1

# create 2022 squares -----------------------------------------------------
rect_2022_data <- expand.grid(x = 14.5:15.5, y = 1:3)
rect_2022_data$xmax <- rect_2022_data$x + 1
rect_2022_data$ymax <- rect_2022_data$y + 1

# create plot -------------------------------------------------------------
ggplot() +
  geom_rect(aes(xmin = -0.5, xmax = 17.5, ymin = -0.5, ymax = 7.5), fill = "black") +
  # labels
  geom_text(aes(x = 9, y = 6, label = "World Heritage Sites"), family = font, size = 11, color = "white", hjust = 0.5) +
  geom_text(aes(x = 0.5, y = 4.5, label = "Country"), family = font, size = 5, color = "white", hjust = 0) +
  geom_text(aes(x = 11.5, y = 4.5, label = "2004"), family = font, size = 5, color = "white", hjust = 0) +
  geom_text(aes(x = 14.5, y = 4.5, label = "2022"), family = font, size = 5, color = "white", hjust = 0) +
  
  # frames
  geom_rect(data = rect_country_data, aes(xmin = 0.45, xmax = 10.55, ymin = 0.95, ymax = 4.05), fill = "#353839") +
  ggchicklet:::geom_rrect(data = rect_country_data, aes(xmin = x, xmax = xmax, ymin = y, ymax = ymax), radius = unit(2.75, "pt"), color = "#353839", fill = "black") +
  geom_rect(data = rect_2004_data, aes(xmin = 11.45, xmax = 13.55, ymin = 0.95, ymax = 4.05), fill = "#353839") +
  ggchicklet:::geom_rrect(data = rect_2004_data, aes(xmin = x, xmax = xmax, ymin = y, ymax = ymax), radius = unit(2.75, "pt"), color = "#353839", fill = "black") +
  geom_rect(data = rect_2022_data, aes(xmin = 14.45, xmax = 16.55, ymin = 0.95, ymax = 4.05), fill = "#353839") +
  ggchicklet:::geom_rrect(data = rect_2022_data, aes(xmin = x, xmax = xmax, ymin = y, ymax = ymax), radius = unit(2.75, "pt"), color = "#353839", fill = "black") +
  
  # data
  geom_text(data = long_data, aes(x = x, y = y, label = letter), color = "white", family = font, size = 8) +
  geom_text(data = year_2004_data, aes(x = x, y = y, label = number), color = "white", family = font, size = 8) +
  geom_text(data = year_2022_data, aes(x = x, y = y, label = number), color = "white", family = font, size = 8) +
  
  # split
  geom_segment(data = rect_country_data, aes(x = min(x), xend = max(xmax), y = y + 0.5, yend = y + 0.5), color = "#353839", linewidth = 0.15) +
  geom_segment(data = rect_2004_data, aes(x = min(x), xend = max(xmax), y = y + 0.5, yend = y + 0.5), color = "#353839", linewidth = 0.15) +
  geom_segment(data = rect_2022_data, aes(x = min(x), xend = max(xmax), y = y + 0.5, yend = y + 0.5), color = "#353839", linewidth = 0.15) +
  
  # hinges
  geom_segment(data = rect_country_data, aes(x = x + 0.08, xend = x + 0.08, y = (y + 0.5) - 0.1, yend = (y + 0.5) + 0.1), color = "#353839", linewidth = 0.5, lineend = "round") +  
  geom_segment(data = rect_country_data, aes(x = xmax - 0.08, xend = xmax - 0.08, y = (y + 0.5) - 0.1, yend = (y + 0.5) + 0.1), color = "#353839", linewidth = 0.5, lineend = "round") +  
  geom_segment(data = rect_2004_data, aes(x = x + 0.08, xend = x + 0.08, y = (y + 0.5) - 0.1, yend = (y + 0.5) + 0.1), color = "#353839", linewidth = 0.5, lineend = "round") +  
  geom_segment(data = rect_2004_data, aes(x = xmax - 0.08, xend = xmax - 0.08, y = (y + 0.5) - 0.1, yend = (y + 0.5) + 0.1), color = "#353839", linewidth = 0.5, lineend = "round") +  
  geom_segment(data = rect_2022_data, aes(x = x + 0.08, xend = x + 0.08, y = (y + 0.5) - 0.1, yend = (y + 0.5) + 0.1), color = "#353839", linewidth = 0.5, lineend = "round") +  
  geom_segment(data = rect_2022_data, aes(x = xmax - 0.08, xend = xmax - 0.08, y = (y + 0.5) - 0.1, yend = (y + 0.5) + 0.1), color = "#353839", linewidth = 0.5, lineend = "round") +  
  
  # percent text
  geom_text(data = perc_data, aes(x = 19, y = y, label = scales::percent(perc_change)), size = 6.5, family = font2, fontface = "bold", color = "#0874d4") +
  
  scale_x_continuous(limits = c(-2, 19)) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.title = element_markdown(family = font, hjust = 0.5, size = 12, color = "#000000", margin = margin(b = 10), lineheight = 1.2),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 8, color = "#000000", margin = margin(t = 10)),
        plot.caption.position = "plot",legend.position = "none",
        plot.margin = unit(c(0.5, 0.25, 0.5, 0.25), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "<span style = 'font-family:Kalam; color: #0874d4;'><b>PERCENT INCREASE</b></span> in the number of World Heritage Sites in three Scandinavian countries<br>designated by UNESCO for their cultural, historical, scientific, or other forms of significance.",
    caption = "#TidyTuesday | Data: UNESCO | Design: Ryan Hart")
                      
  
  # save plot ---------------------------------------------------------------
ggsave(paste0("heritage_display_sign_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 7, height = 4)

                     