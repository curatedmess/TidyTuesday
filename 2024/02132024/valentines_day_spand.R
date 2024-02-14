# #TidyTuesday | 2024—02—03 | Valentine's Day consumer data
# Data source comes from the National Retail Federation in the United States

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggfittext)

# add font ————————————————————————————————————————————————————————————————
font_add_google(name = "Roboto", family = "Roboto")
font <- "Roboto"

font_add_google(name = "Righteous", family = "Righteous")
font2 <- "Righteous"

# turn on showtext ————————————————————————————————————————————————————————
showtext_auto()
showtext_opts(dpi = 320)

# load data ——————————————————————————————————————————————————————————————
historical_spending <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/historical_spending.csv')

# create df ———————————————————————————————————————————————————————————————
df <- historical_spending %>% 
  filter(Year %in% c(2013, 2022)) %>% 
  select(1, 4:10) %>% 
  pivot_longer(cols = -1, names_to = "type", values_to = "amount") %>% 
  mutate(type = toupper(type)) %>% 
  mutate(type = str_replace(type, "GREETINGCARDS", "GREETING CARDS")) %>% 
  mutate(type = str_replace(type, "GIFTCARDS", "GIFT CARDS")) %>% 
  mutate(type = str_replace(type, "EVENINGOUT", "EVENING OUT")) %>% 
  group_by(type) %>% 
  mutate(previous = lag(amount), change = amount - previous, change_percentage  = (change/previous)) %>%
  na.omit()
  
# create plot —————————————————————————————————————————————————————————————
df %>% 
  arrange(change_percentage) %>%
  ggplot(aes(y = change_percentage, x = reorder(type, rev(change_percentage)), label = type)) +
  geom_col(aes(color = ifelse(change_percentage > 0, "#000000", "#FF0000")), fill = NA, linewidth = 1.1) +
  geom_bar_text(aes(color = ifelse(change_percentage > 0, "#000000", "#FF0000")), reflow = TRUE, grow = TRUE, place = "bottom", angle = 90, min.size = 2, fontface = "bold", family = font2) +
  geom_text(aes(label = scales::percent(change_percentage), vjust = ifelse(change_percentage > 0, -0.75, 1.75), color = ifelse(change_percentage > 0, "#000000", "#FF0000")), family = font, fontface = "bold", size = 3) +
  geom_hline(yintercept = 0, linewidth = 1.1) +
  geom_text(aes(x = 5, y = .8, label = "A decade later, Americans are\nspending less money on greeting\ncards for Valentine's Day gifts"), family = font2, size = 5.25, color = "#000000", hjust = 0.5, check_overlap = T) +
  geom_text(aes(x = 5, y = .65, label = "Comparing the average amount spent per person for gifts\nbetween the years 2013 and 2022 as a change in percent."), family = font, size = 3, color = "#000000", hjust = 0.5, check_overlap = T) +
  scale_color_identity() +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7.5, color = "#000000", margin = margin(t = 15)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(caption = "#TidyTuesday | Data: National Retail Federation | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("valentines_spending_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


