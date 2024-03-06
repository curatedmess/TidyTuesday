# #TidyTuesday | 2024—03-05 | Trash Wheel Collection Data
# Data source comes from the Mr. Trash Wheel Baltimore Healthy Harbor initiative

library(tidytuesdayR)
library(tidyverse)
library(showtext)

# add font ————————————————————————————————————————————————————————————————
font_add_google(name = "Londrina Solid", family = "Londrina Solid")
font2 <- "Londrina Solid"

font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Work Sans"

# turn on showtext ————————————————————————————————————————————————————————
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ——————————————————————————————————————————————————————————————
tuesdata <- tidytuesdayR::tt_load(2024, week = 10)

trashwheel <- tuesdata$trashwheel

# create df ---------------------------------------------------------------
df <- trashwheel %>% 
  filter(!ID == "captain") %>% 
  filter(Year == 2022) %>% 
  mutate(Month = stringr::str_to_title(Month)) %>% 
  select(ID, Month, CigaretteButts, PlasticBags, PlasticBottles, Polystyrene, Wrappers) %>% 
  pivot_longer(cols = c("CigaretteButts", "PlasticBags", "PlasticBottles", "Polystyrene", "Wrappers"), names_to = "type", values_to = "number") %>% 
  filter(type == "CigaretteButts") %>% 
  group_by(ID, Month) %>% 
  summarise(total = sum(number)) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(Date = make_date(2022, match(Month, month.name), 1))

# create data frame for area labels ---------------------------------------
df_labels <- data.frame(ID = c("gwynnda", "professor", "mister"), labels = c("Gwynnda the Good Wheel of the West", "Professor Trash Wheel", "Mr. Trash Wheel"), 
  Date = c("2022-04-01", "2022-04-01", "2022-04-01"), 
  total = c(2500, 15000, 35000),
  angle = c(0, 20, 25))

    
# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = Date, y = total, fill = ID)) +
  geom_hline(yintercept = c(25000, 50000, 75000), linetype = "dotted") +
  geom_area(alpha = 1, color = "#000000", linewidth = 0.7) +
  # geom_text(data = df_labels, aes(x = as.Date(Date), y = total, label = labels, angle = angle), family = font, size = 2.5, color = "#000000", hjust = 0, fontface = "bold") +
  geom_text(aes(x = as.Date("2022-08-01"), y = 65000, label = "Peak month to\nforget how to use\nthe trash can!"), family = font2, size = 4, color = "#000000", hjust = 0, check_overlap = T) +
  annotate("curve", x = as.Date("2022-08-01"), y = 73000, xend = as.Date("2022-07-05"), yend = 78000, linewidth = 0.7,  curvature = 0.3, arrow = arrow(length = unit(1.25, "mm")), color = "#000000") +
  scale_fill_manual(values = c("#00FFFF", "#FFFF00", "#FF00FF")) +
  scale_y_continuous(limits = c(NA, 90000), breaks = c(25000, 50000, 75000), labels = scales::comma_format(scale = 1e-3, suffix = "K"), expand = c(0, 0)) +
  scale_x_date(date_labels = "%b", breaks = seq(as.Date("2022-01-01"), as.Date("2022-12-01"), by = "months"), expand = c(0, 0)) +
  theme_void() +
  theme(plot.title = element_text(family = font2, hjust = 0, size = 28, color = "#000000", margin = margin(b = 5)),
        plot.subtitle = element_text(family = font, size = 8.5, color = "#000000", margin = margin(b = 0), lineheight = 1.2),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "none",
        axis.text.y = element_text(family = font, size = 8, color = "#000000", margin = margin(r = 5)),
        axis.text.x = element_text(family = font, size = 8, color = "#000000", margin = margin(t = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Collecting Lots of Cigarette Butts",
       subtitle = "Total number of butts collected by month in 2022 by three trash interceptors from the\nMr. Trash Wheel family of semi-autonomous units in Baltimore, MD placed at the ends of\nrivers or streams helping to save our oceans from trash.",       
       caption = "#TidyTuesday | Data: Mr. Trash Wheel Baltimore Healthy Harbor initiative | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("mr_trash_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


