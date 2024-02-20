# #TidyTuesday | 2024—02—20 | R Consortium ISC Grants
# Data source comes from the R Consortium

library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(ggtext)

# add font ————————————————————————————————————————————————————————————————
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

font_add_google(name = "Permanent Marker", family = "Permanent Marker")
font2 <- "Permanent Marker"

# turn on showtext ————————————————————————————————————————————————————————
showtext_auto()
showtext_opts(dpi = 320)

# load data ——————————————————————————————————————————————————————————————
tuesdata <- tidytuesdayR::tt_load(2024, week = 8)
isc_grants <- tuesdata$isc_grants

# create df ---------------------------------------------------------------
df <- isc_grants %>% 
  group_by(year) %>% 
  summarise(total = sum(funded))

df_text <- df %>%
  slice(which.min(total), which.max(total))

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_line(aes(x = year, y = total), color = "#FFFFFF", linewidth = 0.8) +
  geom_text(data = df_text, aes(x = year - 0.25, y = total, label = year), family = font2, size = 4.5, hjust = 1, color = "#FDFF00") +
  geom_text(data = df_text, aes(x = year + 0.25, y = total, label = scales::unit_format(prefix = "$", unit = "K", scale = 1e-3)(total)), family = font2, size = 4.5, hjust = 0, color = "#FDFF00") +
  geom_point(data = df_text, aes(x = year, y = total), color = "#FDFF00", size = 2) +
  geom_text(aes(x = 2021, y = 200000, label = "Funding\non the\nDecline"), family = font2, size = 6, hjust = 0, color = "#FDFF00") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#FFFFFF", margin = margin(t = 25)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.line = element_line(linewidth = 0.8, color = "#FFFFFF"),
        axis.title.x = element_text(family = font, size = 10, color = "#FFFFFF", margin = margin(t = 10)),
        axis.title.y = element_markdown(family = font, size = 10, color = "#FFFFFF", angle = 90, margin = margin(r = 10)),
        panel.background = element_rect(color = NA, fill = "#000000"),
        plot.background = element_rect(color = NA, fill = "#000000")) +
  labs(x = "from 2016 to 2023",
       y = "Total <b><i><span style = 'font-size:13pt;'>R Consortium ISC Grant</span></b></i> amounts by year",
       caption = "#TidyTuesday | Data: R Consortium | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("r_grants_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


