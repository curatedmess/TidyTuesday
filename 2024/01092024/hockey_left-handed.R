# #TidyTuesday | 2024-01-09 | Canadian NHL Player Birth Dates
# Data source comes from the from Statistics Canada, the NHL team list endpoint, and the NHL API

# libraries ---------------------------------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

font_add_google(name = "Graduate", family = "Graduate")
font2 <- "Graduate"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# load data --------------------------------------------------------------
# canada_births_1991_2022 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/canada_births_1991_2022.csv')
# nhl_player_births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv')
nhl_rosters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv')
# nhl_teams <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_teams.csv')


# wrangle data ------------------------------------------------------------
df2 <- nhl_rosters %>% 
  select(player_id, shoots_catches, season) %>% 
  unique() %>% 
  group_by(season, shoots_catches) %>% 
  na.omit() %>% 
  summarise(total = n()) %>% 
  mutate(perc = total / sum(total)) %>% 
  ungroup()

# create peak value for L players -----------------------------------------
df_annotate_max <- df2 %>%
  filter(shoots_catches == "L") %>%
  arrange(desc(perc)) %>% 
  slice(1)
  
# create mean for L players -----------------------------------------------
df_annotate_avg <- df2 %>%
  filter(shoots_catches == "L") %>%
  summarise(avg = round(mean(perc), 2))

# create custom y axis labels ---------------------------------------------
df_axis <- data.frame(y = c(0.25, 0.5, 0.65, 1),
                        x = c(19131914, 19131914, 19131914, 19131914),
                        color = c("#999999", "#999999", "#00AB6C", "#999999"))

df2$shoots_catches <- factor(df2$shoots_catches, levels = c("R", "L"))

# create plot -------------------------------------------------------------
df2 %>%
  ggplot() +
  geom_area(aes(x = season, y = perc, fill = shoots_catches)) +
  geom_text(data = df_axis, aes(x = x, y = y, label = scales::percent(y), color = color, fontface = ifelse(y == 0.65, "bold", "plain")), family = font2, size = 3) +
  geom_point(data = df_annotate_max, aes(x = season, y = perc), shape = 21, fill = NA, color = "#FFFFFF", size = 8) +
  geom_text(aes(x = min(season), y = 0.67, label = "Left Hand Average"), family = font2, size = 2.5, hjust = -0.05, color = "#00AB6C") +
  geom_segment(data = df_annotate_avg, aes(y = avg, yend = avg, x = 19171918, xend = 20232024), color = "#00AB6C", size = 0.3) +
  geom_segment(aes(x = 19591960, y = 0.7025, xend = 19591960, yend = 0.57), color = "#FFFFFF", linewidth = 0.25) +
  geom_text(data = df_annotate_max, aes(x = season, y = 0.50, label = paste0("During the 1959-1960 season,\nleft-handed players peaked\nat ", scales::percent(perc), " of the roster spots.")), family = font, color = "#FFFFFF", size = 2.75, hjust = 0) +
  scale_y_continuous(labels = scales::percent, breaks = c(0.25, 0.50, 0.65, 0.75, 1)) +
  annotate("text", x = mean(df2$season), y = 0.9, label = "Right", family = font2, size = 6, color = "#FFFFFF") +
  annotate("text", x = mean(df2$season), y = 0.1, label = "Left", family = font2, size = 6, color = "#FFFFFF") +
  scale_color_identity() +
  scale_fill_manual(values = c("#000000", "#00AB6C")) +
  scale_x_continuous(limits = c(19101911, 20232024), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font2, hjust = 0.5, size = 21, color = "#000000", margin = margin(b = 5)),
        plot.subtitle = element_text(family = font, hjust = 0.5, size = 10, color = "#000000", margin = margin(b = 5), lineheight = 1.1),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0.5, size = 8, color = "#000000", margin = margin(t = 10)),
        plot.caption.position = "plot",
        legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Left Handedness in the NHL",
       subtitle = "Percentage of left vs. right-handed hockey players spanning 106 seasons\n(1917-1918 to 2023-2024)",
       caption = "#TidyTuesday | Data: NHL | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("hockey_left-handed_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
