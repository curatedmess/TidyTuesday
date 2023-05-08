# #TidyTuesday | 2023-03-21 | Programming Languages
# Data Source is Programming Language DataBase

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(packcircles)
library(geomtextpath)

# add font ----------------------------------------------------------------
font_add_google(name = "Ubuntu", family = "Ubuntu")
font <- "Ubuntu"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 12)

languages <- tuesdata$languages

# wrangle data and create df ----------------------------------------------
df_raw <- languages %>% 
  select(title, github_language_repos) %>% 
  arrange(desc(github_language_repos)) %>% 
  slice(1:5)

pack <- circleProgressiveLayout(df_raw$github_language_repos, sizetype = "area") %>% 
  mutate(radius = radius - 100)

df_plot <- cbind(df_raw, pack) %>% 
  mutate(id = 1:5)

df <- circleLayoutVertices(pack, npoints = 50)

df$title <- df_plot$title[match(df$id, df_plot$id)]

# create plot -------------------------------------------------------------
ggplot() +
  geom_textpath(data = df, aes(x, y, label = title), hjust = "ymax", family = font, size = 4.5) +
  geom_text(data = df_plot, aes(x , y, label = scales::comma(github_language_repos)), family = font, size = 3) +
  coord_equal() +
  theme_void() +
  scale_pattern_filename_discrete(choices = image) +
  theme(plot.title = element_text(family = font, size = 25, hjust = 0.5, face = "bold", color = "#000000", margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 12, hjust = 0.5, color = "#000000", margin = margin(b = 15)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 15)),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "PROGRAMMING LANGUAGES",
       subtitle = "Top 5 languages according to number of GitHub repos",
       caption = "#TidyTuesday | Data: Programming Language DataBase | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("top_5_repos_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)




