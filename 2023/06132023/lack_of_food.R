# #TidyTuesday | 2023-06-13 | Studying African Farmer-Led Irrigation Survey
# Data Source is subset of data from the SAFI Project via the Data Carpentry Social Sciences workshop

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(stringr)

# add font ----------------------------------------------------------------
font_add_google(name = "Oswald", family = "Oswald")
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font_t <- "Oswald"
font <- "Source Sans Pro"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
safi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv')


# create data frame -------------------------------------------------------
df <- safi_data %>% 
  # mutate(months = sapply(strsplit(months_lack_food,";"), FUN = function(x){length(x[x!="Null"])})) %>% 
  mutate(months = strsplit(as.character(months_lack_food), ";")) %>%
  unnest(months) %>%
  mutate(months = gsub("July", "Jul", months)) %>%
  mutate(months = gsub("June", "Jun", months)) %>%
  mutate(months = gsub("Sept", "Sep", months)) %>%
  group_by(months) %>% 
  summarise(count = n()) %>% 
  filter(!months == "none") %>% 
  mutate(season = ifelse(months %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep"), "Cool and Dry Season", "Hot and Humid Season")) %>% 
  group_by(season) %>% 
  mutate(count_season = sum(count)) %>% 
  ungroup()

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = months, y = count, fill = season)) +
  geom_col() +
  geom_text(aes(label = count), vjust = -0.5, family = font, size = 3) + 
  scale_y_continuous(expand = c(0, 0), limits = c(NA, 80)) +
  scale_x_discrete(limits = month.abb) +
  scale_fill_manual(values = c("#227093", "#b33939")) +
  annotate("text", x = "May", y = 55, label = "79.6% of reported cases of\nlack of food occur during months\nin the Hot and Humid Season", family = font, size = 3.5, hjust = 0.5) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 30, hjust = 0, face = "bold", color = "#000000", margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000", lineheight = 1.2, margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 30)),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = font, color = "#000000"),
        axis.text.x = element_text(family = font, size = 9, color = "#000000", margin = margin(t = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "LACK OF FOOD",
       subtitle = "Monthly household counts by season from three Mozambican villages experiencing\nfood insufficiency. This data represents a subset of the survey information collected\nfrom the Studying African Farmer-Led Irrigation (SAFI) project.",
       caption = "#TidyTuesday | Data: SAFI project | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("lack_of_food_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)



