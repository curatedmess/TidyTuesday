# #TidyTuesday | 2023-04-11 | U.S. Eggs
# Data Source is The Humane League's US Egg Production dataset by Samara Mendez

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggimage)

# add font ----------------------------------------------------------------
font_add_google(name = "Oswald", family = "Oswald")
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font_t <- "Oswald"
font <- "Source Sans Pro"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
eggproduction  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')

# get image ---------------------------------------------------------------
egg <- magick::image_read("https://freesvg.org/img/carlitos_Egg.png") %>%
  magick::image_trim()

egg_img <- magick::image_write(egg, path = "egg.img", format = "png")
image <- egg_img

# wrangle and create data frame -------------------------------------------
df <- eggproduction %>%
  filter(grepl("cage", prod_process)) %>%
  filter(prod_process == "cage-free (non-organic)") %>% 
  # arrange(observed_month) %>%
  # group_by(quarter = paste(lubridate::year(observed_month), quarters(observed_month))) %>%
  # summarise(n = sum(n_eggs)) %>% 
  # arrange(quarter) %>%
  # mutate(mom = (n - lag(n)) / lag(n), label = percent(mom %>% round(2))) %>% 
  # filter(!grepl("2016|2021", quarter)) %>% 
  # separate(quarter, sep = " ", into = c("year", "quarter"))
  arrange(observed_month) %>%
  mutate(mom = (n_eggs - lag(n_eggs)) / lag(n_eggs), label = percent(mom %>% round(2))) %>%
  filter(!grepl("2016|2021", observed_month)) %>% 
  mutate(month = format(observed_month, "%m"), year = format(observed_month, "%Y"))

# create plot -------------------------------------------------------------
df %>%
  ggplot(aes(x = month, y = year)) +
  geom_image(aes(image = image), size = 0.15, by = "height", asp = 1.4, hjust = 0.5) +
  geom_text(aes(label = label, color = ifelse(mom < 0, "red", "black")), family = font_t, size = 3.5, hjust = 0.5) +
  scale_color_identity() +
  scale_size_continuous(range = c(10, 20)) +
  scale_x_discrete(label = month.abb) +
  scale_y_discrete() +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 40, hjust = 0, face = "bold", color = "#FFFFFF"),
          plot.title.position = "plot",
          plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000"),
          plot.caption = element_text(family = font, size = 8, color = "#000000", hjust = 0.5),
          plot.caption.position = "plot",
          legend.position = "Null",
          axis.text.x = element_text(family = font, size = 8, color = "#000000", margin = margin(t = -5)),
          axis.text.y = element_text(family = font, size = 8, color = "#000000", margin = margin(r = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
          plot.background = element_rect(color = "#0ABAB5", fill = "#0ABAB5")) +
    labs(title = "GOT EGGS?",
         subtitle = "Month over month percent change for cage-free (non-organic) egg production in the U.S.\n",
         caption = "\n\n\n#TidyTuesday | Data: The Humane League's US Egg Production dataset | Design: Ryan Hart")
  
  
# save plot
ggsave(paste0("got_eggs_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
  
