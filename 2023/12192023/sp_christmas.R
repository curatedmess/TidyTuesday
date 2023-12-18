  # #TidyTuesday | 2023-12-19 | Holiday Episodes
  # Data source comes from the Internet Movie Database
  
  # libraries ---------------------------------------------------------------
  library(tidyverse)
  library(showtext)
  library(ggtext)
  library(ggimage)
  
  # add font ----------------------------------------------------------------
  font_add_google(name = "Open Sans", family = "Open Sans")
  font <- "Open Sans"
  
  # turn on showtext --------------------------------------------------------
  showtext_auto()
  showtext_opts(dpi = 320)
  
  options(scipen = 999) 
  
  # get image ---------------------------------------------------------------
  title <- magick::image_read("https://famfonts.com/wp-content/uploads/south-park-2-wide.png") %>%
    magick::image_trim()
  
  title_img <- magick::image_write(title, path = "title.img", format = "png")
  image <- title_img
  
  # load data --------------------------------------------------------------
  holiday_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-19/holiday_episodes.csv')
  holiday_episode_genres <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-19/holiday_episode_genres.csv')
  
  # wrangle data frame for South Park ---------------------------------------
  df <- holiday_episodes %>% 
    filter(grepl("South Park", parent_primary_title, ignore.case = TRUE)) %>%
    arrange(year) %>%
    mutate(order = row_number()) %>%
    mutate(original_title = toupper(original_title))
  
  # create angled stripes ---------------------------------------------------
  # used the following as a reference to learn how to create rotating lines
  # https://jiwanheo.rbind.io/post/2021-05-14-how-to-rotate-lines-in-ggplot-to-make-art/
  
  data <- data.frame(x = 1, y = rep(1:8, times = 1))
  
  data <- data %>% 
    mutate(y = y - 3, yend = y + 3) %>% 
    mutate(y_mid = (y + yend) / 2) %>% 
    mutate(angle = 60 * pi / 180) %>% 
    mutate(xend = x + cos(angle), yend = y_mid + sin(angle), x = x - cos(angle), y = y_mid - sin(angle)) %>% 
    mutate(center_x = (x + xend) / 2, center_y = (y + yend) / 2)
  
  # combine df and data for plotting ----------------------------------------
  df2 <- cbind(data, df)
  
# create plot -------------------------------------------------------------
df2 %>% 
  ggplot() +
  geom_image(aes(x = 0.6, y = 9.25, image = image), size = 0.06, by = "height", asp = 1, hjust = 0) +
  geom_text(aes(x = 0.6, y = 8.5, label = "Average rating for Christmas episodes"), family = font, size = 2.9, color = "#000000", hjust = 0) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend, linewidth = average_rating), color = "#D03234") + 
  geom_richtext(aes(x = 1, y = y_mid, label = original_title), family = font, fontface = "bold", hjust = 0.5, vjust = 0.5, angle = 6.4, text.color = "#D03234", label.padding = unit(c(8, 6, 4, 6), "pt"), fill = "#FFFFFF", label.color = NA) +
  geom_text(aes(x = 1, y = y_mid + 0.35, label = year), family = font, fontface = "bold", size = 3.5, hjust = 0.5, vjust = 0.5, angle = 6.4, color = "#DDDDDD") +
  coord_cartesian(xlim = c(0.6, 1.4), ylim = c(NA, 9.25), clip = "off") + 
  theme_void() +
  theme(plot.caption = element_text(family = font, size = 7.25, hjust = 0.9, margin = margin(t = 0)),
        axis.title = element_blank(),
        legend.position = c(0.21, 0.84),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(family = font, hjust = 0.5, size = 7.5, color = "#000000"),
        plot.margin = unit(c(1, 0, 1, 0), "cm"),
        plot.background = element_rect(fill = "#FFFFFF")) +
  labs(caption = "#TidyTuesday | Data: imdb.com | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("sp_christmas_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

