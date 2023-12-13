# #TidyTuesday | 2023-12-12 | Holiday Movies
# Data source comes from the Internet Movie Database

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# load data --------------------------------------------------------------
holiday_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movies.csv')
holiday_movie_genres <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-12/holiday_movie_genres.csv')


# create a list of 12 major colors to use for a search --------------------
colors <- c("red", "yellow", "blue", "orange", "green", "purple", "black", "white", "pink", "grey", "brown")


# search simple title using the colors list -------------------------------
# df <- holiday_movies[grep(paste0("\\b", paste(colors, collapse = "\\b|\\b"), "\\b"), holiday_movies[["simple_title"]], ignore.case = TRUE), ]

# count titles that match by a primary color ------------------------------
result_df <- data.frame(Color = colors, Count = 0)


for (color in colors) {
  color_matches <- holiday_movies[grep(paste0("\\b", color, "\\b"), holiday_movies[["primary_title"]], ignore.case = TRUE), ]
  
  result_df[result_df$Color == color, "Count"] <- nrow(color_matches)
}

# filter out colors with 0 ------------------------------------------------
df <- result_df %>% 
  filter(Count > 0)

# create plot -------------------------------------------------------------
df %>% 
  arrange(desc(Count)) %>% 
  mutate(Color = factor(Color, levels = Color)) %>% 
  ggplot() +
  geom_point(aes(x = Color, y = 0, fill = Color), size = 21, shape = 21, stroke = 1.5) +
  geom_text(aes(x = Color, y = 0, label = Count, color = ifelse(Color %in% c("black", "blue", "red", "brown"), "#FFFFFF", "#000000")), size = 10) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_void() +
  theme(text = element_text(size = 9, family = font, color = "#000000"),
        plot.title = element_text(family = font, size = 30, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 17, hjust = 0.5, margin = margin(t = 5, b = 5)),
        plot.caption = element_text(family = font, size = 8, hjust = 0.5, margin = margin(t = 10)),
        strip.text = element_text(size = 10, family = font, color = "#000000", margin = margin(t = 1, b = 1)),
        axis.title = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(fill = "#FFFFFF")) +
  labs(title = "NUMBER OF COLORS",
       subtitle = "found in 2,265 holiday movie titles",
       caption = "#TidyTuesday | Data: imdb.com | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("colors_holiday_movies_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


