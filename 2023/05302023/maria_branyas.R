# #TidyTuesday | 2023-05-30 | Verified Oldest People
# Data Source is wikipedia.org via frankiethull

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font <- "Source Sans Pro"

font_add_google(name = "Satisfy", family = "Satisfy")
font_s <- "Satisfy"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
centenarians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv')

# create data frame to get all the radians for each year of age -----------
df_radius <- centenarians %>% 
  select(name, birth_date, gender, still_alive, place_of_death_or_residence) %>% 
  filter(name == "Maria Branyas") %>% 
  mutate(birth_date = as.Date(birth_date)) %>%
  mutate(today = as.Date(Sys.Date())) %>% 
  mutate(years = trunc((birth_date %--% today) / years(1))) %>% 
  mutate(id = row_number()) %>% 
  group_by(name) %>% 
  slice(rep(1:n(), each = years)) %>% 
  mutate(x = 1, y = 1, r = 0.1) %>%
  mutate(id = row_number()) %>%
  mutate(r = accumulate(r, ~.x + (runif(n = 1, min = 0.07, max = 0.2)))) %>%
  ungroup() %>% 
  select(r)

# function to create tree rings -------------------------------------------
points = 100

apply(df_radius, 1, function(x){
  
  set.seed(115)
  data.frame(matrix(NA, nrow = points)) %>% 
    mutate(t = seq(0, (2 * pi), length.out = points)) %>% 
    mutate(id = row_number()) %>% 
    mutate(r = x) %>%
    mutate(noise = runif(points)) %>%
    mutate(adjustment = r + cos(id * t + 2 * sqrt(r)) * r/100) %>% 
    mutate(x = 0 + adjustment * cos(t)) %>%
    mutate(y = 0 + adjustment * sin(t))
  
}) -> data

df <- do.call(rbind, data) %>% 
  mutate(space = ifelse(id %in% c(22:23), "#F1BF00", "#AA151B"))


# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_path(aes(group = r, color = space)) +
  geom_path(data = . %>% filter(r > 15), aes(color = space, group = r), linewidth = 4) +
  annotate("text", x = -20, y = -14, label = "116 Tree Rings", family = font_s, size = 4, hjust = 0, color = "#000000") +
  scale_color_identity() +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 40, hjust = 0, face = "bold", color = "#000000", margin = margin(b = 2)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 9, hjust = 0, color = "#000000", margin = margin(b = 10)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 20)),
        strip.text = element_blank(), 
        legend.position = "none",
        plot.margin = unit(c(0.5, 0, 0.5, 0), "cm"),
        panel.background = element_rect(color = NA, fill = "#F1BF00"),
        plot.background = element_rect(color = NA, fill = "#F1BF00")) +
  labs(title = "Maria Branyas",
       subtitle = " World's oldest known living person at 116 years young",
       caption = "#TidyTuesday | Data: wikipedia.org via frankiethull | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("maria_branyas_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

