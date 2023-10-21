# #TidyTuesday | 2023-10-17 | Taylor Swift
# Data Source comes from taylor R package from W. Jake Thompson

# Code is a bit clunky and
# I'd probably approach this
# differently if I were to start over

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(ggfx)
library(ggtext)
library(ggnewscale)

# add font ----------------------------------------------------------------
font_add_google(name = "Satisfy", family = "Satisfy")
font_t <- "Satisfy"

font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

font_add_google(name = "Noto Sans Symbols 2", family = "Noto Sans Symbols 2")
font_s <- "Noto Sans Symbols 2"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# load data --------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 42)
taylor_all_songs <- tuesdata$taylor_all_songs
taylor_albums <- tuesdata$taylor_albums

# wrangle data and create df ----------------------------------------------
df <- taylor_all_songs %>% 
  select(album_name) %>% 
  filter(!grepl("Taylor's Version", album_name, ignore.case = TRUE)) %>% 
  unique() %>% 
  slice(1:10) %>% 
  mutate(count = nchar(album_name))

# df for beads on the left side -------------------------------------------
df_l <- taylor_all_songs %>% 
  select(album_name, danceability, energy, instrumentalness, acousticness) %>% 
  group_by(album_name) %>% 
  mutate(danceability = mean(danceability, na.rm = TRUE), 
         energy = mean(energy, na.rm = TRUE),
         instrumentalness = mean(instrumentalness, na.rm = TRUE),
         acousticness = mean(acousticness, na.rm = TRUE)) %>% 
  filter(!grepl("Taylor's Version", album_name, ignore.case = TRUE)) %>% 
  select(album_name, danceability, energy, instrumentalness, acousticness) %>% 
  unique() %>% 
  ungroup() %>% 
  slice(1:10) %>% 
  mutate(count = 4) %>% 
  mutate(row = row_number())

# df for beads on the right side ------------------------------------------
df_r <- taylor_all_songs %>% 
  select(album_name, liveness, valence, speechiness, loudness) %>% 
  group_by(album_name) %>% 
  mutate(speechiness = mean(speechiness, na.rm = TRUE),
         liveness = mean(liveness, na.rm = TRUE), 
         loudness = mean(loudness, na.rm = TRUE), 
         valence = mean(valence, na.rm = TRUE)) %>% 
  filter(!grepl("Taylor's Version", album_name, ignore.case = TRUE)) %>% 
  select(album_name, speechiness, liveness, valence, loudness) %>% 
  unique() %>% 
  ungroup() %>% 
  slice(1:10) %>% 
  mutate(count = 4) %>% 
  mutate(row = row_number())



df_map_l <- df_l %>% 
  select(2:5, 7) %>% 
  pivot_longer(cols = c("danceability", "energy", "instrumentalness", "acousticness"),
               names_to = "name",
               values_to = "value") %>% 
  mutate(col = case_when(name == "danceability" ~ 2,
                         name == "energy" ~ 3,
                         name == "instrumentalness" ~ 4,
                         name == "acousticness" ~ 1))

df_map_r <- df_r %>% 
  select(2:5, 7) %>% 
  pivot_longer(cols = c("speechiness", "liveness", "loudness", "valence"),
               names_to = "name",
               values_to = "value") %>% 
  mutate(col = case_when(name == "speechiness" ~ 3,
                         name == "liveness" ~ 1,
                         name == "loudness" ~ 2,
                         name == "valence" ~ 4))

# create bracelets --------------------------------------------------------

# outer shape -------------------------------------------------------------
# function to create the word bead shape ----------------------------------
shape_function <- function(x0 = 0, y0 = 0, radius, row, group_id) {
  r <- function(radius, theta){
    radius/(1 - sin(2 * theta)^2/2)^(1/4)
  }
  tibble(angle = seq(0, 2 * pi, length.out = 1000)) %>% 
    mutate(rvec = r(radius, angle)) %>% 
    mutate(x = rvec * cos(angle) + x0) %>% 
    mutate(y = rvec * sin(angle) + y0) %>% 
    mutate(row = row) %>% 
    mutate(group_id = group_id)
  }

# number of character shapes,  one album per row --------------------------
num_shapes_per_row <- df$count

# create df for the word shape data ---------------------------------------
shape_data <- tibble(
  row = rep(1:length(num_shapes_per_row), times = num_shapes_per_row),
  col = sequence(num_shapes_per_row),
  radius = rep(0.5, sum(num_shapes_per_row)),
  group_id = 1:sum(num_shapes_per_row))

# Calculate the x0 and y0 positions based on the number of shapes in each row
shape_data <- shape_data %>%
  group_by(row) %>%
  mutate(
    x0 = (col - 1 - (num_shapes_per_row[row] - 1) / 2) * 1.2,
    y0 = -row * 1.2) %>%
  ungroup()

# generate shapes and bind together in a df -------------------------------
shapes <- shape_data %>%
  rowwise() %>%
  do(shape_function(.$x0, .$y0, .$radius, .$row, .$group_id)) %>%
  ungroup()

# inner shape -------------------------------------------------------------
smaller_radius <- 0.4

# Create a data frame with shapes arranged based on the list
shape_data2 <- tibble(
  row = rep(1:length(num_shapes_per_row), times = num_shapes_per_row),
  col = sequence(num_shapes_per_row),
  radius = rep(smaller_radius, sum(num_shapes_per_row)),  # Use the smaller radius
  group_id = 1:sum(num_shapes_per_row))

# Calculate the x0 and y0 positions based on the number of shapes in each row
shape_data2 <- shape_data2 %>%
  group_by(row) %>%
  mutate(
    x0 = (col - 1 - (num_shapes_per_row[row] - 1) / 2) * 1.2,
    y0 = -row * 1.2) %>%
  ungroup()

# Generate the shapes and bind them into a single data frame
shapes2 <- shape_data2 %>%
  rowwise() %>%
  do(shape_function(.$x0, .$y0, .$radius, .$row, .$group_id)) %>%
  ungroup()

# calculate the centroid for text placement -------------------------------
centroids <- shapes %>%
  group_by(group_id) %>%
  summarize(x = mean(x), y = mean(y))

# get album names into characters -----------------------------------------
word_string <- data.frame(string = paste0(df$album_name, collapse = ''))

word_letters <- str_split(word_string$string, "") 
word_df <- data.frame(letters = Reduce(rbind, word_letters))

letters_df <- cbind(centroids, word_df)

symbols_df <- letters_df %>% 
  filter(row_number() %in% c(7, 26)) %>% 
  mutate(letters = replace(letters, row_number() == 1, "♥")) %>% 
  mutate(letters = replace(letters, row_number() == 2, "★"))

# create other shapes -----------------------------------------------------

# min and max for placement -----------------------------------------------
df_min <- shapes %>% 
  group_by(row) %>% 
  slice(which.min(x))

df_max <- shapes %>% 
  group_by(row) %>% 
  slice(which.max(x))


beads_function <- function(x0 = 0, y0 = 0, radius, row, col, group_id) {
  r <- function(radius, theta){
    radius/(1 - sin(2 * theta)^2/2)^(1/4)
  }
  tibble(angle = seq(0, 2 * pi, length.out = 1000)) %>% 
    mutate(rvec = r(radius, angle)) %>% 
    mutate(x = rvec * cos(angle) * 0.95 + x0) %>% 
    mutate(y = rvec * sin(angle) + y0) %>% 
    mutate(row = row) %>% 
    mutate(col = col) %>% 
    mutate(group_id = group_id)
}

# LEFT SIDE BEADS ---------------------------------------------------------

# number of rows of beads,  one album per row --------------------------
num_beads_per_row_l <- df_l$count

# create df for the bead shape data ---------------------------------------
beads_data_l <- tibble(
  row = rep(1:length(num_beads_per_row_l), times = num_beads_per_row_l),
  col = sequence(num_beads_per_row_l),
  radius = rep(0.3, sum(num_beads_per_row_l)),
  group_id = 1:sum(num_beads_per_row_l))

# Calculate the x0 and y0 positions based on the number of shapes in each row
beads_data_l <- beads_data_l %>%
  group_by(row) %>%
  mutate(
    x0 = (col - df_max$x[row] - (num_beads_per_row_l[row] - 1) / 2) - 3.1,
    y0 = -row * 1.2) %>%
  ungroup()

# generate shapes and bind together in a df -------------------------------
beads_data_l <- beads_data_l %>%
  rowwise() %>%
  do(beads_function(.$x0, .$y0, .$radius, .$row, .$col, .$group_id)) %>%
  ungroup()

left_beads_df <- left_join(beads_data_l, df_map_l) 

# create df to find the max value to highlight ----------------------------
left_beads_df_max <- left_beads_df %>% 
  group_by(name) %>%
  arrange(desc(value)) %>%
  filter(value == max(value)) %>% 
  group_by(name, group_id) %>%
  summarize(x = mean(x), y = mean(y))

# create df to find the min value to highlight ----------------------------
left_beads_df_min <- left_beads_df %>% 
  group_by(name) %>%
  arrange(desc(value)) %>%
  filter(value == min(value)) %>% 
  group_by(name, group_id) %>%
  summarize(x = mean(x), y = mean(y))

# calculate the centroid for placement ------------------------------------
centroids_l <- left_beads_df %>%
  group_by(name, group_id) %>%
  filter(group_id %in%(1:4)) %>% 
  summarize(x = mean(x), y = mean(y))

# RIGHT SIDE BEADS -------------------------------------------------------

# number of rows of beads,  one album per row ----------------------------
num_beads_per_row_r <- df_r$count

# create df for the bead shape data ---------------------------------------
beads_data_r <- tibble(
  row = rep(1:length(num_beads_per_row_r), times = num_beads_per_row_r),
  col = sequence(num_beads_per_row_r),
  radius = rep(0.3, sum(num_beads_per_row_r)),
  group_id = 1:sum(num_beads_per_row_r))

# Calculate the x0 and y0 positions based on the number of shapes in each row
beads_data_r <- beads_data_r %>%
  group_by(row) %>%
  mutate(
    x0 = (col - df_min$x[row] - (num_beads_per_row_r[row] - 1) / 2) + 1.1,
    y0 = -row * 1.2) %>%
  ungroup()

# generate shapes and bind together in a df -------------------------------
beads_data_r <- beads_data_r %>%
  rowwise() %>%
  do(beads_function(.$x0, .$y0, .$radius, .$row, .$col, .$group_id)) %>%
  ungroup()

right_beads_df <- left_join(beads_data_r, df_map_r) 

# create df to find the max value to highlight ----------------------------
right_beads_df_max <- right_beads_df %>% 
  group_by(name) %>%
  arrange(desc(value)) %>%
  filter(value == max(value)) %>% 
  group_by(name, group_id) %>%
  summarize(x = mean(x), y = mean(y))

# create df to find the min value to highlight ----------------------------
right_beads_df_min <- right_beads_df %>% 
  group_by(name) %>%
  arrange(desc(value)) %>%
  filter(value == min(value)) %>% 
  group_by(name, group_id) %>%
  summarize(x = mean(x), y = mean(y))


# calculate the centroid for placement ------------------------------------
centroids_r <- right_beads_df %>%
  group_by(name, group_id) %>%
  filter(group_id %in%(1:4)) %>% 
  summarize(x = mean(x), y = mean(y))

# create the string -------------------------------------------------------
# get the rows for each bracelet ------------------------------------------
line_group_values <- centroids %>% 
  select(y) %>% 
  unique() %>% 
  rename(y_adj = y) %>% 
  mutate(group_id = row_number())

# the string --------------------------------------------------------------

x <- seq(-12, 12, by = 0.1)
y = 0.07 * sin(0.5 * x)

line_df <- data.frame(x, y)

line_df <- do.call(rbind, replicate(10, line_df, simplify = FALSE))

line_df$group_id <- rep(1:10, each = length(x))

test_df <- left_join(line_df, line_group_values) %>% 
  mutate(y = y + y_adj)

# create plot -------------------------------------------------------------
shapes %>% 
  ggplot(aes(x = x, y = y, group = group_id)) +
  
  # string
  with_shadow(geom_line(data = test_df, aes(x, y), linewidth = 0.5), sigma = 3, x_offset = 3, y_offset = 3) +
  
  # letter beads
  with_shadow(geom_polygon(fill = "#F5F5F5"), sigma = 3, x_offset = 3, y_offset = 3) +
  geom_polygon(data = shapes2, aes(x, y), fill = "#FFFFFF") +
  
  # letters
  geom_text(data = letters_df, aes(x, y, label = letters), family = font, fontface = "bold", size = 4.5) +
  geom_text(data = symbols_df, aes(x, y, label = letters, color = letters), family = font_s, size = 4) +
  scale_color_manual(values = c("#FFD700", "#E53935")) +
  
  # left side beads
  with_shadow(geom_polygon(data = left_beads_df %>% filter(col == 1), aes(x, y, fill = value)), sigma = 3, x_offset = 3, y_offset = 3) +
  scale_fill_gradient(low = "#64B5F6", high = "#0D47A1") + # Blue
  new_scale_fill() +
  with_shadow(geom_polygon(data = left_beads_df %>% filter(col == 2), aes(x, y, fill = value)), sigma = 3, x_offset = 3, y_offset = 3) +
  scale_fill_gradient(low = "#81C784", high = "#1B5E20") + # Green
  new_scale_fill() +
  with_shadow(geom_polygon(data = left_beads_df %>% filter(col == 3), aes(x, y, fill = value)), sigma = 3, x_offset = 3, y_offset = 3) +
  scale_fill_gradient(low = "#FFB74D", high = "#FF6F00") + # Orange
  new_scale_fill() +
  with_shadow(geom_polygon(data = left_beads_df %>% filter(col == 4), aes(x, y, fill = value)), sigma = 3, x_offset = 3, y_offset = 3) +
  scale_fill_gradient(low = "#F06292", high = "#880E4F") + # Pink
  new_scale_fill() +

  # right side beads
  with_shadow(geom_polygon(data = right_beads_df %>% filter(col == 1), aes(x, y, fill = value)), sigma = 3, x_offset = 3, y_offset = 3) +
  scale_fill_gradient(low = "#E57373", high = "#B71C1C") + # Red
  new_scale_fill() +
  with_shadow(geom_polygon(data = right_beads_df %>% filter(col == 2), aes(x, y, fill = value)), sigma = 3, x_offset = 3, y_offset = 3) +
  scale_fill_gradient(low = "#BA68C8", high = "#4A148C") + # Purple
  new_scale_fill() +
  with_shadow(geom_polygon(data = right_beads_df %>% filter(col == 3), aes(x, y, fill = value)), sigma = 3, x_offset = 3, y_offset = 3) +
  scale_fill_gradient(low = "#4DB6AC", high = "#004D40") + # Teal
  new_scale_fill() +
  with_shadow(geom_polygon(data = right_beads_df %>% filter(col == 4), aes(x, y, fill = value)), sigma = 3, x_offset = 3, y_offset = 3) +
  scale_fill_gradient(low = "#DCE775", high = "#33691E") + # Lime
  new_scale_fill() +
  
  geom_text(data = left_beads_df_max, aes(x, y, label = "▲"), color = "#FFFFFF", size = 3.5, alpha = 0.7) +
  geom_text(data = right_beads_df_max, aes(x, y, label = "▲"), color = "#FFFFFF", size = 3.5, alpha = 0.7) +
  
  geom_text(data = left_beads_df_min, aes(x, y, label = "▼"), color = "#FFFFFF", size = 3.5, alpha = 0.9) +
  geom_text(data = right_beads_df_min, aes(x, y, label = "▼"), color = "#FFFFFF", size = 3.5, alpha = 0.9) +
  
  # labels
  geom_text(data = centroids_l, aes(x = x + 0.1, y = -0.2, label = name), color = "#000000", size = 2.5, angle = 45, hjust = "bottom") +
  geom_text(data = centroids_r, aes(x = x + 0.1, y = -0.2, label = name), color = "#000000", size = 2.5, angle = 45, hjust = "bottom") +
  
  # How to read
  annotate("richtext", x = 0, y = -14, label = "How to read: Colors go light (lowest value) to dark (highest value) and <span style='font-size:12px;'>▲</span> = highest avg value / <span style='font-size:12px;'>▼</span> = lowest avg value", size = 2.4, hjust = 0.5, fill = NA, label.color = NA) +

  scale_y_continuous(limits = c(-14, 2)) +
  scale_x_continuous(limits = c(-12, 12)) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 30, color = "#000000", hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "#000000", size = 9, hjust = 0.5, margin = margin(t = 5, b = 15)),
        legend.position = "none", 
        plot.caption.position = "plot",
        plot.caption = element_text(size = 8.5, hjust = 0.5, margin = margin(t = 30)),
        plot.margin = unit(c(1, 0, 1, 0), "cm"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "Taylor Swift",
       subtitle = "Crafting Swiftie friendship bracelets to visualize average Spotify song attributes for each album", 
       caption = "Data: Spotify via {taylor} R package | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("swift_bracelets_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)



