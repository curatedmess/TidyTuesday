library(ggplot2)
library(maps)


# URL for colors ----------------------------------
#img_url <- c("https://i.etsystatic.com/32867869/r/il/83c563/4064177108/il_fullxfull.4064177108_bokv.jpg")
img_url <- c("https://i.etsystatic.com/23275689/r/il/70dab9/4406102383/il_1588xN.4406102383_jwce.jpg")

# reduce number of colors for each image in list of URLs ------------------
# set number of colors ----------------------------------------------------
n_colors <- 32

# loop to get colors for each image in list -------------------------------
# code adopted from https://www.r-bloggers.com/2019/01/extracting-colours-from-your-images-with-image-quantization/
lapply(img_url, function(x) {
  
  image_read(x) %>% 
    image_quantize(max = n_colors) %>% 
    magick2cimg() %>%
    RGBtoHSV() %>% 
    as.data.frame(wide = "c") %>% 
    mutate(hex = hsv(rescale(c.1, from = c(0, 360)), c.2, c.3), hue = c.1, sat = c.2, value = c.3) %>%
    count(hex, hue, sat, value, sort = TRUE)
  
}) -> data

# create data frame -------------------------------------------------------
df_colors <- do.call(rbind, data)

# ordering colors using Lab Color Space -----------------------------------
# code adopted from this link ---------------------------------------------
# https://stackoverflow.com/questions/61193516/how-to-sort-colours --------

rgb_df <- col2rgb(df_colors$hex) 

lab_df <- convertColor(t(rgb_df), 'sRGB', 'Lab')
df_colors_ordered <- data.frame(hex = df_colors$hex[order(lab_df[, 'L'])]) %>% 
  mutate(id = row_number())

# random background points colors -----------------------------------------
df_colors <- df_colors_ordered %>% 
  filter(!hex == random_background_colors$hex)


# Load the map of the United States
us_map <- map_data("usa")


usa_sf <- ne_states(country = "United States of America", returnclass = "sf") %>% 
  filter(name == "Texas")


p = st_polygon(list(as.matrix(us_map)))

test <- st_coordinates(us_map)

wb <- 0.015
buf <- st_buffer(st_union(usa_sf), wb)


# create plot -------------------------------------------------------------
ggplot() +
  
  geom_sf(data = buf, color = "black", fill = "grey") +
  geom_sf(data = usa_sf, color = "red", fill = NA) +
  # geom_sf(data = points_sf, size = 3, alpha = 0.75) +
  # annotate(geom = "richtext", y = 48.5, x = -107, label = "<span style='color:#FFFFFF;'>During the<br>2022 basesball season,<br>19.1 million hot dogs<br>and 5 million sausages<br>were sold at the<br><span style='color:#000000;'><b>thirty MLB stadiums</b></span><br>across the<br>United States and Canada.", hjust = "center", family = font, size = 5, fill = NA, label.color = NA, lineheight = 1.2) +
  coord_sf() +
  theme_void()

# save plot ---------------------------------------------------------------
ggsave(paste0("neon_sign_map_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)




ggplot(data = us_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = sample(colors_8bit, nrow(us_map), replace = TRUE),
               color = "black", size = 0.5) +
  theme_void() +
  labs(title = "8-Bit-Style Map of the United States")
