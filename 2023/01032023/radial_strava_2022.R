# #TidyTuesday | January 3 | Bring your Own Data (BYOD)
# My running data from 2022 using Strava API

# load libraries ----------------------------------------------------------
library(tidyverse)
library(rStrava)
library(jsonlite)
library(httr)
library(showtext)
library(lubridate)
library(geomtextpath)

# add font ----------------------------------------------------------------
font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Work Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# Strava API instructions created using
# https://bldavies.com/blog/accessing-strava-api/

# credentials -------------------------------------------------------------
client_id <- enter client id here
secret <- "enter your secret here"

# OAuth application -------------------------------------------------------
app <- oauth_app("strava", client_id, secret)
endpoint <- oauth_endpoint(
  request = NULL,
  authorize = "https://www.strava.com/oauth/authorize",
  access = "https://www.strava.com/oauth/token"
)

# OAuth access token ------------------------------------------------------
token <- oauth2.0_token(endpoint, app, as_header = FALSE,
                        scope = "activity:read_all")

# get Activity List -------------------------------------------------------
strava_df_list <- list()
i <- 1
done <- FALSE
while (!done) {
  req <- GET(
    url = "https://www.strava.com/api/v3/athlete/activities",
    config = token,
    query = list(per_page = 200, page = i)
  )
  strava_df_list[[i]] <- fromJSON(content(req, as = "text"), flatten = TRUE)
  if (length(content(req)) < 200) {
    done <- TRUE
  } else {
    i <- i + 1
  }
}

# combine Activity into a df and wrangle ----------------------------------
strava_df <- rbind_pages(strava_df_list) %>%
  mutate(distance_miles = distance * 0.00062137119224) %>%
  filter(type == "Run") %>%
  mutate(date = lubridate::as_date(start_date_local)) %>% 
  mutate(year = lubridate::year(start_date_local),
         month = lubridate::month(start_date_local),
         day = lubridate::day(start_date_local)) %>%
  filter(year == 2022) %>%
  dplyr::select(distance_miles, year, month, day, date)

# create date scaffold ----------------------------------------------------
scaffold_df <- data.frame(date = date(seq(from = as.Date("2022-01-01"), to = as.Date("2022-12-31"), by = 1)))

# create df with scaffold to show all days --------------------------------
df <- full_join(strava_df, scaffold_df) %>% 
  mutate(distance_miles = ifelse(is.na(distance_miles), 0, distance_miles))

# create df to show points for marathons ----------------------------------
df_points <- df %>% 
  filter(distance_miles > 26.2)

# plot creates some warnings associated with the curved inset text --------
# not sure why and don't have the time to figure this out -----------------
# maybe come back to figure this out later --------------------------------

# plot data ---------------------------------------------------------------
df %>% 
ggplot() +
  
  # inset text
  geom_textpath(aes(x = as.Date("2022-01-01"), y = -3), label = "266 days of running", family = font, size = 5, color = "#FFFFFF") +
  geom_textpath(aes(x = as.Date("2022-07-02"), y = -3), label = "1,956 total miles", family = font, size = 5, color = "#FFFFFF") +
  geom_text(aes(x = as.Date("2022-7-02"), y = -15), label = "2022", family = font, size = 4, color = "#999999") +
  
  # month labels and directional arrows
  geom_textpath(aes(x = as.Date("2022-01-01"), y = 28.5), label = "January", family = font, size = 3, color = "#999999", hjust = 1) +
  annotate("segment", x = as.Date("2022-01-15"), xend = as.Date("2022-01-31"), y = 28.5, yend = 28.5, color = "#999999", arrow = arrow(length = unit(1.5, "mm"))) +
  geom_textpath(aes(x = as.Date("2022-07-01"), y = 28.5), label = "July", family = font, size = 3, color = "#999999", hjust = 1) +
  annotate("segment", x = as.Date("2022-07-10"), xend = as.Date("2022-07-26"), y = 28.5, yend = 28.5, color = "#999999", arrow = arrow(length = unit(1.5, "mm"))) +
  
  # mileage lines
  geom_texthline(yintercept = 5, label = "5 miles", size = 3, color = "#999999", hjust = 0.93, family = font) +
  geom_texthline(yintercept = 10, label = "10 miles", size = 3, color = "#999999", hjust = 0.93, family = font) +
  geom_texthline(yintercept = 15, label = "15 miles", size = 3, color = "#999999", hjust = 0.93, family = font) +
  geom_texthline(yintercept = 20, label = "20 miles", size = 3, color = "#999999", hjust = 0.93, family = font) +
  geom_texthline(yintercept = 26.2, label = "Marathon", size = 3, color = "#999999", linetype = "dashed", hjust = 0.93, family = font) +
  
  # data
  geom_segment(aes(x = date, xend = date, y = 0, yend = distance_miles), color = "#FFFFFF" ) +
  geom_point(data = df_points, aes(x = date, y = distance_miles), color = "#FFFFFF", size = 2) +
  
  # formatting
  ylim(c(-15, 29)) +
  coord_curvedpolar(theta = "x", clip = "off") +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 7, color = "#999999"),
        plot.caption.position = "plot",
        legend.position = "none",
        panel.grid = element_blank(),
        plot.margin = margin(0, 0, 0.5, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#28282B"),
        panel.background = element_rect(color = NA, fill = "#28282B")) +
  labs(caption = "#TidyTuesday | My 2022 Strava Data | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("radial_strava_2022_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


