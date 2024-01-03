# #TidyTuesday | January 2 | Bring your Own Data (BYOD)
# My running data from 2020, 2021, 2023, 2023 using Strava API

# load libraries ----------------------------------------------------------
library(tidyverse)
library(rStrava)
library(jsonlite)
library(httr)
library(showtext)
library(lubridate)
# library(ggchicklet)

# add font ----------------------------------------------------------------
font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Work Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# Strava API instructions created using
# https://bldavies.com/blog/accessing-strava-api/

# # credentials -------------------------------------------------------------
# client_id <- enter client id here
# secret <- "enter your secret here"

client_id <- 44244

secret <- "6801131f54f1f85f9651e93f6fb7659dfe0df0b6"

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
  filter(year >= 2020) %>%
  dplyr::select(distance_miles, year, month, day, date)

# create date scaffold ----------------------------------------------------
scaffold_df <- data.frame(date = date(seq(from = as.Date("2020-01-01"), to = as.Date("2023-12-31"), by = 1)))

# create df with scaffold to show all days --------------------------------
df_data <- full_join(strava_df, scaffold_df) %>% 
  mutate(distance_miles = ifelse(is.na(distance_miles), 0, distance_miles)) %>% 
  group_by(date) %>%
  summarise(sum_distance = sum(distance_miles)) %>%
  distinct(date, .keep_all = TRUE) %>% 
  ungroup() %>% 
  mutate(color = case_when(sum_distance == 0 ~ "#171c22",
                           sum_distance > 0 & sum_distance < 6 ~ "#0E4429",
                           sum_distance > 6 & sum_distance < 12 ~ "#006D32",
                           sum_distance > 12 & sum_distance < 20 ~ "#26A642",
                           sum_distance > 20 ~ "#39D354"))

# create data frame for calendar data -------------------------------------

# start and end date variables --------------------------------------------
start_day <- as.Date("2020-01-01")
end_day <- as.Date("2023-12-31")

df_grid <- tibble(date = seq(start_day, end_day, by = "1 day")) %>% 
  mutate(year = year(date),
         month_abb = month(date, label = TRUE, abbr = TRUE),
         day = wday(date, label = TRUE),
         first_day_of_year = floor_date(date, "year"),
         week_of_year = as.integer((date - first_day_of_year + wday(first_day_of_year) - 1) / 7) + 1) %>%  
  left_join(df_data) %>% 
  group_by(year) %>% 
  arrange(date) %>% 
  mutate(num = row_number())

# create additional data frames for other elements ------------------------

# x-axis labels -----------------------------------------------------------
df_labels <- df_grid %>% 
  group_by(year, month_abb) %>% 
  arrange(date) %>%
  filter(week_of_year == 1 | day == "Sun") %>% 
  slice(1) 


# y-axis labels -----------------------------------------------------------
df_labels_y <- df_grid %>% 
  filter(day %in% c("Mon", "Wed", "Fri"))


# legend color objects ----------------------------------------------------
df_legend <- data.frame(y = c(-1, -1, -1, -1, -1),
                        x = c(44, 45, 46, 47, 48),
                        color = c("#171c22", "#0E4429", "#006D32", "#26A642", "#39D354"))

# legend labels -----------------------------------------------------------
df_legend_labels <- data.frame(y = c(-1, -1),
                               x = c(43, 49),
                               label = c("Less", "More"),
                               hjust = c(1, 0))

# create data frame for total number of runs per year ---------------------
df_total <- df_grid %>% 
  mutate(year = year(date)) %>% 
  filter(sum_distance > 0) %>% 
  group_by(year) %>% 
  summarise(total = n())

# create data frame for total miles per year ------------------------------
df_total_miles <- df_grid %>% 
  mutate(year = year(date)) %>% 
  filter(sum_distance > 0) %>% 
  group_by(year) %>% 
  summarise(miles = round(sum(sum_distance), 1))

# set factor --------------------------------------------------------------
df_grid$day <- factor(df_grid$day, c("Sat", "Fri", "Thu", "Wed", "Tue", "Mon", "Sun"))

# create plot -------------------------------------------------------------
ggplot() +
  statebins:::geom_rtile(data = df_grid, aes(y = day, x = week_of_year, fill = color), radius = unit(1.75, "pt"), color = "#0d1117", size = 1) +
  statebins:::geom_rtile(data = df_legend, aes(y = y, x = x, fill = color), radius = unit(1.75, "pt"), color = "#0d1117", size = 1) +
  # geom_text(data = df_grid, aes(y = day, x = week_of_year, label = num), family = font, color = "red", size = 2) +
  geom_text(data = df_total, aes(x = -2, y = 10, label = paste0(total, " runs in ", year)), family = font, hjust = 0, color = "#FFFFFF", size = 4) +
  geom_text(data = df_labels, aes(x = week_of_year, y = 8, label = month_abb), family = font, hjust = 0.3, color = "#FFFFFF", size = 3, check_overlap = TRUE) +
  geom_text(data = df_labels_y, aes(x = -1.9, y = day, label = day), family = font, color = "#FFFFFF", size = 3, hjust = 0, check_overlap = TRUE) +
  geom_text(data = df_legend_labels, aes(x, y, label = label, hjust = hjust), family = font, color = "#848d97", size = 3) +
  geom_text(data = df_total_miles, aes(x = 0, y = -1, label = paste0("Total number of runs contributed to ", scales::comma(miles), " miles")), family = font, color = "#848d97", size = 4, hjust = 0) +
  scale_y_discrete(breaks = c("Mon", "Wed", "Fri")) +
  expand_limits(y = c(-2, 12)) +
  scale_x_continuous(expand = c(-2, NA)) +
  scale_fill_identity() +
  facet_wrap(~factor(year, levels = c(2023, 2022, 2021, 2020)), ncol = 1) +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 22, hjust = 0, color = "#FFFFFF"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#FFFFFF", margin = margin(t = 8, b = 10)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#FFFFFF", hjust = 0.5, margin = margin(t = 25)),
        legend.position = "none",
        plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#0d1117")) +
  labs(title = "Running Activity",
       subtitle = "Activity is based on the total distance I ran each day and grouped into five categories: 0 miles, < 6 miles,\n6 to 12 miles, 12 to 20 miles, and > 20 miles",
       caption = "#TidyTuesday | Data: My Strava  Data | Design: Ryan Hart")
  
# save plot ---------------------------------------------------------------
ggsave(paste0("runs_github_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 10)

