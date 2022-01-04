# #TidyTuesday | January 4 - BYOD
# My running data from 2019, 2020 and 2021 using Strava plus a few weeks of manually collected data from early 2019

library(tidyverse)
library(rStrava)
library(httr)
library(ggtext)
library(showtext)
library(lubridate)

# add font
font_add_google(name = "Red Hat Display", family = "Red Hat Display")
font_add_google(name = "Red Hat Text", family = "Red Hat Text")

# turn on showtext
showtext_auto()

# Strava API instructions created using
# https://bldavies.com/blog/accessing-strava-api/

# credentials -------------------------------------------------------------
client_id <- XXXX
secret <- "XXXX"

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



# get old manual data pre-Strava ------------------------------------------
App_df <- read_csv("./data/10K App Data.csv")


# get Activity List ---------------------------------------------------
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

# combine Activity into a df and wrangle ----------------------------------------------
strava_df <- rbind_pages(strava_df_list) %>%
  mutate(distance_miles = distance * 0.00062137119224) %>%
  filter(type == "Run") %>%
  mutate(year = lubridate::year(start_date_local),
         month = lubridate::month(start_date_local),
         day = lubridate::day(start_date_local)) %>%
  filter(year < 2022) %>%
  select(distance_miles, year, month, day)

# combine dfs and wrangle ------------------------------------------------------------
data <- bind_rows(strava_df,App_df) %>%
  select(distance_miles, year) %>%
  mutate(mean = mean(distance_miles))

# plot data ---------------------------------------------------------------
data %>%
  ggplot(aes(x = distance_miles)) +
  geom_histogram(bins = 20, fill = "#000000", color = NA) +
  geom_vline(aes(xintercept = mean), col = "#64B5F6") +
  facet_wrap(~ year, nrow = 1) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(text = element_text(family = "Red Hat Text", color = "#000000"),
        plot.title = element_text(family = "Red Hat Display", size = 21, hjust = 0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.subtitle = element_textbox_simple(hjust = 0.5, lineheight = 1.3, padding = margin(10, 35, 10, 35)),
        strip.text.x = element_text(size = 10, color = "#000000", family = "Red Hat Display"),
        plot.caption = element_text(size = 8, hjust = 0.5, margin=margin(5,0,5,0)),
        axis.text = element_text(family = "Red Hat Text", color = "#000000"),
        axis.title.x = element_text(size = 10, family = "Red Hat Text", color = "#000000"),
        axis.title.y = element_text(size = 10, family = "Red Hat Text", color = "#000000"),
        panel.grid = element_line(color = "#d3d3d3"),
        plot.margin = unit(c(2.25,2.25,2.25,2.25), "cm"),
        plot.background = element_rect(color = "#f2f2f2", fill = "#f2f2f2")) +
  labs(title = "RUNNING FARTHER",
      subtitle = "The distribution of my runs by year has shifted with an increasing number of runs longer in distance than the <span style = 'color:#64B5F6;'>three-year mean average of 6.06 miles</span>.",
      caption = "\n#TidyTuesday | Data: Mostly Strava | Design: Ryan Hart",
      x = "Distance (miles)", 
      y = "Count of Runs")

# save plot
ggsave(paste0("RUNNING_FARTHER_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)

