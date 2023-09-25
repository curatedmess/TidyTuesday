# #TidyTuesday | 2023-09-19 | CRAN Package Authors
# Data Source comes from the CRAN collaboration graph, a project by David Schoch

# libraries ---------------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999) 

# Load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 38)

cran_20230905 <- tuesdata$cran_20230905
package_authors <- tuesdata$package_authors

# create data frames ------------------------------------------------------
# How many packages have a dependency? ------------------------------------
# df_check <- cran_20230905 %>% 
#   filter(!is.na(Depends)) %>% # remove NA, assume no dependency
#   mutate(Depends = strsplit(as.character(Depends), ", ")) %>%
#   unnest(Depends) %>% 
#   mutate(Depends = strsplit(as.character(Depends), ",")) %>%
#   unnest(Depends) %>% 
#   mutate(Depends = str_split(Depends, " ", simplify = TRUE)[, 1]) %>%
#   mutate(Depends = gsub("[(>=]", "", Depends)) %>% 
#   #filter(!Depends == "R") %>%  # remove R
#   select(Package) %>% 
#   unique() # 14,620 unique packages with at least one dependency that isn't just R

# create data frame with dependent packages -------------------------------
df_depends <- cran_20230905 %>% 
  filter(!is.na(Depends)) %>% # remove NA, assume no dependency
  mutate(Depends = strsplit(as.character(Depends), ", ")) %>%
  unnest(Depends) %>% 
  mutate(Depends = strsplit(as.character(Depends), ",")) %>%
  unnest(Depends) %>% 
  mutate(Depends = str_split(Depends, " ", simplify = TRUE)[, 1]) %>%
  mutate(Depends = gsub("[(>=]", "", Depends)) %>% 
  select(Package, Depends)

# create data frame with dependent package authors first name -------------
df_depends_name <- df_depends %>% 
  select(Depends) %>% 
  unique() %>% # 1,764 unique packages that are dependencies
  left_join(package_authors, by = join_by(Depends == Package)) %>% 
  extract(authorsR, c("First_Name", "Last_Name"), "([^ ]+) (.*)") %>%
  mutate(David = ifelse(First_Name == "David", "yes", "no")) %>%
  group_by(Depends) %>% 
  summarise(check_david = any(David == "yes", na.rm = TRUE)) #1,764 dependent packages

# join data frames --------------------------------------------------------
df <- df_depends %>% 
  left_join(df_depends_name, by = "Depends") %>% 
  group_by(Package) %>% 
  summarise(check_david = any(check_david == "TRUE", na.rm = TRUE)) %>% #4,812
  ungroup() %>% 
  group_by(check_david) %>% 
  mutate(count = n()) %>% 
  arrange(desc(check_david))

# create points -----------------------------------------------------------
num <- 14620

x <- runif(num)
y <- runif(num)

grid <- data.frame(x = x, y = y) %>% 
  arrange(x)

# combine df to points ----------------------------------------------------
df_test <- cbind(df, grid)

# create data frame for numbers -------------------------------------------
df_text <- df_test %>% 
  select(check_david, count, x) %>% 
  group_by(check_david, count) %>% 
  summarise(location = mean(x))

# create plot -------------------------------------------------------------
df_test %>% 
  ggplot() +
  geom_point(aes(x, y, color = check_david), size = 0.01, alpha = 1) +
  geom_text(data = df_text, aes(x = location, y = 1.15, label = paste0(scales::comma(count)), color = check_david), size = 4, family = font, fontface = "bold") +
  scale_y_continuous(limits = c(-2.5, 3)) +
  scale_color_manual(values = c("#7E7E7E", "#FFFFFF")) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 12, hjust = 0.5, color = "#FFFFFF", face = "bold"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, family = font, color = "#FFFFFF", hjust = 0.5, margin = margin(t = 0)),
        legend.position = "none",
        plot.margin = unit(c(2, 1, 2, 1), "cm"),
        plot.background = element_rect(color = NA, fill = "#1B1212")) +
  labs(title = "About 6% of R packages on CRAN have a dependency\non another R package with an author named David.",
    caption = "#TidyTuesday | Data: CRAN collaboration graph by David Schoch | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("david_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)
  
