# this is the code to generate the graphic in this post:
# https://twitter.com/Drew_Haugen/status/1753810579968475343

# load in packages
library(tidyverse)
library(mlbplotR)
# python script to pull data:
# https://colab.research.google.com/drive/14yYXK6e5VMa0G6dBmxhwnMkNiXdZoNpv?usp=sharing
# read in data (download from google drive after running python script)
game_info <- read_csv("path_to_data")
# create data to link venue and team
venue_with_team <- tribble(
  ~venue, ~team,
  "Oriole Park at Camden Yards", "BAL",
  "Angel Stadium", "LAA",
  "American Family Field", "MIL",
  "Oracle Park", "SF",
  "PNC Park", "PIT",
  "Comerica Park", "DET",
  "T-Mobile Park", "SEA",
  "Coors Field", "COL",
  "Chase Field", "AZ",
  "Rogers Centre", "TOR",
  "Guaranteed Rate Field", "CWS",
  "Citi Field", "NYM",
  "Truist Park", "ATL",
  "Busch Stadium", "STL",
  "Kauffman Stadium", "KC",
  "Citizens Bank Park", "PHI",
  "Target Field", "MIN",
  "Fenway Park", "BOS",
  "Progressive Field", "CLE",
  "Yankee Stadium", "NYY",
  "Nationals Park", "WSH",
  "Tropicana Field", "TB",
  "Great American Ball Park", "CIN",
  "Wrigley Field", "CHC",
  "Minute Maid Park", "HOU",
  "loanDepot park", "MIA",
  "Globe Life Field", "TEX",
  "Petco Park", "SD",
  "Oakland Coliseum", "OAK",
  "Dodger Stadium", "LAD"
) %>%
  mutate(
    venue_name_wrap = str_wrap(venue, width = 20)
  )
# choose colors for weather types
weather_colors <- scale_fill_manual(
  values = c(
    "Clear" = "#76EE00",
    "Cloudy" = "#1874CD",
    "Dome" = "#FF82AB",
    "Drizzle" = "#00BFFF",
    "Overcast" = "#708090",
    "Partly Cloudy" = "#C6E2FF",
    "Rain" = "#1E90FF",
    "Roof Closed" = "#EEB4B4",
    "Snow" = "#EEE9E9",
    "Sunny" = "#EEEE00"
  )
)
# create pie chart by stadium
game_info %>%
  filter(
    !venue_fixed %in% c(
      "Estadio Alfredo Harp Helu", "London Stadium", "Muncy Bank Ballpark"
    )
  ) %>% # drop stadiums where only a few games played
  mutate(
    venue_name_wrap = str_wrap(venue_fixed, width = 20)
  ) %>%
  summarize(
    total_weather_events = n(),
    .by = c(venue_name_wrap, weather_type)
  ) %>%
  mutate(
    total_games = sum(total_weather_events),
    .by = venue_name_wrap
  )
  mutate(
    weather_perc = total_weather_events / total_games,
    weather_type = factor(
      weather_type,
      levels = c(
        "Snow", "Dome", "Roof Closed", "Rain", "Drizzle",
        "Overcast", "Cloudy", "Partly Cloudy", "Sunny", "Clear"
      )
    )
  ) %>%
  ggplot() +
  geom_bar(
    aes(x = "", y = weather_perc, fill = weather_type), width = 1, color = "black", stat = "identity"
  ) +
  weather_colors +
  geom_mlb_dot_logos(
    aes(team_abbr = team),
    x = 0.5, y = 0, height = 0.3,
    data = venue_with_team
  ) +
  coord_polar(theta = "y") +
  facet_wrap(~ venue_name_wrap) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  labs(
    title = "Weather Distribution By MLB Ballpark",
    caption = "Data: MLB Stats API | Plot: @Drew_Haugen"
  )
# create bar plot
game_info %>%
  filter(
    !venue_fixed %in% c(
      "Estadio Alfredo Harp Helu", "London Stadium", "Muncy Bank Ballpark"
    )
  ) %>%
  mutate(
    venue_name_wrap = str_wrap(venue_fixed, width = 20),
    is_good_weather = if_else(
      weather_type %in% c("Sunny", "Clear", "Partly Cloudy"), 1, 0
    )
  ) %>%
  summarize(
    games = n(),
    total_good_weather = sum(is_good_weather),
    .by = venue_name_wrap
  ) %>%
  mutate(
    good_weather_perc = total_good_weather / games
  ) %>%
  left_join(venue_with_team, by = "venue_name_wrap") %>%
  ggplot(aes(reorder(team, -good_weather_perc), good_weather_perc)) +
  geom_col(aes(color = team, fill = team), alpha = 0.7, linewidth = 0.8) +
  scale_y_continuous(
    limits = c(0, 1), labels = scales::label_percent(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_color_mlb(type = "secondary") +
  scale_fill_mlb() +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_mlb_dot_logo(size = 0.8),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 13)
  ) +
  labs(
    y = "Good Weather%",
    title = "Frequency of Good Weather By MLB Ballpark",
    subtitle = "Good Weather Is Sunny, Clear, Or Partly Cloudy",
    caption = "Data: MLB Stats API | Plot: @Drew_Haugen"
  )