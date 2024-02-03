# this is the code to generate the graphic in this post:
# https://twitter.com/Drew_Haugen/status/1691208636918329344

#load libraries
library(tidyverse)
library(mlbplotR)

# read in data
savant_df <- read_csv("your_path_here")

# create is_strike column and find coefficients
fb_strike_slope <- savant_df %>%
  filter(
    pitch_name %in% c("4-Seam Fastball", "Sinker"),
    !is.na(release_speed)
  ) %>%
  mutate(
    is_strike = if_else(zone %in% 1:9, 1, 0),
    team = if_else(inning_topbot == "Top", home_team, away_team)
  ) %>%
  select(player_name, pitcher, pitch_name, release_speed, is_strike, team) %>%
  mutate(
    num_pitches = n(),
    .by = c(player_name, pitcher, pitch_name)
  ) %>%
  filter(num_pitches >= 200) %>%
  nest(.by = c(player_name, pitcher, pitch_name)) %>%
  mutate(
    strike_glm = map(
      data,
      function(x) glm(is_strike ~ release_speed, data = x)
    ),
    coefficients = map(strike_glm, coef),
    intercept = map_dbl(coefficients, 1),
    slope = map_dbl(coefficients, 2),
    avg_velo = map(data, function(x) mean(x$release_speed)),
    velo_seq = map(
      avg_velo, 
      function(x) seq(x - 5, x + 5, by = 0.5)
    ),
    num_pitches = map(data, function(x) nrow(x)),
    team_name = map(data, function(x) first(x$team))
  ) %>%
  select(
    player_name, pitcher, pitch_name, intercept, 
    slope, avg_velo, velo_seq, num_pitches, team_name
  ) %>%
  filter(num_pitches >= 600)

fb_strike_slope %>%
  unnest(c(avg_velo, velo_seq, num_pitches, team_name)) %>%
  mutate(
    strike_prob = intercept + velo_seq * slope,
    diff_from_avg = velo_seq - avg_velo
  ) %>%
  mutate(
    max_prob = max(strike_prob),
    min_prob = min(strike_prob),
    .by = c(pitcher, pitch_name)
  ) %>%
  mutate(
    alpha = if_else(pitcher == 660261, 1, 0.5),
    linewidth = if_else(pitcher == 660261, 2.7, 0.75)
  ) %>%
  filter(num_pitches >= 600, max_prob < 1, min_prob > 0) %>%
  ggplot(aes(diff_from_avg, strike_prob)) +
  geom_line(
    aes(group = pitcher, color = team_name, alpha = alpha, linewidth = linewidth)
  ) +
  scale_color_mlb() +
  scale_alpha_identity() +
  scale_linewidth_identity() +
  geom_mlb_headshots(
    aes(player_id = pitcher),
    data = tibble(
      diff_from_avg = 5,
      pitch_name = "4-Seam Fastball",
      strike_prob = 0.66
      pitcher = 660261
    ),
    height = 0.2
  ) +
  scale_y_continuous(labels = scales::label_percent(1)) +
  facet_wrap(~ pitch_name, ncol = 2) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = "Diff. From Avg FB Velo (MPH)",
    y = "p(Strike)",
    title = "Modeled Strike% From Fastball Velo",
    caption = "Data: Baseball Savant | Plot: @Drew_Haugen"
  )
