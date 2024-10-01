# Load necessary libraries
library(nflfastR)
library(nflreadr)
library(tidyverse)
library(ggplot2)

# Get San Francisco 49ers 2023 games ID
ID_games_2023 <- fast_scraper_schedules(2023) %>%
  filter(away_team == "SF" | home_team == "SF")

# Load the entire play-by-play data for the 2023 season
pbp_2023 <- load_pbp(seasons = 2023)

# Filter play-by-play data to include only the San Francisco 49ers games
games_SF_2023 <- pbp_2023 %>%
  filter(game_id %in% ID_games_2023$game_id)

# Get necessary data and calculate the percentage of run plays for each play
games_SF_2023_perc <- games_SF_2023 %>%
  filter(posteam == "SF") %>%
  select(game_id, posteam, play_type, game_seconds_remaining) %>%
  filter(play_type == "run" | play_type == "pass") %>%
  group_by(game_id, play_type) %>%
  mutate(count_plays = row_number()) %>%
  group_by(game_id) %>%
  mutate(count_all = row_number()) %>%
  ungroup() %>%
  mutate(count_run = ifelse(play_type == "run", count_plays, NA)) %>%
  fill(count_run) %>%
  mutate(perc = (count_run / count_all) * 100)

# Make a plot with 2023 data for the 49ers
ggplot(games_SF_2023_perc, aes(x = game_seconds_remaining, y = perc)) +
  geom_step(size = 1.1, color = "#AA0000") +  # 49ers' primary color
  scale_x_reverse(
    breaks = c(3600, 2700, 1800, 900, 0),
    limits = c(3600, 0),
    labels = c("3600" = "1Q", "2700" = "2Q", "1800" = "3Q", "900" = "4Q", "0" = "END")
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  geom_vline(xintercept = c(3600, 2700, 1800, 900, 0)) +
  geom_hline(yintercept = 50, linetype = 2) +
  labs(
    title = "Percentage of Running Plays After Each Play - 49ers, 2023",
    x = "",
    y = "% of Run Plays",
    caption = "Source: nflfastR package"
  ) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  facet_wrap(~game_id)

# Get Minnesota Vikings 2023 games ID
ID_games_2023_vikings <- fast_scraper_schedules(2023) %>%
  filter(away_team == "MIN" | home_team == "MIN")

# Load the entire play-by-play data for the 2023 season
pbp_2023_vikings <- load_pbp(seasons = 2023)

# Filter play-by-play data to include only the Minnesota Vikings games
games_MIN_2023 <- pbp_2023_vikings %>%
  filter(game_id %in% ID_games_2023_vikings$game_id)

# Get necessary data and calculate the percentage of run plays for each play
games_MIN_2023_perc <- games_MIN_2023 %>%
  filter(posteam == "MIN") %>%
  select(game_id, posteam, play_type, game_seconds_remaining) %>%
  filter(play_type == "run" | play_type == "pass") %>%
  group_by(game_id, play_type) %>%
  mutate(count_plays = row_number()) %>%
  group_by(game_id) %>%
  mutate(count_all = row_number()) %>%
  ungroup() %>%
  mutate(count_run = ifelse(play_type == "run", count_plays, NA)) %>%
  fill(count_run) %>%
  mutate(perc = (count_run / count_all) * 100)

# Make a plot with 2023 data for the Vikings
ggplot(games_MIN_2023_perc, aes(x = game_seconds_remaining, y = perc)) +
  geom_step(size = 1.1, color = "#4F2683") +  # Vikings' primary color
  scale_x_reverse(
    breaks = c(3600, 2700, 1800, 900, 0),
    limits = c(3600, 0),
    labels = c("3600" = "1Q", "2700" = "2Q", "1800" = "3Q", "900" = "4Q", "0" = "END")
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  geom_vline(xintercept = c(3600, 2700, 1800, 900, 0)) +
  geom_hline(yintercept = 50, linetype = 2) +
  labs(
    title = "Percentage of Running Plays After Each Play - Vikings, 2023",
    x = "",
    y = "% of Run Plays",
    caption = "Source: nflfastR package"
  ) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  facet_wrap(~game_id)
