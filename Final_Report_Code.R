library(nloptr)
library(nflfastR)
library(BradleyTerry2)
library(elo)
library(tidyverse)
library(modelsummary)
library(lme4)
library(broom)
library(broom.mixed)

library(sjPlot)
library(sjmisc)

sched_2021 <- fast_scraper_schedules(2021)
games_2021 <- sched_2021 %>% select(game_id, away_team, home_team, 
                                    away_score, home_score, home_result)
sched_2021 <- sched_2021[1:193,]
sched_2021 <- sched_2021 %>%
  mutate(home_avg = mean(home_score),
            away_avg = mean(away_score),
            total_avg = mean(home_score + away_score))
########

pbp_2021 <- load_pbp(2021) %>% 
  distinct(play_id, game_id, week,
           posteam, home_team, away_team,
           epa, pass, rush, special,
           extra_point_attempt,
           field_goal_attempt, two_point_attempt,
           kickoff_attempt, punt_attempt) %>%
  filter(pass == 1 | rush == 1 | special == 1)

epa_game_2021 <- pbp_2021 %>%
  mutate(oppteam = ifelse(posteam == home_team, away_team, home_team),
         home_away = ifelse(posteam == home_team, 1, -1)) %>%
  group_by(game_id, week, posteam, oppteam, home_team,
           pass, rush, special,
           extra_point_attempt,
           field_goal_attempt, two_point_attempt,
           kickoff_attempt, punt_attempt, home_away) %>%
  summarize(epa_play = mean(epa),
            plays = n(),
            plays_per_game = 63,
            total_off = pass + rush) %>%
  ungroup()
  
  
off_summary <- function(df){
    mixed_pass_off <- lmer(epa_play ~ home_team + (1|total_off) + (1|posteam), 
                           data = df)
}


def_summary <- function(df){
  mixed_rush_off <- lmer(epa_play ~ home_team + (1|total_off) + (1|oppteam), 
                         data = df)
}

special_summary <- function(df){
  mixed_special_off <- lmer(epa_play ~ home_team + (1|special) + (1|posteam), 
                            data = df)
}

### Effects
off_2021 <- off_summary(epa_game_2021)
summary(off_2021)

def_2021 <- def_summary(epa_game_2021)
summary(def_2021)

spec_2021 <- special_summary(epa_game_2021)
summary(spec_2021)
###


### Plots
off_eff_plot <- sjPlot::plot_model(off_2021)
def_eff_plot <- sjPlot::plot_model(def_2021)
spec_eff_plot <- sjPlot::plot_model(spec_2021)



### Ranking Functions
off_rank_fnc <- function(df){
  off_eff <- tibble(team = row.names(ranef(df)$posteam),
                    off_eff = ranef(df)$posteam[,1]) %>%
    mutate(team_value = off_eff, 
           team_rank = min_rank(desc(team_value)))
}

def_rank_fnc <- function(df){
  def_eff <- tibble(team = row.names(ranef(df)$oppteam),
                         def_eff = -ranef(df)$oppteam[,1]) %>%
    mutate(team_value = def_eff, 
           team_rank = min_rank(desc(team_value)))
}

spec_rank_fnc <- function(df){
  spec_eff <- tibble(team = row.names(ranef(df)$posteam),
                    spec_eff = ranef(df)$posteam[,1]) %>%
    mutate(team_value = spec_eff, 
           team_rank = min_rank(desc(team_value)))
}

# Power Rankings
off_rank <- off_rank_fnc(off_2021)
def_rank <- def_rank_fnc(def_2021)
spec_rank <- spec_rank_fnc(spec_2021)

# Average points per game
temp_df <- sched_2021 %>% 
  summarize(home_avg = sum(home_score)/n(),
            away_avg = sum(away_score)/n())

total_avg <- 45.89119
play_per_game <- 63
off_rank_spread <- off_rank %>% 
  group_by(team, off_eff, team_rank) %>%
  mutate(spread = off_eff * total_avg) %>%
  ungroup()

def_rank_spread <- def_rank %>% 
  group_by(team, def_eff, team_rank) %>%
  mutate(spread = def_eff * total_avg) %>%
  ungroup()

off_rank_spread <- off_rank %>% 
  group_by(team, off_eff, team_rank) %>%
  mutate(spread = off_eff * play_per_game) %>%
  ungroup()

def_rank_spread <- def_rank %>% 
  group_by(team, def_eff, team_rank) %>%
  mutate(spread = def_eff * play_per_game) %>%
  ungroup()

### Calculate Spread given two teams
off_rank_spread %>%
  filter(team == "MIN" | team == "PIT") %>%
  mutate(off_spread_pred = spread[1] - spread[2]) %>%
  pull(off_spread_pred)


def_rank_spread %>%
  filter(team == "MIN" | team == "PIT") %>%
  mutate(def_spread_pred = spread[1] - spread[2]) %>%
  pull(def_spread_pred)

View(off_rank)
View(def_rank)
View(spec_rank)

write.csv(off_rank_spread, "C:/Users/dalyf/Documents/STAT_4984/Final_Proj/Data/off_rank_spread.csv")
write.csv(def_rank_spread, "C:/Users/dalyf/Documents/STAT_4984/Final_Proj/Data/def_rank_spread.csv")


