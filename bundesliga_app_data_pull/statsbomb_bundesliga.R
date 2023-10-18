# Statsbomb bundesliga event data
events <- fread("bundesliga_event_data.csv") %>% 
  drop_na(player_name_id) %>% 
  filter(type_name %notin% c("Bad Behaviour", "Goal Keeper", "Injury Stoppage", "Offside", 
                             "Own Goal Against", "Player Off", "Player On", "Substitution"))

# statsbomb bundesliga matches for later join
matches <- fread("bundesliga_matches.csv") %>% 
  select(match_id, match_date, season_season_name, home_team_home_team_name, away_team_away_team_name) %>% 
  left_join(events)

# finding opponent and joining date with opponent
events_date <- left_join(events, matches) %>% 
  mutate(opp_team = case_when(team_name == home_team_home_team_name ~ away_team_away_team_name, 
                              TRUE ~ home_team_home_team_name)) %>% 
  unite(col = "match_date_opp", opp_team, match_date, sep = " - ")

# filtering for latest season (current)
latest_season <- events_date %>% filter(season_season_name == "2023/2024")

# creating df for app
fwrite(latest_season, "2023_2024_bundesliga.csv")

# minutes of possession for later calculations
possession <- df %>%
  summarise(poss = sum(duration, na.rm = TRUE) / 60,
            .by = c(possession_team_name, match_id)) %>%
  mutate(team_name = possession_team_name)

# event calculations
match_events <- events_date %>%
  drop_na(player_name_id) %>%
  summarise(tackles = sum(duel_type_id == 11, na.rm = TRUE),
            dribbled_by = sum(type_id == 39, na.rm = TRUE),
            interceptions = sum(type_id == 10, na.rm = TRUE),
            dispod = sum(type_id == 3, na.rm = TRUE),
            key_pass = sum(pass_shot_assist == TRUE | pass_shot_assist == TRUE, na.rm = TRUE),
            shots = sum(type_id == 16, na.rm = TRUE),
            goals = sum(shot_outcome_id == 97, na.rm = TRUE),
            obv = sum(obv_total_net, na.rm = TRUE),
            crosses = sum(pass_cross == TRUE, na.rm = TRUE),
            pressures = sum(type_id == 17, na.rm = TRUE),
            counter_press = sum(counterpress == TRUE, na.rm = TRUE),
            dribbles = sum(type_id == 14, na.rm = TRUE),
            dribble_com = sum(dribble_outcome_id == 8, na.rm = TRUE),
            block = sum(type_id == 6, na.rm = TRUE),
            .by = c(match_id, player_name_id, team_name)) %>%
  mutate(tack_int = interceptions + tackles) %>%
  left_join(possession) %>%
  left_join(matches_select)


match_events_poss <- match_events %>%
  mutate(opp_team = case_when(team_name == home_team_home_team_name ~ away_team_away_team_name,
                              TRUE ~ home_team_home_team_name)) %>%
  mutate(opp_poss = case_when(poss == max(poss) ~ min(poss),
                              TRUE ~ max(poss)),
         .by = match_id) %>%
  mutate(total_poss = max(poss) + min(poss),
         poss_perc = opp_poss / total_poss,
         .by = match_id)

poss_adjusted <- match_events_poss %>%
  mutate(padj_tack_int = tack_int / opp_poss * (total_poss / 2))
