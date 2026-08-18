library(hoopR)
library(tidyverse)
library(wehoop)



getswingScore <- function(first_name, last_name, season_check) {
  # Get player ID
  nba_playerindex()$PlayerIndex %>%
    filter(PLAYER_LAST_NAME == last_name &
             PLAYER_FIRST_NAME == first_name) %>%
    pull(PERSON_ID) -> selected_player_id
  
  #Get the games played by the selected player in the selected season
  nba_playergamelog(season = season_check,
                    player_id = selected_player_id,
                    season_type = "Regular Season")$PlayerGameLog -> selected_player_gamelog
  
  #Label games as home and away and split home and away into separate tables
  selected_player_gamelog %>%
    mutate(home_away = case_when(grepl("vs.", MATCHUP) ~ "Home", .default = "Away")) ->
    selected_player_gamelog
  selected_player_home_games <- selected_player_gamelog %>%
    filter(home_away == "Home")
  selected_player_away_games <- selected_player_gamelog %>%
    filter(home_away == "Away")
  
  #function to retrieve PIE and Net rating for home and away games 
  home_away_game_concat <- function(home_bool,
                                    game_log_df,
                                    input_player_id) {
    final_df <- tibble()
    if (home_bool) {
      for (game in game_log_df$Game_ID) {
        Sys.sleep(.8)
        place_holder_df <- nba_boxscoreadvancedv3(game_id = game)$home_team_player_advanced %>%
          filter(person_id == input_player_id) %>% select(person_id, game_id, net_rating, pie)
        final_df <- rbind(final_df, place_holder_df)
        
      }
    }
    else{
      for (game in game_log_df$Game_ID) {
        Sys.sleep(.8)
        place_holder_df <- nba_boxscoreadvancedv3(game_id = game)$away_team_player_advanced %>%
          filter(person_id == input_player_id) %>% select(person_id, game_id, net_rating, pie)
        final_df <- rbind(final_df, place_holder_df)
      }
    }
    return(final_df)
  }
  #Combines home and away 
  selected_player_adv_games <- rbind(
    home_away_game_concat(TRUE, selected_player_home_games, selected_player_id),
    home_away_game_concat(FALSE, selected_player_away_games, selected_player_id)
  )
  selected_player_adv_games %>%
    mutate(pie_abv_bel = case_when(pie > mean(pie) ~ "Above", pie < mean(pie) ~
                                     "Below")) -> selected_player_adv_games
  
  avg_net <- mean(selected_player_adv_games$net_rating)
  
  
  selected_player_adv_games %>%
    group_by(pie_abv_bel) %>%
    summarise(avg_net_rating = mean(net_rating),
              team_avg_net_rating = avg_net) %>%
    transmute(impact = avg_net_rating - team_avg_net_rating) %>%
    pull(impact) %>%
    sum -> swing_score
  print(c(first_name,last_name))
  return(round(swing_score,1))
}


final_df <- tibble(
  player_fn = c(
    "Kevin",
    "Anthony",
    "LeBron",
    "Joel",
    "Mike",
    "Fred",
    "Kawhi",
    "Donovan",
    "Kyrie",
    "Paul",
    "Jaylen",
    "Zach",
    "Jrue",
    "Bradley",
    "Giannis",
    "Zion",
    "Draymond",
    "Stephen",
    "Luka",
    "Jayson",
    "Damian",
    "Devin",
    "Deandre",
    "Montrezl",
    "Julius",
    "Nikola"
  )
  ,
  player_ln = c(
    "Durant",
    "Davis",
    "James",
    "Embiid",
    "Conley",
    "VanVleet",
    "Leonard",
    "Mitchell",
    "Irving",
    "George",
    "Brown",
    "LaVine",
    "Holiday",
    "Beal",
    "Antetokounmpo",
    "Williamson",
    "Green",
    "Curry",
    "Dončić",
    "Tatum",
    "Lillard",
    "Booker",
    "Ayton",
    "Harrell",
    "Randle",
    "Jokić"
  )
)
final_df <- final_df %>%
  rowwise() %>%
  mutate(swing_score = getswingScore(player_fn, player_ln, "2020-21")) %>%
  ungroup()

write_csv(final_df,"swing_score_2021.csv")


######################################################

espn_wbb_team_news(team_id = 12)  
