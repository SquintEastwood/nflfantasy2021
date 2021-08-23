
# Prep --------------------------------------------------------------------

#install.packages("tidyverse")
#install.packages("ggrepel")
#install.packages("ggimage")
#install.packages("nflfastR")

library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)


# Load Data ---------------------------------------------------------------

data <- load_pbp(2020)
names <- names(data)
names <- tibble(names)
header <- dplyr::select(data,
                        away_team,
                        desc,
                        game_date,
                        game_id,
                        home_team,
                        nfl_api_id,
                        play,
                        play_id,
                        play_type,
                        play_type_nfl,
                        posteam,
                        season,
                        season_type,
                        total,
                        touchdown,
                        week
)

team_names <- dplyr::select(data,home_team) %>% 
  dplyr::distinct(home_team)


# Player Lists ------------------------------------------------------------

fn_players <- function(player_names){
  pid <- "_player_id"
  pname <- "_player_name"
  dplyr::select(data,
                glue::glue('{player_names}{pid}'),
                glue::glue('{player_names}{pname}')) %>% 
    dplyr::rename(player_id=!!as.name(glue::glue('{player_names}{pid}')),
                  player_name=!!as.name(glue::glue('{player_names}{pname}'))) %>% 
    dplyr::distinct(player_id, .keep_all = TRUE) %>% 
    dplyr::filter(!is.na(player_id))
}

players_list_off <- c('kickoff_returner',
           'passer',
           'punt_returner',
           'receiver',
           'rusher',
           'td'
)

players_offense <- purrr::map_dfr(players_list_off,fn_players) %>% 
  dplyr::distinct(player_id,.keep_all = TRUE)

players_list_def <- c('assist_tackle_1',
                      'assist_tackle_2',
                      'forced_fumble_player_1',
                      'half_sack_1',
                      'half_sack_2',
                      'interception',
                      'lateral_interception',
                      'lateral_sack',
                      'pass_defense_1',
                      'qb_hit_1',
                      'sack',
                      'safety',
                      'solo_tackle_1',
                      'tackle_for_loss_1',
                      'tackle_with_assist_1',
                      'td'
)

players_defense <- purrr::map_dfr(players_list_def,fn_players) %>% 
  dplyr::distinct(player_id,.keep_all = TRUE) %>% 
  dplyr::anti_join(.,players_offense, by="player_id")

players_list_kick <- c('kicker')

players_kickers <- purrr::map_dfr(players_list_kick,fn_players) %>% 
  dplyr::distinct(player_id,.keep_all = TRUE)

players_master_list <- dplyr::union(players_offense, players_defense, players_kickers) 

# Game Logs ---------------------------------------------------------------

fn_gameLogOffense <- function(playerID){
  dplyr::filter(data, season_type=='REG',
                passer_player_id==playerID|
                  rusher_player_id==playerID|
                  td_player_id==playerID|
                  fumbled_1_player_id==playerID|
                  fumble_recovery_1_player_id==playerID|
                  lateral_rusher_player_id==playerID|
                  lateral_receiver_player_id==playerID|
                  punt_returner_player_id==playerID|
                  kickoff_returner_player_id==playerID,
                ) %>% 
    dplyr::group_by(., week) %>% 
    dplyr::summarise("Pass Completions"=sum(complete_pass[passer_player_id==playerID],na.rm=TRUE),
                     "Pass Yards"=sum(passing_yards[passer_player_id==playerID],na.rm=TRUE),
                     "Pass TDs"=sum(pass_touchdown,na.rm=TRUE),
                     "Pass Ints"=sum(interception[passer_player_id==playerID],na.rm=TRUE),
                     "Rush Attempts"=sum(rush_attempt[rusher_player_id==playerID],na.rm=TRUE),
                     "Rush Yards"=sum(rushing_yards[rusher_player_id==playerID & lateral_rush==0],na.rm=TRUE),
                     "Rush TDs"=sum(rush_touchdown[td_player_id==playerID],na.rm=TRUE),
                     "Lateral Rush Attempts"=sum(lateral_rush[lateral_rusher_player_id==playerID],na.rm=TRUE),
                     "Lateral Rush Yards"=sum(rushing_yards[lateral_rusher_player_id==playerID & lateral_rush==1],na.rm=TRUE),
                     "Lateral Rush TD"=sum(touchdown[lateral_rusher_player_id==playerID & td_player_id==1],na.rm=TRUE),
                     "Receptions"=sum(complete_pass[receiver_player_id==playerID],na.rm=TRUE),
                     "Receiving Yards"=sum(receiving_yards[receiver_player_id==playerID],na.rm=TRUE),
                     "Receiving TDs"=sum(pass_touchdown[td_player_id==playerID],na.rm=TRUE),
                     "Lateral Rec Attempts"=sum(lateral_reception[lateral_receiver_player_id==playerID],na.rm=TRUE),
                     "Lateral Rec Yards"=sum(receiving_yards[lateral_receiver_player_id==playerID & lateral_reception==1],na.rm=TRUE),
                     "Lateral Rec TD"=sum(touchdown[lateral_receiver_player_id==playerID & td_player_id==1],na.rm=TRUE),
                     "Punt Return Yards"=sum(return_yards[punt_returner_player_id==playerID & punt_attempt==1],na.rm=TRUE),
                     "Kickoff Return Yards"=sum(return_yards[kickoff_returner_player_id==playerID & kickoff_attempt==1],na.rm=TRUE),
                     "Fumble Recovery TD"=sum(touchdown[fumble_recovery_1_player_id==playerID & td_player_id==playerID],na.rm=TRUE),
                     "Fumble Lost"=sum(fumble_lost[fumbled_1_player_id==playerID],na.rm=TRUE),
                     "2 Point Conversions"=sum(two_point_attempt[two_point_conv_result=='success' & (
                       passer_player_id==playerID | rusher_player_id==playerID | receiver_player_id==playerID)],na.rm=TRUE)
    ) %>%
    dplyr::transmute(.,"Week"=week,
                     "Player ID"=playerID,
                     "Pass Completions"=`Pass Completions`,
                     "Pass Yards"=`Pass Yards`,
                     "Pass TDs"=`Pass TDs`,
                     "Pass Ints"=`Pass Ints`,
                     "Rush Attempts"=`Rush Attempts`+`Lateral Rush Attempts`,
                     "Rush Yards"=`Rush Yards`+`Lateral Rush Yards`,
                     "Rush TDs"=`Rush TDs`+`Lateral Rush TD`,
                     "Receptions"=`Receptions`+`Lateral Rec Attempts`,
                     "Receiving Yards"=`Receiving Yards`+`Lateral Rec Yards`,
                     "Receiving TDs"=`Receiving TDs`+`Lateral Rec TD`,
                     "Punt Return Yards"=`Punt Return Yards`,
                     "Kickoff Return Yards"=`Kickoff Return Yards`,
                     "Fumble Recovery TD"=`Fumble Recovery TD`,
                     "Fumble Lost"=`Fumble Lost`,
                     "2 Point Conversions"=`2 Point Conversions`
    ) %>% tidyr::complete(Week=1:17, fill = list(`Player ID` = playerID))
}

fn_gameLogDefense <- function(playerID){
  dplyr::filter(data, season_type=='REG',
                assist_tackle_1_player_id==playerID|
                  assist_tackle_2_player_id==playerID|
                  forced_fumble_player_1_player_id==playerID|
                  fumble_recovery_1_player_id==playerID|
                  half_sack_1_player_id==playerID|
                  half_sack_2_player_id==playerID|
                  interception_player_id==playerID|
                  lateral_interception_player_id==playerID|
                  lateral_sack_player_id==playerID|
                  pass_defense_1_player_id==playerID|
                  qb_hit_1_player_id==playerID|
                  qb_hit_2_player_id==playerID|
                  sack_player_id==playerID|
                  safety_player_id==playerID|
                  solo_tackle_1_player_id==playerID|
                  tackle_for_loss_1_player_id==playerID|
                  tackle_with_assist_1_player_id==playerID|
                  tackle_with_assist_2_player_id==playerID|
                  td_player_id==playerID
                ) %>% 
    dplyr::group_by(., week) %>% 
    dplyr::summarise("Solo Tackles"=sum(solo_tackle[solo_tackle_1_player_id==playerID],na.rm=TRUE),
                     "Assist Tackles1"=sum(assist_tackle[assist_tackle_1_player_id==playerID],na.rm=TRUE),
                     "Assist Tackles2"=sum(assist_tackle[assist_tackle_2_player_id==playerID],na.rm=TRUE),
                     "Assist Tackles3"=sum(tackle_with_assist[tackle_with_assist_1_player_id==playerID],na.rm=TRUE),
                     "Assist Tackles4"=sum(tackle_with_assist[tackle_with_assist_2_player_id==playerID],na.rm=TRUE),
                     "Full Sacks"=sum(sack[sack_player_id==playerID | lateral_sack_player_id==playerID],na.rm=TRUE),
                     "Half Sacks"=sum(sack[half_sack_1_player_id==playerID | half_sack_2_player_id==playerID],na.rm=TRUE)*.5,
                     "Tackles for Loss"=sum(tackled_for_loss[tackle_for_loss_1_player_id==playerID],na.rm=TRUE),
                     "Sack Yards"=sum(yards_gained[sack_player_id==playerID],na.rm=TRUE),
                     "Half Sack Yards1"=sum(yards_gained[half_sack_1_player_id==playerID],na.rm=TRUE),
                     "Half Sack Yards2"=sum(yards_gained[half_sack_2_player_id==playerID],na.rm=TRUE),
                     "Interceptions"=sum(interception[interception_player_id==playerID],na.rm=TRUE),
                     "Forced Fumbles"=sum(fumble_forced[forced_fumble_player_1_player_id==playerID],na.rm=TRUE),
                     "Fumbles Recovered"=sum(fumble_lost[fumble_recovery_1_player_id==playerID],na.rm=TRUE),
                     "Touchdowns"=sum(touchdown[td_player_id==playerID],na.rm=TRUE),
                     "Safeties"=sum(safety[safety_player_id==playerID],na.rm=TRUE),
                     "Passes Defensed"=sum(pass_attempt[pass_defense_1_player_id==playerID],na.rm=TRUE),
                     "QB Hits"=sum(qb_hit[qb_hit_1_player_id==playerID|qb_hit_2_player_id==playerID],na.rm=TRUE),
                     "Int Return Yards"=sum(return_yards[interception_player_id==playerID],na.rm=TRUE),
                     "Fum Return Yards"=sum(return_yards[fumble_recovery_1_player_id],na.rm=TRUE)
    ) %>%
    dplyr::transmute(.,"Week"=week,
                     "Player ID"=playerID,
                     "Solo Tackles"=`Solo Tackles`,
                     "Assist Tackles"=`Assist Tackles1`+`Assist Tackles2`+`Assist Tackles3`+`Assist Tackles4`,
                     "Sacks"=`Full Sacks`+`Half Sacks`,
                     "Tackles for Loss"=`Tackles for Loss`,
                     "Sack Yards"=`Sack Yards`+`Half Sack Yards1`+`Half Sack Yards2`,
                     "Interceptions"=`Interceptions`,
                     "Forced Fumbles"=`Forced Fumbles`,
                     "Fumbles Recovered"=`Fumbles Recovered`,
                     "Touchdowns"=`Touchdowns`,
                     "Safeties"=`Safeties`,
                     "Passes Defensed"=`Passes Defensed`,
                     "QB Hits"=`QB Hits`,
                     "Return Yards"=`Int Return Yards`+`Fum Return Yards`
    ) %>% tidyr::complete(Week=1:17, fill = list(`Player ID` = playerID))
}

fn_gameLogKicks <- function(playerID){
  dplyr::filter(data, season_type=='REG',
                kicker_player_id==playerID) %>% 
    dplyr::mutate(., PAT=case_when(extra_point_result=='good'~as.numeric(1),
                                      extra_point_result!='good'~as.numeric(0)),
                  KIK=case_when(field_goal_result=='made'~as.numeric(1),
                                field_goal_result!='made'~as.numeric(0))
                  ) %>%
    dplyr::group_by(., week) %>% 
    dplyr::summarise("PAT Made"=sum(PAT[kicker_player_id==playerID],na.rm=TRUE),
                     "FG Made 0-19"=sum(KIK[kicker_player_id==playerID & kick_distance<20],na.rm=TRUE),
                     "FG Made 20-29"=sum(KIK[kicker_player_id==playerID & kick_distance<30 & kick_distance>20],na.rm=TRUE),
                     "FG Made 30-39"=sum(KIK[kicker_player_id==playerID & kick_distance<40 & kick_distance>30],na.rm=TRUE),
                     "FG Made 40-49"=sum(KIK[kicker_player_id==playerID & kick_distance<50 & kick_distance>40],na.rm=TRUE),
                     "FG Made 50+"=sum(KIK[kicker_player_id==playerID & kick_distance>=50],na.rm=TRUE),
    ) %>%
    dplyr::transmute(.,"Week"=week,
                     "Player ID"=playerID,
                     "PAT Made"=`PAT Made`,
                     "FG Made 0-19"=`FG Made 0-19`,
                     "FG Made 20-29"=`FG Made 20-29`,
                     "FG Made 30-39"=`FG Made 30-39`,
                     "FG Made 40-49"=`FG Made 40-49`,
                     "FG Made 50+"=`FG Made 50+`
    ) %>% tidyr::complete(Week=1:17, fill = list(`Player ID` = playerID))
}

gameLogsOffense <- purrr::map_dfr(players_offense$player_id,fn_gameLogOffense)

gameLogsDefense <- purrr::map_dfr(players_defense$player_id,fn_gameLogDefense)

gameLogsKicks <- purrr::map_dfr(players_kickers$player_id,fn_gameLogKicks)


# Scoring Models ----------------------------------------------------------

modelOffense <- tribble(
                        ~Stat, ~Modifier, 
                        "Passing Completions",    .1, 
                        "Passing Yards",          .02,
                        "Passing Touchdowns",     4,
                        "Interceptions Thrown",   -2,
                        "Rushing Attempts",       .1,
                        "Rushing Yards",          1/8,
                        "Rushing Touchdowns",     6,
                        "Receptions",             2,
                        "Receiving Yards",        .02,
                        "Receiving Touchdowns",   5.5,
                        "Return Yards",           1/25,
                        "Recovered Fumble TD",    6,
                        "2Point Conversions",     2,
                        "Fumbles Lost",           -3
)
