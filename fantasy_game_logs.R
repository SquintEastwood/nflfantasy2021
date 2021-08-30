
# Prep --------------------------------------------------------------------

#install.packages("tidyverse")
#install.packages("ggrepel")
#install.packages("ggimage")
#install.packages("nflfastR")

library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(xml2)

options(scipen = 9999)


# Load Data ---------------------------------------------------------------

data <- load_pbp(2020)
roster <- nflfastR::fast_scraper_roster(2020) %>% 
  dplyr::select(team, position, full_name, gsis_id)
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

#nfl_activeroster loaded from external KNIME workflow, .xlsx

url <- "https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Football_League/National_Football_League_team_abbreviations"
team_url <- rvest::html_table(rvest::read_html(url) %>% rvest::html_elements(xpath="/html/body/div[3]/div[3]/div[5]/div[1]/table"), header = TRUE)
team_table <- team_url[[1]]

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
  dplyr::distinct(player_id,.keep_all = TRUE) %>% 
  dplyr::left_join(roster, by=c("player_id"="gsis_id"))

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
  dplyr::anti_join(.,players_offense, by="player_id") %>% 
  dplyr::left_join(roster, by=c("player_id"="gsis_id"))

players_list_kick <- c('kicker')

players_kickers <- purrr::map_dfr(players_list_kick,fn_players) %>% 
  dplyr::distinct(player_id,.keep_all = TRUE) %>% 
  dplyr::left_join(roster, by=c("player_id"="gsis_id")) %>% 
  dplyr::filter(position=='K')


players_master_list <- dplyr::union(players_offense, players_defense) %>% 
  dplyr::union(players_kickers) %>% 
  dplyr::filter(!is.na(position)) %>% 
  dplyr::filter(!position %in% c('C','FB','P','OG','OT','G','T','LS'))

players_master_list$position[players_master_list$position=='CB'] <- 'DB'
players_master_list$position[players_master_list$position=='FS'] <- 'DB'
players_master_list$position[players_master_list$position=='SAF'] <- 'DB'
players_master_list$position[players_master_list$position=='SS'] <- 'DB'
players_master_list$position[players_master_list$position=='DE'] <- 'DL'
players_master_list$position[players_master_list$position=='DT'] <- 'DL'
players_master_list$position[players_master_list$position=='NT'] <- 'DL'
players_master_list$position[players_master_list$position=='ILB'] <- 'LB'
players_master_list$position[players_master_list$position=='MLB'] <- 'LB'
players_master_list$position[players_master_list$position=='OLB'] <- 'LB'

position_count <- dplyr::count(players_master_list, position)

# Game Logs ---------------------------------------------------------------

debugWR <- dplyr::filter(data, game_id=='2020_05_BUF_TEN' & posteam=='TEN')

fn_gameLogOffense <- function(playerID){
  dplyr::filter(data, season_type=='REG',
                passer_player_id==playerID|
                  rusher_player_id==playerID|
                  receiver_player_id==playerID|
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
                     "2 Point Conversions"=`2 Point Conversions`,
                     "PassBonus"=case_when(`Pass Yards`>300~as.numeric(5),
                                            `Pass Yards`<300~as.numeric(0)),
                     "RushBonus"=case_when(`Rush Yards`>100~as.numeric(5),
                                           `Rush Yards`<100~as.numeric(0)),
                     "CatchBonus"=case_when(`Receiving Yards`>100~as.numeric(5),
                                           `Receiving Yards`<100~as.numeric(0)),
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
                     "Return Yards"=`Int Return Yards`+`Fum Return Yards`,
                     "TackleBonus"=case_when((`Solo Tackles`+`Assist Tackles`)>10~as.numeric(5),
                                           (`Solo Tackles`+`Assist Tackles`)<10~as.numeric(0)),
                     "SackBonus"=case_when(`Sacks`>2~as.numeric(5),
                                           `Sacks`<2~as.numeric(0)),
                     "BlockBonus"=case_when(`Passes Defensed`>3~as.numeric(5),
                                            `Passes Defensed`<3~as.numeric(0)),
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

Passing_Completions <-   .1 
Passing_Yards <-         .02
Passing_Touchdowns <-    3.5
Interceptions_Thrown <-  -2
Rushing_Attempts <-      .1
Rushing_Yards <-         1/15
Rushing_Touchdowns <-    5
Receptions <-            1.5
Receiving_Yards <-       1/25
Receiving_Touchdowns <-  5
Return_Yards_O <-        1/25
Recovered_Fumble_TD <-   6
Two_Point_Conversions <- 2
Fumbles_Lost <-          -3

Solo_Tackles <-          1
Assist_Tackles <-        .5
Sacks <-                 5
Tackles_for_Loss <-      1
Sack_Yards <-            1
Interceptions <-         4
Forced_Fumbles <-        2
Fumbles_Recovered <-     2
Touchdowns <-            6
Safeties <-              2
Passes_Defensed <-       7
QB_Hits <-               1
Return_Yards_D <-        1/5

PAT_Made <-              2
FG_Made_0_19 <-          3
FG_Made_20_29 <-         3
FG_Made_30_39 <-         5
FG_Made_40_49 <-         7
FG_Made_50 <-            10


# Offense Scoring Applied ---------------------------------------------------------

gameScoreOffense <- gameLogsOffense

gameScoreOffense$`Pass Completions` <- gameScoreOffense$`Pass Completions`*Passing_Completions
gameScoreOffense$`Pass Yards` <- gameScoreOffense$`Pass Yards`*Passing_Yards
gameScoreOffense$`Pass TDs` <- gameScoreOffense$`Pass TDs`*Passing_Touchdowns
gameScoreOffense$`Pass Ints` <- gameScoreOffense$`Pass Ints`*Interceptions_Thrown
gameScoreOffense$`Rush Attempts` <- gameScoreOffense$`Rush Attempts`*Rushing_Attempts
gameScoreOffense$`Rush Yards` <- gameScoreOffense$`Rush Yards`*Rushing_Yards
gameScoreOffense$`Rush TDs` <- gameScoreOffense$`Rush TDs`*Rushing_Touchdowns
gameScoreOffense$Receptions <- gameScoreOffense$Receptions*Receptions
gameScoreOffense$`Receiving Yards` <- gameScoreOffense$`Receiving Yards`*Receiving_Yards
gameScoreOffense$`Receiving TDs` <- gameScoreOffense$`Receiving TDs`*Receiving_Touchdowns
gameScoreOffense$`Punt Return Yards` <- gameScoreOffense$`Punt Return Yards`*Return_Yards_O
gameScoreOffense$`Kickoff Return Yards` <- gameScoreOffense$`Kickoff Return Yards`*Return_Yards_O
gameScoreOffense$`Fumble Recovery TD` <- gameScoreOffense$`Fumble Recovery TD`*Recovered_Fumble_TD
gameScoreOffense$`2 Point Conversions` <- gameScoreOffense$`2 Point Conversions`*Two_Point_Conversions
gameScoreOffense$`Fumble Lost` <- gameScoreOffense$`Fumble Lost`*Fumbles_Lost

gameScoreOffense <- gameScoreOffense %>%
  mutate(Total = select(., `Pass Completions`:`CatchBonus`) %>% rowSums(na.rm = TRUE))


# Defense Scoring Applied -------------------------------------------------

gameScoreDefense <- gameLogsDefense

gameScoreDefense$`Solo Tackles` <- gameScoreDefense$`Solo Tackles`*Solo_Tackles
gameScoreDefense$`Assist Tackles` <- gameScoreDefense$`Assist Tackles`*Assist_Tackles
gameScoreDefense$Sacks <- gameScoreDefense$Sacks*Sacks
gameScoreDefense$`Tackles for Loss` <- gameScoreDefense$`Tackles for Loss`*Tackles_for_Loss
gameScoreDefense$`Sack Yards` <- abs(gameScoreDefense$`Sack Yards`*Sack_Yards)
gameScoreDefense$Interceptions <- gameScoreDefense$Interceptions*Interceptions
gameScoreDefense$`Forced Fumbles` <- gameScoreDefense$`Forced Fumbles`*Forced_Fumbles
gameScoreDefense$`Fumbles Recovered` <- gameScoreDefense$`Fumbles Recovered`*Fumbles_Recovered
gameScoreDefense$Touchdowns <- gameScoreDefense$Touchdowns*Touchdowns
gameScoreDefense$Safeties <- gameScoreDefense$Safeties*Safeties
gameScoreDefense$`Passes Defensed` <- gameScoreDefense$`Passes Defensed`*Passes_Defensed
gameScoreDefense$`QB Hits` <- gameScoreDefense$`QB Hits`*QB_Hits
gameScoreDefense$`Return Yards` <- gameScoreDefense$`Return Yards`*Return_Yards_D

gameScoreDefense <- gameScoreDefense %>%
  mutate(Total = select(., `Solo Tackles`:`BlockBonus`) %>% rowSums(na.rm = TRUE))


# Kicking Scoring Applied -------------------------------------------------

gameScoreKicks <- gameLogsKicks

gameScoreKicks$`PAT Made` <- gameScoreKicks$`PAT Made`*PAT_Made
gameScoreKicks$`FG Made 0-19` <- gameScoreKicks$`FG Made 0-19`*FG_Made_0_19
gameScoreKicks$`FG Made 20-29` <- gameScoreKicks$`FG Made 20-29`*FG_Made_20_29
gameScoreKicks$`FG Made 30-39` <- gameScoreKicks$`FG Made 30-39`*FG_Made_30_39
gameScoreKicks$`FG Made 40-49` <- gameScoreKicks$`FG Made 40-49`*FG_Made_40_49
gameScoreKicks$`FG Made 50+` <- gameScoreKicks$`FG Made 50+`*FG_Made_50

gameScoreKicks <- gameScoreKicks %>%
  mutate(Total = select(., `PAT Made`:`FG Made 50+`) %>% rowSums(na.rm = TRUE))


# Fantasy Season Totals ----------------------------------------------------------

seasonScoreOffense <- dplyr::group_by(gameScoreOffense, `Player ID`) %>% 
  dplyr::summarise("Season Pts"=sum(Total, na.rm = TRUE),
                   "Active Games"=n_distinct(Total, na.rm = TRUE)) %>% 
  dplyr::mutate("Pts/Game"=`Season Pts`/`Active Games`) %>% 
  dplyr::left_join(players_master_list, by=c("Player ID"="player_id"))

seasonScoreDefense <- dplyr::group_by(gameScoreDefense, `Player ID`) %>% 
  dplyr::summarise("Season Pts"=sum(Total, na.rm = TRUE),
                   "Active Games"=n_distinct(Total, na.rm = TRUE)) %>% 
  dplyr::mutate("Pts/Game"=`Season Pts`/`Active Games`) %>% 
  dplyr::left_join(players_master_list, by=c("Player ID"="player_id"))

seasonScoreKicks <- dplyr::group_by(gameScoreKicks, `Player ID`) %>% 
  dplyr::summarise("Season Pts"=sum(Total, na.rm = TRUE),
                   "Active Games"=n_distinct(Total, na.rm = TRUE)) %>% 
  dplyr::mutate("Pts/Game"=`Season Pts`/`Active Games`) %>% 
  dplyr::left_join(players_master_list, by=c("Player ID"="player_id"))

seasonQBs <- dplyr::filter(seasonScoreOffense, position == 'QB') %>% 
  dplyr::arrange(desc(`Season Pts`)) %>% 
  dplyr::slice_head(.,n = 16)

seasonRBs <- dplyr::filter(seasonScoreOffense, position == 'RB') %>% 
  dplyr::arrange(desc(`Season Pts`)) %>% 
  dplyr::slice_head(.,n = 16)

seasonTEs <- dplyr::filter(seasonScoreOffense, position == 'TE') %>% 
  dplyr::arrange(desc(`Season Pts`)) %>% 
  dplyr::slice_head(.,n = 16)

seasonWRs <- dplyr::filter(seasonScoreOffense, position == 'WR') %>% 
  dplyr::arrange(desc(`Season Pts`)) %>% 
  dplyr::slice_head(.,n = 32)

seasonLBs <-  dplyr::filter(seasonScoreDefense, position == 'LB') %>% 
  dplyr::arrange(desc(`Season Pts`)) %>% 
  dplyr::slice_head(.,n = 16)

seasonDLs <- dplyr::filter(seasonScoreDefense, position == 'DL') %>% 
  dplyr::arrange(desc(`Season Pts`)) %>% 
  dplyr::slice_head(.,n = 16)

seasonDBs <- dplyr::filter(seasonScoreDefense, position == 'DB') %>% 
  dplyr::arrange(desc(`Season Pts`)) %>% 
  dplyr::slice_head(.,n = 16)

seasonKs <- dplyr::filter(seasonScoreKicks, position == 'K') %>% 
  dplyr::arrange(desc(`Season Pts`)) %>% 
  dplyr::slice_head(.,n = 16)

seasonWinners <- dplyr::bind_rows(seasonQBs, seasonRBs, seasonTEs, seasonWRs, seasonLBs,seasonDLs, seasonDBs,seasonKs)
seasonWinnerIds <- dplyr::select(seasonWinners, `Player ID`, position, full_name)


# Plotting ----------------------------------------------------------------

nameorder<-seasonWinners$full_name[order(seasonWinners$position, seasonWinners$`Season Pts`)]
seasonWinners$full_name<-factor(seasonWinners$full_name, levels=nameorder)

ggplot2::ggplot(seasonWinners, aes(x=full_name, y=`Season Pts`)) +
  ggplot2::geom_segment(aes(xend=full_name), yend=0, colour="grey80") +
  ggplot2::geom_point(shape='circle open', size=2, aes(colour=position)) +
  #ggplot2::geom_point(aes(colour=position), size=2)
  ggplot2::scale_colour_brewer(palette="Set1", limits=c("QB","RB","WR","TE","K","LB", "DL","DB")) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major.y = element_line(colour="grey70",linetype="dashed"),
        panel.grid.major.x=element_blank(),
        axis.text.x=element_text(angle=45,hjust=1,size=3),
        legend.position=c(1, 0.55), # Put legend inside plot area
        legend.justification=c(2, 0.05))

medians <- dplyr::group_by(seasonWinners, position) %>% 
  dplyr::summarise(median(`Season Pts`),
                   mean(`Season Pts`))

gameStatsOffense <- dplyr::left_join(gameLogsOffense, players_master_list, by=c("Player ID"="player_id")) %>% 
  dplyr::inner_join(seasonWinnerIds)

gameStatsDefense <- dplyr::left_join(gameLogsDefense, players_master_list, by=c("Player ID"="player_id")) %>% 
  dplyr::inner_join(seasonWinnerIds) %>% 
  dplyr::filter(position!='K')

sumsOffense <- dplyr::group_by(gameStatsOffense, `full_name`, position) %>% 
  dplyr::summarise(across(`Pass Completions`:`CatchBonus`,~sum(.x, na.rm = TRUE)))

sumsDefense <- dplyr::group_by(gameStatsDefense, `full_name`, position) %>% 
  dplyr::summarise(across(`Solo Tackles`:`BlockBonus`,~sum(.x, na.rm = TRUE)))
