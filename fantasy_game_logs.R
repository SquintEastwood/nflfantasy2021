
# Prep --------------------------------------------------------------------

#install.packages("tidyverse")
install.packages("ggrepel")
install.packages("ggimage")
install.packages("nflfastR")

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

players_offense <- dplyr::select(data,
                                 kickoff_returner_player_id,
                                 kickoff_returner_player_name) %>% 
                   dplyr::filter(!is.na(kickoff_returner_player_id)) %>% 
                   dplyr::transmute("id"=kickoff_returner_player_id,"name"=kickoff_returner_player_name) %>% 
                   dplyr::distinct(id,name) %>% 
                   dplyr::union(.,dplyr::select(data,
                                                passer_player_id,
                                                passer_player_name) %>% 
                                  dplyr::filter(!is.na(passer_player_id)) %>% 
                                  dplyr::transmute("id"=passer_player_id,"name"=passer_player_name) %>% 
                                  dplyr::distinct(id,name)
                                ) %>% 
                   dplyr::union(.,dplyr::select(data,
                                                punt_returner_player_id,
                                                punt_returner_player_name) %>% 
                                  dplyr::filter(!is.na(punt_returner_player_id)) %>% 
                                  dplyr::transmute("id"=punt_returner_player_id,"name"=punt_returner_player_name) %>% 
                                  dplyr::distinct(id,name)
                  ) %>% 
                  dplyr::union(.,dplyr::select(data,
                                               receiver_player_id,
                                               receiver_player_name) %>% 
                                 dplyr::filter(!is.na(receiver_player_id)) %>% 
                                 dplyr::transmute("id"=receiver_player_id,"name"=receiver_player_name) %>% 
                                 dplyr::distinct(id,name)
                  ) %>% 
                  dplyr::union(.,dplyr::select(data,
                                               rusher_player_id,
                                               rusher_player_name) %>% 
                                 dplyr::filter(!is.na(rusher_player_id)) %>% 
                                 dplyr::transmute("id"=rusher_player_id,"name"=rusher_player_name) %>% 
                                 dplyr::distinct(id,name)
                  ) %>% 
                  dplyr::union(.,dplyr::select(data,
                                               td_player_id,
                                               td_player_name) %>% 
                                 dplyr::filter(!is.na(td_player_id)) %>% 
                                 dplyr::transmute("id"=td_player_id,"name"=td_player_name) %>% 
                                 dplyr::distinct(id,name)
                  ) %>% 
                  dplyr::distinct(.,id,name)

players_defense <- dplyr::select(data,
                                 assist_tackle_1_player_id,
                                 assist_tackle_1_player_name) %>% 
                 dplyr::filter(!is.na(assist_tackle_1_player_id)) %>% 
                 dplyr::transmute("id"=assist_tackle_1_player_id,"name"=assist_tackle_1_player_name) %>% 
                 dplyr::distinct(id,name) %>% 
                 dplyr::union(.,dplyr::select(data,
                                              assist_tackle_2_player_id,
                                              assist_tackle_2_player_name) %>% 
                 dplyr::filter(!is.na(assist_tackle_2_player_id)) %>% 
                 dplyr::transmute("id"=assist_tackle_2_player_id,"name"=assist_tackle_2_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                                              forced_fumble_player_1_player_id,
                                              forced_fumble_player_1_player_name) %>% 
                 dplyr::filter(!is.na(forced_fumble_player_1_player_id)) %>% 
                 dplyr::transmute("id"=forced_fumble_player_1_player_id,"name"=forced_fumble_player_1_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                                              fumble_recovery_1_player_id,
                                              fumble_recovery_1_player_name) %>% 
                 dplyr::filter(!is.na(fumble_recovery_1_player_id)) %>% 
                 dplyr::transmute("id"=fumble_recovery_1_player_id,"name"=fumble_recovery_1_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                                              half_sack_1_player_id,
                                              half_sack_1_player_name) %>% 
                 dplyr::filter(!is.na(half_sack_1_player_id)) %>% 
                 dplyr::transmute("id"=half_sack_1_player_id,"name"=half_sack_1_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                                              half_sack_2_player_id,
                                              half_sack_2_player_name) %>% 
                 dplyr::filter(!is.na(half_sack_2_player_id)) %>% 
                 dplyr::transmute("id"=half_sack_2_player_id,"name"=half_sack_2_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>%  
                 dplyr::union(.,dplyr::select(data,
                               interception_player_id,
                               interception_player_name) %>% 
                 dplyr::filter(!is.na(interception_player_id)) %>% 
                 dplyr::transmute("id"=interception_player_id,"name"=interception_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                               lateral_interception_player_id,
                               lateral_interception_player_name) %>% 
                 dplyr::filter(!is.na(lateral_interception_player_id)) %>% 
                 dplyr::transmute("id"=lateral_interception_player_id,"name"=lateral_interception_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                               lateral_sack_player_id,
                               lateral_sack_player_name) %>% 
                 dplyr::filter(!is.na(lateral_sack_player_id)) %>% 
                 dplyr::transmute("id"=lateral_sack_player_id,"name"=lateral_sack_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                               pass_defense_1_player_id,
                               pass_defense_1_player_name) %>% 
                 dplyr::filter(!is.na(pass_defense_1_player_id)) %>% 
                 dplyr::transmute("id"=pass_defense_1_player_id,"name"=pass_defense_1_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>%  
                 dplyr::union(.,dplyr::select(data,
                               qb_hit_1_player_id,
                               qb_hit_1_player_name) %>% 
                 dplyr::filter(!is.na(qb_hit_1_player_id)) %>% 
                 dplyr::transmute("id"=qb_hit_1_player_id,"name"=qb_hit_1_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                               sack_player_id,
                               sack_player_name) %>% 
                 dplyr::filter(!is.na(sack_player_id)) %>% 
                 dplyr::transmute("id"=sack_player_id,"name"=sack_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                               safety_player_id,
                               safety_player_name) %>% 
                 dplyr::filter(!is.na(safety_player_id)) %>% 
                 dplyr::transmute("id"=safety_player_id,"name"=safety_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                               solo_tackle_1_player_id,
                               solo_tackle_1_player_name) %>% 
                 dplyr::filter(!is.na(solo_tackle_1_player_id)) %>% 
                 dplyr::transmute("id"=solo_tackle_1_player_id,"name"=solo_tackle_1_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                               tackle_for_loss_1_player_id,
                               tackle_for_loss_1_player_name) %>% 
                 dplyr::filter(!is.na(tackle_for_loss_1_player_id)) %>% 
                 dplyr::transmute("id"=tackle_for_loss_1_player_id,"name"=tackle_for_loss_1_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                               tackle_with_assist_1_player_id,
                               tackle_with_assist_1_player_name) %>% 
                 dplyr::filter(!is.na(tackle_with_assist_1_player_id)) %>% 
                 dplyr::transmute("id"=tackle_with_assist_1_player_id,"name"=tackle_with_assist_1_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::union(.,dplyr::select(data,
                               td_player_id,
                               td_player_name) %>% 
                 dplyr::filter(!is.na(td_player_id)) %>% 
                 dplyr::transmute("id"=td_player_id,"name"=td_player_name) %>% 
                 dplyr::distinct(id,name)
  ) %>% 
                 dplyr::distinct(.,id,name) %>% 
                 dplyr::anti_join(.,players_offense, by="id")

players_kickers <- dplyr::select(data,
                                 kicker_player_id,
                                 kicker_player_name) %>% 
                   dplyr::filter(!is.na(kicker_player_id)) %>% 
                   dplyr::transmute("id"=kicker_player_id,"name"=kicker_player_name) %>% 
                   dplyr::distinct(id,name)

fn_gameLogOffense <- function(playerID){
  #Passing
  dplyr::filter(data, passer_player_id==playerID) %>% 
    dplyr::group_by(.,week) %>% 
    dplyr::summarise("Pass Completions"=sum(complete_pass),
                     "Pass Yards"=sum(passing_yards),
                     "Pass TDs"=sum(pass_touchdown),
                     "Pass Ints"=sum(interception)
      
    )
}

.$passing_yards[is.na(.$passing_yards)]<- 0
playerID <- '00-0033873'
testtbl <- fn_gameLogOffense(00-0033873)
mahomietest <- dplyr::filter(data, passer_player_id=='00-0033873'|
                               fumbled_1_player_id==playerID|
                               rusher_player_id==playerID|
                               td_player_id==playerID,) %>% 
  dplyr::select(., week,
                season,
                season_type,
                desc,
                complete_pass,
                passing_yards,
                pass_touchdown,
                interception,
                rush_attempt,
                rushing_yards,
                rush_touchdown,
                passer_player_id,
                fumbled_1_player_id,
                lateral_rusher_player_id,
                rusher_player_id,
                td_player_id,
                lateral_rush
                )



mahomies <- dplyr::filter(data, season_type=='REG',
                            passer_player_id==playerID|
                            fumbled_1_player_id==playerID|
                            lateral_rusher_player_id==playerID|
                            rusher_player_id==playerID|
                            td_player_id==playerID,
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
  dplyr::transmute(.,
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
                   )
