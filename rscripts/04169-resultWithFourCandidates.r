library("dplyr")
library("magrittr")
#library(purrr)

load("./dta_objects/last_rank.RData")

load("./dta_objects/freq_ranks_inferred.RData")

## * Defining the functions

get_plurality_table <- function (df) {

  plurality_table <- df[, "choice1"] %>% table %>% prop.table
  return(plurality_table)
}


get_antiPlurality_table <- function (df) {
  antiPlurality_table <- df[, "choice4"] %>% table %>% prop.table
  return(antiPlurality_table)
}

get_antiPlurality_table(last_rank)

get_borda_scores <- function (df) {

  df[,"choice1"] %>% table -> choice1tab
  df[,"choice2"] %>% table -> choice2tab
  df[,"choice3"] %>% table -> choice3tab
  df[,"choice4"] %>% table -> choice4tab

  ((4 * choice1tab) +
  (3 * choice2tab) +
  (2 * choice3tab) +
  (1 * choice4tab)) %>% sort(., decreasing  = TRUE) -> borda_scores

  return(borda_scores)

}

get_tally_score <- function (candidate,candidate2,df){
((df %>%
  filter(., choice1 == candidate | choice1 ==  candidate2) %>%
  ( \(x) table(x[,"choice1"]) )(.)) +

(df %>%
  filter(., (choice1 != candidate & choice1 !=  candidate2) &
            (choice2 == candidate | choice2 ==  candidate2)) %>%
  ( \(x) table(x[,"choice2"]) )(.)) +

(df %>%
  filter(., (choice1 != candidate & choice1 !=  candidate2) &
            (choice2 != candidate & choice2 !=  candidate2) &
            (choice3 == candidate | choice3 ==  candidate2)) %>%
 ( \(x) table(x[,"choice3"]) )(.))) -> tally_score
tally_score %>%
    sort(., decreasing = TRUE) %>%
  .[1:2] %>% prop.table -> top_two
top_two$margin <- as.numeric((top_two[1] - top_two[2]))

return(as.data.frame(top_two))
}

get_tally_winner <- function(candidate, candidate2, df) {
    get_tally_score(candidate, candidate2, df) %>%
      which.max(.) %>%  names -> tally_winner
    return(tally_winner)
}

get_tally_winner("bolsonaro", "haddad", last_rank)

get_tallies_winners <- function (x,df) {
  candidates <- c("alckmin", "haddad", "bolsonaro", "ciro")
  othercandidates <- candidates[candidates != x]
  acc <- list()
  for (i in othercandidates){
    acc <- c(acc,get_tally_winner(x,i,df))
  }
  return(acc)
}

is_cw_winner <- function (x,df) {
 all(get_tallies_winners(x,df) == x)
}

get_plurality_top_two_margin <- function(df) {

   get_plurality_table(last_rank) %>%
    sort(., decreasing = TRUE) %>%
     .[1:2] -> top_two
    top_two$margin <- as.numeric((top_two[1] - top_two[2]))

    return(as.data.frame(top_two))
}

## * Using the functions
get_tally_score("bolsonaro", "ciro", last_rank)

get_borda_scores(last_rank)
