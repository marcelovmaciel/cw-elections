library("dplyr")
library("magrittr")
library(purrr)

load("./dta_objects/last_rank.RData")

load("./dta_objects/freq_ranks_inferred.RData")

freq_ranks_inferred %>% head


last_rank[, "choice1"] %>% table %>% prop.table -> plurality_result
last_rank[, "choice4"] %>% table %>% prop.table -> antiPlurality_result

antiPlurality_result


last_rank[,"choice1"] %>% table -> choice1tab
last_rank[,"choice2"] %>% table -> choice2tab
last_rank[,"choice3"] %>% table -> choice3tab
last_rank[,"choice4"] %>% table -> choice4tab


(4 * choice1tab) +
  (3 * choice2tab) +
  (2 * choice3tab) +
  (1 * choice4tab) -> borda_result



tally <- function (candidate,candidate2){
((last_rank %>%
  filter(., choice1 == candidate | choice1 ==  candidate2) %>%
  ( \(x) table(x[,"choice1"]) )(.)) +

(last_rank %>%
  filter(., (choice1 != candidate & choice1 !=  candidate2) &
            (choice2 == candidate | choice2 ==  candidate2)) %>%
  ( \(x) table(x[,"choice2"]) )(.)) +

(last_rank %>%
  filter(., (choice1 != candidate & choice1 !=  candidate2) &
            (choice2 != candidate & choice2 !=  candidate2) &
            (choice3 == candidate | choice3 ==  candidate2)) %>%
 ( \(x) table(x[,"choice3"]) )(.))) %>%
  which.max(.) %>% attributes %>% map(.,1)
}


tallies <- function (x) {
  candidates <- c("alckmin", "haddad", "bolsonaro", "ciro")
  othercandidates <- candidates[candidates != x]
  acc <- list()
  for (i in othercandidates){
    acc <- c(acc,tally(x,i))
  }
  return(acc)
}


is_cw_winner <- function (x) {
 all(tallies(x) == x)
}


cw_winner("bolsonaro")
