library("dplyr")
library(purrr)
library("magrittr")
load("./dta_objects/corrected_freq_ranks.RData")

library(votesys)
library(dplyr)
library(tidyr)

# load("./dta_objects/freq_ranks_inferred.RData")

## last_row <- tribble(~`1` ,~`2`,~`3`,~`4`, ~freq,
##   "other", "other", "other", "other", 228)

## freq_ranks_inferred <-  rbind(freq_ranks_inferred, last_row, stringsAsFactors = FALSE)

##  freq_ranks_inferred %>%
##   group_by(`1`) %>%
##   summarise(cnt = sum(freq))  %>%
##   mutate(prop = cnt / sum(cnt)) %>%
##   arrange(desc(prop))

print(corrected_freq_ranks, n = 29)


# BUG : THERE IS SOMETHING WRONG WITH THIS COMPUTATION!!!
## corrected_freq_ranks %>%
##   group_by(`1`) %>%
##   summarise(cnt = sum(freq))  %>%
##   mutate(prop = cnt / sum(cnt)) %>%
##   arrange(desc(prop)) -> plurality_table

## corrected_freq_ranks %>%
##   group_by(`4`) %>%
##   summarise(cnt = sum(freq))  %>%
##   mutate(prop = cnt / sum(cnt)) %>%
##   arrange(prop) -> anti_plurality_table

## corrected_freq_ranks %>%
##   group_by(`1`) %>%
##   summarise(cnt = sum(freq))  %>%
##   arrange(`1`) -> firsto_table

## corrected_freq_ranks %>%
##   group_by(`2`) %>%
##   summarise(cnt = sum(freq))  %>%
##   arrange(`2`) -> secondo_table

## corrected_freq_ranks %>%
##   group_by(`3`) %>%
##   summarise(cnt = sum(freq))  %>%
##   arrange(`3`) -> thirdo_table

## corrected_freq_ranks %>%
##   group_by(`4`) %>%
##   summarise(cnt = sum(freq))  %>%
##   arrange(`4`) -> quarto_table

## firsto_table

## secondo_table

## thirdo_table

## quarto_table

## tibble(quarto_table[,"4"],
## (4 * firsto_table[,"cnt"] +
##  3 * secondo_table[,"cnt"] +
##  2 * thirdo_table[,"cnt"] +
##  1 * quarto_table[,"cnt"])) -> borda_table

## borda_table

## get_tally_scores_from_proportions <- function(df,candidate, candidate2) {

## df %>%
##   filter(`1` == candidate | `1` == candidate2) %>%
##   group_by(`1`) %>%
##   summarise(cnt = sum(freq)) -> tc1

## df %>%
##     filter(., (`1` != candidate & `1` !=  candidate2) &
##             (`2` == candidate | `2` ==  candidate2)) %>%
##   group_by(`2`) %>%
##     summarise(cnt = sum(freq)) -> tc2

## df %>%
## filter(., (`1` != candidate & `1` !=  candidate2) &
##             (`2` != candidate & `2` !=  candidate2) &
##             (`3` == candidate | `3` ==  candidate2)) %>%
##   group_by(`3`) %>%
##   summarise(cnt = sum(freq)) -> tc3

##   tibble(tc1[,"1"],
##        tc1[,"cnt"] +
##        tc2[,"cnt"] +
##        tc2[,"cnt"]) -> tally

##   tally %<>% mutate(prop = cnt / sum(cnt) )
##   return(tally)
##   }

## get_tally_winner <- function(df,c1,c2){

## tw <- (get_tally_scores_from_proportions(corrected_freq_ranks,
##                                   c1,
##                                   c2) %>% filter(cnt == max(cnt)))[1,"1"]
##   return(as.character(tw[[1]]))
## }

## get_tally_winner(corrected_freq_ranks,
##                                   "bolsonaro",
##                                   "haddad")

## get_tallies_winners <- function (x,df) {
##   candidates <- c("alckmin", "haddad", "bolsonaro", "ciro")

##   othercandidates <- candidates[candidates != x]

##   acc <- list()
##   for (i in othercandidates){
##     acc <- c(acc,get_tally_winner(df,x,i))
##   }
##   return(flatten_chr(acc))
## }

## bar <- get_tallies_winners("bolsonaro", corrected_freq_ranks)

## ## b
## 1611 + 1619 + 1771

## ## h
## 1165 + 1325 + 1459

## ## c
## 1547 + 1881 + 2537


## get_tally_scores_from_proportions(corrected_freq_ranks,"bolsonaro","haddad")
## get_tally_scores_from_proportions(corrected_freq_ranks, "bolsonaro","ciro")
## get_tally_scores_from_proportions(corrected_freq_ranks, "bolsonaro","alckmin")


## get_tally_scores_from_proportions(corrected_freq_ranks,"haddad","bolsonaro")
## get_tally_scores_from_proportions(corrected_freq_ranks, "haddad","ciro")
## get_tally_scores_from_proportions(corrected_freq_ranks, "haddad","alckmin")

## get_tally_scores_from_proportions(corrected_freq_ranks,"ciro","haddad")
## get_tally_scores_from_proportions(corrected_freq_ranks, "ciro","bolsonaro")
## get_tally_scores_from_proportions(corrected_freq_ranks, "ciro","alckmin")

## # FIXME: check if anything changed here
## corrected_freq_ranks %>% summarise(cnt = sum(freq))

# Bolsonaro is the borda winner, but not the condorcet winner !!!


#  THE ACTUAL COMPUTATION happens here, through a pkg

corrected_raw <- read.csv("./dfs/corrected_freq_raw.csv")

foooz <- create_vote(corrected_raw,
                     xtype = 2,
                     candidate = c("alckmin","bolsonaro",  "ciro", "haddad", "other"))

cdc_simple(foooz)
borda_method(foooz, modified = TRUE)
