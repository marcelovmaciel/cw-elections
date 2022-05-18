library("dplyr")
library("magrittr")
library(purrr)
library("igraph")
library(ggraph)



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

return(as.data.frame(top_two) * 100)
}

get_tally_winner <- function(candidate, candidate2, df) {
    get_tally_score(candidate, candidate2, df) %>%
      which.max(.) %>%  names -> tally_winner
    return(tally_winner)
}

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




get_tally_loser<- function (c1,c2,df){
  cs <- c(c1,c2)
loser <- cs[which(cs != get_tally_winner(c1,c2,df))]
return(loser)
}



get_pairwise_table <- function (df){

candidates <- c("alckmin", "haddad", "bolsonaro", "ciro")

candidate_pairs <- combn(candidates, 2)

candidates_1 <- candidate_pairs[1,]
candidates_2 <-candidate_pairs[2,]

pairwise_winners <- vector()


for (i in 1:6){
  winner <- get_tally_winner(candidates_1[i], candidates_2[i], df)
  pairwise_winners <- c(pairwise_winners, winner)
}

  pairwise_losers <- vector()


for (i in 1:6){
  loser <- get_tally_loser(candidates_1[i], candidates_2[i], df)
  pairwise_losers <- c(pairwise_losers, loser)
}



margins<-vector()

for (i in 1:6){
  margin <- get_tally_score(candidates_1[i], candidates_2[i], df)$margin
  margins<-c(margins,margin)

}

margins <- map_dbl(margins, \(x) round(x, digits = 2))

  result <- data.frame(candidates_1, candidates_2,
                       pairwise_winners, pairwise_losers,
                       margins)

  return(result)

}


make_pairwise_graph <- function(pairwise_table) {
  actors <- data.frame(name = c("ciro","haddad","alckmin","bolsonaro"))
  relations <- data.frame(from = pairwise_table$pairwise_winners ,
                          to = pairwise_table$pairwise_losers)


  g<- graph_from_data_frame(relations, directed = TRUE, vertices = actors)

  cw_graph <- ggraph(g, layout = 'graphopt') +
    geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name),
                       label = pairwise_table$margins),
                   angle_calc = 'along',
                   label_dodge = unit(4, 'mm'),
                   label_push =  unit(4, 'mm'),
                   arrow = arrow(type = "closed",
                                 length = unit(3, 'mm'))) +
  geom_node_label(aes(label = name))
  ggsave("cw.png")
  return(cw_graph)
}


## * Using the functions
#pairwise_table <- get_pairwise_table(last_rank)
#make_pairwise_graph(pairwise_table) -> plt
