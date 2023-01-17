library("BayesMallows")
library("dplyr")
#library("ggraph")
library("haven")
library("igraph")
library("magrittr")
library("purrr")
##library("tidyr")
#library("tidytable")
#library("votevizr")
#library("xtable")
## library(reshape2)
#library("ggpubr")
library(readr)
library(mice)
library(naniar)



df <- read_sav("../04619/04619.SAV")
#load("bmm_rank_index.RData")

## Main objects
## df
## p6df
## df ->  subset_df
## (subset_df - crazies_missing) -> subset_df
## tc + init_rank -> bmm_test
## bmm_test -> last_iter


## Main output objects
## p6df -> global_pairwise_comparisons
## acc_pairs -> frequencies_pairwise_comparisons
## last_rank ->  freq_ranks_inferred

## Main outputs!
## last_indexes ->  position_counts
## last_rank -> choice_plt
## freq_ranks_inferred -> tab_freq_ranks_inferred
## global_pairwise_comparisons -> cw_graph

## Main intermediate objects
## crazies_missing, I use it to filter all missings from subset_df
## subset_df -> acc_cyclic, I use it to filter the acc_pairs
## acc_cyclic -> noncyclic_agents ->  acc_pairs
## acc_pairs -> tc, used for init_rank and bmm_test
## tc -> init_rank, used for bmm_test
## last_iter -> last_rank
## last_iter -> last_indexes

## * Define subset
## ** p2, who_Iwill_vote_for
df[, "p2"] -> p2df

## ** ★★★ p3b, whos_my_second_if_uncertain
## Thiswill require some munging
df[, "p3a"] |> unique()
df[, "p3b"] |> unique()



## ** p5"s", those_I_reject
acc <- vector()
df[,grepl("p5", names(df))] %>%
  apply(., 1,as.vector) -> p5s

for (unit in 1:dim(p5s)[2]){
  acc[unit]<- list(p5s[,unit])
}

those_i_rejects <-  map(acc, ~.x[!is.na(.x)])

## ** p6, the pairwise comparisons!
p6df <- df[, grepl("p6", names(df))]





p6df

## ** p7, who_will_win
p7df <- df[, "p7"]


## ** p10, on_lula
p10df <- df[, "p10"]

## **  concat chosen subsets into subset_df

cbind(p2df,
  p6df,
  p7df,
  p10df) -> subset_df

foofn <- function (x) {
  myl <- length(subset_df$p6b)
  foolish <- round(table(subset_df[,x])/myl,2)
  foolish

}


global_pairwise_comparisons <- map(names(p6df), foofn)

lapply(global_pairwise_comparisons, (\(x) as_tibble(as.data.frame(x))) ) -> global_pairwise_comparisons


save(global_pairwise_comparisons,
     file="./dta_objects/global_pairwise_comparisons.RData")


## *  preprocessing pairs
subset_df <- rename(subset_df,
                    who_i_votes_for = p2,
                    who_will_win = p7,
                    on_lula = p10,
                    pair_36 = p6a,
                    pair_69 = p6b,
                    pair_39 = p6c,
                    pair_56 = p6d,
                    pair_59 = p6e,
                    pair_35 = p6f
                    )

get_pair <- function(indx, x, col, pair){
  if (is.element(x[,col], pair)){
  tibble(assessor = indx,
         bottom_item = setdiff(pair, x[,col]),
         top_item = x[,col])}
}



get_agent_pairs <- function(indx,x){
  ## too lazy to discover how to write a map over a zip in R
  pair_36 <- get_pair(indx, x, "pair_36", c(3,6))
  pair_69 <- get_pair(indx, x, "pair_69", c(6,9))
  pair_39 <- get_pair(indx, x, "pair_39", c(3,9))
  pair_56 <- get_pair(indx, x, "pair_56", c(5,6))
  pair_59 <- get_pair(indx, x, "pair_59", c(5,9))
  pair_35 <- get_pair(indx, x, "pair_35", c(3,5))

  bind_rows(pair_36,
        pair_69,
        pair_39,
        pair_56,
        pair_59,
        pair_35)
}


crazies_missing <- vector()

for (i in 1:nrow(subset_df)) {
  foo <- get_agent_pairs(i, subset_df[i, ])
  if(length(foo) ==0)  {crazies_missing <- c(crazies_missing, i)}
  if(length(foo) ==0) next
}


subset_df %>%
  filter(!(is.element(row_number(), crazies_missing))) -> subset_df


# https://stackoverflow.com/questions/55091438/r-igraph-find-all-cycles
FindCycles = function(g) {
    Cycles = NULL
    for(v1 in V(g)) {
        if(degree(g, v1, mode="in") == 0) { next }
        GoodNeighbors = neighbors(g, v1, mode="out")
        GoodNeighbors = GoodNeighbors[GoodNeighbors > v1]
        for(v2 in GoodNeighbors) {
            TempCyc = lapply(all_simple_paths(g, v2,v1, mode="out"), function(p) c(v1,p))
            TempCyc = TempCyc[which(sapply(TempCyc, length) > 3)]
          TempCyc = TempCyc[sapply(TempCyc, min) == sapply(TempCyc, `[`, 1)]
          Cycles  = c(Cycles, TempCyc)
        }
    }
    Cycles
}

acc_cyclic <- vector()
for (i in 1:nrow(subset_df)) {
  foo <- get_agent_pairs(i, subset_df[i, ])
  actors <- data.frame(name = c(3,5,6,9))
  relations <- data.frame(from = foo$top_item,
                        to = foo$bottom_item)
  g<- graph_from_data_frame(relations, directed = TRUE, vertices = actors)

  acc_cyclic <- c(acc_cyclic, length(FindCycles(g)) > 0 )
}



noncyclic_agents <- subset_df[!acc_cyclic,]


noncyclic_agents %<>% replace_with_na_all(condition = ~.x %in% c(99,96))


foo1 <- zap_label(noncyclic_agents)

data.frame(foo1) %>%
  mutate(across(everything(), as.factor))-> foo2


foo2[,grepl("pair", names(foo2))] -> foo2

miced <- mice(foo2)


bora <- complete(miced)


acc_cyclic2 <- vector()
for (i in 1:nrow(bora)) {
  foo <- get_agent_pairs(i, bora[i, ])
  actors <- data.frame(name = c(3,5,6,9))
  relations <- data.frame(from = foo$top_item,
                        to = foo$bottom_item)
  g<- graph_from_data_frame(relations, directed = TRUE, vertices = actors)

  acc_cyclic2 <- c(acc_cyclic2, length(FindCycles(g)) > 0 )
}


noncyclic_agents <- bora[!acc_cyclic2,]



get_pair <- function(indx, x, col, pair){
  if (is.element(x[,col], pair)){
  tibble(assessor = indx,
         bottom_item = setdiff(pair, x[,col]),
         top_item = x[,col])} ## For some reason this is becoming a factor
}


acc_pairs <-   tibble(assessor = numeric(),
         bottom_item = numeric(),
         top_item = factor())


for (i in 1:nrow(noncyclic_agents)) {
  acc_pairs <- bind_rows(acc_pairs,
            get_agent_pairs(i, noncyclic_agents[i,]))
}


acc_pairs$top_item <-as.numeric(as.character(acc_pairs$top_item))

acc_pairs$bottom_item[acc_pairs$bottom_item == 3] <- 1 ## ciro
acc_pairs$bottom_item[acc_pairs$bottom_item == 5] <- 2 ## haddad
acc_pairs$bottom_item[acc_pairs$bottom_item == 6] <- 3 ## alckmin
acc_pairs$bottom_item[acc_pairs$bottom_item == 9] <- 4 ## bolsonaro


acc_pairs$top_item[acc_pairs$top_item == 3] <- 1
acc_pairs$top_item[acc_pairs$top_item == 5] <- 2
acc_pairs$top_item[acc_pairs$top_item == 6] <- 3
acc_pairs$top_item[acc_pairs$top_item == 9] <- 4

funfadisgrama <- generate_transitive_closure(acc_pairs)
funfadisgrama2<- generate_initial_ranking(funfadisgrama)


rankings_df <- as.data.frame(funfadisgrama2)

colnames(rankings_df) <- c(c("Ciro", "Haddad", "Alckmin", "Bolsonaro"))

write_csv(rankings_df, "./dfs/simpler_rankings.csv")

freq_ranks_inferred<- read_csv("./dfs/freq_ranks_inferred.csv")
