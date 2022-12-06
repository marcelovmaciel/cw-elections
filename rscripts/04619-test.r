library("BayesMallows")
library("dplyr")
library("ggraph")
library("haven")
library("igraph")
library("magrittr")
library("purrr")
##library("tidyr")
library("tidytable")
#library("votevizr")
library("xtable")
## library(reshape2)
library("ggpubr")

df <- read_sav("../04619/04619.SAV")
load("bmm_rank_index.RData")

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


acc_pairs <-   tibble(assessor = numeric(),
         bottom_item = numeric(),
         top_item = numeric())

for (i in 1:nrow(noncyclic_agents)) {
  acc_pairs <- bind_rows(acc_pairs,
            get_agent_pairs(i, noncyclic_agents[i,]))
}



## there is some bullshit about labeling here
## Accordinly, I'll relabe this to 1:4

acc_pairs$bottom_item %<>%
  as.factor(.) %>%
  recode_factor(.,
              `3`= 1,
              `5` = 2,
              `6` = 3,
              `9` = 4) %>%
  as.numeric(.)



acc_pairs$top_item %<>%
  as.factor(.) %>%
  recode_factor(.,
              `3`= 1,
              `5` = 2,
              `6` = 3,
              `9` = 4) %>%
  as.numeric(.)

## ** Save acc_pairs
save(acc_pairs, file="./dta_objects/acc_pairs.RData")

## * Create bayesian model: bmm_test

tc <- generate_transitive_closure(acc_pairs)

init_rank <- generate_initial_ranking(tc)


bmm_test <- compute_mallows(rankings = init_rank,
                             preferences = tc, save_aug = TRUE)


# compute_posterior_intervals(bmm_test,parameter = "rho")

#plot(bmm_test, parameter = "rho", items = 1:4)
## Rtilde = latent ranking vector

## ** Save bayesian model : bmm_test
save(bmm_test, file="./dta_objects/bmm_test.RData")

## * Assess baeysian model
bmm_test$burnin <- 1000

convergence<- assess_convergence(bmm_test,
                   parameter = "rho",
                   items = c(1, 2,3,4),
                   assessors = 1:100)
convergence

#foo <- compute_consensus(bmm_test, type = "MAP")

#plot(bmm_test , parameter = "rho")


##compute_posterior_intervals(bmm_test, parameter= "rho")


##ggsave("convergence.png")


## * Use bmm_test to draw conclusions
lastiter <- filter(bmm_test$augmented_data, iteration == 2000)

## DONE: understand wtf is going on here
## OK, init_rank order the following way:
## names(init_rank) == c(alternative1,alternative2, ....)
## so its values are the INDEXES of each alternative
## on the other hand last_rank as I've applied lists the order of alternatives
## at each row. So, it is an ACTUAL ranking.

## ** Make: frequencies_pairwise_comparisons

acc_pairs %>%
  group_by(assessor) %>%
  group_size %>%
  table   %>% data.frame -> frequencies_pairwise_comparisons


## ** Make: last_indexes,last_rank,freq_ranks_inferred

lastiter %>%
  group_by(assessor) %>%
  arrange(.,value,  .by_group = TRUE) %>%
  summarise(ranking = item) %>%
  mutate(ranking = recode(ranking,
                          `Item 1`= 1,
                          `Item 2` = 2,
                          `Item 3` = 3,
                          `Item 4` = 4))  %>%
  group_by(assessor) %>%
  group_split()  %>%
  lapply(.,  function (x) {x$ranking}) %>%
  do.call(rbind, .)  -> last_rank

freq_ranks_inferred <- rank_freq_distr(last_rank)
freq_ranks_inferred <- freq_ranks_inferred[order(freq_ranks_inferred[,5], decreasing = TRUE),]

freq_ranks_inferred %<>% data.frame

names(freq_ranks_inferred) <- c(1,2,3,4,"freq")


for (i in 1:4){
freq_ranks_inferred[,i] %>%
  as.factor %>%
  recode_factor(.,
              `1`= "ciro",
              `2` = "haddad",
              `3` = "alckmin",
              `4` = "bolsonaro") -> freq_ranks_inferred[,i]
}



lastiter %>%
  group_by(assessor) %>%
  summarise(ranking = value) %>%
  group_split()  %>%
  lapply(.,  function (x) {as.numeric(x$ranking)}) %>%
  do.call(rbind, .)  -> last_indexes



## freq_ranks <- rank_freq_distr(last_indexes)
## freq_ranks<- freq_ranks[order(freq_ranks[,5], decreasing = TRUE),]

last_indexes %<>% data.frame
names(last_indexes) <- c("ciro", "haddad", "alckmin", "bolsonaro")

last_rank %<>% data.frame

names(last_rank) <- c("choice1", "choice2", "choice3", "choice4")


for (i in c("choice1", "choice2", "choice3", "choice4")) {
last_rank[,i] %>% as.factor %>%
  recode_factor(.,
              `1`= "ciro",
              `2` = "haddad",
              `3` = "alckmin",
              `4` = "bolsonaro") -> last_rank[,i]
}



## ** Save frequencies_pairwise_comparisons
save(frequencies_pairwise_comparisons,
     file = "./dta_objects/frequencies_pairwise_comparisons.RData")

frequencies_pairwise_comparisons %>%
  xtable(.,  type = "latex", tabular.environment = "longtable")


## apply(init_rank == last_indexes, 1, all) |> sum()

## get_borda_from_indexes <- function(x){
## 4*tabulate(x[,1]) +
## 3*tabulate(x[,2]) +
## 2*tabulate(x[,3]) +
## 1*tabulate(x[,4])
## }

## FIXME: this is leads to an error. tell them!!!!
## TODO: show them the error


## ** Save last_rank, last_indexes, freq_ranks_inferred
save(freq_ranks_inferred, file="./dta_objects/freq_ranks_inferred.RData")
save(last_indexes, file = "./dta_objects/last_indexes.RData")
save(last_rank, file =  "./dta_objects/last_rank.RData" )



## ** Make frequency of inferred ranks table


tab_freq_ranks_inferred <- xtable(freq_ranks_inferred,
       type = "latex",
       tabular.environment = "longtable",
       digits = 0)

## ** Make plot of each candidate count


## HERE
load("./dta_objects/last_indexes.RData")

corrected_indexes1<- read.csv("./dfs/corrected1_indexes.csv")
corrected_indexes2 <- read.csv("./dfs/corrected2_indexes.csv")

## list.files("./")


fn_that_should_be_anonymous <-  function (candidatename,df) {
df %>% ggplot(
aes(x = .data[[candidatename]])) +
  geom_bar(aes( fill = .data[[candidatename]]  ),
           position = position_dodge()) + theme_bw() }

# R doesn't fucking understand pointers. Outrageous.
ciroplot1 <- fn_that_should_be_anonymous("ciro", corrected_indexes1)
haddadplot1 <- fn_that_should_be_anonymous("haddad", corrected_indexes1)
alckminplot1 <- fn_that_should_be_anonymous("alckmin", corrected_indexes1)
bozoplot1 <- fn_that_should_be_anonymous("bolsonaro", corrected_indexes1)

position_counts1 <- ggarrange(ciroplot1, haddadplot1, alckminplot1, bozoplot1 , ncol = 2, nrow = 2)


ciroplot2 <- fn_that_should_be_anonymous("ciro", corrected_indexes2)
haddadplot2 <- fn_that_should_be_anonymous("haddad", corrected_indexes2)
alckminplot2 <- fn_that_should_be_anonymous("alckmin", corrected_indexes2)
bozoplot2 <- fn_that_should_be_anonymous("bolsonaro", corrected_indexes2)

position_counts2 <- ggarrange(ciroplot2, haddadplot2, alckminplot2, bozoplot2 , ncol = 2, nrow = 2)


position_counts1 %>% ggsave("./plots/corrected1_indexes_plot.png",
                            plot = ., dpi = 500)

position_counts2 %>% ggsave("./plots/corrected2_indexes_plot.png",
                           plot = ., dpi = 500)



## ** Make plot of each choice dist among candidates
load("./dta_objects/last_rank.RData")



fn_that_should_be_anonymous2 <- function (colname) {
last_rank %>% ggplot(
aes(x = .data[[colname]])) +
  geom_bar(aes( fill = .data[[colname]]  ),
           position = position_dodge()) +
  theme_bw(base_size=24) +
  theme(legend.position="none") }

choice1df <- fn_that_should_be_anonymous2("choice1")
choice2df <- fn_that_should_be_anonymous2("choice2")
choice3df <- fn_that_should_be_anonymous2("choice3")
choice4df <- fn_that_should_be_anonymous2("choice4")

choice_plt <- ggarrange(choice1df, choice2df, choice3df, choice4df, ncol = 2,
                    nrow = 2)

choice_plt

choice_plt %>% ggsave("./plots/inferred_choice_barplot.png",
                           plot = ., dpi = 500)
## test_rankings %>% mutate(prop = formattable::percent(freq / sum(freq)))
## test_rankings %>% head
## test_indexes %>% head




## ** Make cw graph

actors <- data.frame(name = c("ciro","haddad","alckmin","bolsonaro"))
relations <- data.frame(from = c("ciro",
                                 "ciro",
                                 "ciro",
                                 "alckmin",
                                 "alckmin",
                                 "bolsonaro"),
                        to = c("alckmin",
                               "bolsonaro",
                               "haddad",
                               "bolsonaro",
                               "haddad",
                               "haddad"))


g<- graph_from_data_frame(relations, directed = TRUE, vertices = actors)

cw_graph <- ggraph(g, layout = 'graphopt') +
    geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)),
                   arrow = arrow(type = "closed", length = unit(3, 'mm'))) +
    geom_node_text(aes(label = name)) + theme_graph()
cw_graph

ggsave("cw.png")

## ** Make table of pairwise comparison problem

map(global_pairwise_comparisons,
    ~xtable(.x,  type = "latex", tabular.environment = "longtable"))
## ** Electoral results
acc_borda_result <- vector()
for (i in 1:4){
  apply(last_rank,
        1,
        function (x) {match(i,rev(x))}) %>%
    sum -> borda_result
  acc_borda_result <- c(acc_borda_result,
                        borda_result)
}

acc_borda_result




v<- test

acc_first_round <- vector()

for (i in c("ciro", "haddad", "alckmin", "bolsonaro")){
acc_first_round<- c(acc_first_round,(v[,5][v[,1] == i]|> sum()))
}


sanity_check <- data.frame(acc_first_round)

sanity_check$candidate <- c("ciro", "haddad", "alckmin", "bolsonaro")

sanity_check<- sanity_check[order(sanity_check$acc_first_round, decreasing =TRUE),]

xtable(sanity_check,  type = "latex", tabular.environment = "longtable", digits = 0)

# FIXME: this should match the acc_borda_result
get_borda_from_indexes(last_indexes)


## 4*tabulate(init_rank[,1]) +
## 3*tabulate(init_rank[,2]) +
## 2*tabulate(init_rank[,3]) +
## 1*tabulate(init_rank[,4])



## ** Test convergence

plot(bmm_test, parameter = "rho")

beach_tc <- generate_transitive_closure(beach_preferences)

beach_init_rank <- generate_initial_ranking(beach_tc)


bmm_test2 <- compute_mallows(rankings = beach_init_rank,
                            preferences = beach_tc, save_aug = TRUE)

convergence2 <- assess_convergence(bmm_test2, parameter = "rho", items = 1:20)


convergence2


bmm_visual <- compute_mallows(potato_visual, nmc = 501000)


bmm_visual$burnin <- 1000
plot(bmm_visual, parameter = "rho", items = 1:20)
