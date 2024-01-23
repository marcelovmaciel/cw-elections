#packages <- c("dplyr", "haven", "igraph", "magrittr", "purrr", "readr", "mice", "naniar")

#install.packages(packages)


library("dplyr")
library("haven")
library("igraph")
library("magrittr")
library("purrr")
library(readr)
library(naniar)
library(stringr)
library(POSetR)
# library(BayesMallows)

df <- read_sav("../04619/04619.SAV")


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


## global_pairwise_comparisons <- map(names(p6df), foofn)
## lapply(global_pairwise_comparisons, (\(x) as_tibble(as.data.frame(x))) ) -> global_pairwise_comparisons
## save(global_pairwise_comparisons,
##      file="./dta_objects/global_pairwise_comparisons.RData")
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
write_csv(foo2, "./dfs/foo2.csv")



## table(unlist(lapply(helper_acc, length))))





row_na_counts <- rowSums(is.na(foo2))

# Display NA counts per row
print(row_na_counts)


# Calculate the total number of NAs in the dataset

total_na_count <- table(row_na_counts)

foo2 <- foo2 %>% mutate(na_count = rowSums(is.na(.)))

# Split the dataset into a list of datasets based on na_count
split_datasets <- foo2 %>%
  group_by(na_count) %>%
  group_split()

# Name the datasets for clarity

names(split_datasets) <- paste0("dataset_", 0:5)


clean_rankings <- function (imputted_noncyclic){
  acc_pairs <-   tibble(assessor = numeric(),
         bottom_item = numeric(),
         top_item = factor())
  for (i in 1:nrow(imputted_noncyclic)) {
  acc_pairs <- bind_rows(acc_pairs,
                         get_agent_pairs(i, imputted_noncyclic[i,]))
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
  return(acc_pairs)
}


foo2 %>% clean_rankings -> renamed_pairs

renamed_pairs  %<>%
  rename(to = bottom_item,
         from = top_item)

get_lo_for_agent <- function(agent_df,none) {
  namess<- union(unique(agent_df$from), unique(agent_df$to))
  actors <- data.frame(name = namess)
  relations <- data.frame(from = agent_df$from,
                          to = agent_df$to)

  g<- graph_from_data_frame(relations, directed = TRUE)

  toposg <- topo_sort(g) # TODO: see if this is where this is going astray
  vertex_names <- attr(toposg, "names")
    # Join the names into a single string, with space as separator
  single_string <- paste(vertex_names, collapse = "")

  return(single_string)

}

renamed_pairs %>%
  group_by(assessor) %>%
  group_split -> pairwise_comparisons_by_voter

map(pairwise_comparisons_by_voter, get_lo_for_agent) -> lo_each_voter


df <- data.frame(lo = unlist(lo_each_voter))


df %>% head # BUG: contrast it with foo2 %>% head (row 3). What is going on here ?

## write_csv(df, "./dfs/lo_each_voter.csv")
## df <- read_csv("./dfs/lo_each_voter.csv")

df %>%
  group_by(lo) %>%
  summarise(count = n()) %>%
  mutate(length = nchar(lo)) %>%  # Create a new column with the length of each string
  arrange(desc(count)) %>%
  mutate(proportion = 100*count / sum(count)) -> bar




write_csv(bar, "./dfs/lo_with_props.csv")

## bar %>%
##   mutate(first_char = substr(as.character(lo), 1, 1)) %>%
##   group_by(first_char) %>%
##   summarise(total_proportion = sum(proporportion))




## In this part I'm gonna do it differently! I'm gonna build posets  (pos)

helper <- function(index){

  foo <- get_agent_pairs(index, foo2[index,])
  actors <- data.frame(name = c(3,5,6,9))
  relations <- data.frame(from = foo$top_item,
                        to = foo$bottom_item)
  g<- graph_from_data_frame(relations, directed = TRUE, vertices = actors)
  return(g)
}



pos_acc <- list()



for (i in 1:nrow(foo2)) {
  pos_acc[[i]] <-  poset_from_igraph(helper(i))$elements

}

pos_acc %>% head


replacement_map <- setNames(c("1", "2", "3", "4"), c("3", "5", "6", "9"))

# Function to replace values in each vector using the mapping
replace_values <- function(vec) {
  sapply(vec, function(x) replacement_map[x])
}

# Apply the function to each vector in the list
helper_acc_replaced <- lapply(pos_acc, replace_values)

helper_acc_replaced %>% head



helper_acc_glued <- lapply(helper_acc_replaced, paste, collapse = "")

helper_acc_glued %>% head

renamed_pos <- data.frame(pos = unlist(helper_acc_glued))


renamed_pos %>%
  group_by(pos) %>%
  summarise(count = n()) %>%
  mutate(length = nchar(pos)) %>%  # Create a new column with the length of each string
  arrange(desc(count)) %>%
  mutate(proportion = 100*count / sum(count)) -> baranga



baranga
bar
# TODO BUG : those two should match .......... they are soooo different WTF
