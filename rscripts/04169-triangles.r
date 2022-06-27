library("dplyr")
library("magrittr")
library("votevizr")
library(readr)
library(gridExtra)
library(ggplot2)

load("./dta_objects/last_rank.RData")
load("./dta_objects/freq_ranks_inferred.RData")
ls()

acc <- list()

## freq_ranks_inferred
## write.csv(freq_ranks_inferred,"./dfs/freq_ranks_inferred.csv",
##           row.names = FALSE)

## I've prepocessed it in julia

noalckmin_df<- read_csv("./dfs/noalckmin_df.csv")
nohaddad_df<- read_csv("./dfs/nohaddad_df.csv")
nociro_df<- read_csv("./dfs/nociro_df.csv")
nobolsonaro_df <- read_csv("./dfs/nobolsonaro_df.csv")

#data(brexit_prefs)

## a <- 0.22 * 2 +
##   0.27 * 2 +
##   0.201 * 1 +
##   0.174 * 3 +
##   0.0756 * 1 +
##   0.0575 * 3

## b <-  0.22 * 3 +
##   0.27 * 1 +
##   0.201 * 3 +
##   0.174 * 1 +
##   0.0756 * 2 +
##   0.0575 * 2

## a

## b

get_prefs_list <- function (df) {
  prefs <- as.list(df$prop)
  names(prefs) <- df$ranking_vectors
  return(prefs)
}


nohaddad_df


nohaddad_prefs <- get_prefs_list(nohaddad_df)


nohaddad_prefs

noalckmin_prefs <- get_prefs_list(noalckmin_df)
nobolsonaro_prefs <- get_prefs_list(nobolsonaro_df)
nociro_prefs <- get_prefs_list(nociro_df)


possible_worlds <- function(df, title) {

df %>%
  qplot_votevizr(split = " > ",
                 "borda")  +
  labs(title  = "Borda") +
  theme(plot.title = element_text(hjust = 0.5)) -> plt1

df %>%
  qplot_votevizr(split = " > ",
                 "plurality")  +
  labs(title  = "Plurality") +
  theme(plot.title = element_text(hjust = 0.5)) -> plt2

df %>%
  qplot_votevizr(split = " > ",
                 "Condorcet")  +
  labs(title  = "Condorcet") +
  theme(plot.title = element_text(hjust = 0.5)) -> plt3

df %>%
  qplot_votevizr(split = " > ",
                 "antiplurality")  +
  labs(title  = "Antiplurality") +
  theme(plot.title = element_text(hjust = 0.5))  -> plt4

grid.arrange(plt1,plt2,plt3,plt4, top=title)

}


nohaddad_plts <- function () {possible_worlds(nohaddad_prefs, "Election without Haddad ")}
noalckmin_plts <- function () {possible_worlds(noalckmin_prefs, "Election without Alckmin")}
nobolsonaro_plts <- function () {possible_worlds(nobolsonaro_prefs, "Election without Bolsonaro")}
nociro_plts <- function () {possible_worlds(nociro_prefs, "Election without Ciro")}


nohaddad_plts()


nohaddad_plts() %>% ggsave("./plots/nohaddad.png",
                           plot = ., dpi = 500)

noalckmin_plts()  %>% ggsave("./plots/noalckmin.png",
                           plot = ., dpi = 500)
nobolsonaro_plts()  %>% ggsave("./plots/nobolsonaro.png",
                           plot = ., dpi = 500)
nociro_plts()  %>% ggsave("./plots/nociro.png",
                           plot = ., dpi = 500)
