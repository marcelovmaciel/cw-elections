
library("dplyr")
library(purrr)
library("magrittr")
#load("./dta_objects/corrected_freq_ranks.RData")

library(votesys)
library(dplyr)
library(tidyr)
library(xtable)
library(vote)

test1 <- read.csv("./dfs/min_raw_1.csv")
test2 <- read.csv("./dfs/min_raw_2.csv")
test3 <- read.csv("./dfs/min_raw_3.csv")
test4 <- read.csv("./dfs/min_raw_4.csv")

foooz1 <- create_vote(test1,
                     xtype = 2,
                     candidate = c("Alckmin","Bolsonaro",  "Ciro", "Haddad"))
foooz2 <- create_vote(test2,
                     xtype = 2,
                     candidate = c("Alckmin","Bolsonaro",  "Ciro", "Haddad"))
foooz3 <- create_vote(test3,
                     xtype = 2,
                     candidate = c("Alckmin","Bolsonaro",  "Ciro", "Haddad"))

foooz4 <- create_vote(test4,
                     xtype = 2,
                     candidate = c("Alckmin","Bolsonaro",  "Ciro", "Haddad"))

cdc1 <- cdc_simple(foooz1)
cdc2 <- cdc_simple(foooz2)
cdc3 <- cdc_simple(foooz3)
cdc4 <- cdc_simple(foooz4)

borda1 <- borda_method(foooz1, modified = TRUE)
borda2 <- borda_method(foooz2, modified = TRUE)
borda3 <- borda_method(foooz3, modified = TRUE)
borda4 <- borda_method(foooz4, modified = TRUE)


cdc1_tab <- xtable(cdc1$binary,
                   caption = "Pairwise Majority Comparisons",
                   label = "tbl:subtab1", digits = c(0,0,0,0,0))
cdc2_tab <- xtable(cdc2$binary,
                   caption = "Pairwise Majority Comparisons, Eq class 2",
                   label = "tbl:subtab2", digits = c(0,0,0,0,0))
cdc3_tab <- xtable(cdc3$binary,
                   caption = "Pairwise Majority Comparisons, Eq class 3",
                   label = "tbl:subtab3", digits = c(0,0,0,0,0))
cdc4_tab <- xtable(cdc4$binary,
                   caption = "Pairwise Majority Comparisons, Eq class 4",
                   label = "tbl:subtab4", digits = c(0,0,0,0,0))

print(cdc1_tab,
      file = "../writing/images/cdc1_tab.tex", compress = FALSE)

print(cdc2_tab,
      file = "../writing/images/cdc2_tab.tex", compress = FALSE)

print(cdc3_tab,
      file = "../writing/images/cdc3_tab.tex", compress = FALSE)

print(cdc4_tab,
      file = "../writing/images/cdc4_tab.tex", compress = FALSE)


borda1_tab<- xtable(as.data.frame(borda1$other_info$count_max),
                    caption = "Borda Scores ", label = "tbl:subtab2",
                    digits = c(0,0))
borda2_tab<- xtable(as.data.frame(borda2$other_info$count_max),
                    caption = "Borda Scores , Eq class 2",
                    label = "tbl:subtab2_eq2",
                    digits = c(0,0))
borda3_tab<- xtable(as.data.frame(borda3$other_info$count_max),
                    caption = "Borda Scores , Eq class 3",
                    label = "tbl:subtab2_eq3",
                    digits = c(0,0))
borda4_tab<- xtable(as.data.frame(borda4$other_info$count_max),
                    caption = "Borda Scores , Eq class 4",
                    label = "tbl:subtab2_eq4",
                    digits = c(0,0))

print(borda1_tab,
      file = "../writing/images/borda1_tab.tex", compress = FALSE)

print(borda2_tab,
      file = "../writing/images/borda2_tab.tex", compress = FALSE)
print(borda3_tab,
      file = "../writing/images/borda3_tab.tex", compress = FALSE)

print(borda4_tab,
      file = "../writing/images/borda4_tab.tex", compress = FALSE)
