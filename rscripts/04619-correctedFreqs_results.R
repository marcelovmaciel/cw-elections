

library("dplyr")
library(purrr)
library("magrittr")
#load("./dta_objects/corrected_freq_ranks.RData")

library(votesys)
library(dplyr)
library(tidyr)
library(xtable)
library(vote)

test1 <- read.csv("./dfs/poly_imp_min_raw_1.csv")

                                        #test2 <- read.csv("./dfs/min_raw_2.csv")
#test3 <- read.csv("./dfs/min_raw_3.csv")
#test4 <- read.csv("./dfs/min_raw_4.csv")

foooz1 <- create_vote(test1,
                     xtype = 2,
                     candidate = c("Alckmin","Bolsonaro",  "Ciro", "Haddad"))


kemenystuff <- cdc_kemenyyoung(foooz1, keep_all_link = TRUE)



kemenystuff$other_info

cdc1 <- cdc_simple(foooz1)


borda1 <- borda_method(foooz1, modified = TRUE)



borda1


cdc1_tab <- xtable(cdc1$binary,
                   caption = "Pairwise Majority Comparisons",
                   label = "tbl:subtab1", digits = c(0,0,0,0,0))


print(cdc1_tab,
      file = "../writing/images/cdc1_tab.tex", compress = FALSE)



borda1_tab<- xtable(as.data.frame(borda1$other_info$count_max),
                    caption = "Borda Scores ", label = "tbl:subtab2",
                    digits = c(0,0))

print(borda1_tab,
      file = "../writing/images/borda1_tab.tex", compress = FALSE)
