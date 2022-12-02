library(xtable)
load("./dta_objects/freq_ranks_inferred.RData")


library("roperators")
library("dplyr")

freq_ranks_inferred %>% summarize(foo = sum(freq))

sum(freq_ranks_inferred[,"freq"])

## 2937 -> 92
## x -> 8




freq_ranks_inferred


freq_ranks_inferred[freq_ranks_inferred[,"1"] == "alckmin",]

corrected_freq_ranks <- tibble(freq_ranks_inferred)

print(corrected_freq_ranks, n = 29)

last_row <- tribble(~`1` ,~`2`,~`3`,~`4`, ~freq,
  "other", "other", "other", "other", 228)

corrected_freq_ranks<- rbind(corrected_freq_ranks, last_row, stringsAsFactors = FALSE)

corrected_freq_ranks %>%
  group_by(`1`) %>%
  summarise(cnt = sum(freq)) %>% mutate(prop = cnt / sum(cnt))


## transfer chab to hcab
corrected_freq_ranks[5, "freq"] %-=% 70
corrected_freq_ranks[2,"freq"] %+=% 70

## cbah to bcah
corrected_freq_ranks[17, "freq"] %-=% 50
corrected_freq_ranks[2, "freq"] %+=% 50

## cabh to bcah
corrected_freq_ranks[15, "freq"] %-=% 66
corrected_freq_ranks[3, "freq"] %+=% 66


## transfer abch to bach
corrected_freq_ranks[14, "freq"] %-=% 82
corrected_freq_ranks[1, "freq"] %+=% 82

## transfer abhc to bach
corrected_freq_ranks[19, "freq"] %-=% 34
corrected_freq_ranks[7, "freq"] %+=% 34

## transfer acbh to bach
corrected_freq_ranks[13, "freq"] %-=% 80
corrected_freq_ranks[1, "freq"] %+=% 80

## remove from achb
corrected_freq_ranks[9, "freq"] %-=% 120

corrected_freq_ranks <- corrected_freq_ranks[order(corrected_freq_ranks[,"freq"],
                          decreasing = TRUE),]

print(corrected_freq_ranks, n=29)

corrected_freq_ranks %>%
  group_by(`1`) %>%
  summarise(cnt = sum(freq)) %>% mutate(prop = cnt / sum(cnt))

save(corrected_freq_ranks,
     file="./dta_objects/corrected_freq_ranks.RData")

# load("./dta_objects/corrected_freq_ranks.RData")o
