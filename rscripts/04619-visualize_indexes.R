library("ggpubr")


corrected_indexes1<- read.csv("./dfs/corrected_indexes_1.csv")
corrected_indexes2 <- read.csv("./dfs/corrected_indexes_2.csv")

## list.files("./")


fn_that_should_be_anonymous <-  function (candidatename,df) {
df %>% ggplot(
aes(x = .data[[candidatename]])) +
  geom_bar(aes( fill = .data[[candidatename]]  ),
           position = position_dodge()) + theme_bw() }

# R doesn't fucking understand pointers. Outrageous.
ciroplot1 <- fn_that_should_be_anonymous("Ciro", corrected_indexes1)
haddadplot1 <- fn_that_should_be_anonymous("Haddad", corrected_indexes1)
alckminplot1 <- fn_that_should_be_anonymous("Alckmin", corrected_indexes1)
bozoplot1 <- fn_that_should_be_anonymous("Bolsonaro", corrected_indexes1)

position_counts1 <- ggarrange(ciroplot1, haddadplot1, alckminplot1, bozoplot1 , ncol = 2, nrow = 2)


position_counts1



ciroplot2 <- fn_that_should_be_anonymous("Ciro", corrected_indexes2)
haddadplot2 <- fn_that_should_be_anonymous("Haddad", corrected_indexes2)
alckminplot2 <- fn_that_should_be_anonymous("Alckmin", corrected_indexes2)
bozoplot2 <- fn_that_should_be_anonymous("Bolsonaro", corrected_indexes2)

position_counts2 <- ggarrange(ciroplot2, haddadplot2, alckminplot2, bozoplot2 , ncol = 2, nrow = 2)

position_counts2


position_counts1 %>% ggsave("./plots/corrected1_indexes_plot.png",
                            plot = ., dpi = 500)

position_counts2 %>% ggsave("./plots/corrected2_indexes_plot.png",
                           plot = ., dpi = 500)
