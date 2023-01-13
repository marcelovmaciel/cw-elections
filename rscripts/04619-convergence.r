library("BayesMallows")
library("dplyr")
library(ggplot2)

library("magrittr")
library("purrr")
library(coda)
load("./dta_objects/acc_pairs.RData")




tc <- generate_transitive_closure(acc_pairs)

init_rank <- generate_initial_ranking(tc)


## load("./dta_objects/bmm_test.RData")

bmm_test <- compute_mallows(rankings = init_rank,
                            preferences = tc,
                            save_aug = TRUE,
                            nmc = 5000)

alpha_conv <- assess_convergence(bmm_test)

rho_conv <- assess_convergence(bmm_test,
                               parameter = "rho")

plot(bmm_test, parameter ="rho", burnin = 1000)



alpha_conv

rho_conv

bmm_test2 <- compute_mallows(rankings = init_rank,
                            preferences = tc,
                            save_aug = TRUE,
                            nmc = 5000,
                            rho_init = c(1,2,3,4),
                            n_clusters = 2)

plot(bmm_test2, parameter ="rho", burnin = 1000)


assess_convergence(bmm_test2,
                   parameter = "rho")


bmm_test3 <- compute_mallows(rankings = init_rank,
                            preferences = tc,
                            save_aug = TRUE,
                            nmc = 5000,
                            rho_init = c(2,1,3,4),
                            n_clusters = 2)


assess_convergence(bmm_test3,
                   parameter = "rho")

plot(bmm_test3, parameter ="rho", burnin = 1000)


test_different_rho <- function (n,rho,nc) {
  bmm_test2 <- compute_mallows(rankings = init_rank,
                            preferences = tc,
                            save_aug = TRUE,
                            nmc = n,
                            rho_init = rho,
                            n_clusters = nc)
  return(assess_convergence(bmm_test2,
                   parameter = "rho"))
}

plot(bmm_test, parameter ="rho", burnin = 1000)


# Items 1,2,3,4 are respectively Ciro,Haddad,Alckmin,Bolsonaro.

test_different_rho(10,c(1,2,3,4),2)

test_different_rho(20,c(1,2,3,4),2)

test_different_rho(100,c(1,2,3,4),2)

test_different_rho(5000,c(1,2,3,4),2)




test_different_rho(10,c(2,1,3,4),2)

test_different_rho(20,c(2,1,3,4),2)

test_different_rho(100,c(2,1,3,4),2)

test_different_rho(5000,c(2,1,3,4),2)




test_different_rho(10,c(4,3,1,2),2)

test_different_rho(20,c(4,3,1,2),2)

test_different_rho(100,c(4,3,1,2),2)

test_different_rho(5000,c(4,3,1,2),2)







plot(bmm_test2,
     parameter ="rho",
     burnin = 1000)


vignette("BayesMallows")

bmm_test$augmented_data$iteration |> unique()

newalphaconv<- alpha_conv   + theme_classic() + theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change axis line
  axis.line = element_line(colour = "black")
  )

newrhoconv <- rho_conv   + theme_classic() + theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change axis line
  axis.line = element_line(colour = "black")
  )


ggsave("alpha_conv.png", plot = newalphaconv)

ggsave("rho_conv.png", plot = newrhoconv)


plot(bmm_test, parameter = "rho")
