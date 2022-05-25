# Tables
# The code for Tables 1, S2.1, and S2.2 are in 01. Empirical Example.R.
library(sjstats)
library(pwr)
library(tidyverse)
library(knitr)
library(kableExtra)

# ANOVA
rm(list = ls())
load("sim_results.RData")
results <- results %>% 
  mutate(method = ifelse(method == "OLS", "OLS-CRVE",
                         ifelse(method == "FE", "FE-CRVE", "CCREM")))
results$method <- factor(results$method, 
                         levels = c("CCREM", "OLS-CRVE", "FE-CRVE"))

results_X <- results %>% 
  mutate(beta = gamma100,
         beta = as.factor(beta),
         H = as.factor(H),
         J = as.factor(J),
         ICC = as.factor(ICC),
         rho = as.factor(rho),
         assumption = factor(assumption, 
                             levels = c("Assumptions met", 
                                        "Homoscedasticity violated", 
                                        "Exogeneity violated"))) %>% 
         # assumption = factor(assumption, levels = c("met", "heterosced", "exogeneity"))) %>% 
  filter(cov == "X") 


# ANOVA functions ---------------------------------------------------------------
fml <- " ~ as.factor(method)+as.factor(beta)+as.factor(H)+as.factor(J)+as.factor(ICC)+as.factor(rho)+
        as.factor(method)*(as.factor(beta)+as.factor(H)+as.factor(J)+as.factor(ICC)+as.factor(rho))+
        as.factor(beta)*(as.factor(H)+as.factor(J)+as.factor(ICC)+as.factor(rho))+
        as.factor(H)*(as.factor(J)+as.factor(ICC)+as.factor(rho))+
        as.factor(J)*(as.factor(ICC)+as.factor(rho))+
        as.factor(ICC)*as.factor(rho)"

tidy_func <- function(obj){
  anova_stats(obj) %>% as_tibble %>% 
    select(term, partial.etasq) %>% 
    mutate(size = ifelse(partial.etasq >= 0.14, "large", NA),
           size = ifelse(partial.etasq >= 0.06 & partial.etasq < 0.14, "medium", size),
           size = ifelse(partial.etasq >= 0.01 & partial.etasq < 0.06, "small", size)) %>% 
    mutate_if(is.numeric, round, 3)
}

table_anova <- function(outcome){
  fit1 <- aov(as.formula(paste0(outcome, fml)), data = results_X %>% filter(assumption == "met"))
  fit1 <- tidy_func(fit1)
  
  fit2 <- aov(as.formula(paste0(outcome, fml)), data = results_X %>% filter(assumption == "heterosced"))
  fit2 <- tidy_func(fit2)
  
  fit3 <- aov(as.formula(paste0(outcome, fml)), data = results_X %>% filter(assumption == "exogeneity"))
  fit3 <- tidy_func(fit3)
  
  fit1 %>% left_join(fit2, by = "term") %>% 
    left_join(fit3, by = "term") %>% 
    kable(digits = 4, col.names = c("", rep(c("partial_eta_sq", "size"), 3))) %>% 
    kable_styling(full_width = F) %>% 
    add_header_above(c(" " = 1, "Assumptions met" = 2, "heterosced" = 2, "exogeneity" = 2))
}

# Table S4.1---------------------------------------------------------------
results %>% filter(cov == "X" & assumption == "exogeneity") %>% 
  group_by(method, gamma100) %>% 
  summarise(avg = mean(bias)) %>% 
  mutate(prop_avg = avg/gamma100) %>% 
  kable(digits = 7) %>%
  kable_styling(full_width = F) 

# Table S4.2---------------------------------------------------------------
table_anova("bias") 

# Table S4.3---------------------------------------------------------------
table_anova("rmse")

# Table S4.4---------------------------------------------------------------
table_anova("rej_rate")

# Table S4.5---------------------------------------------------------------
results_assump <- results %>% 
  mutate(assumption = ifelse(assumption == "met", "Assumptions met",
                             ifelse(assumption == "heterosced", 
                                    "Homoscedasticity violated", 
                                    "Exogeneity violated")))
results_assump$assumption <- factor(results_assump$assumption, 
                                    levels = c("Assumptions met", 
                                               "Homoscedasticity violated", 
                                               "Exogeneity violated"))
results_assump %>% 
  filter(cov == "X") %>% 
  group_by(assumption, method) %>% 
  summarise(min = min(rej_rate),
            avg = mean(rej_rate),
            max = max(rej_rate)) %>% 
  kable(digits = 3) %>%
  kable_styling(full_width = F) 

# Table S4.6---------------------------------------------------------------
table_anova("coverage")
