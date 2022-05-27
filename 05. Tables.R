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
results <- 
  results %>% 
  mutate(method = recode(method, 
                         "OLS" = "OLS-CRVE",
                         "FE" = "FE-CRVE"),
         method = factor(method, levels = c("CCREM", "OLS-CRVE", "FE-CRVE")),
         beta = as.factor(gamma100),
         H = as.factor(H),
         J = factor(J, levels = c("30", "100")),
         ICC_h = as.factor(ICC_h))

results_assump <- 
  results %>% 
  filter(assumption %in% c("met", "heterosced", "exogeneity")) %>% 
  mutate(assumption = recode(assumption,
                             "met" = "Assumptions met",
                             "heterosced" = "Homoscedasticity violated",
                             "exogeneity" = "Exogeneity violated"),
         assumption = factor(assumption, 
                             levels = c("Assumptions met", 
                                        "Homoscedasticity violated", 
                                        "Exogeneity violated")))

X <- results_assump %>% filter(cov == "X")
W <- results_assump %>% filter(cov == "W")


# ANOVA functions ---------------------------------------------------------------
fml <- " ~ as.factor(method)+as.factor(beta)+as.factor(H)+as.factor(J)+as.factor(ICC_h)+
        as.factor(method)*(as.factor(beta)+as.factor(H)+as.factor(J)+as.factor(ICC_h))+
        as.factor(beta)*(as.factor(H)+as.factor(J)+as.factor(ICC_h))+
        as.factor(H)*(as.factor(J)+as.factor(ICC_h))+
        as.factor(J)*(as.factor(ICC_h))"

tidy_func <- function(obj){
  anova_stats(obj) %>% as_tibble %>% 
    select(term, partial.etasq) %>% 
    mutate(size = ifelse(partial.etasq >= 0.14, "large", NA),
           size = ifelse(partial.etasq >= 0.06 & partial.etasq < 0.14, "medium", size),
           size = ifelse(partial.etasq >= 0.01 & partial.etasq < 0.06, "small", size)) %>% 
    mutate_if(is.numeric, round, 3)
}

table_anova <- function(outcome, cov){
  fit1 <- aov(as.formula(paste0(outcome, fml)), data = cov %>% filter(assumption == "Assumptions met"))
  fit1 <- tidy_func(fit1)
  
  fit2 <- aov(as.formula(paste0(outcome, fml)), data = cov %>% filter(assumption == "Homoscedasticity violated"))
  fit2 <- tidy_func(fit2)
  
  fit3 <- aov(as.formula(paste0(outcome, fml)), data = cov %>% filter(assumption == "Exogeneity violated"))
  fit3 <- tidy_func(fit3)
  
  options(knitr.kable.NA = '')
  fit1 %>% left_join(fit2, by = "term") %>% 
    left_join(fit3, by = "term") %>% filter(term != "Residuals") %>% 
    kable(digits = 4, col.names = c("", rep(c("partial_eta_sq", "size"), 3))) %>% 
    kable_styling(full_width = F) %>% 
    add_header_above(c(" " = 1, "Assumptions met" = 2, "heterosced" = 2, "exogeneity" = 2)) %>% 
    scroll_box(height = "255px")
}

# Table S5.1---------------------------------------------------------------
results %>% filter(cov == "X" & assumption == "exogeneity") %>% 
  group_by(method, gamma100) %>% 
  summarise(avg = mean(bias), .groups = "drop") %>% 
  mutate(prop_avg = avg/gamma100) %>% 
  kable(digits = 7) %>%
  kable_styling(full_width = F) 

# Table S5.2---------------------------------------------------------------
table_anova("bias", X) 

# Table S5.3---------------------------------------------------------------
table_anova("rmse", X)

# Table S5.4---------------------------------------------------------------
table_anova("rej_rate", X)

# Table S5.5---------------------------------------------------------------
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
  summarise(min = min(rej_rate, na.rm = TRUE),
            avg = mean(rej_rate, na.rm = TRUE),
            max = max(rej_rate, na.rm = TRUE), .groups = "drop") %>% 
  kable(digits = 3) %>%
  kable_styling(full_width = F) 

# Table S5.6---------------------------------------------------------------
table_anova("coverage", X)
