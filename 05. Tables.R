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
  mutate(method = recode(method, "OLS" = "OLS-CRVE", "FE" = "FE-CRVE"),
         method = factor(method, levels = c("CCREM", "OLS-CRVE", "FE-CRVE")),
         beta = as.factor(gamma100),
         H = as.factor(H),
         J_f = recode(J, "30" = "Students per school = 30",
                      "100" = "Students per school = 100"),
         J_f = factor(J_f, levels = c("Students per school = 30", 
                                      "Students per school = 100")),
         ICC_h = as.factor(ICC_h), 
         assumption = recode(assumption,
                             "met" = "Assumptions met",
                             "heterosced" = "Homoscedasticity",
                             "exogeneity" = "Exogeneity",
                             "random slopes" = "Random Slopes"),
         assumption = factor(assumption, 
                             levels = c("Assumptions met", 
                                        "Homoscedasticity", 
                                        "Exogeneity",
                                        "Random Slopes")))

X <- results %>% filter(cov == "X") # level-1 cov
W <- results %>% filter(cov == "W") # level-2 cov, neighborhood
Z <- results %>% filter(cov == "Z") # level-2 cov, school

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
  fit1 <- aov(as.formula(paste0(outcome, fml)), 
              data = cov %>% filter(assumption == "Assumptions met"))
  fit1 <- tidy_func(fit1)
  
  fit2 <- aov(as.formula(paste0(outcome, fml)), 
              data = cov %>% filter(assumption == "Homoscedasticity"))
  fit2 <- tidy_func(fit2)
  
  fit3 <- aov(as.formula(paste0(outcome, fml)), 
              data = cov %>% filter(assumption == "Exogeneity"))
  fit3 <- tidy_func(fit3)
  
  fit4 <- aov(as.formula(paste0(outcome, fml)), 
              data = cov %>% filter(assumption == "Random Slopes"))
  fit4 <- tidy_func(fit4)
  
  options(knitr.kable.NA = '')
  fit1 %>% 
    left_join(fit2, by = "term") %>% 
    left_join(fit3, by = "term") %>% 
    left_join(fit4, by = "term") %>% 
    filter(term != "Residuals") %>% 
    mutate(term = c("method", "beta", "Number of Schools (H)", 
                    "Students per School (J)", "ICC_h", "method:beta", 
                    "method:H", "method:J", "method:ICC_h", "beta:H", 
                    "beta:J", "beta:ICC_h", "H:J", "H:ICC_h", "J:ICC_h")) %>% 
    kable(digits = 4, col.names = c("", rep(c("p_eta_sq", "size"), 4))) %>% 
    kable_styling(full_width = F) %>% 
    add_header_above(c(" " = 1, "Assumptions met" = 2, "Heterosced" = 2, 
                       "Exogeneity" = 2, "Random Slopes" = 2)) 
  # scroll_box(height = "255px")
}

# Table S5.1---------------------------------------------------------------
X %>%  
  group_by(assumption, gamma100, method) %>% 
  summarise(avg = mean(bias), .groups = "drop") %>% 
  mutate(prop_avg = round(avg/gamma100, 3)) %>% select(-avg) %>% 
  pivot_wider(., names_from = "method", values_from = "prop_avg") %>% 
  kable(digits = 3) %>%
  kable_styling(full_width = F) 

# Table S5.2---------------------------------------------------------------
table_anova("bias", X)

# Table S5.3---------------------------------------------------------------
X %>% 
  group_by(assumption, method) %>% 
  summarise(mcse = mean(bias_mcse, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(., names_from = "method", values_from = "mcse") %>% 
  kable(digits = 5) %>%
  kable_styling(full_width = F)


# Table S5.4---------------------------------------------------------------
table_anova("rmse", X)

# Table S5.5---------------------------------------------------------------
table_anova("rej_rate", X)

# Table S5.6---------------------------------------------------------------
X %>% 
  group_by(assumption, method) %>% 
  summarise(min = min(rej_rate, na.rm = TRUE),
            avg = mean(rej_rate, na.rm = TRUE),
            max = max(rej_rate, na.rm = TRUE), .groups = "drop") %>% 
  pivot_longer(3:5, names_to = "calc", values_to = "value") %>% 
  pivot_wider(., names_from = "method", values_from = "value") %>% 
  kable(digits = 3) %>%
  kable_styling(full_width = F) 

# Table S6.1.1-------------------------------------------------------------
W %>%  
  group_by(assumption, gamma010, method) %>% 
  summarise(avg = mean(bias), .groups = "drop") %>% 
  mutate(prop_avg = round(avg/gamma010, 3)) %>% select(-avg) %>% 
  pivot_wider(., names_from = "method", values_from = "prop_avg") %>% 
  kable(digits = 3) %>%
  kable_styling(full_width = F) 

Z %>%  
  group_by(assumption, gamma002, method) %>% 
  summarise(avg = mean(bias), .groups = "drop") %>% 
  mutate(prop_avg = round(avg/gamma002, 3)) %>% select(-avg) %>% 
  pivot_wider(., names_from = "method", values_from = "prop_avg") %>% 
  kable(digits = 3) %>%
  kable_styling(full_width = F)

# Table S6.1.2-------------------------------------------------------------
table_anova("bias", W)

# Table S6.1.3-------------------------------------------------------------
table_anova("bias", Z)

# Table S6.2.1-------------------------------------------------------------
table_anova("rmse", W)

# Table S6.2.2-------------------------------------------------------------
table_anova("rmse", Z)

# Table S6.3.1-------------------------------------------------------------
table_anova("coverage", W)

# Table S6.3.2-------------------------------------------------------------
table_anova("coverage", Z)