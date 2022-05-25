library(tidyverse)
library(lme4)
library(sandwich)
library(lmtest)
library(lmerTest)
library(lfe)
library(knitr)
library(kableExtra)
library(truncnorm)
library(purrr)
library(simhelpers) # citation("simhelpers")
library(psych)
library(future)
library(furrr)

rm(list = ls())
source("04. Codes/02. simulation functions.R")

# Data Generating Model----------------------------------------------------

dat <- generate_dat(
  # model parameters
  gamma000 = 0,        # overall average
  gamma100 = 0.03,     # coefficient for X (student)
  gamma010 = 0.3,      # coefficient for W (school)
  gamma002 = 0.3,      # coefficient for Z (neighborhood)
  G = 70,              # neighbor g = {1..G}
  H = 20,              # school h = {1..H}
  ICC_g = 0.05,        # neighbor ICC
  ICC_h = 0.15,        # school ICC
  tau_G10 = .05,       # random slope variance (neighborhood)
  sparse = .1,         # sparsity 
  J = 30,              # average number of students per school
  L1cov_m = 0,
  L1cov_sd = 10,
  L2cov_m = 0,
  L2cov_sd = 1, 
  assumption = "exogeneity" # heterosced, exogeneity or met
)  

dat %>% head()

# Model-fitting/Estimation-------------------------------------------------
estimate_ccrem(dat)
estimate_ols(dat)
estimate_fe(dat)
estimate(dat = dat)

# Performance calculations ------------------------------------------------
gamma100 = 0.03
gamma010 = 0.3
gamma002 = 0.3

results <-
  rerun(10, {
    generate_dat(
      gamma000 = 0,        # overall average
      gamma100 = gamma100, # coefficient for X (student)
      gamma010 = gamma010, # coefficient for W (school)
      gamma002 = gamma002, # coefficient for Z (neighborhood)
      G = 70,              # neighbor g = {1..G}
      H = 20,              # school h = {1..H}
      ICC_g = 0.05,        # neighbor ICC
      ICC_h = 0.15,        # school ICC
      tau_G10 = .05,       # random slope variance (neighborhood)
      sparse = .1,         # sparsity 
      J = 30,              # average number of students per school
      L1cov_m = 0,
      L1cov_sd = 10,
      L2cov_m = 0,
      L2cov_sd = 1, 
      assumption = "exogeneity"
    ) %>%
      estimate()
  }) %>%
  bind_rows() %>%
  mutate(
    param = recode(cov, X = gamma100, W = gamma010, Z = gamma002)
  )

calc_performance(results)

# Simulation driver -------------------------------------------------------
run_sim(iterations = 10, 
        gamma000 = 0, gamma100 = 0.03, gamma010 = 0.3, gamma002 = 0.3,  
        G = 70, H = 20, ICC_g = 0.05, ICC_h = 0.15, tau_G10 = .05, 
        sparse = .1, J = 30,
        L1cov_m = 0, L1cov_sd = 10,  L2cov_m = 0, L2cov_sd = 1, 
        assumption = "met")


# Check "met" condition --------------------------------------------------------
gamma100 = 0.03
gamma010 = 0.3
gamma002 = 0.3
ICC_g = 0.05
ICC_h = 0.15
L1cov_sd = 10
tau_G10 = 0

dat_met <- generate_dat(
  gamma000 = 0,        # overall average
  gamma100 = gamma100, # coefficient for X (student)
  gamma010 = gamma010, # coefficient for W (school)
  gamma002 = gamma002, # coefficient for Z (neighborhood)
  G = 1000,            # neighbor g = {1..G}
  H = 1000,            # school h = {1..H}
  ICC_g = ICC_g,       # neighbor ICC
  ICC_h = ICC_h,       # school ICC
  tau_G10 = tau_G10,   # random slope variance (neighborhood)
  sparse = .1,         # sparsity 
  J = 30,              # average number of students per school
  L1cov_m = 0,
  L1cov_sd = L1cov_sd,
  L2cov_m = 0,
  L2cov_sd = 1, 
  assumption = "met" # heterosced, exogeneity or met
)

model <- lmer(y ~ 1 + X + W + Z + (1 | schid) + (1 | neighid), data = dat_met)
summary(model) # Variance components and fixed effects should closely match parameters

# neighborhood effects
dat_neigh <- 
  dat_met %>% 
  select(neighid, W, b_0g0, b_1g0, X_bw_neigh) %>%
  distinct()
dat_neigh %>%
  summarise(
    across(-neighid, list(M = ~ mean(.x), SD = ~ sd(.x)))
  )
sqrt(ICC_g)          # should match sd of b_0g0
L1cov_sd * sqrt(0.2) # should match sd of X_bw_neigh
sqrt(tau_G10)        # should match sd of b_1g0

cor(dat_neigh) # should all be near zero

# school effects
dat_sch <- 
  dat_met %>% 
  select(schid, Z, c_00h, X_bw_school) %>%
  distinct()
dat_sch %>%
  summarise(
    across(-schid, list(M = ~ mean(.x), SD = ~ sd(.x)))
  )
sqrt(ICC_h)          # should match sd of c_00h
L1cov_sd * sqrt(0.2) # should match sd of X_bw_schol

cor(dat_sch) # should all be near zero

# student effects
sd(dat_met$u)
sqrt(1 - ICC_g - ICC_h) # should match sd of u
lm_fit <- lm(u ~ X, data = dat_met)
plot(lm_fit)

# Check random slopes condition --------------------------------------------------------
tau_G10 = 0.25

dat_slopes <- generate_dat(
  gamma000 = 0,        # overall average
  gamma100 = gamma100, # coefficient for X (student)
  gamma010 = gamma010, # coefficient for W (school)
  gamma002 = gamma002, # coefficient for Z (neighborhood)
  G = 1000,            # neighbor g = {1..G}
  H = 1000,            # school h = {1..H}
  ICC_g = ICC_g,       # neighbor ICC
  ICC_h = ICC_h,       # school ICC
  tau_G10 = tau_G10,   # random slope variance (neighborhood)
  sparse = .1,         # sparsity 
  J = 30,              # average number of students per school
  L1cov_m = 0,
  L1cov_sd = L1cov_sd,
  L2cov_m = 0,
  L2cov_sd = 1, 
  assumption = "met" # heterosced, exogeneity or met
)

model <- lmer(y ~ 1 + X + W + Z + (1 | schid) + (1 + W | neighid), data = dat_slopes)
summary(model) # Variance components and fixed effects should closely match parameters

# neighborhood effects
dat_neigh <- 
  dat_slopes %>% 
  select(neighid, W, b_0g0, b_1g0, X_bw_neigh) %>%
  distinct()
dat_neigh %>%
  summarise(
    across(-neighid, list(M = ~ mean(.x), SD = ~ sd(.x)))
  )
sqrt(ICC_g)          # should match sd of b_0g0
L1cov_sd * sqrt(0.2) # should match sd of X_bw_neigh
sqrt(tau_G10)        # should match sd of b_1g0

# Check endogeneity condition --------------------------------------------------

dat_endo <- generate_dat(
  gamma000 = 0,        # overall average
  gamma100 = gamma100, # coefficient for X (student)
  gamma010 = gamma010, # coefficient for W (school)
  gamma002 = gamma002, # coefficient for Z (neighborhood)
  G = 1000,              # neighbor g = {1..G}
  H = 1000,              # school h = {1..H}
  ICC_g = ICC_g,        # neighbor ICC
  ICC_h = ICC_h,        # school ICC
  tau_G10 = tau_G10,       # random slope variance (neighborhood)
  sparse = .1,         # sparsity 
  J = 30,              # average number of students per school
  L1cov_m = 0,
  L1cov_sd = L1cov_sd,
  L2cov_m = 0,
  L2cov_sd = 1, 
  assumption = "exogeneity" # heterosced, exogeneity or met
)  

# neighborhood effects
dat_neigh <- 
  dat_endo %>% 
  select(neighid, W, b_0g0, b_1g0, X_bw_neigh) %>%
  distinct()
dat_neigh %>%
  summarise(
    across(-neighid, list(M = ~ mean(.x), SD = ~ sd(.x)))
  )
sqrt(ICC_g)          # should match sd of b_0g0
L1cov_sd * sqrt(0.2) # should match sd of X_bw_neigh
sqrt(tau_G10)        # should match sd of b_1g0

cor(dat_neigh) # should be correlation of 0.4 between X_bw_neigh and b_0g0 and b_1g0

# school effects
dat_sch <- 
  dat_endo %>% 
  select(schid, Z, c_00h, X_bw_school) %>%
  distinct()
dat_sch %>%
  summarise(
    across(-schid, list(M = ~ mean(.x), SD = ~ sd(.x)))
  )
sqrt(ICC_h)          # should match sd of c_00h
L1cov_sd * sqrt(0.2) # should match sd of X_bw_schol

cor(dat_sch) # should all be near zero

# student effects
sd(dat_endo$u)
sqrt(1 - ICC_g - ICC_h) # should match sd of u
lm_fit <- lm(u ~ X, data = dat_endo)
plot(lm_fit)

# Check heteroskedasticity condition -------------------------------------------

dat_het <- generate_dat(
  gamma000 = 0,        # overall average
  gamma100 = gamma100, # coefficient for X (student)
  gamma010 = gamma010, # coefficient for W (school)
  gamma002 = gamma002, # coefficient for Z (neighborhood)
  G = 1000,            # neighbor g = {1..G}
  H = 1000,            # school h = {1..H}
  ICC_g = ICC_g,       # neighbor ICC
  ICC_h = ICC_h,       # school ICC
  tau_G10 = tau_G10,   # random slope variance (neighborhood)
  sparse = .1,         # sparsity 
  J = 30,              # average number of students per school
  L1cov_m = 0,
  L1cov_sd = L1cov_sd,
  L2cov_m = 0,
  L2cov_sd = 1, 
  assumption = "heterosced" # heterosced, exogeneity or met
)  


# neighborhood effects
dat_neigh <- 
  dat_het %>% 
  select(neighid, W, b_0g0, b_1g0, X_bw_neigh) %>%
  distinct()
dat_neigh %>%
  summarise(
    across(-neighid, list(M = ~ mean(.x), SD = ~ sd(.x)))
  )
sqrt(ICC_g)          # should match sd of b_0g0
L1cov_sd * sqrt(0.2) # should match sd of X_bw_neigh
sqrt(tau_G10)        # should match sd of b_1g0

cor(dat_neigh) # should all be near zero

# school effects
dat_sch <- 
  dat_het %>% 
  select(schid, Z, c_00h, X_bw_school) %>%
  distinct()
dat_sch %>%
  summarise(
    across(-schid, list(M = ~ mean(.x), SD = ~ sd(.x)))
  )
sqrt(ICC_h)          # should match sd of c_00h
L1cov_sd * sqrt(0.2) # should match sd of X_bw_schol

cor(dat_sch) # should all be near zero

# student effects
sd(dat_het$u)
sqrt(1 - ICC_g - ICC_h) # should match sd of u
lm_fit <- lm(u ~ X, data = dat_het)
plot(lm_fit)

