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
source("01. simulation functions.R")

# simulation parameters as vectors/lists
design_factors <- list(
  gamma000 = 0, gamma100 = c(0, 0.01, 0.03, 0.05), 
  gamma010 = c(0, 0.1, 0.3, 0.5), gamma002 = c(0, 0.1, 0.3, 0.5), 
  H = c(20, 70, 150), G = c(70, 245, 525), 
  ICC_H = c(0.05, 0.15, 0.25), ICC_G = c(.05),
  rho = c(0), sparse = .1, J = c(30, 100),
  L1cov_m = 0, L1cov_sd = 10,  L2cov_m = 0, L2cov_sd = 1,
  assumption = c("met", "heterosced", "exogeneity")
)

params <- cross_df(design_factors) %>%
  mutate(iterations = 2, 
         seed = 20210712 + 1:n()) %>%
  # filter only relevant conditions
  filter((gamma100 == 0.01 & gamma010 == 0.1 & gamma002 == 0.1) |
         (gamma100 == 0.03 & gamma010 == 0.3 & gamma002 == 0.3) |
         (gamma100 == 0.05 & gamma010 == 0.5 & gamma002 == 0.5)) %>%
  filter((H == 20 & G == 70) | (H == 70 & G == 245) | (H == 150 & G ==525))

#--------------------------------------------------------
# run simulations in parallel - future + furrr workflow
#--------------------------------------------------------
# pmap
options(error=recover)
plan(multisession) 
system.time(
  results$res <- pmap(params, .f = run_sim)
  # results <- params %>% mutate(res = pmap(., .f = run_sim)) %>%
  #   unnest(cols = res)
) 

results <- unnest(results, cols = res)

#--------------------------------------------------------
# Save results and details
#--------------------------------------------------------
session_info <- sessionInfo() 
run_date <- date() 
save(results, params, session_info, run_date, file = "sim_results.Rdata")