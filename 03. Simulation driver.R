library(tidyverse)
library(lme4)
library(sandwich)
library(lmtest)
library(lmerTest)
library(lfe)
library(truncnorm)
library(purrr)
library(simhelpers) # citation("simhelpers")
library(psych)
library(future)
library(furrr)

rm(list = ls())
source("04. Codes/02. simulation functions.R")

# simulation parameters as vectors/lists
design_factors <- list(
  gamma000 = 0, gamma100 = c(0.01, 0.03, 0.05), 
  gamma010 = c(0.1, 0.3, 0.5), gamma002 = c(0.1, 0.3, 0.5), 
  G = c(70, 245, 525), H = c(20, 70, 150), # H = school; G = neighborhood
  ICC_g = c(.05), ICC_h = c(0.05, 0.15, 0.25), 
  sparse = .1, J = c(30, 100),
  L1cov_m = 0, L1cov_sd = 10,  L2cov_m = 0, L2cov_sd = 1,
  assumption = c("met", "heterosced", "exogeneity", "random slopes")
)

params <- 
  cross_df(design_factors) %>%
  # filter only relevant conditions
  filter(
    (gamma100 == 0.01 & gamma010 == 0.1 & gamma002 == 0.1) |
    (gamma100 == 0.03 & gamma010 == 0.3 & gamma002 == 0.3) |
    (gamma100 == 0.05 & gamma010 == 0.5 & gamma002 == 0.5),
    (G == 70 & H == 20) | (G == 245 & H == 70) | (G ==525 & H == 150)
  ) %>%
  mutate(
    iterations = 20, 
    seed = 20220525 + 1:n()
  )
  
nrow(params)
head(params)

#--------------------------------------------------------
# run simulations in parallel - future + furrr workflow
#--------------------------------------------------------
# pmap
options(error=recover)
plan("multisession") 

system.time(
  results <-
    params %>%
    mutate(res = future_pmap(., .f = run_sim, .options = furrr_options(seed=NULL))) %>%
    unnest(cols = res)
) 

#--------------------------------------------------------
# Save results and details
#--------------------------------------------------------
session_info <- sessionInfo() 
run_date <- date() 
save(results, params, session_info, run_date, file = "sim_results.Rdata")
