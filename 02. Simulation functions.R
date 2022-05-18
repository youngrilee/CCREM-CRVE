# Data Generating Model----------------------------------------------------
generate_dat <- function(gamma000, gamma100, gamma010, gamma002,
                         G, H, ICC_g, ICC_h, tau_G10, sparse, J,
                         L1cov_m, L1cov_sd, L2cov_m, L2cov_sd,
                         assumption){
  
  # set sigma, tau_G00 and tau_H00 based on ICC
  tau_G00 = ICC_g # Neighborhood
  tau_H00 = ICC_h # School
  sigma = sqrt(1-tau_G00-tau_H00)
  
  # data assignment
  dat <- rerun(.n = H, runif(J, min = 1, max = sparse*G)) %>% 
    map_df(~ tibble(sparsity = .x),
           .id = "schid") %>% as.data.frame() %>% 
    mutate(schid = as.numeric(schid),
           neighid = schid * (G / H),
           neighid = round(neighid + sparsity),
           neighid = ifelse(neighid > G, neighid-G, neighid)) 
  
  # neighborhood data
  # create between-neighborhood variance of X 
  X_bw_neigh <- dat %>% group_by(neighid) %>% tally() %>% 
    dplyr::select(-n) %>% ungroup() %>% 
    mutate(X_bw_neigh = rnorm(nrow(.), mean = L1cov_m, sd = sqrt(.2*L1cov_sd^2)))
  
  if (assumption == "exogeneity"){
    # when exogeneity assumption is violated
    r_g <- 0.4 # correlation between X_bw_neigh and b_0g0
    
    neighbordata <-
      X_bw_neigh %>% 
      mutate(W = rnorm(nrow(.), mean = L2cov_m, sd = L2cov_sd),
             v_0g0 = rnorm(nrow(.), mean = 0, sd = sqrt((1-r_g^2)*tau_G00)),
             b_0g0 = r_g*sqrt(tau_G00/(.2*L1cov_sd^2))*X_bw_neigh + v_0g0, # neighborhood random effect
             v_1g0 = rnorm(nrow(.), mean = 0, sd = sqrt((1-r_g^2)*tau_G10)),
             b_1g0 = r_g*sqrt(tau_G00/(.2*L1cov_sd^2))*X_bw_neigh + v_1g0) %>% # random slope for X
      dplyr::select(-c(v_0g0, v_1g0))
  } else {
    # when all assumptions are met:
    neighbordata <-
      X_bw_neigh %>% 
      mutate(W = rnorm(nrow(.), mean = L2cov_m, sd = L2cov_sd),
             b_0g0 = rnorm(nrow(.), mean = 0, sd = sqrt(tau_G00)), # neighborhood random effect
             b_1g0 = rnorm(nrow(.), mean = 0, sd = sqrt(tau_G10))) # random slope for X
  }
  
  dat <- dat %>% left_join(neighbordata, by = "neighid") 
  
  # school data
  schooldata <- dat %>% 
    group_by(schid, neighid) %>%
    summarise(b_0g0 = mean(b_0g0)) %>% # random effect per neighborhood
    ungroup() %>% group_by(schid) %>%
    summarise(sumI_b_0g0 = sum(b_0g0), # sum of neighborhood random effects per school
              Q = n()) %>% # average number of neighborhood connected to each school
    ungroup %>% 
    mutate(Z = rnorm(H, mean = L2cov_m, sd = L2cov_sd), # neighborhood-level W
           c_00h = rnorm(H, mean = 0, sd = sqrt(tau_H00)))
  
  dat <- dat %>% left_join(schooldata, by = "schid")
  
  # student data
  # between-school and within variance of X
  X_bw_school <- dat %>% group_by(schid) %>% tally() %>% 
    dplyr::select(-n) %>% ungroup() %>% 
    mutate(X_bw_school = rnorm(nrow(.), mean = L1cov_m, sd = sqrt(.2*L1cov_sd^2)))
  
  X_within <- data.frame(
    X_within = rnorm(nrow(dat), mean = L1cov_m, sd = sqrt(.6*L1cov_sd^2)))
  
  # student-level X
  dat <- dat %>% 
    left_join(X_bw_school, by = "schid") %>% 
    bind_cols(X_within) %>% 
    mutate(X = X_bw_neigh + X_bw_school + X_within)
  
  # student-level residuals u
  if (assumption == "heterosced") {
    dat <- dat %>% 
      mutate(u = rnorm(nrow(.), mean = 0, 
                       sd = L1cov_sd*sqrt(exp((15*X - 50)/15^2)))) 
  } else { # assumption met
    dat <- dat %>% 
      mutate(u = rnorm(nrow(.), mean = 0, sd = sigma))
  }
  
  dat <- dat %>% 
    mutate(stuid = 1:nrow(.)) %>% 
    dplyr::select(stuid, schid, neighid, X, W, Z, b_1g0, b_0g0, c_00h, u) %>% 
    mutate(y = gamma000 + (gamma100 + b_1g0)*X + gamma010*W + gamma002*Z + b_0g0 + c_00h + u)
  
  return(dat)
}

# Model-fitting/Estimation-------------------------------------------------

## CCREM
estimate_ccrem <- function(dat){
  
  # estimation
  model <- lmer(y ~ 1 + X + W + Z + (1 | schid) + (1 | neighid), 
                data = dat)
  summary <- summary(model)
  
  fixed_est <- summary$coefficients %>% as.data.frame() %>% 
    dplyr::select(Estimate, `Std. Error`, `Pr(>|t|)`)
  fixed_est <- cbind(cov = rownames(fixed_est), fixed_est)
  fixed_est <- remove_rownames(fixed_est)
  fixed_est <- fixed_est %>% 
    mutate(cov = as.character(cov), method = "CCREM") %>% 
    rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>% 
    dplyr::select(cov, method, everything()) %>% 
    filter(cov %in% c("X", "W", "Z"))
  
  #convergence
  if(is.na(is.na(model@optinfo$conv$lme4)[1])){
    fixed_est <- fixed_est

  } else{
    fixed_est <- fixed_est %>%
      mutate(est = NA, se = NA, pval = NA)
  }
  
  return(fixed_est)
}

## OLS-CRVE
estimate_ols <- function(dat){
  # estimation
  model_ols <- felm(y ~ X + W + Z | 0 | 0 | schid + neighid, 
                    data = dat, psdef = TRUE) # see the vignette
  summary <- coeftest(model_ols)
  
  # fixed effects
  fixed_est <- summary[2:4, c(1, 2, 4)] %>% as.data.frame() %>% 
    rownames_to_column("cov") %>% 
    mutate_if(is.factor, as.character) %>%
    mutate(method = "OLS") %>% 
    rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>% 
    dplyr::select(cov, method, est, se, pval) 
  return(fixed_est)
}

## FE-CRVE
estimate_fe <- function(dat){
  # estimation
  ## felm(equation | fixed effect | 0 | clustering)
  model_fem <- felm(y ~ X | schid + neighid | 0 | schid + neighid, 
                    data = dat, psdef = TRUE)
  summary <- summary(model_fem)
  
  # fixed effects
  fixed_est <- summary$coefficients[, c(1, 2, 4)] %>% t() %>% 
    as.data.frame() %>% 
    mutate(cov = "X", method = "FE") %>% 
    dplyr::select(cov, method, everything()) %>% 
    rename(est = Estimate, se = `Cluster s.e.`, pval = `Pr(>|t|)`)
  return(fixed_est)
}

# bind_results
estimate <- function(dat, gamma100, gamma010, gamma002){
  
  results <- bind_rows(estimate_ccrem(dat),
                       estimate_ols(dat),
                       estimate_fe(dat))
  results <- results %>% 
    mutate(var = se^2,
           lower_bound = est - 1.96*se,
           upper_bound = est + 1.96*se,
           param = ifelse(cov == "X", gamma100,
                          ifelse(cov == "W", gamma010, 
                                 ifelse(cov == "Z", gamma002, NA)))) %>% 
    as_tibble()

  return(results)
}


# Performance calculations ------------------------------------------------
calc_performance <- function(results) {
  
  abs_crit <- results %>%
    group_by(method, cov) %>%
    group_modify(~ calc_absolute(.x, estimates = est, true_param = param))
  
  rel_crit <- results %>%
    group_by(method, cov) %>%
    group_modify(~ calc_relative(.x, estimates = est, true_param = param)) 
  
  # Relative Criteria for Variance Estimators
  rel_crit_val <- results %>% 
    group_by(method, cov) %>%
    group_modify(~ calc_relative_var(.x, estimates = est, var_estimates = var))
  
  # Hypothesis Testing
  rejection_rate <- results %>% 
    dplyr::select(-var) %>% 
    group_by(method, cov) %>% 
    mutate(rej_rate = mean(ifelse(abs(est - param)/se >= 1.96, 1, 0), na.rm = T))
  
  power <- results %>%
    group_by(method, cov) %>%
    group_modify(~ calc_rejection(.x, p_values = pval)) %>% 
    rename(power = rej_rate) 
  
  #  Confidence Intervals
  conf_int <- results %>%
    group_by(method, cov) %>%
    group_modify(~ calc_coverage(.x, lower_bound = lower_bound, 
                                 upper_bound = upper_bound, 
                                 true_param = param))
  
  performance_measures <- rejection_rate %>% 
    left_join(abs_crit, by = c("method", "cov")) %>% 
    left_join(rel_crit, by = c("method", "cov", "K")) %>% 
    left_join(rel_crit_val, by = c("method", "cov", "K")) %>% 
    left_join(power, by = c("method", "cov", "K")) %>% 
    left_join(conf_int, by = c("method", "cov", "K"))
  
  return(performance_measures)
}

# Simulation driver -------------------------------------------------------
run_sim <- function(iterations, gamma000, gamma100, gamma010, gamma002, 
                    G, H, ICC_g, ICC_h, tau_G10, sparse, J, 
                    L1cov_m, L1cov_sd, L2cov_m, L2cov_sd, assumption,
                    seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  results <-
    rerun(iterations, {
      data <- generate_dat(
        gamma000 = gamma000, gamma100 = gamma100,
        gamma010 = gamma010, gamma002 = gamma002,
        G = G, H = H, ICC_g = ICC_g, ICC_h = ICC_h, tau_G10 = tau_G10,
        sparse = sparse, J = J,
        L1cov_m = L1cov_m, L1cov_sd = L1cov_sd,
        L2cov_m = L2cov_m, L2cov_sd = L2cov_sd, assumption = assumption)
      estimate(dat = data, gamma100 = gamma100, gamma010 = gamma010,
               gamma002 = gamma002)
    }) %>%
    bind_rows()
  
  calc_performance(results)

}
