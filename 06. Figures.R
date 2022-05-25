# Figures
library(tidyverse)
library(wesanderson)

rm(list = ls())
load("sim_results.RData")
results <- results %>% 
  mutate(method = ifelse(method == "OLS", "OLS-CRVE",
                  ifelse(method == "FE", "FE-CRVE", "CCREM")))
results$method <- factor(results$method, 
                         levels = c("CCREM", "OLS-CRVE", "FE-CRVE"))
results_assump <- results %>% 
  mutate(assumption = ifelse(assumption == "met", "Assumptions met",
                             ifelse(assumption == "heterosced", 
                                    "Homoscedasticity violated", 
                                    "Exogeneity violated")))
results_assump$assumption <- factor(results_assump$assumption, 
                                    levels = c("Assumptions met", 
                                               "Homoscedasticity violated", 
                                               "Exogeneity violated"))

# Figure 1----------------------------------------------------------------
fig1_bias <- results %>%
  mutate(beta = gamma100,
         J = as.character(J)) %>%  
  filter(cov == "X" & assumption == "exogeneity") %>% 
  ggplot(aes(x = J, y = bias, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid( ~ paste0("IUCC = ", ICC_h), scales = "free_y") +  
  labs(x = "Students per school", y = "Parameter Bias") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("30","100")) +
  scale_y_continuous(limits = c(0, .01)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("fig1_bias.tiff", units="in", width=8, height=4.5, res=300)
fig1_bias
dev.off()

# Figure 2----------------------------------------------------------------
fig2_rmse <- results_assump %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X") %>%
  group_by(gamma100, gamma010, gamma002, H, G, ICC_h, J, assumption) %>% 
  mutate(rmse_std = 100*rmse/rmse[method == "CCREM"]) %>% 
  ggplot(aes(x = H, y = rmse_std, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ paste0("IUCC = ", ICC_h), scales = "free_y") + 
  labs(x = "Number of Schools", y = "Root Mean Squared Error (Standardized)") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_y_continuous(limits = c(50, 550)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("fig2_rmse.tiff", units="in", width=8, height=7, res=300)
fig2_rmse
dev.off()

# Figure 3----------------------------------------------------------------
fig3_rej <- results_assump %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X") %>% 
  ggplot(aes(x = H, y = rej_rate, fill = method, color = method)) + 
  geom_hline(yintercept = .05, linetype = "dashed") +
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ paste0("IUCC = ", ICC_h), scales = "free_y") + 
  labs(x = "Number of Schools", y = "Type I error rate") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  # scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("fig3_rej.tiff", units="in", width=8, height=7, res=300)
fig3_rej
dev.off()

# Figure 4----------------------------------------------------------------
fig4_cov <- results_assump %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X") %>% 
  ggplot(aes(x = H, y = coverage, fill = method, color = method)) + 
  geom_hline(yintercept = c(.925, .975), linetype = "dashed") +
  geom_hline(yintercept = c(.95)) +
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ paste0("IUCC = ", ICC_h), scales = "free_y") + 
  labs(x = "Number of Schools", y = "Coverage") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  # scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("fig4_cov.tiff", units="in", width=8, height=7, res=300)
fig4_cov
dev.off()


# Figure S4.1-------------------------------------------------------------
s4_1_bias_met <- results %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X" & assumption == "met") %>% 
  ggplot(aes(x = H, y = bias, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(paste("\u03C1 = ", rho) ~ paste0("IUCC = ", ICC_h), scales = "free_y") +  
  labs(x = "Number of Schools", y = "Parameter Bias") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s4_1_bias_met.tiff", units="in", width=8, height=4.5, res=300)
s4_1_bias_met
dev.off()

# Figure S4.2-------------------------------------------------------------
s4_2_bias_het <- results %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X" & assumption == "heterosced") %>% 
  ggplot(aes(x = H, y = bias, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(paste("\u03C1 = ", rho) ~ paste0("IUCC = ", ICC_h), scales = "free_y") +  
  labs(x = "Number of Schools", y = "Parameter Bias") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s4_2_bias_het.tiff", units="in", width=8, height=4.5, res=300)
s4_2_bias_het
dev.off()



# Figure S4.3-------------------------------------------------------------
s4_3_rmse_met <- results_assump %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X") %>%
  ggplot(aes(x = H, y = rmse, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ paste0("IUCC = ", ICC_h), scales = "free_y") + 
  labs(x = "Number of Schools", y = "Root Mean Squared Error") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s4_3_rmse_met.tiff", units="in", width=8, height=7, res=300)
s4_3_rmse_met
dev.off()

# Figure S4.4-------------------------------------------------------------
s4_4_rmse <- results_assump %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X" & assumption == "Exogeneity violated") %>% 
  group_by(assumption, ICC_h, H) %>% 
  mutate(rmse_std = 100*rmse/rmse[method == "CCREM"]) %>% 
  ggplot(aes(x = H, y = rmse_std, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(paste("\u03C1 = ", rho) ~ paste0("IUCC = ", ICC_h), scales = "free_y") +
  labs(x = "Number of Schools", y = "Root Mean Squared Error") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s4_4_rmse_exo.tiff", units="in", width=8, height=4.5, res=300)
s4_4_rmse
dev.off()





# Figure S4.5 ------------------------------------------------------------
s4_5_convg <- results_assump %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X" & method == "CCREM") %>% 
  group_by(assumption, method, ICC_h, H, J, rho) %>% 
  summarise(non_na_count = sum(!is.na(est)),
            all_count = n()) %>% 
  mutate(pct = non_na_count/all_count) %>% 
  ggplot(aes(x = H, y = pct, fill = method, color = method)) + 
  geom_bar(stat="identity", position=position_dodge(), 
           width=0.75, alpha = .6) +
  facet_grid(assumption + paste("\u03C1 = ", rho) ~ paste0("IUCC = ", ICC_h) + paste("J = ", J), 
             scales = "free_y") +
  labs(x = "Number of Schools", y = "Rate of Convergence") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s4_5_convg.tiff", units="in", width=8, height=10, res=300)
s4_5_convg
dev.off()


