# Figures
library(tidyverse)
library(wesanderson)

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

# Figure 1----------------------------------------------------------------
fig1_bias <- X %>%
  filter(assumption == "Exogeneity violated") %>% 
  ggplot(aes(x = H, y = bias, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(J_f ~ paste0("School IUCC = ", ICC_h), 
             scales = "free_y") +  
  labs(x = "Number of Schools", y = "Parameter Bias") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_y_continuous(limits = c(0, .005)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("fig1_bias.tiff", units="in", width=8, height=4.5, res=300)
fig1_bias
dev.off()

# Figure 2----------------------------------------------------------------
fig2_rmse <- X %>%
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
fig3_rej <- X %>%
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
fig4_cov <- X %>%
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


# Figure S5.1-------------------------------------------------------------
s5_1_bias_met <- results_assump %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X" & assumption == "Assumptions met") %>% 
  ggplot(aes(x = H, y = bias, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid( ~ paste0("IUCC = ", ICC_h), scales = "free_y") +  
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

tiff("s5_1_bias_met.tiff", units="in", width=8, height=4.5, res=300)
s5_1_bias_met
dev.off()

# Figure S5.2-------------------------------------------------------------
s5_2_bias_het <- results_assump %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X" & assumption == "Homoscedasticity violated") %>% 
  ggplot(aes(x = H, y = bias, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid( ~ paste0("IUCC = ", ICC_h), scales = "free_y") +  
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

tiff("s5_2_bias_het.tiff", units="in", width=8, height=4.5, res=300)
s5_2_bias_het
dev.off()



# Figure S5.3-------------------------------------------------------------
s5_3_rmse_met <- results_assump %>%
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

tiff("s5_3_rmse_met.tiff", units="in", width=8, height=7, res=300)
s5_3_rmse_met
dev.off()

# Figure S5.4-------------------------------------------------------------
s5_4_rmse <- results_assump %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(cov == "X" & assumption == "Exogeneity violated") %>% 
  group_by(assumption, ICC_h, H) %>% 
  mutate(rmse_std = 100*rmse/rmse[method == "CCREM"]) %>% 
  ggplot(aes(x = H, y = rmse_std, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid( ~ paste0("IUCC = ", ICC_h), scales = "free_y") +
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

tiff("s5_4_rmse_exo.tiff", units="in", width=8, height=4.5, res=300)
s5_4_rmse
dev.off()





# Figure S5.5 ------------------------------------------------------------
s5_5_convg <- results_assump %>%
  mutate(beta = gamma100,
         H = as.character(H)) %>% 
  filter(method == "CCREM") %>% 
  ggplot(aes(x = H, y = convergence_rate, fill = method, color = method)) + 
  geom_bar(stat="identity", position=position_dodge(), 
           width=0.75, alpha = .6) +
  facet_grid(assumption ~ paste0("IUCC = ", ICC_h) + paste("J = ", J), 
             scales = "free_y") +
  labs(x = "Number of Schools", y = "Rate of Convergence") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_fill_manual(values = c("grey", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("grey", "#00A08A", "#046C9A"))

tiff("s5_5_convg.tiff", units="in", width=8, height=10, res=300)
s5_5_convg
dev.off()


