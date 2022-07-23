# Figures
library(tidyverse)
library(wesanderson)

load("04. Codes/sim_results.RData")
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

# Figure 1----------------------------------------------------------------
fig1_bias <- X %>%
  ggplot(aes(x = H, y = bias, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ J_f, scales = "free_y") +  
  labs(x = "Number of Schools", y = "Parameter Bias") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_y_continuous(limits = c(-.004, .005)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("fig1_bias.tiff", units="in", width=8, height=7, res=300)
fig1_bias
dev.off()

# Figure 2----------------------------------------------------------------
fig2_rmse <- X %>%
  mutate(rmse_std = sqrt(G * J) * rmse) %>% 
  ggplot(aes(x = H, y = rmse_std, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ J_f, scales = "free_y") + 
  labs(x = "Number of Schools", y = expression(RMSE %*% sqrt(N))) + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  expand_limits(y = 0) + 
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("fig2_rmse.tiff", units="in", width=8, height=7, res=300)
fig2_rmse
dev.off()


# Figure 3----------------------------------------------------------------
fig3_cov <- X %>%
  ggplot(aes(x = H, y = coverage, fill = method, color = method)) + 
  geom_hline(yintercept = c(.925, .975), linetype = "dashed") +
  geom_hline(yintercept = c(.95)) +
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ J_f, scales = "free_y") + 
  labs(x = "Number of Schools", y = "Coverage") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_y_continuous(limits = c(NA, 1)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("fig3_cov.tiff", units="in", width=8, height=7, res=300)
fig3_cov
dev.off()

# Figure S4.1-------------------------------------------------------------
s4_1_convg <- X %>% 
  filter(method == "CCREM") %>%
  ggplot(aes(x = H, y = convergence_rate, fill = method, color = method)) + 
  geom_bar(stat="identity", position=position_dodge(), 
           width=0.75, alpha = .6) +
  facet_grid(assumption ~ J_f, 
             scales = "free_y") +
  labs(x = "Number of Schools", y = "Rate of Convergence") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "none",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_fill_manual(values = c("dark grey", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("dark grey", "#00A08A", "#046C9A"))

tiff("s4_1_convg.tiff", units="in", width=8, height=7, res=300)
s4_1_convg
dev.off()


# Figure S5.1-------------------------------------------------------------
s5_1_rmse_met <- X %>%
  ggplot(aes(x = H, y = rmse, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ J_f, scales = "free_y") + 
  labs(x = "Number of Schools", y = "Root Mean Squared Error") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_y_continuous(limits = c(0, .035)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s5_1_rmse_met.tiff", units="in", width=8, height=7, res=300)
s5_1_rmse_met
dev.off()


# Figure S5.2-------------------------------------------------------------
s5_2_rmse <- X %>% 
  mutate(rmse_std = sqrt(G * J) * rmse) %>% 
  ggplot(aes(x = H, y = rmse_std, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ paste0("IUCC = ", ICC_h), scales = "free_y") +
  labs(x = "Number of Schools", y = expression(RMSE %*% sqrt(N))) + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  expand_limits(y = 0) + 
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s5_2_rmse.tiff", units="in", width=8, height=7, res=300)
s5_2_rmse
dev.off()

# Figure S5.3-------------------------------------------------------------
s5_3_rej <- X %>%
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

tiff("s5_3_rej.tiff", units="in", width=8, height=7, res=300)
s5_3_rej
dev.off()

# Figure S6.1.1------------------------------------------------------------
s6_1_1_bias <- W %>%
  ggplot(aes(x = H, y = bias, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ paste0("IUCC = ", ICC_h), 
             scales = "free_y") +  
  labs(x = "Number of Schools", y = "Parameter Bias") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_y_continuous(limits = c(-.006, .01)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s6_1_1_bias.tiff", units="in", width=8, height=7, res=300)
s6_1_1_bias
dev.off()

# Figure S6.1.2------------------------------------------------------------
s6_1_2_bias <- Z %>%
  ggplot(aes(x = H, y = bias, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ paste0("IUCC = ", ICC_h), 
             scales = "free_y") +  
  labs(x = "Number of Schools", y = "Parameter Bias") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_y_continuous(limits = c(-.015, .01)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s6_1_2_bias.tiff", units="in", width=8, height=7, res=300)
s6_1_2_bias
dev.off()

# Figure S6.2.1------------------------------------------------------------
s6_2_1_rmse <- W %>% 
  mutate(rmse_std = sqrt(G) * rmse) %>%  
  ggplot(aes(x = H, y = rmse_std, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ J_f, scales = "free_y") + 
  labs(x = "Number of Schools", y = expression(RMSE %*% sqrt(G))) + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  expand_limits(y = 0) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s6_2_1_rmse.tiff", units="in", width=8, height=7, res=300)
s6_2_1_rmse
dev.off()

# Figure S6.2.2------------------------------------------------------------
s6_2_2_rmse <- Z %>% 
  mutate(rmse_std = sqrt(as.numeric(H)) * rmse) %>%   
  ggplot(aes(x = H, y = rmse_std, fill = method, color = method)) + 
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ J_f, scales = "free_y") + 
  labs(x = "Number of Schools", y = expression(RMSE %*% sqrt(H))) + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  expand_limits(y = 0) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s6_2_2_rmse.tiff", units="in", width=8, height=7, res=300)
s6_2_2_rmse
dev.off()

# Figure S6.3.1------------------------------------------------------------
s6_3_1_cov <- W %>% 
  ggplot(aes(x = H, y = coverage, fill = method, color = method)) + 
  geom_hline(yintercept = c(.925, .975), linetype = "dashed") +
  geom_hline(yintercept = c(.95)) +
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ J_f, scales = "free_y") + 
  labs(x = "Number of Schools", y = "Coverage") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_y_continuous(limits = c(NA, 1)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s6_3_1_cov.tiff", units="in", width=8, height=7, res=300)
s6_3_1_cov
dev.off()

# Figure S6.3.2------------------------------------------------------------
s6_3_2_cov <- Z %>% 
  ggplot(aes(x = H, y = coverage, fill = method, color = method)) + 
  geom_hline(yintercept = c(.925, .975), linetype = "dashed") +
  geom_hline(yintercept = c(.95)) +
  geom_boxplot(alpha = .6, lwd = .1) + 
  facet_grid(assumption ~ J_f, scales = "free_y") + 
  labs(x = "Number of Schools", y = "Coverage") + 
  theme_bw() +
  theme(text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.caption=element_text(hjust = 0),
        axis.text.x = element_text(angle=90, hjust=1)) +
  scale_x_discrete(limits=c("20","70","150")) +
  scale_y_continuous(limits = c(NA, 1)) +
  scale_fill_manual(values = c("#FF0000", "#00A08A", "#046C9A")) +
  scale_color_manual(values = c("#FF0000", "#00A08A", "#046C9A"))

tiff("s6_3_2_cov.tiff", units="in", width=8, height=7, res=300)
s6_3_2_cov
dev.off()