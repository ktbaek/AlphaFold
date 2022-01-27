coverage <- read_csv("data/coverage_no_mut_no_mid.csv")

coverage %>% 
  ggplot() +
  geom_point(aes(AF_len, Chain_len), alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  coord_obs_pred() +
  labs(x = "AlphaFold sequence length", y = "Experimental sequence length")

ggsave("figures/Figure_S1.png", width = 15, height = 15, units = "cm", dpi = 600)