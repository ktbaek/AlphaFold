all_data <- read_csv("data/all_data.csv")

p1 <- all_data %>% 
  filter(AF_id == "P62937", Mer == "Monomer") %>%
  group_by(Number_af) %>% 
  summarize(sd = sd(main_rel_exp, na.rm = TRUE)) %>% 
  ggplot() +
  geom_bar(aes(Number_af, sd), stat = "identity") +
  scale_x_continuous(expand = expansion(mult = 0.01)) +
  labs(
    x = "Residue number",
    y = expression(SD["RSA"]))

p2 <- all_data %>% 
  filter(AF_id == "P62937", Mer == "Monomer") %>%
  group_by(Number_af) %>% 
  summarize(abs = abs(main_rel_af - main_rel_exp)) %>% 
  ggplot() +
  geom_point(aes(Number_af, abs), size = rel(0.15)) + 
  scale_x_continuous(expand = expansion(mult = 0.01)) +
  labs(
    x = "Residue number",
    y = "Absolute error"
  )

plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 1), labels = "AUTO", scale = 1)

ggsave("figures/Figure_S9.png", width = 19, height = 15, unit = "cm", dpi = 600)