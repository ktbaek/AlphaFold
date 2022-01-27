interface <- read_csv("data/interface.csv") %>% 
  unite("Experimental", c(PDB, Chain_exp), sep = ":") %>% 
  mutate(Interface = TRUE) %>% 
  select(-X1)

PDBs <- unique(interface$Experimental)

plot_data <- read_csv("data/all_data.csv") %>% 
  filter(
    Mer == "Multimer",
    Experimental %in% PDBs) %>% 
  full_join(interface, by = c("Experimental", "Number_exp")) %>% 
  mutate(Interface = ifelse(!is.na(Interface), "Interface residues", "Without interface residues"))

p1 <- plot_data %>% 
  group_by(Interface) %>% 
  mutate(R = sqrt(rsquared(main_rel_exp, main_rel_af))) %>% 
  ggplot() +
  geom_point(aes(main_rel_af, main_rel_exp), size = rel(0.3),stroke = 0, alpha = 0.3) +
  scale_x_continuous(limits = c(0,2)) +
  scale_y_continuous(limits = c(0,2)) +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  geom_text(
    aes(
      x = 0, 
      y = 2, 
      label = paste0("R = ", sprintf("%.2f",round(R,2)))
    ),
    hjust = 0,
    vjust = 1,
    size = rel(4),
    check_overlap = TRUE) +
  labs(x = expression(RSA["AF"]), y = expression(RSA["Exp"])) +
  facet_grid(~Interface) 


p2 <- plot_data %>% 
  group_by(AF, Number_af, Interface) %>% 
  summarize(
    main_rel_exp = mean(main_rel_exp, na.rm = TRUE),
    main_rel_af = mean(main_rel_af, na.rm = TRUE)
  ) %>% 
  group_by(Interface) %>% 
  mutate(R = sqrt(rsquared(main_rel_exp, main_rel_af))) %>% 
  ggplot() +
  geom_point(aes(main_rel_af, main_rel_exp), size = rel(0.3),stroke = 0, alpha = 0.3) +
  scale_x_continuous(limits = c(0,2)) +
  scale_y_continuous(limits = c(0,2)) +
  geom_abline(slope = 1, intercept = 0, color = "orange") +
  geom_text(
    aes(
      x = 0, 
      y = 2, 
      label = paste0("R = ", sprintf("%.2f",round(R,2)))
    ),
    hjust = 0,
    vjust = 1,
    size = rel(4),
    check_overlap = TRUE) +
  labs(x = expression(RSA["AF"]), y = expression(RSA["Exp"])) +
  facet_grid(~Interface) 

plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 1), labels = "AUTO", scale = 1)

ggsave("figures/Figure_5.png", width = 12, height = 14, unit = "cm", dpi = 600)