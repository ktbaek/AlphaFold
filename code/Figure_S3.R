all_data <- read_csv("data/all_data.csv")

p1 <- all_data %>% 
  group_by(AF_id, Mer) %>% 
  summarize(
    n_pair = n_distinct(pair)
  ) %>% 
  ggplot() +
  geom_bar(aes(AF_id, n_pair, fill = Mer), stat = "identity", position = "stack") +
  labs(y = "Number of pairs", x = "AF2 structure (UniProt number)") +
  scale_fill_manual(name = "", values = c("#00AFBB", "#FC4E07")) +
  scale_y_continuous(expand = expansion(mult = c(0.003, 0.1))) +
  coord_flip() +
  theme(
    axis.text.y = element_text(size = rel(0.6)),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )

p2 <- all_data %>% 
  group_by(pair) %>% 
  mutate(MAE = mean(abs(main_rel_exp - main_rel_af), na.rm = TRUE)) %>% 
  select(AF_id, MAE, Mer) %>% 
  distinct() %>% 
  ggplot() +
  geom_point(aes(AF_id, MAE, color = Mer), alpha = 0.5, stroke = 0) +
  labs(y = "MAE", x = "AF2 structure (UniProt number)") +
  scale_color_manual(guide = FALSE, name = "", values = c("#00AFBB", "#FC4E07")) +
  scale_y_continuous(limits = c(0, NA)) +
  coord_flip() +
  theme(
    axis.text.y = element_text(size = rel(0.6)),
    legend.position = "bottom"
  )

p1 + p2 + plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("figures/Figure_S3.png", width = 19, height = 29, unit = "cm", dpi = 600)