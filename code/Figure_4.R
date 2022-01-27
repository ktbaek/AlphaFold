all_data <- read_csv("data/all_data.csv")

dataset_names <- c(
  "all_100" = "100%",
  "hi_99" = ">99%, <100%, <1.5Å",
  "lo_99" = ">99%, <100%, 1.5-2.0Å")

error_names <- c(
  "abs" = "Absolute error",
  "signed" = "Signed deviation")

af_average <- function(df) {
  
  df %>% 
    group_by(AF, Number_af, Wild, Mer) %>% 
    summarize(
      main_rel_exp = mean(main_rel_exp, na.rm = TRUE),
      main_rel_af = mean(main_rel_af, na.rm = TRUE)
    ) %>% 
    mutate(
      abs = abs(main_rel_af - main_rel_exp),
      signed = main_rel_af - main_rel_exp
    ) %>% 
    pivot_longer(c(abs, signed), names_to = "error_type", values_to = "error") %>% 
    group_by(Wild, error_type, Mer) %>%
    mutate(n = n())
  
}

all_data %>% 
  af_average() %>% 
  filter(Mer == "Monomer") %>% 
  ggplot() +
  stat_summary(
    aes(Wild, error),
    fun = sd,
    size = 1,
    geom = "point",
    color = "blue"
  ) +
  geom_bar(aes(Wild, error), stat="summary", fun="mean") +
  labs(y = "Error (RSA)", x = "Amino acid") +
  facet_wrap(~error_type, labeller = labeller(
    error_type = error_names), scales = "free_y") +
  scale_color_manual(values = rep(c("#00AFBB", "#FC4E07"), 10)) +
  
  theme(
    legend.position = "none"
  )

ggsave("figures/Figure_4.png", width = 17, height = 9, unit = "cm", dpi = 600)