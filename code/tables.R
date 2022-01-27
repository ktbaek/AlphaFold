all_data <- read_csv("data/all_data.csv")

# min, max, avg sequence length
all_data %>% 
  select(Experimental, count) %>% 
  distinct() %>% 
  summarize(
    mean = mean(count),
    min = min(count),
    max = max(count)
  )

af_average <- function(df) {
  
  df %>% 
    group_by(AF, Number_af, group, dataset, Mer) %>% 
    summarize(
      main_rel_exp = mean(main_rel_exp, na.rm = TRUE),
      main_rel_af = mean(main_rel_af, na.rm = TRUE)
    ) %>% 
    mutate(
      abs = abs(main_rel_af - main_rel_exp),
      signed = main_rel_af - main_rel_exp
    ) 
  
}

# Table 1 ------------------------------------------------

x <- all_data %>% 
  group_by(dataset, Mer) %>% 
  summarize(
    n = n(),
    n_pairs = n_distinct(pair),
    n_af = n_distinct(AF), 
    n_exp = n_distinct(Experimental),
    n_pdb = n_distinct(str_sub(Experimental, 1, 4)),
    MAE = mean(abs(main_rel_exp - main_rel_af), na.rm = TRUE),
    MSD = mean(main_rel_af - main_rel_exp, na.rm = TRUE),
    SD = sd(abs(main_rel_exp - main_rel_af), na.rm = TRUE)
  ) %>% 
  mutate(
    MAE = sprintf("%.3f",round(MAE,3)),
    MSD = sprintf("%.3f",round(MSD,3)),
    SD = sprintf("%.3f",round(SD,3)),
    sequence_overlap = case_when(
      dataset == "all_100" ~ "100%",
      dataset == "hi_99" ~ ">99% <100%",
      dataset == "lo_99" ~ ">99% <100%"
    ),
    resolution = case_when(
      dataset == "all_100" ~ "<=2.0",
      dataset == "hi_99" ~ "<= 1.5",
      dataset == "lo_99" ~ ">1.5 <=2.0"
    )
  ) %>% 
  ungroup() %>% 
  select(sequence_overlap, resolution, Mer, n, n_pairs, n_af, MAE, MSD, SD) %>% 
  mutate(Type = "not_weighted")

y <- all_data %>% 
  group_by(AF_id, Number_af, dataset, Mer) %>% 
  summarize(
    abs_error = mean(abs(main_rel_exp - main_rel_af), na.rm = TRUE),
    sign_error = mean(main_rel_af - main_rel_exp, na.rm = TRUE)
  ) %>% 
  group_by(dataset, Mer) %>% 
  summarize(
    n = n(),
    n_af = n_distinct(AF_id), 
    MAE = mean(abs_error, na.rm = TRUE),
    MSD = mean(sign_error, na.rm = TRUE),
    SD = sd(abs_error, na.rm = TRUE)
  ) %>% 
  mutate(
    MAE = sprintf("%.3f",round(MAE,3)),
    MSD = sprintf("%.3f",round(MSD,3)),
    SD = sprintf("%.3f",round(SD,3)),
    sequence_overlap = case_when(
      dataset == "all_100" ~ "100%",
      dataset == "hi_99" ~ ">99% <100%",
      dataset == "lo_99" ~ ">99% <100%"
    ),
    resolution = case_when(
      dataset == "all_100" ~ "<=2.0",
      dataset == "hi_99" ~ "<= 1.5",
      dataset == "lo_99" ~ ">1.5 <=2.0"
    )
  ) %>% 
  ungroup() %>% 
  select(sequence_overlap, resolution, Mer, n, n_af, MAE, MSD, SD) %>% 
  mutate(Type = "weighted")

x %>% bind_rows(y)


# Table 2 ------------------------------------------------

all_data %>% 
  group_by(dataset, Mer, ligands) %>% 
  summarize(
    n = n(),
    n_pairs = n_distinct(pair),
    n_af = n_distinct(AF), 
    n_exp = n_distinct(Experimental),
    n_pdb = n_distinct(str_sub(Experimental, 1, 4)),
    MAE = mean(abs(main_rel_exp - main_rel_af), na.rm = TRUE),
    MSD = mean(main_rel_af - main_rel_exp, na.rm = TRUE),
    SD = sd(abs(main_rel_exp - main_rel_af), na.rm = TRUE)
  ) %>% 
  mutate(
    MAE = sprintf("%.3f",round(MAE,3)),
    MSD = sprintf("%.3f",round(MSD,3)),
    SD = sprintf("%.3f",round(SD,3)),
    ligands = ifelse(ligands, "Yes", "No"),
    Mer = ifelse(Mer == "Monomer", "Mono", "Multi"),
    sequence_overlap = case_when(
      dataset == "all_100" ~ "100",
      dataset == "hi_99" ~ ">99 <100",
      dataset == "lo_99" ~ ">99 <100"
    ),
    resolution = case_when(
      dataset == "all_100" ~ "<=2.0",
      dataset == "hi_99" ~ "<= 1.5",
      dataset == "lo_99" ~ ">1.5 <=2.0"
    )
  ) %>% 
  ungroup() %>% 
  select(sequence_overlap, resolution, Mer, ligands, n, n_pairs, n_af,MAE, MSD, SD) 


# Table S1 ------------------------------------------------------

all_data %>% 
  group_by(AF, Experimental, dataset, Mer, Resolution) %>% 
  summarize(
    MAE = mean(abs(main_rel_exp - main_rel_af), na.rm = TRUE),
    MSD = mean(main_rel_exp - main_rel_af, na.rm = TRUE),
    SD_error = sd(abs(main_rel_exp - main_rel_af), na.rm = TRUE)
  ) %>% 
  mutate(
    AF = str_sub(AF, 1, length(AF) - 6),
    Exp = str_sub(Experimental, 1, 4),
    Chain = str_sub(Experimental, 6, 6),
    MAE = sprintf("%.3f",round(MAE,3)),
    MSD = sprintf("%.3f",round(MSD,3)),
    SD_error = sprintf("%.3f",round(SD_error,3)),
    Identity = case_when(
      dataset == "all_100" ~ "100%",
      dataset == "lo_99" ~ ">99% <100%",
      dataset == "hi_99" ~ ">99% <100%"
    )
  ) %>%
  ungroup() %>% 
  select(AF, Exp, Chain, Resolution, Identity, Mer, MAE, MSD, SD_error) 
# Table S2 ---------------------------------------------------------

all_data %>% 
  mutate(group = cut_width(pLDDT, 10, center = 5),
         group = str_remove_all(group, "[()\\]\\[]"),
         group = str_replace_all(group, ",", "-"),
  ) %>% 
  af_average() %>%
  group_by(dataset, Mer, group) %>% 
  summarize(
    n = n(),
    MAE = mean(abs, na.rm = TRUE),
    MSD= mean(signed, na.rm = TRUE),
    SD = sd(abs, na.rm = TRUE)
  ) %>% 
  mutate(
    MAE = sprintf("%.3f",round(MAE,3)),
    MSD = sprintf("%.3f",round(MSD,3)),
    SD = sprintf("%.3f",round(SD,3))
  ) %>% 
  mutate(
    sequence_overlap = case_when(
      dataset == "all_100" ~ "100%",
      dataset == "hi_99" ~ ">99% <100%",
      dataset == "lo_99" ~ ">99% <100%"
    ),
    resolution = case_when(
      dataset == "all_100" ~ "<=2.0",
      dataset == "hi_99" ~ "<= 1.5",
      dataset == "lo_99" ~ ">1.5 <=2.0"
    )) %>% 
  ungroup() %>% 
  select(sequence_overlap, resolution, Mer, group,n, MAE, MSD, SD) 


# Table S3 ----------------------------------------------------

all_data %>% 
  mutate(group = cut_width(main_rel_exp, 0.4, center = 0.2),
         group = str_remove_all(group, "[()\\]\\[]"),
         group = str_replace_all(group, ",", "-")) %>% 
  af_average() %>%
  group_by(dataset, Mer, group) %>% 
  summarize(
    n = n(),
    MAE = mean(abs, na.rm = TRUE),
    MSD= mean(signed, na.rm = TRUE),
    SD = sd(abs, na.rm = TRUE)
  ) %>% 
  mutate(
    MAE = sprintf("%.3f",round(MAE,3)),
    MSD = sprintf("%.3f",round(MSD,3)),
    SD = sprintf("%.3f",round(SD,3))
  ) %>% 
  mutate(
    sequence_overlap = case_when(
      dataset == "all_100" ~ "100%",
      dataset == "hi_99" ~ ">99% <100%",
      dataset == "lo_99" ~ ">99% <100%"
    ),
    resolution = case_when(
      dataset == "all_100" ~ "<=2.0",
      dataset == "hi_99" ~ "<= 1.5",
      dataset == "lo_99" ~ ">1.5 <=2.0"
    )) %>% 
  ungroup() %>% 
  select(sequence_overlap, resolution, Mer, group,n, MAE, MSD, SD) 

