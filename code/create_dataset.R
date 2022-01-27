# read data ---------------------------------------------------------------

hi_100 <- read_csv("data/rsa_hi_100.csv")
hi_99 <- read_csv("data/rsa_hi_99.csv")
lo_100 <- read_csv("data/rsa_lo_100.csv")
lo_99 <- read_csv("data/rsa_lo_99.csv")

no_ligands <- read_csv("data/PDB_no_ligands.csv")

# group data ---------------------------------------------------------------

all_100 <- hi_100 %>% 
  bind_rows(lo_100) %>% 
  mutate(dataset = "all_100")

hi_99 %<>% 
  mutate(dataset = "hi_99")

lo_99 %<>% 
  mutate(dataset = "lo_99")

all_data <- all_100 %>% 
  bind_rows(hi_99) %>% 
  bind_rows(lo_99) %>% 
  rowwise() %>% 
  mutate(
    AF_id = str_split(AF, "-")[[1]][2],
    pair = paste0(AF_id, " v. ", Experimental)
    ) %>% 
  group_by(pair) %>% 
  mutate(count = sum(!(is.na(main_rel_exp)))) %>%
  arrange(pair) %>% 
  # remove first and last residue in each structure
  filter(Number_af != min(Number_af, na.rm = TRUE),
         Number_af != max(Number_af, na.rm = TRUE),
         Number_exp != min(Number_exp, na.rm = TRUE),
         Number_exp != max(Number_exp, na.rm = TRUE)
  ) %>% 
  mutate(Mer = case_when(
    Num_chains_exp == 1 ~ "Monomer",
    TRUE ~ "Multimer"
  )) %>% 
  mutate(ligands = !str_sub(Experimental, 1, 4) %in% no_ligands$Experimental) %>% 
  select(AF, AF_id, Experimental, pair, dataset, Mer, everything()) %T>%
  write_csv("data/all_data.csv")