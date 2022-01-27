coverage <- read_csv("data/coverage_no_mut_no_mid.csv")

#sanity_check
coverage %>% 
  mutate(
    AF_len_check = AF_len == Overlap + Gap_start + Gap_end,
    Chain_len_check = Chain_len == Overlap + Insertion_start + Insertion_end
  ) %>% 
  summarize(
    AF_len_check = all(AF_len_check),
    Chain_len_check = all(Chain_len_check)
  )
  
# Table of subsets
coverage %>% 
  mutate(
    PDB = str_sub(Chain, 1, 4),
    coverage = case_when(
      Overlap / AF_len == 1 & Overlap / Chain_len == 1 ~ "100%",
      Overlap / AF_len > 0.99 & Overlap / Chain_len > 0.99 ~ "99%",
      Overlap / AF_len > 0.95 & Overlap / Chain_len > 0.95 ~ "95%",
      TRUE ~ "Rest"),
      Resolution = case_when(
        Resolution <= 1.5 ~ "1.5",
        TRUE ~ "Rest"
      )
  ) %>% 
 group_by(coverage,Resolution) %>% 
  summarize(
    n_pairs = n(),
    n_AF = n_distinct(AF),
    n_Chain = n_distinct(Chain),
    n_PDB = n_distinct(PDB)) 
  
# Export 
# Test if each pair has a unique combination of AF and Chain name
coverage %>% 
  select(AF, Chain) %>% 
  summarize(n = n(),
    n_unique = n_distinct(AF, Chain))
    
coverage %<>% 
  mutate(
    coverage = case_when(
      Overlap / AF_len == 1 & Overlap / Chain_len == 1 ~ "100%",
      Overlap / AF_len > 0.99 & Overlap / Chain_len > 0.99 ~ "99%",
      TRUE ~ "Rest"
      ),
  res_group = case_when(
      Resolution <= 1.5 ~ "1.5",
      TRUE ~ "Rest"
    ))

coverage %>% 
  group_by(coverage, res_group) %>% 
  summarize(n = n())

coverage %>% 
  filter(coverage == "100%",
         res_group == "1.5") %>% 
  select(AF, Chain) %>% 
  write_csv("data/group_hi_100.csv")

coverage %>% 
  filter(coverage == "99%",
         res_group == "1.5") %>% 
  select(AF, Chain) %>% 
  write_csv("data/group_hi_99.csv")

coverage %>% 
  filter(coverage == "100%",
         res_group == "Rest") %>% 
  select(AF, Chain) %>% 
  write_csv("data/group_low_100.csv")

coverage %>% 
  filter(coverage == "99%",
         res_group == "Rest") %>% 
  select(AF, Chain) %>% 
  write_csv("data/group_low_99.csv")