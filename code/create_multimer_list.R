read_csv("data/all_data.csv") %>% 
  filter(Mer == "Multimer") %>% 
  select(Experimental) %>% 
  distinct() %>% 
  separate(Experimental, c("PDB", "Chain"),sep = ":") %>% 
  write_csv("data/multimer_pdbs.csv")
