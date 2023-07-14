source("script/import_data.R")

G <- readxl::read_excel("data/data_generalist_specialist_denelle.xlsx", sheet = "generalist")
S <- readxl::read_excel("data/data_generalist_specialist_denelle.xlsx", sheet = "specialist")

mysp <- species_list %>% 
  mutate(Species = toupper(species)) %>% 
  pull(Species)

G %>% 
  filter(Generalists %in% mysp)

S %>% 
  filter(Specialists %in% mysp)
