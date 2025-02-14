library(tidyverse)

sp_list <- read.csv2("data/species_experiment.csv") %>% 
  rename(Species = species)

taxon <- read.csv2("data/taxon.csv")


# BiolFlor ####
 


# mating system
MS <- read.csv2("data/BiolFlor_mating_system.csv") %>% 
  mutate(Species = word(species,1,2,sep=" ")) %>% 
  filter(Species %in% sp_list$Species) %>% 
  select(Species,Breeding.system) #,Reference

# # pollen vector 
# PV <- read.csv2("data/BiolFlor_pollen_vector.csv") %>% 
#   mutate(Species = word(species,1,2,sep=" ")) %>% 
#   filter(Species %in% sp_list$Species) %>% 
#   select(Species,Pollen.vector)
# 
# # self-incompatibility
# SI <- read.csv2("data/BiolFlor_self_incompatibility.csv") %>% 
#   mutate(Species = word(species,1,2,sep=" ")) %>% 
#   filter(Species %in% sp_list$Species) %>% 
#   select(Species,Self.sterility.and.self.incompatibility)

# Keep breeding system and the sources of the info
info_biolflor <- sp_list %>% 
  select(Species,populations) %>% 
  left_join(MS) %>% 
  mutate(Species = case_when(Species == "Myosotis ramosissima" ~"Myosotis ramosissima subsp. ramosissima",
                             Species == "Erophila verna" ~ "Draba verna",
                             TRUE ~ Species))



#____________________

N_CSR <- read.csv2("data/info_species.csv") %>% 
  mutate(species = case_when(species == "Myosotis ramosissima" ~"Myosotis ramosissima subsp. ramosissima",
                             species == "Erophila verna" ~ "Draba verna",
                             TRUE ~ species)) %>% 
  mutate(N_braun = if_else(species == "Bupleurum baldense", "2",N_braun)) %>% # BUPLBALD N value from Ellenberg
  rename(Species = species,
         CSR = OrigValueStr,
         nutrient_requirement = N_braun) %>% 
  select(Species,CSR,nutrient_requirement) 

baseflor <- read.csv2("data/info_baseflor.csv",fileEncoding = "latin1") %>% 
  mutate(species = species_1) %>% 
  mutate(species = case_when(species == "Myosotis ramosissima" ~"Myosotis ramosissima subsp. ramosissima",
                             species == "Erophila verna" ~ "Draba verna",
                             TRUE ~ species)) %>% 
  rename(Species = species) %>% 
  select(Species,dissémination,TYPE_BIOLOGIQUE) %>%  #,CARACTERISATION_ECOLOGIQUE_.HABITAT_OPTIMAL.,sexualité,pollinisation,
  unique() %>% 
  mutate(dispersal = case_when(dissémination == "barochore" ~ "gravity",
                               dissémination == "anémochore" ~ "wind",
                               dissémination == "épizoochore" ~ "epizoochorous",
                               dissémination == "autochore" ~ "autochorous"))
  # filter(!(Species == "Minuartia hybrida" & pollinisation == "entomogame"))

sp_info <- sp_list %>% 
  select(Species,code_sp,populations) %>% 
  mutate(Species = case_when(Species == "Myosotis ramosissima" ~"Myosotis ramosissima subsp. ramosissima",
                             Species == "Erophila verna" ~ "Draba verna",
                             TRUE ~ Species)) %>% 
  left_join(taxon) %>% 
  left_join(N_CSR) %>% 
  left_join(info_biolflor) %>% 
  left_join(baseflor) %>% 
  select(scientificName,code_sp,populations,Family,
         Breeding.system,
         dispersal,
         # TYPE_BIOLOGIQUE,
         nutrient_requirement,
         CSR)


write.csv2(sp_info,"output/data/species_info.csv",row.names=F,fileEncoding = "latin1")



sp_info %>% 
  group_by(Breeding.system) %>% 
  summarize(n = n())

sp_info %>% 
  group_by(dissémination) %>% 
  summarize(n = n())
