library(tidyverse)

# Information on species ####
info_sp <- read.csv2("data/info_species.csv",fileEncoding = "latin1") %>% 
  mutate(OrigValueStr = if_else(code_sp == "BUPLBALD", "sr", OrigValueStr))


# In situ trait measurements ####
MEAN <- read.csv2("data/mean_attribute_per_treatment_subset_nat_sab_int.csv") %>% 
  mutate(code_sp = if_else(species == "Myosotis ramosissima subsp. ramosissima","MYOSRAMO",code_sp))

species_list <- read.csv2("data/species_experiment.csv")
sp_exp <- species_list %>% pull(code_sp)

fMEAN <- MEAN %>% 
  filter(code_sp %in% sp_exp) %>% 
  select(species,code_sp,treatment,L_Area,LDMC,SLA,LCC,LNC,Hveg,Hrepro,Dmin,Dmax) %>% 
  mutate(log_Hveg = log(Hveg),log_Hrepro = log(Hrepro),log_LA = log(L_Area),log_Dmax = log(Dmax))


# Check which species of the experiment was not measured in situ
nona <- fMEAN %>% 
  filter(treatment=="Nat") %>% 
  na.omit() %>% 
  pull(code_sp)
setdiff(sp_exp,nona)

# on a tout le monde dans le fer !
# Il manque dans le Nat:
# LeafMorpho: EROP, TRIF, VULP
# LeafC&N: same 3 + MYOS
# Biovolume: CERAGLOM, FILA, VULP, BUPL



# Trait values in controlled conditions ####
t2_traits <- read.csv2("data/t2_traits.csv") 