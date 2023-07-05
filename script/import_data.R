library(tidyverse)

# Information on species ####
info_sp <- read.csv2("data/info_species.csv",fileEncoding = "latin1")

sp_fam <- info_sp %>% 
  select(code_sp,family) %>% 
  unique()

# species with one population only
sp_nat = c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO')


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


## traits we consider ####
traits_nutrients <- c("N","C")
traits_allocation <- c("RMF","SMF","LMF")
traits_leaf <- c("log_LA", "LDMC","SLA") 
traits_root <- c("SRL", "RTD", "RDMC", "diam","BI")
FTRAITS <- c("log_Hveg","log_plant_dry_mass",traits_nutrients,traits_allocation,traits_leaf,traits_root)

# trait values per pot ####
t2_traits <- read.csv2("data/t2_traits.csv")%>% 
  mutate(absortive_root_dry_mass = root_dry_mass - pivot_dry_mass) %>% 
  mutate(tot_RL = SRL * absortive_root_dry_mass) %>% # in m (with SRL in m/g and mass in g)
  mutate(log_tot_RL = log10(tot_RL)) %>% 
  
  mutate(tot_RA = tot_RL * 1000 * pi *diam ) %>% # root area, in mm² (convert tot_RL in mm, because diam is is mm)
  mutate(log_tot_RA = log10(tot_RA)) %>%
  
  mutate(tot_LA = SLA * leaf_dry_mass*1000) %>% # convert leaf dry mass into mg so that tot_LA in is mm² (with SLA in mm²/mg) 
  mutate(log_tot_LA = log10(tot_LA)) %>%
  
  mutate(log_plant_dry_mass = log10(plant_dry_mass),
         log_leaf_dry_mass = log10(leaf_dry_mass),
         log_stem_dry_mass = log10(stem_dry_mass),
         log_root_dry_mass = log10(root_dry_mass)) %>% 
  
  mutate(RMF = root_dry_mass/plant_dry_mass,
         SMF = stem_dry_mass/plant_dry_mass,
         LMF = leaf_dry_mass/plant_dry_mass) %>% 
  mutate(log_LA = log10(LA)) %>% 
  mutate(log_Hveg = log10(Hveg))




## traits values per population ####
traits_pop <- read.csv2("output/data/traits_pop.csv") %>% 
  mutate(log_plant_dry_mass = log(plant_dry_mass)) %>% 
  mutate(log_tot_LA = log10(tot_LA)) %>% 
  mutate(log_tot_RL = log10(tot_RL)) %>% 
  mutate(log_tot_RA = log10(tot_RA)) %>%
  mutate(log_RGRslope = log10(RGRslope)) %>% 
  merge(sp_fam) %>% 
  mutate(log_Hveg = log10(Hveg))
