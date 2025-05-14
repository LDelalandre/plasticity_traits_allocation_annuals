library(tidyverse)

# Information on species ####
# info_sp <- read.csv2("data/info_species.csv",fileEncoding = "utf-8")
# species_info <- read.csv2("data/species_info.csv",fileEncoding = "utf-8")

species_info <- read.csv2("data/Delalandre2025AnnBotSpecies.csv")

sp_fam <- species_info %>%
  dplyr::select(code_sp,family) %>%
  unique()

# species with one population only
sp_nat = c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO')

# correspondence between abbreviated and complete trait name
trait_name <- data.frame(trait = c("plant_dry_mass","Hveg",
                                   "LA","SLA","LDMC",
                                   "N","LMF",
                                   "SRL", "RTD", "RDMC", 
                                   "SMF","RMF",
                                   "diam","BI"),
                         name = c("Plant dry mass", "Vegetative height", 
                                  "Leaf Area","Specific Leaf Area","Leaf Dry Matter content",
                                  "Plant Nitrogen content per mass", "Leaf Mass Fraction",
                                  "Specific Root Length", "Root Tissue Density","Root Dry Matter Content",
                                  "Stem Mass Fraction","Root Mass Fraction",
                                  "Mean root diameter","Branching Intensity"))


## List of traits by category
traits_allocation <- c("LMF","SMF","RMF")
traits_leaf <- c("log_LA","SLA", "LDMC")
traits_root <- c("diam", "SRL", "RTD", "RDMC", "BI")
FTRAITS <- c("log_Hveg",traits_leaf,traits_root,"log_plant_dry_mass",
             "N",traits_allocation)



# Import raw (already curated) data ####
t2_traits0 <- read.csv2("data/Delalandre2025AnnBotTraits.csv",fileEncoding = "UTF-8") 


## Create some new variables ####
t2_traits <- t2_traits0 %>% 
  dplyr::mutate(absortive_root_dry_mass = root_dry_mass - pivot_dry_mass) %>%
  mutate(tot_RL = SRL * absortive_root_dry_mass) %>% # in m (because SRL in m/g and mass in g)
  mutate(log_tot_RL = log10(tot_RL)) %>% 
  
  mutate(tot_RA = tot_RL * 1000 * pi *diam ) %>% # total root area, in mm² (convert tot_RL in mm, because diam is is mm)
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
  mutate(log_Hveg = log10(Hveg),
         log_SLA = log10(SLA),
         log_LDMC = log10(LDMC),
         log_LA = log10(LA),
         log_SRL = log10(SRL),
         log_RDMC= log10(RDMC),
         log_RTD = log10(RTD),
         log_diam = log10(diam),
         log_BI = log10(BI),
         log_N = log10(N),
         log_RMF=log10(RMF))
  # dplyr::filter(!(pop == "ALYSALYS_Nat")) # re-included after AnnBot review




## Average traits values per population ####
traits_pop <- t2_traits %>%
  group_by(code_sp,origin,fertilization) %>%
  dplyr::select(all_of(FTRAITS),"LA","plant_dry_mass","Hveg","tot_RL","tot_RA","tot_LA") %>% 
  summarize_all(mean,na.rm=T) %>% 
  mutate(log_plant_dry_mass = log(plant_dry_mass)) %>% 
  mutate(log_tot_LA = log10(tot_LA)) %>% 
  mutate(log_tot_RL = log10(tot_RL)) %>% 
  mutate(log_tot_RA = log10(tot_RA)) %>%
  merge(sp_fam) %>% 
  mutate(log_Hveg = log10(Hveg)) %>% 
  mutate(pop = paste(code_sp,origin,sep="_"))

## Average traits values per species ####
traits_sp <- t2_traits %>%
  group_by(code_sp,fertilization) %>%
  dplyr::select(all_of(FTRAITS),"LA","plant_dry_mass","Hveg","tot_RL","tot_RA","tot_LA") %>% 
  summarize_all(mean,na.rm=T)

