library(tidyverse)
source("script/import_data.R")

species_info <- read.csv2("output/data/species_info.csv")


info_sp_traits <- species_info %>% 
  merge(fMEAN)
  # filter(!code_sp == "FILAPYRA")
  

# N and SLA in situ
data_sla_nutr <- info_sp_traits %>% 
  filter(!nutrient_requirement %in% c("x")) %>% 
  mutate(nutrient_requirement = as.numeric(nutrient_requirement)) %>% 
  filter(!is.na(nutrient_requirement)) 

# nutrient requirements relate to plant SLA in the field
data_sla_nutr %>% 
  ggplot(aes(x = nutrient_requirement, y = LDMC, label = code_sp)) +
  # geom_label(aes(color = treatment)) +
  geom_point() +
  geom_smooth(method="lm")

data_sla_nutr %>% 
  filter(!is.na(CSR)) %>% 
  ggplot(aes(y=nutrient_requirement,x=CSR)) +
  geom_boxplot() +
  geom_point() 

 
  
mod <- lm(SLA~nutrient_requirement,data = data_sla_nutr)
anova(mod)
summary(mod)

# magnitude of genetic differentiation ####
## Compute RDPI on all traits ####
compute_genet_diff <- function(ftrait){
  genet_diff <- traits_pop %>% 
    select(pop,fertilization,all_of(ftrait)) %>% 
    spread(key = fertilization,value = ftrait )
  # mutate(plast = (`N+` - `N-`)/`N+` )
  # merge(trait_moy)
  plast[,ftrait] <-  (plast$`N+` - plast$`N-`) / (plast$`N+`) 
  
  plast %>% 
    ungroup() %>% 
    select(pop,ftrait)
}
