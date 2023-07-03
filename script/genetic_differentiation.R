library(tidyverse)
source("script/import_data.R")

info_sp %>% 
  group_by(pollinisation) %>% 
  summarize(n=n())


info_sp_traits <- info_sp %>% 
  merge(fMEAN)
  # filter(!code_sp == "FILAPYRA")
  

# CSR and SLA
info_sp_traits %>% 
  ggplot(aes(x = OrigValueStr, y = SLA, label = code_sp)) +
  geom_boxplot() +
  # geom_label(aes(color = treatment)) +
  geom_point() 
 
  
mod <- lm(SLA~OrigValueStr,data = info_sp_traits)
anova(mod)

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