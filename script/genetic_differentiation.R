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
ffer <- "N-"
## Compute RDPI on all traits ####
compute_genet_diff <- function(ftrait){
  genet_diff <- traits_pop %>% 
    filter(fertilization == ffer) %>% 
    select(code_sp,origin,all_of(ftrait)) %>% 
    spread(key = origin,value = ftrait ) %>% 
    na.omit() %>% 
    `rownames<-`( NULL ) %>% 
    column_to_rownames("code_sp")
  # mutate(plast = (`N+` - `N-`)/`N+` )
  # merge(trait_moy)
  genet_diff[,ftrait] <-  abs((genet_diff$Fer - genet_diff$Nat) / (genet_diff$Fer)) # take the absolute value
  
  genet_diff %>% 
    ungroup() %>% 
    select(ftrait)
}


rdpi_origin <- lapply(FTRAITS,compute_genet_diff) %>% 
  rlist::list.cbind() %>% 
  as.data.frame()
  # rownames_to_column("code_sp")


cumulated_diff_origin <- function(rdpi_origin){
  rdpi_origin %>% 
    t() %>% 
    as.data.frame() %>% 
    summarize_all(sum) %>% 
    t() %>% 
    as.data.frame() 
}

cumulated_diff_origin(rdpi_origin) %>% View
  cumulated_diff_origin() %>% 
  ggplot(aes(x= V1)) + geom_density()

rdpi_origin %>%  
  mutate(across(.fns = sample)) %>%
  cumulated_diff_origin() %>% 
  ggplot(aes(x= V1)) + geom_density()



distrib_cumulated_diff <- c()
for (i in c(1:1000)){
  distrib_cumulated_diff <- rdpi_origin %>%  
    mutate(across(.fns = sample)) %>%
    cumulated_diff_origin() %>% 
    pull(V1) %>% 
    c(distrib_cumulated_diff,.)
}
  
length(which(distrib_cumulated_diff>7)) / length(distrib_cumulated_diff)
