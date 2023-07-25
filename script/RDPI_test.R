library(tidyverse)
library(FactoMineR)
library(factoextra)
source("script/import_data.R")
sp_info <- read.csv2("output/data/species_info.csv")

## Compute RDPI on all traits ####

# By averaging trait values per population
compute_plast_pop <- function(ftrait){
  plast <- traits_pop %>% 
    select(pop,fertilization,all_of(ftrait)) %>% 
    spread(key = fertilization,value = ftrait )
  # mutate(plast = (`N+` - `N-`)/`N+` )
  # merge(trait_moy)
  plast[,ftrait] <-  (plast$`N+` - plast$`N-`) / (plast$`N+`) 
  
  plast %>% 
    ungroup() %>% 
    select(pop,ftrait)
}



traits = c("plant_dry_mass","Hveg",
          "LA","SLA","LDMC",
          "N","LMF",
          "SRL", "RTD", "RDMC", 
          "SMF","RMF",
          "diam","BI")

PLAST_pop <- compute_plast_pop(traits[1])
for( i in c(2:length(traits)) ){
  plast <- compute_plast_pop(traits[i])
  PLAST_pop <- merge(PLAST_pop,plast)
}

rdpi <- PLAST_pop %>% 
  separate(pop, into = c("code_sp","origin"),remove = F) %>% 
  merge(sp_info)


rdpi %>% 
  ggplot(aes(x = origin, y = RTD,label = code_sp)) +
  geom_boxplot()+
  geom_point() +
  geom_hline(yintercept = 0)+
  ggrepel::geom_label_repel() +
  geom_line(aes(group = code_sp))


# ACP

pca <- PLAST_pop %>% 
  column_to_rownames("pop") %>% 
  PCA()

pca_toplot <- pca$ind$coord %>%
  as.data.frame() %>% 
  rownames_to_column("pop") %>% 
  separate(pop, into = c("code_sp","origin"),remove = F) %>% 
  merge(sp_info)


pca_toplot %>% 
  ggplot(aes(x = Dim.1, y = Dim.3, color = origin)) +
  geom_point() +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0)



library(Factoshiny)
PLAST_pop %>% 
  filter(!(pop %in% c("CERAPUMI_Fer","CERAPUMI_Nat","MYOSRAMO_Nat"))) %>% 
  column_to_rownames("pop") %>%
  PCAshiny() 



