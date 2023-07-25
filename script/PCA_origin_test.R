library(tidyverse)
library(FactoMineR)
library(factoextra)
source("script/import_data.R")
sp_info <- read.csv2("output/data/species_info.csv")

euclidean <- function(a, b) sqrt(sum((a - b)^2))


traits = c("plant_dry_mass","Hveg",
           "LA","SLA","LDMC",
           "N","LMF",
           "SRL", "RTD", "RDMC", 
           "SMF","RMF",
           "diam","BI")

acp <- traits_pop %>% 
  filter(fertilization == "N+") %>% 
  select(pop,all_of(traits)) %>% 
  column_to_rownames("pop") %>% 
  PCA()

res_pca <- acp$ind$coord %>% 
  as.data.frame() %>% 
  rownames_to_column("pop") %>% 
  separate(pop,into=c("code_sp","origin"),remove = F)



dist_fsp <- function(fsp){
  coord <- res_pca %>% 
    filter(code_sp == fsp) %>% 
    select(Dim.1,Dim.2,Dim.3,Dim.4,Dim.5)
  distantia::distance(coord[1,], coord[2,], method = "euclidean")
}

dist_Np <- lapply(sp_info %>% pull(code_sp) %>% unique() %>% as.list(),
       dist_fsp) %>% 
  rlist::list.rbind() %>% 
  as.data.frame() %>% 
  cbind(sp_info %>% pull(code_sp) %>% unique()) %>% 
  filter(!is.na(V1)) 

colnames(dist_Np) <- c("dist","code_sp")

dist_Np %>% 
  merge(sp_info) %>% 
  ggplot(aes(x = dispersal, y = dist)) +
  geom_point()

hist(dist_Np$dist)


fsp <- "CERAPUMI"
ftrait <- "RMF"
t2_traits %>% 
  filter(code_sp == fsp) %>% 
  ggplot(aes_string(x = "origin", y = ftrait)) +
  geom_boxplot() +
  geom_point()

