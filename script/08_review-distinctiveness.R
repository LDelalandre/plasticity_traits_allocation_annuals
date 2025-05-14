library(tidyverse)
library(funrar)

ftraits_sp <- traits_sp %>% 
  filter(fertilization == "N-") %>% 
  merge(species_info %>% dplyr::select(code_sp,scientificNameShort)) %>% 
  column_to_rownames("scientificNameShort") %>% 
  dplyr::select(-code_sp) %>% 
  dplyr::select(all_of(FTRAITS)  )

dist_tot<-compute_dist_matrix(ftraits_sp,metric='euclidean')
tidy<-as.data.frame(rownames(ftraits_sp)) # tidy format for computing distinctiveness in the fonction below
colnames(tidy)<-"SName"
distinct_tot<-distinctiveness_com(com_df=tidy,
                                  sp_col=colnames(tidy),abund=NULL,
                                  dist_matrix=dist_tot,relative=F)
ftraits_sp$Di<-distinct_tot$Di 

plot_distinctiveness <- ftraits_sp %>%
  rownames_to_column("scientificNameShort") %>% 
  merge(species_info) %>% 
  mutate(origin = if_else(code_sp %in% sp_nat, "nat", "both")) %>% 
  ggplot(aes(x = origin, y = Di, label = scientificNameShort)) +
  geom_violin() +
  geom_point() +
  ggrepel::geom_label_repel() +
  theme_classic() +
  ylab("Functional distinctiveness") 
  # theme(text=element_text(size=5))

ggsave(filename = "draft/08_review-distinctiveness_plot-distinctiveness.jpg",
       plot = plot_distinctiveness,
       height = 150, 
       width = 150, 
       unit = "mm")  

# PCA ------------------------------
library(FactoMineR)

data_pca <- ftraits_sp %>% 
  dplyr::select(-Di) %>% 
  rename(log_M = log_plant_dry_mass)

ACP1<-PCA(data_pca,graph=F) 
pca_var <- plot(ACP1,choix ="var")
pca_ind <- plot(ACP1,choix ="ind")

pca_tot <- cowplot::plot_grid(pca_var, pca_ind, nrow = 2, labels = c("A.", "B."))

ggsave("draft/08_review-distinctiveness_pca.jpg",
       pca_tot,
       height = 11,
       width = 11)
