library(tidyverse)
library("openxlsx")


MEAN <- read.csv2("data/mean_attribute_per_treatment_subset_nat_sab_int.csv") %>% 
    mutate(code_sp = if_else(species == "Myosotis ramosissima subsp. ramosissima","MYOSRAMO",code_sp))

species_list <- read.csv2("data/species_experiment.csv")
sp_exp <- species_list %>% pull(code_sp)

fMEAN <- MEAN %>% 
  filter(code_sp %in% sp_exp) %>% 
  select(species,code_sp,treatment,L_Area,LDMC,SLA,LCC,LNC,Hveg,Hrepro,Dmin,Dmax) %>% 
  mutate(log_Hveg = log(Hveg),log_Hrepro = log(Hrepro),log_LA = log(L_Area),log_Dmax = log(Dmax))

nona <- fMEAN %>% 
  filter(treatment=="Nat") %>% 
  na.omit() %>% 
  pull(code_sp)
setdiff(code_sp,nona)

# on a tout le monde dans le fer !
# Il manque dans le Nat:
# LeafMorpho: EROP, TRIF, VULP
# LeafC&N: same 3 + MYOS
# Biovolume: CERAGLOM, FILA, VULP, BUPL

plot_in_situ <- function(ftrait){
  fMEAN %>% 
    mutate(treatment = factor(treatment,levels = c("Nat","Fer"))) %>% 
    ggplot(aes_string(x = "treatment", y = ftrait)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(color = treatment), size = 2) +
    geom_line(aes(group=code_sp)) +
    theme_classic() +
    scale_color_brewer(palette = "Set1",direction = -1) +
    theme(legend.position = "none") +
    ggtitle(ftrait) +
    theme(axis.title = element_blank())
    # scale_shape_manual(values=1:length(unique(fMEAN$code_sp)))
    # scale_shape_manual(values = c(15,16))
}

FTRAITS <- list("SLA","LDMC","log_LA","LNC","log_Hrepro","log_Dmax")

PLOTS_in_situ <- lapply(X = FTRAITS,FUN = plot_in_situ)
plot_in_situ <- ggarrange(PLOTS_in_situ[[1]],PLOTS_in_situ[[2]],PLOTS_in_situ[[3]],PLOTS_in_situ[[4]],PLOTS_in_situ[[5]],PLOTS_in_situ[[6]])

ggsave("output/plot/plot_in_situ.jpg",plot_in_situ,height = 9, width = 10)

plot_in_situ_leaf <- ggarrange(PLOTS_in_situ[[1]],PLOTS_in_situ[[2]],PLOTS_in_situ[[3]],ncol = 1,nrow =3)
ggsave("output/plot/plot_in_situ_leaf.jpg",plot_in_situ_leaf,height = 9, width = 5)

plot_in_situ_N_vol <- ggarrange(PLOTS_in_situ[[4]],PLOTS_in_situ[[5]],PLOTS_in_situ[[6]],ncol = 1,nrow =3)
ggsave("output/plot/plot_in_situ_N_vol.jpg",plot_in_situ_N_vol,height = 9, width = 5)


# différences signif selon l'origine sur LDMC et log_LA ! (mais faibles)
mod <- lme4::lmer (log_LA ~ treatment + (1|code_sp), data = fMEAN)
car::Anova(mod)

