library(tidyverse)
library("openxlsx")
library(scales)


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
setdiff(sp_exp,nona)

# on a tout le monde dans le fer !
# Il manque dans le Nat:
# LeafMorpho: EROP, TRIF, VULP
# LeafC&N: same 3 + MYOS
# Biovolume: CERAGLOM, FILA, VULP, BUPL

plot_in_situ <- function(ftrait){
  # keep sepcies present in the two management regimes for the trait considered
  sp_fer <- fMEAN %>% 
    filter(treatment == "Fer") %>% 
    filter(!is.na(get(ftrait))) %>% 
    pull(code_sp)
  sp_nat <- fMEAN %>% 
    filter(treatment == "Nat") %>% 
    filter(!is.na(get(ftrait))) %>% 
    pull(code_sp)
  sp_ftrait <- intersect(sp_fer,sp_nat)
    
  fMEAN2 <- fMEAN %>% 
    filter(code_sp %in% sp_ftrait) %>% 
    mutate(treatment = factor(treatment,levels = c("Nat","Fer"))) 
  
  form <- as.formula(paste(ftrait, "~ treatment + (1|code_sp)"))
  mod <- lme4::lmer (form, data = fMEAN)
  
  stat <- car::Anova(mod) %>% 
    as.data.frame() %>% 
    rename(pvalue = `Pr(>Chisq)`) %>% 
    mutate(group1 = "Int.",group2 = "Ext.") %>% 
    mutate(stars = case_when (pvalue > 0.05 ~ "n.s.", 
                              between(pvalue, 0.01, 0.05) ~ "*",
                              between(pvalue, 0.001,0.01) ~ "**",
                              pvalue < 0.001 ~ "***",
                              
    )) %>% 
    mutate(pvalue = scales::scientific(pvalue, digits = 2))

  plot <- fMEAN2 %>% 
    mutate(Management = if_else(treatment == "Nat","Ext.","Int.")) %>% 
    mutate(Management = factor(Management, levels = c("Int.","Ext."))) %>% 
    ggplot(aes_string(x = "Management", y = ftrait)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(color = Management), size = 2) +
    geom_line(aes(group=code_sp)) +
    theme_classic() +
    # scale_color_brewer(palette = "Set1",direction = -1) +
    
    theme(legend.position = "none") +
    ggtitle(ftrait) +
    theme(axis.title = element_blank())
    # scale_shape_manual(values=1:length(unique(fMEAN$code_sp)))
    # scale_shape_manual(values = c(15,16))
  ypos <- fMEAN2 %>% 
    pull(ftrait) %>% 
    max()
  ypos <- ypos + 1/20*ypos
  plot +
    ggpubr::stat_pvalue_manual(
      stat, 
      y.position = ypos, step.increase = 0.1,
      label = "stars",
      size = 4
    )  + 
    theme(text=element_text(size=13))
}

# lengend
forleg <- plot_in_situ("SLA") + 
  theme(legend.position = "right")
leg <- ggpubr::get_legend(forleg) %>% 
  ggpubr::as_ggplot()

FTRAITS <- list("LDMC","SLA","log_LA","LNC","log_Hrepro","log_Dmax")

PLOTS_in_situ <- lapply(X = FTRAITS,FUN = plot_in_situ)
plot_in_situ <- 
  ggpubr::ggarrange(PLOTS_in_situ[[1]],PLOTS_in_situ[[2]],PLOTS_in_situ[[3]],PLOTS_in_situ[[4]],PLOTS_in_situ[[5]],leg) 

ggsave("draft/plot_in_situ.jpg",plot_in_situ,height = 6, width = 7)


PLOTS_in_situ[[6]]


