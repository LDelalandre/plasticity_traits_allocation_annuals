# This script focuses on intraspecific trait variation in the field

library(tidyverse)
library("openxlsx")
library(scales)

source("script/import_data.R")

# Plots ITV in the field ####
plot_in_situ <- function(ftrait){
  # function to generate a boxplot of trait values in situ, in the two treatments
  # points belonging to the same speices are connected by lines
  # signifiance of aa mixed model is indicated by stars above the plots
  
  # keep species present in the two management regimes for the trait considered
  sp_fer <- fMEAN %>% 
    rename(LA = L_Area) %>% 
    filter(treatment == "Fer") %>% 
    filter(!is.na(get(ftrait))) %>% 
    pull(code_sp)
  sp_nat <- fMEAN %>% 
    rename(LA = L_Area) %>% 
    filter(treatment == "Nat") %>% 
    filter(!is.na(get(ftrait))) %>% 
    pull(code_sp)
  sp_ftrait <- intersect(sp_fer,sp_nat)
    
  fMEAN2 <- fMEAN %>% 
    rename(LA = L_Area) %>% 
    filter(code_sp %in% sp_ftrait) %>% 
    mutate(treatment = factor(treatment,levels = c("Nat","Fer"))) 
  
  # mixed model
  if(ftrait == "LA"){
    ftrait2 <- "log_LA"
  }else if(ftrait == "Dmax"){
    ftrait2 <- "log_Dmax"
  }else if(ftrait == "Hrepro"){
    ftrait2 <- "log_Hrepro"
  }else{
    ftrait2 <- ftrait
  }
  
  form <- as.formula(paste(ftrait2, "~ treatment + (1|code_sp)"))
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
    mutate(Management = factor(Management, levels = c("Ext.","Int."))) %>% 
    ggplot(aes_string(x = "Management", y = ftrait)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(color = Management), size = 2) +
    geom_line(aes(group=code_sp)) +
    theme_classic() +
    scale_color_brewer(palette = "Set1",direction = -1) +
    
    theme(legend.position = "none") +
    ggtitle(ftrait) +
    theme(axis.title = element_blank())
    # scale_shape_manual(values=1:length(unique(fMEAN$code_sp)))
    # scale_shape_manual(values = c(15,16))
  ypos <- fMEAN2 %>% 
    pull(ftrait) %>% 
    max()
  if (ftrait %in% c("Dmax","Hrepro","LA")){ypos <- log10(ypos) }
  ypos <- ypos + 1/20*ypos
  plot +
    ggpubr::stat_pvalue_manual(
      stat, 
      y.position = ypos, step.increase = 0.1,
      label = "stars",
      size = 4
    )  + 
    theme(text=element_text(size=13)) +
    {if (ftrait %in% c("Dmax","Hrepro","LA")) scale_y_continuous(trans='log10')} 
}

## legend of the plots ####
forleg <- plot_in_situ("SLA") + 
  theme(legend.position = "right")
leg <- ggpubr::get_legend(forleg) %>% 
  ggpubr::as_ggplot()

## plots ####
FTRAITS <- list("LDMC","SLA","LA","LNC","Hrepro","Dmax")

PLOTS_in_situ <- lapply(X = FTRAITS,FUN = plot_in_situ)
plot_in_situ <- 
  ggpubr::ggarrange(PLOTS_in_situ[[1]],PLOTS_in_situ[[2]],PLOTS_in_situ[[3]],PLOTS_in_situ[[4]],PLOTS_in_situ[[5]],leg) 

ggsave("draft/plot_in_situ.jpg",plot_in_situ,height = 6, width = 7)


PLOTS_in_situ[[6]]


