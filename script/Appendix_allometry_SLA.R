library(tidyverse)
source("script/import_data.R")

# SMA on SLA per species ####
# in each treatment
fferti <- "N+"
ftrait <- "log_SLA"

sma_ftrait_sp <- t2_traits %>% 
  filter(fertilization == fferti) %>% 
  sma(as.formula(paste(ftrait, "~ log_plant_dry_mass + code_sp")),data = .) 
sma_ftrait_sp
coefs_ftrait<- coef(sma_ftrait_sp)

sp <- rownames(coefs_ftrait)
PLOT <- NULL
for (i in c(1:length(sp))){
  spi <- sp[i]
  plot <- t2_traits %>% 
    # filter(fertilization == fferti) %>% 
    filter(code_sp == spi) %>% 
    ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait)) +
    geom_point(aes(shape = fertilization))+
    theme_classic()+
    geom_abline(slope =coefs_ftrait[i,2],intercept = coefs_ftrait[i,1])+
    ggtitle(spi) +
    theme_bw() +
    theme(legend.position = "none")
  
  PLOT[[i]] <- plot
  # ggsave(paste0("output/plot/sma_sla/",spi,".png"),plot)
}


plot_sla <- ggpubr::ggarrange(plotlist=PLOT, ncol = 3,nrow = 5)
# ggsave(paste0("draft/plot_allom_sla",fferti,".jpg"),plot_sla,height = 15, width = 10)
ggsave(paste0("draft/plot_allom_sla.jpg"),plot_sla,height = 15, width = 10)
