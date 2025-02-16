library(tidyverse)
library(smatr)
source("script/01_import-data.R")

ftrait <- "log_SLA"

# Mixed model ####
mmod <- lme4::lmer(as.formula(paste(ftrait, "~ log_plant_dry_mass + fertilization + (1|family/code_sp) ")),  data=t2_traits)

plot_SLA <- t2_traits %>% 
  mutate(fertilization = if_else(fertilization == "N+", "F+","F-")) %>% 
  merge(species_info) %>% 
  # mutate(species2 = case_when (code_sp == "VULPMYUR" ~ "Vulpia alopecuros",
  #                             code_sp == "TRIFCAMP" ~"Trifolium aureum",
  #                             code_sp == "MINUHYBR" ~ "Sabulina tenuifolia",
  #                             code_sp == "EROPVERN","Draba verna",
  #                             TRUE ~ species))
  ggplot( aes(x = plant_dry_mass, y = SLA)) +
  geom_point(aes( color = fertilization)) +
  facet_wrap(~scientificName_short,ncol = 3) +
  geom_smooth(method = "lm",se = F) +
  theme_bw() +
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  xlab("Plant dry mass (g)") +
  ylab ("Specific leaf area (mm²/mg)") +
  scale_color_brewer(palette = "Set2")

ggsave(paste0("draft/plot_allom_sla.jpg"),plot_SLA,height = 8, width = 6)

# Diagnostic 
library(DHARMa)
simulationOutput <- DHARMa::simulateResiduals(fittedModel = mmod, plot = F)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput) # trop de zéros pour une poisson

car::Anova(mmod)
summary(mmod)


# SMA on SLA per species ####
# in each treatment


sma_ftrait_sp <- t2_traits %>% 
  # filter(fertilization == fferti) %>% 
  sma(as.formula(paste(ftrait, "~ log_plant_dry_mass ")),data = .) 
sma_ftrait_sp
coefs_ftrait<- coef(sma_ftrait_sp)
sma_ftrait_sp$pval

sp <- t2_traits %>% pull(code_sp) %>% unique() %>% sort()
PLOT <- NULL

for (i in c(1:length(sp))){
  spi <- sp[i]
  species <- t2_traits %>%
    merge(species_info) %>% 
    filter(code_sp == spi) %>%
    pull(scientificName_short) %>%
    unique()
  
  sma_ftrait_sp <- t2_traits %>% 
    filter(code_sp == spi) %>% 
    sma(as.formula(paste(ftrait, "~ log_plant_dry_mass ")),data = .) 
  
  coefs_ftrait<- coef(sma_ftrait_sp)
  pval<-sma_ftrait_sp$pval

  plot <- t2_traits %>%
    filter(code_sp == spi) %>%
    ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait)) +
    geom_point(aes(color = fertilization), size=3)+
    scale_color_brewer(palette = "Set2")+
    theme_classic()+
    {if (pval < 0.05) geom_abline(slope =coefs_ftrait[2],intercept = coefs_ftrait[1], size=1)} +
    {if (pval >= 0.05) geom_abline(slope =coefs_ftrait[2],intercept = coefs_ftrait[1], 
                                   linetype="dotted", size=1 )} +
    ggtitle(species) +
    theme_bw() +
    theme(legend.position = "none") +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
    ylim(c(1.1,1.7)) +
    xlim(c(-2.25,0.2)) +
    theme(text=element_text(size=15), # change font size of all text
              plot.title=element_text(size=20)) # change font size of plot title


  PLOT[[i]] <- plot
}


plot_sla <- ggpubr::ggarrange(plotlist=PLOT, ncol = 3,nrow = 6)
ggsave(paste0("draft/07_Appendix-allometry-SLA_plot-allom-sla.jpg"),plot_sla,height = 15, width = 10)
