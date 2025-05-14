library(tidyverse)
library(smatr)
source("script/01_import-data.R")

ftrait <- "log_SLA"

# For which species does plant dry mass decrease? ------------------------------
direction_change_mass <- traits_pop %>% 
  dplyr::select(pop,fertilization,plant_dry_mass) %>% 
  spread(key = fertilization, value = plant_dry_mass) %>% 
  rename (Nm = `N-`, Np = `N+`) %>% 
  mutate(change_mass = if_else (Nm < Np, "increase","decrease"))

pop_mass_decreases <- direction_change_mass %>% 
  filter(change_mass == "decrease")




# Mixed model ------------------------------------------------------------------

# Reviewer 1 asked whether the pattern may not result from some species being
# smaller in N- than in N+ because of osmotic stress.
# To test this, we removed these species from the analysis.
# The results are qualitatively unchanged.

modif_reviewer_1 <- F

if (modif_reviewer_1 == T) {
  data_mod <- 
    t2_traits %>% 
    filter(! (pop %in% pop_mass_decreases))  # Asked by reviewer 1
} else {
  data_mod <- 
    t2_traits
}



mmod <- lme4::lmer(as.formula(paste(ftrait, "~ log_plant_dry_mass + fertilization + (1|family/code_sp) ")),  
                   data=data_mod %>% merge(species_info))

plot_SLA <- data_mod %>%
  merge(species_info) %>% 
  mutate(fertilization = if_else(fertilization == "N+", "F+","F-")) %>% 
  merge(species_info) %>% 
  ggplot( aes(x = plant_dry_mass, y = SLA)) +
  geom_point(aes( color = fertilization)) +
  facet_wrap(~scientificNameShort,ncol = 3) +
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


sma_ftrait_sp <- data_mod %>% 
  # filter(fertilization == fferti) %>% 
  sma(as.formula(paste(ftrait, "~ log_plant_dry_mass ")),data = .) 
sma_ftrait_sp
coefs_ftrait<- coef(sma_ftrait_sp)
sma_ftrait_sp$pval

sp <- data_mod %>% pull(code_sp) %>% unique() %>% sort()
PLOT <- NULL

for (i in c(1:length(sp))){
  spi <- sp[i]
  species <- data_mod %>%
    merge(species_info) %>% 
    filter(code_sp == spi) %>%
    pull(scientificNameShort) %>%
    unique()
  
  sma_ftrait_sp <- data_mod %>% 
    filter(code_sp == spi) %>% 
    sma(as.formula(paste(ftrait, "~ log_plant_dry_mass ")),data = .) 
  
  coefs_ftrait<- coef(sma_ftrait_sp)
  pval<-sma_ftrait_sp$pval

  plot <- data_mod %>%
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
    theme(text=element_text(size=12), # change font size of all text
              plot.title=element_text(size=17)) # change font size of plot title


  PLOT[[i]] <- plot
}



plot_sla <- ggpubr::ggarrange(plotlist=PLOT, ncol = 3,nrow = 6)

ggsave(paste0("draft/07_Appendix-allometry-SLA_plot-allom-sla.jpg"),
       plot_sla,
       height = 198, 
       width = 220, 
       units = "mm")

