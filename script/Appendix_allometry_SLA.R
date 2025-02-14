library(tidyverse)
library(smatr)
source("script/import_data.R")

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



t2_traits_all <- read.csv2("data/t2_traits_all.csv")
t2_traits_all %>% colnames()

#__________
# Check Eric par artefact SLA

# proportionally more leaf mass for small plants
t2_traits_all %>% 
  mutate(fract_leaf_scan = leaf_scan_dry_mass/leaf_dry_mass) %>% 
  ggplot(aes(x = log(plant_dry_mass), y = log(fract_leaf_scan))) +
  geom_point() +
  geom_smooth(method = "lm")

t2_traits_all %>% 
  mutate(fract_leaf_scan = leaf_scan_dry_mass/leaf_dry_mass) %>% 
  ggplot(aes(x = plant_dry_mass, y = fract_leaf_scan)) +
  geom_point() 

t2_traits_all %>% 
  mutate(fract_leaf_scan = leaf_scan_dry_mass/leaf_dry_mass) %>% 
  ggplot(aes(x = fract_leaf_scan)) +
  geom_histogram()

# globalement, on a pris plus de feuilles pour les plantes plus grosses
# (même si ce n'est pas valable pour toutes les espèces)
# Il y a surtout de la variation sur le nombre de feuilles prélevées entre individus de taille proche
t2_traits_all %>% 
  ggplot(aes(x = log(plant_dry_mass), y = log(nb_feuilles))) +
  geom_point() +
  facet_wrap(~code_sp)

# Globalement, on a pris un peu plus de masse de feuilles pour les plantes plus grosses
t2_traits_all %>% 
  ggplot(aes(x = log(plant_dry_mass), y = log(leaf_scan_dry_mass))) +
  geom_point() +
  facet_wrap(~code_sp)

#__________________________
t2_traits_all %>% 
  ggplot(aes(x = log(nb_feuilles), y = SLA)) +
  geom_point() +
  facet_wrap(~code_sp)



# we measured more leaf biomass as the plants were bigger
t2_traits_all %>% 
  ggplot(aes(x = plant_dry_mass, y = nb_feuilles * leaf_dry_mass)) +
  geom_point() 
  # geom_smooth(method = "lm")

for_sma <- t2_traits_all %>% 
  filter(nb_feuilles > 0) %>% 
  mutate(log_leaf_measured_dry_mass = log10( nb_feuilles * leaf_dry_mass),
         log_plant_dry_mass = log10( plant_dry_mass)) %>% 
  dplyr::select(log_plant_dry_mass, log_leaf_measured_dry_mass) %>% 
  na.omit()

for_sma %>% ggplot(aes(x = log_plant_dry_mass, y  = log_leaf_measured_dry_mass)) + geom_point()
out_sma <- sma(log_leaf_measured_dry_mass ~ log_plant_dry_mass,data = for_sma,na.action = na.omit)
summary(out_sma)

# leaves measured represented a constant proportion of the plants
# except from Trifolium and Medicago
t2_traits_all %>% 
  ggplot(aes(x = log(plant_dry_mass), y = log((nb_feuilles * leaf_dry_mass)/leaf_dry_mass))) +
  geom_point() +
  facet_wrap(~code_sp) +
  geom_smooth(method = "lm")

for_sma <- t2_traits_all %>% 
  filter(nb_feuilles > 0) %>% 
  mutate(leaf_measured_dry_mass =  nb_feuilles * leaf_dry_mass) %>% 
  dplyr::select(code_sp,origin, fertilization,plant_dry_mass,leaf_dry_mass, leaf_measured_dry_mass) %>% 
  mutate(fract_leaves = leaf_measured_dry_mass/leaf_dry_mass/1000) %>% 
  na.omit() %>% 
  filter(fract_leaves < 0.050)

for_sma %>% 
  ggplot(aes(x = plant_dry_mass, y  = fract_leaves)) + 
  geom_point() 
  geom_vline(xintercept = 0.05)

for_sma %>% dim()
for_sma %>% filter(plant_dry_mass <0.05) %>% dim()
for_sma2 <- for_sma %>% filter(plant_dry_mass > 0.1)

# out_sma <- sma(log_leaf_measured_dry_mass ~ log_plant_dry_mass,data = for_sma,na.action = na.omit)
summary(out_sma)





#___________________________________
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
  # ggsave(paste0("output/plot/sma_sla/",spi,".png"),plot)
}


plot_sla <- ggpubr::ggarrange(plotlist=PLOT, ncol = 3,nrow = 6)
# ggsave(paste0("draft/plot_allom_sla",fferti,".jpg"),plot_sla,height = 15, width = 10)
ggsave(paste0("draft/plot_allom_sla.jpg"),plot_sla,height = 15, width = 10)
