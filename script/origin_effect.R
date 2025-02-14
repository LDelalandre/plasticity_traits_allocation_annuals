library(tidyverse)
t2_traits <- read.csv2("data/t2_traits.csv") %>% 
  mutate(log_Hveg = log10(Hveg), log_plant_dry_mass = log10(plant_dry_mass),log_LA = log10(LA)) %>% 
  mutate(RMF = root_dry_mass/plant_dry_mass,
         SMF = stem_dry_mass/plant_dry_mass,
         LMF = leaf_dry_mass/plant_dry_mass) 

fsp <- "ALYSALYS"
ftrait <- "plant_dry_mass"

PLOT <- NULL
i <- 0
# for (fsp in t2_traits %>% pull(code_sp) %>% unique()){
for (ftrait in FTRAITS){
  fspecies <- t2_traits %>% 
    dplyr::filter(code_sp==fsp) %>% 
    pull(species) %>% 
    unique()
  i <- i+1
  fdata <- t2_traits %>% 
    dplyr::filter(code_sp == fsp)

  plot <- fdata %>% 
    mutate(fertilization = if_else(fertilization == "N-","F-","F+")) %>% 
    mutate(origin = if_else(origin == "Nat","Ext.","Int.")) %>% 
    mutate(origin = factor(origin,levels = c("Ext.","Int."))) %>% 
    ggplot(aes_string(x = "fertilization",y = ftrait, fill = "origin")) + #color = "origin",
    geom_boxplot() +
    # geom_point() +
    {if (ftrait %in% c("plant_dry_mass","Hveg","LA")) scale_y_continuous(trans='log10')} +
    theme_classic() +
    ggtitle(fspecies) +
    geom_point(position = position_jitterdodge(jitter.width = 0)) +
    scale_fill_brewer(palette = "Set1",direction = -1) +
    # ylab ("Plant dry mass (g)") +
    xlab("fertilization")
  plot
  # ggsave("draft/ALYSALYS.png",plot,width = 3, height = 3)
  PLOT[[i]] <- plot
}
PLOT

formula <- as.formula(paste0(ftrait, " ~ origin * fertilization "))
mod <- lm(formula, data = fdata ) # %>% filter(fertilization == "N+")
summary(mod)
car::Anova(mod)
plot(mod)

mod0 <- lm(as.formula(paste0(ftrait, " ~ 1 ")), data = fdata )
anova(mod,mod0)

# La suite dans Q4_strategy_plasticity.R, à la fin

student <- t.test(log_plant_dry_mass~fertilization, data = t2_traits %>% 
         filter(code_sp == "ALYSALYS") %>% 
         filter(origin == "Nat") %>% 
         mutate(log_plant_dry_mass = log(plant_dry_mass)))


plot_alys_nat <- t2_traits %>% 
  mutate(log_plant_dry_mass = plant_dry_mass) %>% 
  filter(code_sp == "ALYSALYS") %>% 
  filter(origin == "Nat") %>% 
  mutate(fertilization = if_else(fertilization == "N-","F-","F+")) %>% 
  mutate(origin = if_else(origin == "Nat","Ext.","Int.")) %>% 
  mutate(origin = factor(origin,levels = c("Ext.","Int."))) %>% 
  ggplot(aes(x = fertilization, y = log_plant_dry_mass,fill=fertilization)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2",direction = 1) +
  theme_classic() +
  theme(legend.position = "none") +
  geom_point() +
  ggsignif::geom_signif(comparisons = list(c("F-", "F+")),test = "t.test",
                        map_signif_level = T,
                        textsize = 10) +
  ylab ("Plant dry mass(g)") +
  xlab("fertilization") +
  ylim(c(-2,0)) +
  scale_y_continuous(trans='log10')

ggsave("draft/ALYSALYS_nat.png",plot_alys_nat,width = 3, height = 3)

