library(tidyverse)
library(ggpubr)
library(smatr)
library(cowplot())

# Import data ####
t2_traits <- read.csv2("data/t2_traits.csv")%>% 
  mutate(absortive_root_dry_mass = root_dry_mass - pivot_dry_mass) %>% 
  mutate(tot_RL = SRL * absortive_root_dry_mass) %>% # in m (with SRL in m/g and mass in g)
  mutate(log_tot_RL = log10(tot_RL)) %>% 
  
  mutate(tot_RA = tot_RL * 1000 * pi *diam ) %>% # root area, in mm² (convert tot_RL in mm, because diam is is mm)
  mutate(log_tot_RA = log10(tot_RA)) %>%
  
  mutate(tot_LA = SLA * leaf_dry_mass*1000) %>% # convert leaf dry mass into mg so that tot_LA in is mm² (with SLA in mm²/mg) 
  mutate(log_tot_LA = log10(tot_LA)) %>%
  
  mutate(log_plant_dry_mass = log10(plant_dry_mass),
         log_leaf_dry_mass = log10(leaf_dry_mass),
         log_stem_dry_mass = log10(stem_dry_mass),
         log_root_dry_mass = log10(root_dry_mass)) %>% 
  
  mutate(RMF = root_dry_mass/plant_dry_mass,
         SMF = stem_dry_mass/plant_dry_mass,
         LMF = leaf_dry_mass/plant_dry_mass)

sp_fam <- t2_traits %>% 
  select(code_sp,family) %>% 
  unique()

sp_nat = c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO')

traits_pop <- read.csv2("output/data/traits_pop.csv") %>% 
  mutate(log_plant_dry_mass = log(plant_dry_mass)) %>% 
  mutate(log_tot_LA = log10(tot_LA)) %>% 
  mutate(log_tot_RL = log10(tot_RL)) %>% 
  mutate(log_tot_RA = log10(tot_RA)) %>%
  mutate(log_RGRslope = log10(RGRslope)) %>% 
  merge(sp_fam) %>% 
  mutate(log_Hveg = log10(Hveg))


# Boxplot of the three traits that move ####
bp_plast_simple <- function(ftrait){
  traits_pop %>%
    mutate(origin = factor(origin, levels = c("Fer","Nat"))) %>%
    mutate(fertilization = factor(fertilization, levels = c("N+","N-"))) %>%
    ggplot(aes_string(x="fertilization", y=ftrait,label = "pop"
                      # shape = "code_sp",
    )) + #fill = "fertilization"
    theme_classic() +
    geom_boxplot(outlier.shape = NA)+
    
    geom_line(aes(group = pop),alpha=0.4,color = "black") + 
    geom_point(size = 2,aes(color = fertilization),)+
    theme(legend.position="none") +
    theme(axis.ticks.x=element_blank() ,
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    ggtitle(ftrait) +
    scale_color_brewer(palette = "Set2",direction = -1) + 
    theme(text=element_text(size=13))
}
bps <- lapply(list("N","RMF","log_plant_dry_mass"), bp_plast_simple)
plots1 <- ggpubr::ggarrange(plotlist=bps, ncol = 3,nrow = 1)
ggsave("draft/bp_ferti.png", plots1,width = 8, height = 3)


# Theoretical plot population and ferti ####
# - Pour le papier : shape = origin et color = ferti avec   # scale_color_brewer(palette = "Set1")
# - Pour l'oral de thèse : color = origin (pour coller avec le papier JVS)

ftrait <- "LDMC"
plot_th_ftrait <- traits_pop %>% 
  group_by(code_sp,origin,pop,fertilization) %>% 
  mutate(fertilization = factor(fertilization, levels = c("N+","N-"))) %>% 

  filter(!(code_sp  %in% c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO'))) %>% 
  
  ggplot(aes_string(label = "pop")) +
  theme_classic() +
  
  geom_point(aes_string(x="fertilization", y=ftrait,color = "origin"),size = 2) + #,shape = "origin"

  geom_line(aes_string(x="fertilization", y=ftrait,group = "pop"),alpha=0.4) +
  
  theme(legend.position="none") +
  theme(axis.ticks.x=element_blank() ,
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  ) +
  ggtitle(ftrait)+
  scale_shape_manual(values = c(16,15)) +
  facet_wrap(~code_sp)
ggsave("output/plot/plot_th_ftrait.png",plot_th_ftrait)


