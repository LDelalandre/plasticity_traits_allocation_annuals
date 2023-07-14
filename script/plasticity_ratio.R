library(tidyverse)

source("script/import_data.R")

RGR <- traits_pop %>% 
  select(code_sp,origin,fertilization,RGR01,RGR02)

traits_plast2 <- c(
  "plant_dry_mass",
  # leaf traits
  "log_LA", "LDMC","SLA",
  # root traits
  # "SRL", "RTD",  "diam","BI", # NOT SIGNIFICANT!
  "RDMC",
  # nutrient
  # "C", n.s.
  "N",
  "RMF","LMF","SMF" )

# By averaging trait values per species ####
compute_plast_sp <- function(ftrait){
  plast <- traits_sp %>% 
    select(code_sp,fertilization,all_of(ftrait)) %>% 
    spread(key = fertilization,value = ftrait )
  plast[,ftrait] <-  plast$`N-` / plast$`N+` 
  
  plast %>% 
    ungroup() %>% 
    select(code_sp,ftrait)
}

PLAST_sp <- compute_plast_sp(traits_plast2[1])
for( i in c(2:length(traits_plast2)) ){
  plast_sp <- compute_plast_sp(traits_plast2[i])
  PLAST_sp <- merge(PLAST_sp,plast_sp)
}


PLAST_sp_ellenberg <- PLAST_sp %>% 
  merge(species_info,by="code_sp") %>% 
  filter(!(nutrient_requirement == "x")) %>% 
  mutate(nutrient_requirement = as.numeric(nutrient_requirement)) %>% 
  filter(!is.na(nutrient_requirement)) 



PLAST_sp_ellenberg %>% 
  ggplot(aes(x = nutrient_requirement, y = N)) +
  geom_point() +
  # geom_smooth(method = "lm") +
  geom_hline(yintercept  = 1)

mod <- lm(N ~ nutrient_requirement, data = PLAST_sp_ellenberg)
anova(mod)
plot(mod)


# Idem, per pop ####
compute_plast_pop <- function(ftrait){
  plast <- traits_pop %>% 
    select(pop,fertilization,all_of(ftrait)) %>% 
    spread(key = fertilization,value = ftrait )
  plast[,ftrait] <-  plast$`N-` / plast$`N+`
  
  plast %>% 
    ungroup() %>% 
    select(pop,ftrait)
}

PLAST_pop <- compute_plast_pop(traits_plast2[1])
for( i in c(2:length(traits_plast)) ){
  plast <- compute_plast_pop(traits_plast[i])
  PLAST_pop <- merge(PLAST_pop,plast)
}


PLAST_pop %>% 
  merge(RGR %>% filter(fertilization == "N+")) %>% 
  ggplot(aes(x = RGR01, y = N))+
  geom_point() 
  geom_smooth(method = "lm")

PLAST_pop_ellenberg <- PLAST_pop %>% 
  separate(pop,into = c("code_sp","origin")) %>% 
  merge(RGR) %>% 
  merge(species_info,by="code_sp") %>% 
  filter(!(nutrient_requirement == "x")) %>% 
  mutate(nutrient_requirement = as.numeric(nutrient_requirement)) %>% 
  filter(!is.na(nutrient_requirement)) 

PLAST_pop_ellenberg %>% 
  ggplot(aes(x = nutrient_requirement, y = plant_dry_mass,label=code_sp)) +
  geom_point() +
  # ggrepel::geom_label_repel()
  # geom_smooth(method = "lm")
  geom_hline(yintercept  = 1)

mod <- lm(plant_dry_mass~ nutrient_requirement , data = PLAST_pop_ellenberg )
anova(mod)
plot(mod)

PLAST_pop_ellenberg %>% 
  ggplot(aes(x = origin, y = N)) +
  geom_boxplot() +
  geom_point() +
  geom_line(aes(group=code_sp))


# 
traits_pop_ellenberg <- traits_pop %>% 
  merge(species_info,by="code_sp") %>% 
  filter(!(nutrient_requirement == "x")) %>% 
  mutate(nutrient_requirement = as.numeric(nutrient_requirement)) %>% 
  filter(!is.na(nutrient_requirement)) 


traits_pop_ellenberg %>% 
  ggplot(aes(x=nutrient_requirement, y = SLA,color = fertilization)) +
  geom_point() +
  # geom_smooth(method="lm") +
  facet_wrap(~fertilization) +
  theme_classic()

mod <- lm(N~ nutrient_requirement* fertilization ,data=traits_pop_ellenberg)
anova(mod)


traits_pop_ellenberg %>% 
  ggplot(aes(x=RGR01, y = N,color = fertilization)) +
  geom_point() 
  facet_wrap(~fertilization)

traits_pop_ellenberg %>% 
  ggplot(aes(x=fertilization, y = RGR02,color = fertilization)) +
  geom_boxplot() +geom_point()


# Lien Ellenberg - val de trait ####
fMEAN %>% 
  merge(species_info) %>% 
  filter(!(nutrient_requirement == "x")) %>% 
  mutate(nutrient_requirement = as.numeric(nutrient_requirement)) %>% 
  filter(!is.na(nutrient_requirement)) %>% 
  ggplot(aes(x=nutrient_requirement, y = SLA )) +
  geom_point() +
  # geom_smooth(method="lm") +
  theme_classic()
  # facet_wrap(~treatment)
# LNC, LDMC, SLA, LA, height, LCC