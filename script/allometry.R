library(tidyverse)
library(smatr)

# import data ####

t0 <- read.csv2("data/t0_height_dry_mass.csv")
t1_traits <- read.csv2("data/t1_traits.csv",fileEncoding = "Latin1")  %>% 
  mutate(pop = paste(code_sp,origin,sep="_"))
t2_traits <- read.csv2("data/t2_traits.csv") %>% 
  mutate(pop = paste(code_sp,origin,sep="_"))

t1_mass <- t1_traits %>% 
  mutate(pop = paste(code_sp,origin,sep="_")) %>% 
  select(code_sp,pot,origin,pop,fertilization,time,
         "plant_dry_mass", "stem_dry_mass","leaf_dry_mass","root_dry_mass",
         "plant_fresh_mass", "stem_fresh_mass","leaf_fresh_mass","root_fresh_mass") 


t2_mass <- t2_traits %>% 
  mutate(pop = paste(code_sp,origin,sep="_")) %>% 
  select(code_sp,pot,origin,pop,fertilization,time,
         "plant_dry_mass", "stem_dry_mass","leaf_dry_mass","root_dry_mass",
         "plant_fresh_mass", "stem_fresh_mass","leaf_fresh_mass","root_fresh_mass") 



t1_t2_mass <- rbind(t1_mass,t2_mass) %>% 
  
  mutate(pop_fer = paste(pop,fertilization,sep ="_")) %>% 
  mutate(sp_fer = paste(code_sp,fertilization,sep ="_")) %>% 
  
  # dry
  mutate(shoot_dry_mass = stem_dry_mass + leaf_dry_mass) %>%
  mutate(log_plant_dry_mass = log10(plant_dry_mass),
         log_stem_dry_mass = log10(stem_dry_mass),
         log_leaf_dry_mass = log10(leaf_dry_mass),
         log_root_dry_mass = log10(root_dry_mass),
         log_shoot_dry_mass = log10(shoot_dry_mass)) %>% 
  mutate(root_shoot_dry = root_dry_mass/shoot_dry_mass) %>% 
  mutate(log_root_shoot_dry = log(root_shoot_dry)) %>% 
  
  # fresh
  mutate(shoot_fresh_mass = stem_fresh_mass + leaf_fresh_mass) %>%
  mutate(log_plant_fresh_mass = log10(plant_fresh_mass),
         log_stem_fresh_mass = log10(stem_fresh_mass),
         log_leaf_fresh_mass = log10(leaf_fresh_mass),
         log_root_fresh_mass = log10(root_fresh_mass),
         log_shoot_fresh_mass = log10(shoot_fresh_mass)) %>% 
  mutate(root_shoot_fresh = root_fresh_mass/shoot_fresh_mass) %>% 
  mutate(log_root_shoot_fresh = log(root_shoot_fresh)) 

# list_sp = sort(unique(t1_dry_mass$code_sp)) #liste de toutes les espèces
# list_sp_nat = c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO') #liste des espèces qui sont uniquement dans le milieu natif
# list_sp_no_nat = list_sp[!(list_sp %in% list_sp_nat)]
# list_pop = sort(unique(t1_dry_mass$sp_or))
# list_pop_fer = sort(unique(t1_dry_mass$sp_or_fer))
# list_sp_fer = sort(unique(t1_dry_mass$sp_fer))

## _____________________________________________________________________________
## Comparaison allométrie à t1 et t2 ####
## On compare la pente globale à t1 et à t2 pour voir si on a une modification de l'allométrie avec l'ontogénie

## Sans séparer par population
t1_t2_mass %>% 
  filter(fertilization == "N+") %>%
  ggplot(aes(x = log_plant_dry_mass, y = log_root_dry_mass, color = time,label = pot))+
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_color_manual(values = c("green","red")) 
t1_t2_mass %>% 
  filter(time=="t2") %>%
  ggplot(aes(x = log_plant_dry_mass, y = log_root_dry_mass, color = fertilization,label = pot))+
  geom_point() + 
  geom_smooth(method = 'lm') +
  facet_wrap(~pop)
  # facet_wrap(~time)

# compare root_plant f(time):
sma(log_stem_dry_mass ~ log_plant_dry_mass+time,t1_t2_mass, type = "elevation") #pas de diff entre t1 et t2
# no difference in slope but shift in elevation

# compare root_plant f(ferti):
sma(log_root_dry_mass ~ log_plant_dry_mass+fertilization, 
    t1_t2_mass %>% filter(time == "t2"), 
    type = "elevation") %>% 
  coefficients()

#pas de diff entre t1 et t2
# no difference in slope but shift in elevation

sma(log_root_dry_mass ~ log_plant_dry_mass+fertilization, 
    t1_t2_mass %>% filter(time == "t2"), 
    type = "elevation") %>% 
  coefficients()

sma(log_root_dry_mass ~ log_leaf_dry_mass*time,t1_t2_mass) #pas de diff entre t1 et t2
sma(log_leaf_dry_mass ~ log_stem_dry_mass*time,t1_t2_mass) #pour le leaf:stem les pentes sont tres différentes entre t1 et t2

comp <- sma(log_root_dry_mass ~ log_plant_dry_mass*time,t1_t2_mass) #pour le leaf:stem les pentes sont tres différentes entre t1 et t2
summary(comp)

## En séparant par population 

t1_t2_mass %>% 
  filter(fertilization == "N+") %>% 
  ggplot(aes(x = log_plant_dry_mass, y = log_root_dry_mass,shape = fertilization))+
  geom_point(aes(color = time))+
  # geom_smooth(method = 'lm')+
  facet_wrap(~pop)

sma_t1_t2 <- function(pop){ #fonction qui calcule la pvalue de la diff de pente entre t1 et t2 pour la sma de root = f(shoot) pour une pop donnée
  a <- smatr::sma(log_root_dry_mass ~log_stem_dry_mass*time,t1_t2_dry_mass[t1_t2_dry_mass$pop == pop,])
  b <- a$commoncoef[[2]]
  b
}

list_pop <- t1_t2_mass %>% pull(pop) %>% unique()

# diff_t1_t2 <- data.frame(pop = list_pop, pval_diff_t1_t2 = unlist(lapply(lapply(list_pop,sma_t1_t2),as.data.frame)))

#Y a 4 populations pour lesquelles on a une différence significative de pente entre t1 et t2
#Pour MYOSRAMO_Nat à t2 on a un r2 de 0,035 et une pvalue de 0,6
#Pour MINUHYBR_Fer on a clairement une diff, des pvalue OK et r2 entre 0,5 et 0,7
#Pour GERADISS_Nat on  a un mauvais r2 et une mauvaise p value à t1 
#Pour SHERARVE_Fer pvalue et r2 pas trop mauvaises 

# NETTOYER ENCORE LES DONNEES ICI!!!
t2_traits %>% 
  ggplot(aes(x = log10(plant_dry_mass), y = log10(RDMC), color = fertilization,label = pot))+
  geom_point() +
  facet_wrap(~code_sp)
  # geom_smooth(method = 'lm')

## _____________________________________________________________________________
## Allométrie pour chaque paire d'organes ####
## Etude des relations allométriques en combinant les données à t1 et t2
## On étudie 5 relations : root:shoot ; leaf:stem ; leaf:root ; root:stem ; root:masse_tot


# Plot rapide des relations allométriques pour chaque espèce selon la fertilisation
t1_t2_mass %>% 
  # filter(fertilization == "N+") %>% 
  ggplot(aes(x = log_plant_dry_mass,y = log_root_dry_mass,color = fertilization)) +
  geom_point() +
  # facet_wrap(~pop) +
  geom_smooth(method = 'lm') +
  scale_color_hue(h = c(180, 300))


t1_t2_mass %>% 
  # filter(time == "t1") %>% 
  ggplot(aes(x = shoot_dry_mass, y = root_dry_mass,color = fertilization)) +
  geom_point() 
  # geom_smooth(method="lm") 


#________________________________
# Vasseur 2023 ####
t2_traits2 <- t2_traits %>% 
  mutate(absortive_root_dry_mass = root_dry_mass - pivot_dry_mass) %>% 
  mutate(tot_RL = SRL * absortive_root_dry_mass) %>% 
  mutate(log_tot_RL = log10(tot_RL)) %>%
  
  mutate(tot_RA = SRL * absortive_root_dry_mass * diam ) %>% # root area
  mutate(log_tot_RA = log10(tot_RA)) %>%
  
  mutate(tot_LA = SLA * leaf_dry_mass) %>% 
  mutate(log_tot_LA = log10(tot_LA)) %>%

  mutate(log_plant_dry_mass = log10(plant_dry_mass))

## Leaf exchange surface ####
t2_traits2 %>% 
  ggplot(aes(x = fertilization, y = log_tot_LA)) +
  geom_boxplot() +
  geom_point()
# la surface totale des feuilles augmente ! Le montrer !

t2_traits2 %>% 
  ggplot(aes(x = log_plant_dry_mass,y = log_tot_LA, color = fertilization)) + # ou tot_RL
  geom_point() +
  geom_smooth(method = "lm")

# Pas de changement des relations d'allométrie entre conditions, ni pente, ni elevation (sma n.s.)
# juste un shift le long de la relation d'allométrie (plantes plus grandes en N+)

sma_LA <- sma(log_tot_LA ~ log_plant_dry_mass + fertilization, type = "shift",data = t2_traits2)
sma_LA
sma_LA %>% 
  coefficients()


## Root exchange surface ####

### Root length ####
t2_traits2 %>% 
  ggplot(aes(x = fertilization, y = log_tot_RL)) +
  geom_boxplot() +
  geom_point()

t2_traits2 %>% 
  ggplot(aes(x = log_plant_dry_mass,y = log_tot_RL, color = fertilization)) + # ou tot_RL
  geom_point() +
  geom_smooth(method = "lm")

sma_RL <- sma(log_tot_RL ~ log_plant_dry_mass + fertilization, type = "elevation",data = t2_traits2)
sma_RL %>% 
  coefficients()

### Root area ####
t2_traits2 %>% 
  ggplot(aes(x = fertilization, y = log_tot_RA)) +
  geom_boxplot() +
  geom_point()

t2_traits2 %>% 
  ggplot(aes(x = log_plant_dry_mass,y = log_tot_RA, color = fertilization)) + # ou tot_RL
  geom_point() +
  geom_smooth(method = "lm")

sma_RA <- sma(log_tot_RA ~ log_plant_dry_mass + fertilization, type = "elevation",data = t2_traits2)
sma_RA
sma_RA %>% 
  coefficients()





# Freschet 2015 ####
t2_traits2 %>% 
  ggplot(aes(x = SLA/SRL, y = leaf_dry_mass/root_dry_mass, color = fertilization)) +
  geom_point() +
  geom_smooth()



#_____________________________________________

# SMA per pop*trt ####


# functions to extract pvalue and r2
get_pval <- function(sma_output){
  sma_output$pval[[1]]
}
get_r2 <- function(sma_output){
  sma_output$r2[[1]]
}

# perform sma and give coefs etc.
perform_sma <- function(fdata){
  output_sma <- with(fdata,
                     by(fdata, pop_fer,
                        function(x) smatr::sma(ord ~ abs, data = x)))
  
  coefs <- sapply(output_sma, coef)  %>% 
    t() %>%
    as.data.frame() %>% 
    rownames_to_column("pop_fer") %>% 
    separate(pop_fer,into = c("code_sp","origin","fertilization"),sep="_") %>% 
    mutate(pop = paste (code_sp,origin,sep="_"))
  
  pval <- sapply(output_sma, get_pval) %>% 
    as.data.frame() %>% 
    rownames_to_column("pop_fer") %>% 
    separate(pop_fer,into = c("code_sp","origin","fertilization"),sep="_") %>% 
    mutate(pop = paste (code_sp,origin,sep="_"))
  colnames(pval)[4] <- "pval"
  
  r2 <- sapply(output_sma,get_r2)%>% 
    as.data.frame() %>% 
    rownames_to_column("pop_fer") %>% 
    separate(pop_fer,into = c("code_sp","origin","fertilization"),sep="_") %>% 
    mutate(pop = paste (code_sp,origin,sep="_"))
  colnames(r2)[4] <- "r2"
  
  sma_pop <- merge(coefs,pval) %>% merge(r2)
  sma_pop
}


# axes possibles pour les SMA :
# log_leaf_dry_mass ; log_shoot_dry_mass ; log_stem_dry_mass ; log_plant_dry_mass ; 
# log_root_dry_mass ; log_root_dry_mass_estim

x_axis <- "log_plant_dry_mass"
y_axis <- "log_root_dry_mass"

fdata <- t1_t2_mass %>% 
  # filter(time == "t2") %>% 
  select(pot,code_sp,fertilization,origin,sp_fer,pop_fer,pop,time,x_axis,y_axis) %>% 
  dplyr::rename(ord = y_axis) %>% 
  dplyr::rename(abs = x_axis)


fdata %>% 
  ggplot(aes(x = abs, y = ord, color = fertilization)) +
  geom_point() +
  facet_wrap(~pop) +
  geom_smooth(method = "lm")
  
pop_lack_observations_t2 <- fdata %>% 
  group_by(pop,fertilization) %>% 
  summarize(n=n()) %>% 
  arrange(n) %>% 
  filter(n<5) %>% 
  pull(pop)
# retiré 6 pops sur 30 en bossant à t2 sur RMF
fdata <- fdata %>% filter(!(pop %in% pop_lack_observations_t2))

# check that we have the same data as for boxplots of plasticity
# CHECKED

# boxplot of ratio ord/abs on values of mass averaged per pop
# x_axis <- "shoot_dry_mass"
# y_axis <- "root_dry_mass"
# fdata %>% 
#   group_by(pop,fertilization) %>% 
#   summarize(ord = mean(ord,na.rm=T), abs = mean(abs,na.rm = T)) %>% 
#   ggplot(aes(x = fertilization, y = ord/abs)) +
#   geom_boxplot() +
#   geom_point() +
#   geom_line(aes(group = pop))


# perform SMA to relate the two biomasses

sma_pop <- perform_sma(fdata)
sma_pop_nonat <- sma_pop %>% filter(!code_sp %in% list_sp_nat)

# write.csv2(sma_pop,'output/data/sma_dry_mass_t1_t2_rs.csv',row.names=F)


# graphes pour voir
sma_pop %>% 
  ggplot(aes(x = fertilization, y = elevation)) +
  geom_boxplot() +
  # geom_line(aes(group = pop)) +
  geom_point()

sma_pop %>% 
  ggplot(aes(x = fertilization, y = slope)) +
  geom_boxplot() +
  geom_point()


sma_pop_intercept <- sma_pop %>% 
  select(-c(pval,r2,slope)) %>% 
  spread(key = fertilization,value = elevation)

sma_pop_slope <- sma_pop %>% 
  select(-c(pval,r2,elevation)) %>% 
  spread(key = fertilization,value = slope)

t.test(sma_pop_intercept$`N+`,sma_pop_intercept$`N-`)
t.test(sma_pop_slope$`N+`,sma_pop_slope$`N-`)




# RESTE DES SCRIPTS : REGARDER LE SCRIPT DE LILA (il marche)