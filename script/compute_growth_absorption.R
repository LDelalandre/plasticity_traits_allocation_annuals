library(tidyverse)


# Import data ####

t0 <- read.csv2("data/t0_height_dry_mass.csv")
t1_traits <- read.csv2("data/t1_traits.csv")
t2_traits <- read.csv2("data/t2_traits.csv") %>% 
  mutate(absortive_root_dry_mass = root_dry_mass - pivot_dry_mass) %>% 
  mutate(tot_RL = SRL * absortive_root_dry_mass) %>% 
  mutate(log_tot_RL = log10(tot_RL)) %>%
  
  mutate(tot_RA = SRL * absortive_root_dry_mass * diam ) %>% # root area
  mutate(log_tot_RA = log10(tot_RA)) %>%
  
  mutate(tot_LA = SLA * leaf_dry_mass) %>% 
  mutate(log_tot_LA = log10(tot_LA))

traits_height_mass <- c(    
  # height
  "Hveg" ,# "Hrepro",
  # mass
  "plant_fresh_mass", "plant_dry_mass",
  # stem mass
  "stem_fresh_mass", "stem_dry_mass",
  # leaf mass
  "leaf_fresh_mass", "leaf_dry_mass",
  "leaf_scan_fresh_mass", "leaf_scan_dry_mass",
  # root mass
  "pivot_fresh_mass","pivot_dry_mass",
  # "root_scan_fresh_mass","root_scan_dry_mass",
  "root_fresh_mass","root_dry_mass")

traits <- c(
  # leaf traits
  "LA", "LDMC","SLA",
  # root traits
  "SRL", "RTD", "RDMC", "diam","BI",
  # nutrient
  "C","N",
  # plant_traits
  "tot_RL","tot_RA","tot_LA")


# Process data ####

## average traits values at pop*trt level ####
mean_traits_pop <- t2_traits %>%  
  select(pop,code_sp,origin,fertilization,
         all_of(traits), 
         all_of(traits_height_mass)) %>% 
  # ajouter ici les fractions de biomasses voulues!
  mutate(RMF = root_dry_mass/plant_dry_mass, # Root mass fraction
         LMF = leaf_dry_mass/plant_dry_mass,
         SMF = stem_dry_mass/plant_dry_mass,
         root_shoot = root_dry_mass/(stem_dry_mass+leaf_dry_mass),
         LAplant = SLA * leaf_dry_mass,
         LAR = LAplant/plant_dry_mass, # leaf area ratio 
         log_LA = log(LA) ,
         tot_LA = SLA * leaf_dry_mass,
         tot_RL = SRL * root_dry_mass) %>% # total root length (/!\ neglected pivot !!)
  group_by(pop,code_sp,origin,fertilization) %>% 
  summarize_all(.funs = mean,na.rm=T)

## compute RGR ####
data_rgr_sar <- rbind(
  select(t0,pot,code_sp,origin,fertilization,day_of_year,time,plant_dry_mass,root_dry_mass) %>% mutate(N = NA,leaf_dry_mass = NA),
  select(t1_traits,pot,code_sp,origin,fertilization,day_of_year,time,plant_dry_mass,root_dry_mass,leaf_dry_mass,N),
  select(t2_traits,pot,code_sp,origin,fertilization,day_of_year,time,plant_dry_mass,root_dry_mass,leaf_dry_mass,N)) %>% 
  mutate(pop = paste(code_sp,origin, sep = "_")) %>% 
  mutate(log_plant_dry_mass = log(plant_dry_mass))

write.csv2(data_rgr_sar,"output/data/data_rgr_sar.csv",row.names=F)

# plots size =f(time)
data_rgr_sar %>% 
  # filter(!(time == "t2")) %>% 
  ggplot(aes(x = day_of_year, y = log_plant_dry_mass, color = fertilization)) +
  geom_point() +
  facet_wrap(~code_sp) +
  geom_smooth(method = "lm")

data_rgr_sar %>% 
  # filter(!(time == "t2")) %>% 
  ggplot(aes(x = day_of_year, y = log_plant_dry_mass)) +
  geom_point() +
  facet_wrap(~fertilization) +
  geom_smooth(method = "lm")

plant_masses <- data_rgr_sar %>% 
  dplyr::group_by(code_sp,origin,pop,fertilization,time) %>% 
  summarize(mean_dry_mass = mean(plant_dry_mass,na.rm=T)) %>% 
  mutate(time = paste0(time,"_mass")) %>% 
  spread(key = time,value = mean_dry_mass)

root_masses <- data_rgr_sar %>% 
  dplyr::group_by(code_sp,origin,pop,fertilization,time) %>% 
  summarize(root_dry_mass = mean(root_dry_mass, na.rm = T)) %>% 
  mutate(time = paste0(time,"_root_mass")) %>% 
  spread(key = time,value = root_dry_mass)

leaf_masses <- data_rgr_sar %>% 
  dplyr::group_by(code_sp,origin,pop,fertilization,time) %>% 
  summarize(leaf_dry_mass = mean(leaf_dry_mass, na.rm = T)) %>% 
  mutate(time = paste0(time,"_leaf_mass")) %>% 
  spread(key = time,value = leaf_dry_mass)

times <- data_rgr_sar %>% 
  dplyr::group_by(code_sp,origin,pop,fertilization,time) %>% 
  summarize(mean_day_of_year = mean(day_of_year,na.rm = T)) %>% 
  mutate(time = paste0(time,"_date")) %>% 
  spread(key = time,value = mean_day_of_year)

# nitrogen concentration (remove)
# nitrogen <- data_rgr_sar %>% 
#   group_by(code_sp,origin,pop,fertilization) %>% 
#   mutate(Ic = mean(N,na.rm=T)) %>% # Hyp used to compute Sarm: Ic = constant concentration of nutrients (cf. Garnier Kock 1989 Oecologia)
#   # /!\ Assez mal vérifiée dans le N- !!
#   dplyr::group_by(code_sp,origin,pop,fertilization,time,Ic) %>% 
#   summarize(mean_N = mean(N,na.rm=T)
#   ) %>% 
#   mutate(time = paste0(time,"_N")) %>% 
#   spread(key = time,value = mean_N)

# nitrogen quantify
nitrogen_quantity <- data_rgr_sar %>% 
  mutate(N_mass = plant_dry_mass * N) %>% 
  dplyr::group_by(code_sp,origin,pop,fertilization,time) %>% 
  summarize(mean_N = mean(N_mass,na.rm=T)) %>% 
  mutate(time = paste0(time,"_N")) %>% 
  spread(key = time,value = mean_N)

# compute RGRslope
data_RGR_pop <- data_rgr_sar %>%
  group_by(fertilization,code_sp,origin) %>% 
  group_modify(.f = ~ broom::tidy(lm(log_plant_dry_mass ~ day_of_year, data = .x), conf.int = F)) %>% 
  filter(term == "day_of_year") %>% 
  dplyr::rename(RGRslope = estimate) %>% 
  select(fertilization,code_sp,RGRslope)

# compute RGR with two points
# and SAR between t1 and t2
mean_rgr_sar <- merge(plant_masses,times) %>% 
  merge(root_masses) %>% 
  merge(leaf_masses) %>% 
  merge(nitrogen_quantity) %>% 
  # RGR
  mutate(RGR01 = (log(t1_mass) - log(t0_mass)) / (t1_date - t0_date) ) %>% 
  mutate(RGR02 = (log(t2_mass) - log(t0_mass)) / (t2_date - t0_date) ) %>% 
  mutate(RGR12 = (log(t2_mass) - log(t1_mass)) / (t2_date - t1_date) ) %>% 
  merge(data_RGR_pop) %>% 
  # SAR
  mutate( SAR = (t2_N - t1_N)/(t2_date - t1_date) * (log(t2_root_mass) - log(t1_root_mass))/(t2_root_mass - t1_root_mass) )

# 
traits_pop <- merge(mean_traits_pop,mean_rgr_sar) %>% 
  mutate(log_root_dry_mass = log10(root_dry_mass)) %>% 
  mutate(log_leaf_dry_mass = log10(leaf_dry_mass)) %>% 
  mutate(log_stem_dry_mass = log10(stem_dry_mass)) %>% 
  mutate(log_plant_dry_mass = log10(plant_dry_mass)) %>% 
  mutate(LAplant_t1 = SLA * t1_leaf_mass) %>% 
  mutate(LAplant_t2 = SLA * t2_leaf_mass) %>% # vérifié : grosso modo = LAplant (ie moyenner avant ou après avoir calculé le trait)
# /!\ log à calculer a priori (avant de moyenner) ? = on considère que le log_root_dry_mass est un trait
# ou a posteriori (après avoir moyenné les val de traits) ? = on considère qu'on loggue les valeurs de traits pour les calculs
  group_by(fertilization,code_sp,origin,pop) %>%
  # ULR
  mutate(ULR12 = (t2_mass - t1_mass) / (t2_date - t1_date) * (log(LAplant_t2) - log(LAplant_t1))/(LAplant_t2 - LAplant_t1) )

write.csv2(traits_pop,"output/data/traits_pop.csv",row.names=F)

#_______________________________________________________________________________
# Plots ####

# vérif approx LAplant
traits_pop %>% 
  ggplot(aes(x = LAplant, y = LAplant_t2)) +
  geom_point()
# Yesss ! (moyenner avant ou après ne change pas grand-chose)

traits_pop %>% 
  # filter(fertilization == "N-") %>% 
  ggplot(aes(x = RMF,y = SRL))+
  geom_point() +
  # geom_smooth(method = "lm") +
  facet_wrap(~fertilization)

traits_pop %>% 
  # filter(fertilization == "N-") %>% 
  ggplot(aes(x = SRL,y = SAR))+
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~fertilization)

traits_pop %>% 
  # filter(fertilization == "N-") %>% 
  ggplot(aes(x = RMF,y = RGRslope))+
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~fertilization)

traits_pop %>% 
  ggplot(aes(x = fertilization, y = SAR,label = pop)) +
  geom_boxplot() +
  geom_point() +
  # ggrepel::geom_label_repel()+
  geom_line(aes(group = pop))

traits_pop %>% 
  filter(code_sp == "EROPVERN") %>% 
  ggplot(aes(x = fertilization, y = RGR01)) +
  geom_boxplot() +
  geom_point() +
  geom_line(aes(group = pop))
