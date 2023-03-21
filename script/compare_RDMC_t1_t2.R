A <- t1_traits %>%
  mutate(RDMC = root_dry_mass*1000/root_fresh_mass) %>% # RDMC en mg/g
  group_by(time,pop,fertilization) %>% 
  dplyr::summarize(RDMC = mean(RDMC,na.rm=T),
                   root_fresh_mass = mean(root_fresh_mass,na.rm=T),
                   root_dry_mass = mean(root_dry_mass,na.rm=T))

B <- t2_traits %>% 
  # filter(plant_fresh_mass<1) %>% 
  group_by(time,pop,fertilization) %>% 
  summarize(RDMC = mean(RDMC,na.rm=T),
            root_fresh_mass = mean(root_fresh_mass,na.rm=T), 
            root_dry_mass = mean(root_dry_mass,na.rm=T))

data_RDMC <- rbind(A,B)

data_RDMC %>% 
  ggplot(aes(x = time, y = RDMC))+
  geom_boxplot() +
  geom_point()

data_merged <- merge(A,B,by=c("pop","fertilization"),suffixes = c("_t1","_t2")) %>% 
  mutate(RDMC_pop_t2 = root_dry_mass_t2*1000/root_fresh_mass_t2) %>% # on calcule le RDMC après avoir moyenné les masses
  # alors que RDMC_t2 autre est une moyenne des RDMC par pot
  mutate(root_dry_mass_estim_t2 = root_fresh_mass_t2 * (RDMC_t2)/1000)

data_merged %>% 
  ggplot(aes(x = log10(root_dry_mass_t2),y=log10(root_dry_mass_estim_t2))) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0)

data_merged %>% 
  ggplot(aes(x = RDMC_pop_t2,y=RDMC_t2 )) +
  geom_point() +
  geom_abline(slope = 1,intercept = 0) +
  xlim(c(0,300)) +
  ylim(c(0,300))


t2_traits %>% 
  ggplot(aes(x = plant_fresh_mass,y=plant_dry_mass))+
  geom_point() +
  geom_abline(slope = 1,intercept = 0) +
  xlim(c(0,10)) +
  ylim(c(0,1.5))




# Reproduire fictivement
# moyenne des ratios = ratio des moyennes ?
set.seed(10)
v_FM <- rnorm(n = 100,mean = 10,sd = 1) # fresh mass
v_DM <- v_FM/10  +  rnorm(n = 100,mean = 0,sd = 0.05)
plot(v_FM,v_DM)

group = c(rep("A",25),rep("B",25),rep("C",25),rep("D",25))

df <- data.frame(v_FM,v_DM,group) %>% 
  mutate(R = v_DM/v_FM) %>% 
  group_by(group) %>% 
  summarize(
    v_DM = mean(v_DM),
    v_FM = mean(v_FM),
    R = mean(R)) %>% 
  mutate(R2 = v_DM/v_FM)

plot(df$R,df$R2)
abline(a = 0, b = 1)
