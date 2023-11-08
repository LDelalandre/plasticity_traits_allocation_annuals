library(tidyverse)

seed <- read.csv2("data/seed_production.csv") %>% 
  filter(!code_sp == "TRIFCAMP") %>% # pas fini de compter
  filter(!code_sp == "TRICAMP") # pas fini de compter
sp_info <- read.csv2("output/data/species_info.csv") %>% 
  rename(family = Family)



# model ####
seed2 <- merge(seed,sp_info,by = "code_sp") %>% 
  mutate(origin = as.factor(origin) ) %>% 
  mutate(fertilization = as.factor(fertilization)) %>% 
  group_by(family,code_sp,pot,origin,fertilization) %>%
  summarize(nb_seeds = sum(seeds_tot,na.rm=T)) %>%
  mutate(nb_seeds = as.integer(floor(nb_seeds))) %>% 
  mutate(log_nb_seeds = log10(1+nb_seeds))

ftrait2 <- "nb_seeds"
ftrait <- ftrait2

formula <- as.formula(paste0(ftrait, " ~ fertilization + origin", " + (1|family/code_sp)"))

mmod <- lme4::glmer(formula = formula, data = seed2, family = quasipoisson)

# Diagnostic ####
library("DHARMa")
simulationOutput <- simulateResiduals(fittedModel = mmod, plot = F)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput) # trop de zéros pour une poisson
# package glmmTMB for zero-inflated models
library("glmmTMB")
mmod <- glmmTMB(nb_seeds ~ code_sp +   (1|origin), 
                 ziformula = ~ code_sp,  data=seed2 , family = nbinom1)



summary(mmod)

anov <- car::Anova(mmod)
anov

MuMIn::r.squaredGLMM(mmod)
library(partR2)

R2_mmod <- partR2(mmod, partvars = c("fertilization", "origin"), 
                 R2_type = "conditional", nboot = 10)
R2_mmod
summary(R2_mmod)

summary(mmod)

library(insight)

mmod <- lme4::glmer(nb_seeds ~ code_sp +   (1|origin), data = seed2 
                      , family = poisson)
car::Anova(mmod)
summary(mmod)

fix <- insight::get_variance_fixed(mmod)
ran <- insight::get_variance_random(mmod)
res <- insight::get_variance_residual(mmod)

fix/(fix+ran+res)
ran/(fix+ran+res)
res/(fix+ran+res)


model_info(mmod)

# How much fertilization N+ increases or decreases trait value
EM <- emmeans::emmeans(mmod, specs = c("fertilization"),  type = "response",
                       adjust = "tuckey")
# emmeans (version 1.5.2-1)

library(multcomp)
posthoc <- summary(glht(mmod, linfct = mcp(fertilization = "Tukey")), test = adjusted("fdr"))

mean_trait_Nm <- EM %>% as.data.frame () %>% filter(fertilization == "N-") %>% pull(emmean) %>% round(digits = 2)
mean_trait_Np <- EM %>% as.data.frame () %>% filter(fertilization == "N+") %>% pull(emmean) %>% round(digits = 2)
if (ftrait2 %in% c("plant_dry_mass","Hveg","LA")){
  mean_trait_Nm <- 10^(mean_trait_Nm) %>% round(digits = 2)
  mean_trait_Np <- 10^(mean_trait_Np) %>% round(digits = 2)
}



# diff <- posthoc$comparisons$estimate %>% round(digits = 3)
# if(posthoc$comparisons$contrast == "(N-) - (N+)"){
#   diff2 = -diff} else{
#     diff2=diff
#   }

pval <- anov %>%
  as.data.frame() %>% 
  rename(pval = 'Pr(>Chisq)') %>%
  pull(pval) %>% 
  format(scientific = TRUE, digits = 2) %>%
  as.numeric()

variance <- MuMIn::r.squaredGLMM(mmod) %>% 
  round(digits = 2)

ftrait_name <- trait_name %>% filter(trait == ftrait2) %>% pull(name)

table_pval <-  data.frame(Trait = ftrait_name,
                          fertilization = pval[1],
                          origin = pval[2],
                          # interaction = pval[3],
                          var_fixed =   variance[1,1], # R2m # variance explained by the fixed effects
                          var_tot =   variance[1,2],
                          mean_Nm= mean_trait_Nm,
                          mean_Np = mean_trait_Np
                          # Interaction = pval[3]
)

seed2 %>% 
  ggplot(aes(x = origin, y = nb_seeds)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  geom_point() +
  facet_wrap(~code_sp)





#____________
# Plots ####
seed2 %>% 
  filter(fertilization == "N+") %>% 
  ggplot(aes(x = nutrient_requirement, y = seeds_tot)) +
  geom_boxplot() +
  geom_point() +
  scale_y_continuous(trans='log10') 
  
seed2 %>% ggplot(aes(x = nb_seeds)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~code_sp)

seed %>% 
  # filter(!(code_sp %in% c("MINUHYBR","CERAPUMI")))  %>%
  mutate(pop = paste(code_sp,origin,sep="_")) %>% 
  # filter(fertilization == "N-") %>% 
  ggplot(aes(x = fertilization, y = seeds_tot)) +
  geom_boxplot() +
  # scale_y_continuous(trans='log10') +
  geom_point() +
  facet_wrap(~pop)

seed %>% 
  # filter(!(code_sp %in% c("MINUHYBR","CERAPUMI")))  %>%
  mutate(pop = paste(code_sp,origin,sep="_")) %>% 
  # filter(fertilization == "N-") %>% 
  ggplot(aes(x = code_sp, y = seeds_tot)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  geom_point()

# more seeds produced when pops come from fer. Model it (random effect on species identity)
# --> demonstrates differentiation among populations ? (pourquoi plus de graines)
# raw
seed %>%
  mutate(pop = paste(code_sp,origin,sep="_")) %>%
  group_by(code_sp,pop,fertilization,origin) %>% 
  summarize(sum_seeds_tot = sum(seeds_tot,na.rm = T)) %>% 
  filter(fertilization == "N-") %>%
  ggplot(aes(x = origin, y = sum_seeds_tot,label = code_sp)) +
  geom_boxplot() +
  geom_line(aes(group = code_sp))+
  scale_y_continuous(trans='log10') +
  geom_point() +
  geom_text()

# averaged per pop
seed %>%
  mutate(pop = paste(code_sp,origin,sep="_")) %>% 
  filter(fertilization == "N-") %>%
  ggplot(aes(x = origin, y = seeds_tot)) +
  geom_boxplot() +
  geom_line(aes(group = code_sp))+
  scale_y_continuous(trans='log10') +
  geom_point() 

seed %>%
  mutate(pop = paste(code_sp,origin,sep="_")) %>% 
  filter(fertilization == "N+") %>%
  ggplot(aes(x = origin, y = seeds_tot)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  geom_point() 

# fertilization does not alter seed production across species... (in terms of seed number)
seed %>%
  mutate(pop = paste(code_sp,origin,sep="_")) %>% 
  # filter(origin == "Nat") %>% 
  ggplot(aes(x = fertilization, y = seeds_tot)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10') +
  geom_point() 

