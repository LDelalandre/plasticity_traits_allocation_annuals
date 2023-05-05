library(tidyverse)

# Data ####
t2_traits <- read.csv2("data/t2_traits.csv")

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


traits_plant <- c("t0_mass","t1_mass","t2_mass")
# "RGR01","RGR02","RGR12","SAR" sont calculés au niveau pop, pas individu --> pas même modèle... Mais je peux faire un modèle direct sur la taille
# (dans le script "compute_growth_absorption")
data_rgr_sar <- read.csv2("output/data/data_rgr_sar.csv")


fdata <- data_rgr_sar %>% 
  mutate(log_plant_dry_mass = log(plant_dry_mass)) %>%
  # filter(!(time == "t2")) %>% 
  filter(!code_sp %in% c("BUPLBALD","MYOSRAMO","HORNPETR","FILAPYRA")) 
  # filter(fertilization == "N-") %>% 


ggplot(fdata,aes(x = day_of_year, y = log_plant_dry_mass,color = origin)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~fertilization)

mod_rgr <- lme4::lmer(log(plant_dry_mass) ~ day_of_year *  fertilization * population + (1|code_sp) , 
                      data = fdata %>% 
                        rename(population = origin)) 
ano <- car::Anova(mod_rgr)
sum <- summary(mod_rgr)

dfano <- ano %>% 
  as.data.frame()  %>% 
  rename(pval =`Pr(>Chisq)`) %>% 
  mutate(across(where(is.numeric), ~ round(., 3)))
# sum$coefficients


library(kableExtra)
table_mass_time <- dfano %>% 
  kableExtra::kable( escape = F,
                     col.names = c("Chisq ", "df", "pval")) %>%
  kableExtra::kable_styling("hover", full_width = F)



cat(table_trait_diff, file = "draft/table_mixed_model_mass.doc")

#_____________________________________
# à un point dans le temps : 
# l'effet origine est net au début (les plantes du N- sont plus petites), et décroit avec le temps. Nul à t2.
fdata_tx <- data_rgr_sar %>% 
  mutate(log_plant_dry_mass = log(plant_dry_mass)) %>% 
  filter(time =="t2")
mod_rgr <- lme4::lmer(log(plant_dry_mass) ~ fertilization * origin + (1|code_sp) , data = fdata_tx) 
car::Anova(mod_rgr)
summary(mod_rgr)

mod <- lm(log(plant_dry_mass) ~   fertilization * origin, data = fdata )
anova(mod)
summary(mod)


traits_nutrients <- c("N","C")
traits_allocation <- c("RMF","SMF","LMF")
traits_leaf <- c("log_LA", "LDMC","SLA") 
traits_root <- c("SRL", "RTD", "RDMC", "diam","BI")



FTRAITS <- c(traits_nutrients,traits_allocation,traits_leaf,traits_root)

data_mod <- t2_traits %>% 
  # filter(!code_sp %in% c("BUPLBALD","MYOSRAMO","HORNPETR","FILAPYRA")) %>%
  mutate(log_LA = log(LA),
         
         RMF = root_dry_mass/plant_dry_mass, # Root mass fraction
         SMF = stem_dry_mass/plant_dry_mass,
         LMF = leaf_dry_mass/plant_dry_mass,
         
         LAR = (SLA * leaf_dry_mass)/plant_dry_mass ) %>% # leaf area ratio 
  mutate(origin = as.factor(origin) ) %>% 
  mutate(fertilization = as.factor(fertilization))

# Mixed models on raw data ####
TABLE_PVAL <- NULL
for (ftrait in FTRAITS){
  formula0 <- as.formula(paste0(ftrait, " ~ 1 + (1|code_sp)"))
  formula <- as.formula(paste0(ftrait, " ~ fertilization + origin", " + (1|code_sp)"))
  
  mmod0 <- lme4::lmer( formula0 , data = data_mod,na.action = "na.omit")
  
  mmod <- lme4::lmer( formula , data = data_mod) # /!\ choose fdata (includes sp just measured in on treatment)
  
  anov <- car::Anova(mmod)
  
  # mean trait value in N-
  mean_trait_Nm <- data_mod %>% 
    filter(fertilization =="N-") %>% 
    summarize(mean_trait_Nm = mean(get(ftrait),na.rm = T)) %>% 
    pull(mean_trait_Nm)
  
  # How much fertilization N+ increases or decreases trait value
  EM <- emmeans::emmeans(mmod, specs = c("fertilization"),  type = "response",
                         adjust = "tuckey")
  # emmeans (version 1.5.2-1)
  posthoc <- multcomp::cld(EM, #emmeans::as.glht(EM)
                           Letters = "abcdefghi", details = T)
  diff <- posthoc$comparisons$estimate %>% round(digits = 3)
  if(posthoc$comparisons$contrast == "(N-) - (N+)"){
    diff2 = -diff} else{
      diff2=diff
    }
  
  pval <- anov %>%
    as.data.frame() %>% 
    rename(pval = 'Pr(>Chisq)') %>%
    pull(pval) %>% 
    format(scientific = TRUE, digits = 2) %>%
    as.numeric()
  variance <- MuMIn::r.squaredGLMM(mmod) %>% 
    round(digits = 2)
  
  if(pval[1] < 0.05){
    diff3 <- diff2
  }else{
    diff3 <- NA
  }

  table_pval <-  data.frame(Trait = ftrait,
                            fertilization = pval[1],
                            origin = pval[2],
                            var_fixed =   variance[1,1], # R2m # variance explained by the fixed effects
                            var_tot =   variance[1,2],
                            mean_N= mean_trait_Nm,
                            effect_ferti = diff3
                            # Interaction = pval[3]
  )

  
  TABLE_PVAL <- rbind(TABLE_PVAL,table_pval)
}
rownames(TABLE_PVAL) <- NULL
TABLE_PVAL$Organ <- c("Whole plant trait","Whole plant trait",
                      "Allocation","Allocation","Allocation",
                      "Leaf trait","Leaf trait","Leaf trait",
                      "Root trait","Root trait","Root trait","Root trait","Root trait")

table_origin_ferti <- TABLE_PVAL %>% 
  select(Organ,everything()) %>%
  kableExtra::kable( escape = F,
                     col.names = c("Property","Trait", "Fertilization","Origin", 
                                   "Variance explained (fixed)","Variance explained (fixed + random)",
                                   "Mean trait value in N-","Effect of fertilization"
                     )) %>%
  kableExtra::kable_styling("hover", full_width = F)



cat(table_origin_ferti, file = "draft/table_origin_ferti_effects.doc")


# boxplot ####

traits_pop <- read.csv2("output/data/traits_pop.csv") %>% 
  mutate(log_plant_dry_mass = log(plant_dry_mass)) %>% 
  mutate(log_tot_LA = log10(tot_LA)) %>% 
  mutate(log_tot_RL = log10(tot_RL)) %>% 
  mutate(log_RGRslope = log10(RGRslope))

# effet origine
traits_pop %>% 
  ggplot(aes(x = origin,y = plant_dry_mass)) +
  geom_boxplot() +
  geom_point()+
  geom_line(aes(group=code_sp))+
  facet_wrap(~fertilization)

