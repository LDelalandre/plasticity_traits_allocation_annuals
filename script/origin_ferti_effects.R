library(tidyverse)
source("script/import_data.R")




# Fixed effects ####
# hyp mod linéaires

library(lmtest)

ftrait <-  FTRAITS[16]
mod <- lm(get(ftrait) ~  code_sp + fertilization  + origin + fertilization:code_sp + origin:code_sp, data = t2_traits)
# mod <- lm(log_C ~  code_sp + fertilization  + origin + fertilization:code_sp + origin:code_sp, data = t2_traits %>% mutate(log_C = log10(C)))
ftrait
par(mfrow = c(2,2)) ; plot(mod)

HYP <- NULL
for (ftrait in FTRAITS){
  mod <- lm(get(ftrait) ~  code_sp + fertilization  + origin + fertilization:code_sp + origin:code_sp, data = t2_traits)
  ftrait
  par(mfrow = c(2,2)) ; plot(mod)
  
  sh <- shapiro.test(residuals(mod)) # normality
  bp <- bptest(mod) # homoscedasticity
  dw <- dwtest(mod) # autocorrelation
  hyp <- data.frame(trait = ftrait,
                    shapiro = sh$p.value,
                    breusch = bp$p.value,
                    durbin = dw$p.value)
  
  HYP <- rbind(HYP,hyp)
}
HYP

# Species differ on their plasticity and genetic differentiation ?

# if interaction fertilization * species : different species respond differently to fertilization
interaction_sp_ferti <- function(ftrait){
  # 1) variable selection
  # ne garder que les variables nécessaires
  
  # 2) part of variance explained by adding the variable (compared to what ?)
  
  mod <- lm(get(ftrait) ~  code_sp + fertilization  + origin + fertilization:code_sp + origin:code_sp, data = t2_traits)
  # summary(mod)
  anov <- anova(mod)
  
  # 3) centralize the values in a table
  c(ftrait,anov$`Pr(>F)`)
}

mod_fix <- lapply(as.list(FTRAITS), interaction_sp_ferti) %>% 
  rlist::list.rbind() %>% 
  as.data.frame() %>% 
  mutate(trait = V1,species = as.numeric(V2),fertilization=as.numeric(V3),origin = as.numeric(V4),
         species_fertilization = as.numeric(V5),species_origin = as.numeric (V6)) %>% 
  select(-c(V1,V2,V3,V4,V5,V6,V7) ) 

mod_fix %>% 
  mutate(species_fertilization = if_else(fertilization > 0.05,10, species_fertilization)) %>% 
  mutate(species_origin = if_else(origin > 0.05,10, species_origin)) 


# scientific notation
mod_fix %>% 
  mutate(species = scales::scientific(species,digits = 2)) %>%
  mutate(fertilization = scales::scientific(fertilization,digits = 2)) %>% 
  mutate(origin = scales::scientific(origin,digits = 2)) %>%
  mutate(species_fertilization  = scales::scientific(species_fertilization ,digits = 2)) %>%
  mutate(species_origin = scales::scientific(species_origin,digits = 2))




table_mod_fix <- mod_fix %>% 
  kableExtra::kable( escape = F,
                     col.names = c("Trait","Species","Fertilization","Origin","Species_Fertilization","Species_Origin"
                     )) %>%
  kableExtra::kable_styling("hover", full_width = F)



cat(table_mod_fix, file = "draft/table_difference_plast_sp.doc")



# Mixed models ####


# traits_height_mass <- c(
#   # height
#   "Hveg" ,# "Hrepro",
#   # mass
#   "plant_fresh_mass", "plant_dry_mass",
#   # stem mass
#   "stem_fresh_mass", "stem_dry_mass",
#   # leaf mass
#   "leaf_fresh_mass", "leaf_dry_mass",
#   "leaf_scan_fresh_mass", "leaf_scan_dry_mass",
#   # root mass
#   "pivot_fresh_mass","pivot_dry_mass",
#   # "root_scan_fresh_mass","root_scan_dry_mass",
#   "root_fresh_mass","root_dry_mass")




data_mod <- t2_traits %>% 
  # filter(!code_sp %in% c("BUPLBALD","MYOSRAMO","HORNPETR","FILAPYRA")) %>%
  mutate(log_LA = log(LA),
         log_plant_dry_mass = log10(plant_dry_mass),
         
         RMF = root_dry_mass/plant_dry_mass, # Root mass fraction
         SMF = stem_dry_mass/plant_dry_mass,
         LMF = leaf_dry_mass/plant_dry_mass,
         
         LAR = (SLA * leaf_dry_mass)/plant_dry_mass ) %>% # leaf area ratio 
  mutate(origin = as.factor(origin) ) %>% 
  mutate(fertilization = as.factor(fertilization))

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
    pull(mean_trait_Nm) %>% 
    round(digits = 2)
  
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
    diff3 <- diff2 %>% 
      round(digits = 2)
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
TABLE_PVAL$Organ <- c("Whole plant trait","Whole plant trait","Whole plant trait","Whole plant trait",
                      "Allocation","Allocation","Allocation",
                      "Leaf trait","Leaf trait","Leaf trait",
                      "Root trait","Root trait","Root trait","Root trait","Root trait")
TABLE_PVAL$unit <- c("cm","g","%","%","%","%","%",
                     "cm2", "mg/g","mm2/mg","m/g","g/cm3","mg/g","mm","")

table_origin_ferti <- TABLE_PVAL %>%
  mutate(fertilization = scales::scientific(fertilization,digits = 2)) %>% 
  mutate(origin = scales::scientific(origin,digits = 2)) %>% 
  
  select(Organ,Trait,unit,origin,fertilization,everything()) %>%
  kableExtra::kable( escape = F,
                     col.names = c("Property","Trait","Unit","pval (Origin)","pval (Fertilization)", 
                                   "Variance explained (fixed)","Variance explained (fixed + random)",
                                   "Mean trait value in N-","Effect of fertilization"
                     )) %>%
  kableExtra::kable_styling("hover", full_width = F)



cat(table_origin_ferti, file = "draft/table_origin_ferti_effects.doc")


# boxplot origin ####

 


# effet origine
traits_pop %>% 
  filter(!code_sp %in% sp_nat) %>% 
  ggplot(aes(x = origin,y = SMF)) +
  geom_boxplot() +
  geom_point()+
  geom_line(aes(group=code_sp))+
  ggrepel::geom_text_repel(aes(label = code_sp))+
  facet_wrap(~fertilization) 

traits_pop %>% 
  filter(!code_sp %in% sp_nat) %>% 
  ggplot(aes(x = origin,y = log_Hveg)) +
  geom_boxplot() +
  geom_point()+
  geom_line(aes(group=code_sp))+
  ggrepel::geom_text_repel(aes(label = code_sp))+
  facet_wrap(~fertilization)

