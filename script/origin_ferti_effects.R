library(tidyverse)
source("script/import_data.R")
# Data ####


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

