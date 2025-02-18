library(tidyverse)
library(multcomp)


# Functions---------------------------------------------------------------------


computeNewTraits <- function(t2_traits) {
  new_traits <- t2_traits %>% 
    mutate(log_LA = log(LA),
           log_plant_dry_mass = log10(plant_dry_mass),
           
           RMF = root_dry_mass/plant_dry_mass, # Root mass fraction
           SMF = stem_dry_mass/plant_dry_mass,
           LMF = leaf_dry_mass/plant_dry_mass,
           
           LAR = (SLA * leaf_dry_mass)/plant_dry_mass ) %>% # leaf area ratio 
    mutate(origin = as.factor(origin) ) %>% 
    mutate(fertilization = as.factor(fertilization))
  return(new_traits)
}


makeLinearRegression <- function(ftrait,data_mod) {
  # formula0 <- as.formula(paste0(ftrait, " ~ 1 + (1|family/code_sp)"))
  formula <- as.formula(paste0(ftrait, " ~ fertilization + origin", " + (1|family/code_sp)"))
  mod_effect_ferti_origin <- glmmTMB::glmmTMB(formula,  data = data_mod ,family = gaussian)
  return(mod_effect_ferti_origin)
}

estimateFertiEffect <- function(mod) {
  # How much does fertilizing increase or decrease trait value
  estimates <- emmeans::emmeans(mod, specs = c("fertilization"),  type = "response",
                         adjust = "tuckey") # emmeans version 1.5.2-1
  return(estimates)
}


computeMeanTraitValue <- function(estimated_ferti_effect,treatment) {
  #treatment is either "N-" or "N+"
  mean_trait_value <-    estimated_ferti_effect %>% 
    as.data.frame () %>% 
    filter(fertilization == treatment) %>% 
    pull(emmean) %>% 
    round(digits = 2)
  return(mean_trait_value)
}


extractPval <- function(anova_output) {
  pval <- 
    anova_output %>%
    as.data.frame() %>% 
    rename(pval = 'Pr(>Chisq)') %>%
    pull(pval) %>% 
    format(scientific = TRUE, digits = 2) %>%
    as.numeric()
  return(pval)
}


buildRowOfTable <- function(trait, pvalue, variance_exp, mean_Nm, mean_Np) {
  one_row <-   data.frame(Trait = ftrait_name,
                          fertilization = pval[1],
                          origin = pval[2],
                          # interaction = pval[3],
                          var_fixed =   variance_explained[1,1]*100, # R2m # variance explained by the fixed effects
                          var_tot =   variance_explained[1,2]*100,
                          mean_Nm= mean_trait_Nm,
                          mean_Np = mean_trait_Np
  )
  return(one_row)
}


# Import data-------------------------------------------------------------------

source("script/01_import-data.R") # load t2_traits and FTRAITS
data_mod <- computeNewTraits(t2_traits) %>% 
  merge(sp_fam)



# Analyse data------------------------------------------------------------------

TABLE_PVAL <- NULL
for (ftrait in FTRAITS){
  if (ftrait == "log_Hveg"){
    ftrait2 <- "Hveg"
  }else if (ftrait == "log_plant_dry_mass"){
    ftrait2 <- "plant_dry_mass"
  }else if(ftrait == "log_LA"){
    ftrait2 <- "LA"
  }else{ftrait2 <- ftrait}
  
  mod_effect_ferti_origin <- makeLinearRegression(ftrait,data_mod)
  
  anov <- car::Anova(mod_effect_ferti_origin)
  
  estimated_ferti_effect <- estimateFertiEffect(mod_effect_ferti_origin)
  
  posthoc <- summary(glht(mod_effect_ferti_origin, 
                          linfct = mcp(fertilization = "Tukey")), 
                     test = adjusted("fdr"))
  
  mean_trait_Np <- computeMeanTraitValue(estimated_ferti_effect, treatment = "N+")
  mean_trait_Nm <- computeMeanTraitValue(estimated_ferti_effect, treatment = "N-")
  
  # Log values for some traits
  if (ftrait2 %in% c("plant_dry_mass","Hveg","LA")){
    mean_trait_Nm <- 10^(mean_trait_Nm) %>% round(digits = 2)
    mean_trait_Np <- 10^(mean_trait_Np) %>% round(digits = 2)
  }
  
  
  pval <- extractPval(anova_output = anov)
  
  variance_explained <- MuMIn::r.squaredGLMM(mod_effect_ferti_origin) %>% 
    round(digits = 2)
  
  # Get extensive trait name
  ftrait_name <- trait_name %>% 
    filter(trait == ftrait2) %>% 
    pull(name)
  
  table_pval <-  buildRowOfTable(trait = ftrait_name, 
                                 pvalue = pval, 
                                 variance_exp = variance_explained,
                                 mean_Nm = mean_Nm,
                                 mean_Np = mean_Np)
  TABLE_PVAL <- rbind(TABLE_PVAL,table_pval)
}


rownames(TABLE_PVAL) <- NULL
TABLE_PVAL$Perspective <- c("Ecological strategies","Whole-plant integration","Whole-plant integration","Whole-plant integration",
                      "Whole-plant integration","Whole-plant integration",
                      "Ecological strategies","Ecological strategies","Ecological strategies",
                      "Below-ground traits","Below-ground traits","Below-ground traits","Below-ground traits","Below-ground traits")
TABLE_PVAL$unit <- c("cm","g","mg/g","g/g","g/g","g/g",
                     "cm2", "mg/g","mm2/mg","m/g","g/cm3","mg/g","mm","cm-1")

TABLE_PVAL$Perspective <- factor(TABLE_PVAL$Perspective,levels = c("Ecological strategies","Whole-plant integration","Below-ground traits"))
TABLE_PVAL <- TABLE_PVAL %>% 
  arrange(Scheme = factor(Perspective, levels = c("Ecological strategies","Whole-plant integration","Below-ground traits"))) %>% 
  arrange(Trait = factor (Trait , levels = c(
    "Vegetative height","Leaf Area","Specific Leaf Area","Leaf Dry Matter content",
    "Mean root diameter","Specific Root Length","Root Tissue Density","Root Dry Matter Content","Branching Intensity","Plant dry mass",
    "Plant Nitrogen content per mass","Leaf Mass Fraction",
    "Stem Mass Fraction","Root Mass Fraction")
  )) %>% 
  dplyr::select(Perspective, everything())

write.csv2(TABLE_PVAL,"output/data/table_origin_ferti_effect")





table_origin_ferti <- TABLE_PVAL %>%
  # mutate(origin = scales::scientific(origin,digits = 2)) %>% 
  
  mutate(fertilization = case_when(fertilization < 0.0001 ~ 0.0001,
                                   fertilization < 0.001 ~ 0.001,
                                   # fertilization < 0.0036 ~ 0.0036,
                                   # fertilization < 0.01 ~ 0.01,
                                   # fertilization < 0.05 ~ 0.05,
                                   TRUE ~ fertilization)
         ) %>% 
  mutate(fertilization = as.character(fertilization)) %>% 
  mutate(fertilization = case_when(fertilization == "1e-04" ~ "< 0.0001",
                                   fertilization == "0.001" ~ "< 0.001",
                                   # fertilization == "0.0036" ~ "< 0.0036",
                                   # fertilization == "0.01" ~ "< 0.01",
                                   # fertilization == "0.05" ~ "< 0.05",
                                   TRUE ~ fertilization)
  ) %>% 
  
  mutate(origin  = case_when(origin  < 0.0001 ~ 0.0001,
                             origin  < 0.001 ~ 0.001,
                             # origin  < 0.0036 ~ 0.0036,
                             # origin  < 0.01 ~ 0.01,
                             # origin  < 0.05 ~ 0.05,
                                   TRUE ~ origin )
  ) %>% 
  mutate(origin  = as.character(origin )) %>% 
  mutate(origin  = case_when(origin  == "1e-04" ~ "< 0.0001",
                             origin  == "0.001" ~ "< 0.001",
                             # origin  == "0.036" ~ "< 0.036",
                             # origin  == "0.01" ~ "< 0.01",
                             # origin  == "0.05" ~ "< 0.05",
                                   TRUE ~ origin )
  ) %>% 
  
  dplyr::select(Perspective,Trait,unit,mean_Nm,mean_Np,origin,fertilization,everything()) %>%
  
  mutate(mean_Nm = round(mean_Nm,digits = 2)) %>% 
  mutate(mean_Np = round(mean_Np,digits = 2)) %>% 
  
  kableExtra::kable( escape = F,
                     col.names = c("Perspective","Trait","Unit","Mean value in F-","Mean value in F+",
                                   "pval (Origin)","pval (Fertilization)", 
                                   "Percentage of variance explained (fixed)","Percentage of variance explained (fixed + random)"
                     )) %>%
  kableExtra::kable_styling("hover", full_width = F)



cat(table_origin_ferti, file = "draft/03_tests-effects-origin-fert_table_origin_ferti_effects.doc")


