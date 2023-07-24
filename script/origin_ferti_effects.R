library(tidyverse)
source("script/import_data.R")





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

trait_name <- data.frame(trait = c("plant_dry_mass","Hveg",
                                    "LA","SLA","LDMC",
                                    "N","LMF",
                                    "SRL", "RTD", "RDMC", 
                                    "SMF","RMF",
                                    "diam","BI"),
                          name = c("Plant dry mass", "Vegetative height", 
                                    "Leaf Area","Specific Leaf Area","Leaf Dry Matter content",
                                    "Plant Nitrogen content per mass", "Leaf Mass Fraction",
                                    "Specific Root Length", "Root Tissue Density","Root Dry Matter Content",
                                    "Stem Mass Fraction","Root Mass Fraction",
                                    "Mean root diameter","Branching Intensity"))


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
  if (ftrait == "log_Hveg"){
    ftrait2 <- "Hveg"
  }else if (ftrait == "log_plant_dry_mass"){
    ftrait2 <- "plant_dry_mass"
  }else if(ftrait == "log_LA"){
    ftrait2 <- "LA"
  }else{ftrait2 <- ftrait}
  formula0 <- as.formula(paste0(ftrait, " ~ 1 + (1|family/code_sp)"))
  formula <- as.formula(paste0(ftrait, " ~ fertilization + origin", " + (1|family/code_sp)"))
  
  mmod0 <- lme4::lmer( formula0 , data = data_mod,na.action = "na.omit")
  
  mmod <- lme4::lmer( formula , data = data_mod) # /!\ choose fdata (includes sp just measured in on treatment)
  
  anov <- car::Anova(mmod)
  
  # mean trait value in N-
  # mean_trait_Nm <- data_mod %>% 
  #   filter(fertilization =="N-") %>% 
  #   summarize(mean_trait_Nm = mean(get(ftrait2),na.rm = T)) %>% 
  #   pull(mean_trait_Nm) %>% 
  #   round(digits = 2)
  # mean_trait_Np <- data_mod %>% 
  #   filter(fertilization =="N+") %>% 
  #   summarize(mean_trait_Np = mean(get(ftrait2),na.rm = T)) %>% 
  #   pull(mean_trait_Np) %>% 
  #   round(digits = 2)
  
  # How much fertilization N+ increases or decreases trait value
  EM <- emmeans::emmeans(mmod, specs = c("fertilization"),  type = "response",
                         adjust = "tuckey")
  # emmeans (version 1.5.2-1)
  posthoc <- multcomp::cld(EM, #emmeans::as.glht(EM),
                           Letters = "abcdefghi", details = T)
  
  mean_trait_Nm <- posthoc$emmeans %>% filter(fertilization == "N-") %>% pull(emmean) %>% round(digits = 2)
  mean_trait_Np <- posthoc$emmeans %>% filter(fertilization == "N+") %>% pull(emmean) %>% round(digits = 2)
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

  
  TABLE_PVAL <- rbind(TABLE_PVAL,table_pval)
}
rownames(TABLE_PVAL) <- NULL
TABLE_PVAL$Scheme <- c("Above-ground traits","Whole-plant traits","Whole-plant traits","Whole-plant traits",
                      "Whole-plant traits","Whole-plant traits",
                      "Above-ground traits","Above-ground traits","Above-ground traits",
                      "Below-ground traits","Below-ground traits","Below-ground traits","Below-ground traits","Below-ground traits")
TABLE_PVAL$unit <- c("cm","g","mg/g","g/g","g/g","g/g",
                     "cm2", "mg/g","mm2/mg","m/g","g/cm3","mg/g","mm","cm-1")

TABLE_PVAL$Scheme <- factor(TABLE_PVAL$Scheme,levels = c("Above-ground traits","Below-ground traits","Whole-plant traits"))
TABLE_PVAL <- TABLE_PVAL %>% 
  arrange(Scheme = factor(Scheme, levels = c("Above-ground traits","Below-ground traits","Whole-plant traits"))) %>% 
  arrange(Trait = factor (Trait , levels = c(
    "Vegetative height","Leaf Area","Specific Leaf Area","Leaf Dry Matter content",
    "Mean root diameter","Specific Root Length","Root Tissue Density","Root Dry Matter Content","Branching Intensity","Plant dry mass",
    "Plant Nitrogen content per mass","Leaf Mass Fraction",
    "Stem Mass Fraction","Root Mass Fraction")
  ))

write.csv2(TABLE_PVAL,"output/data/table_origin_ferti_effect")


table_origin_ferti <- TABLE_PVAL %>%
  # mutate(fertilization = scales::scientific(fertilization,digits = 2)) %>% 
  # mutate(origin = scales::scientific(origin,digits = 2)) %>% 
  mutate(fertilization = case_when(fertilization < 0.0001 ~ 0.0001,
                                   fertilization < 0.001 ~ 0.001,
                                   fertilization < 0.01 ~ 0.01,
                                   fertilization < 0.05 ~ 0.05,
                                   TRUE ~ fertilization)
         ) %>% 
  mutate(fertilization = as.character(fertilization)) %>% 
  mutate(fertilization = case_when(fertilization == "1e-04" ~ "< 0.0001",
                                   fertilization == "0.001" ~ "< 0.001",
                                   fertilization == "0.01" ~ "< 0.01",
                                   fertilization == "0.05" ~ "< 0.05",
                                   TRUE ~ fertilization)
  ) %>% 
  
  mutate(origin  = case_when(origin  < 0.0001 ~ 0.0001,
                             origin  < 0.001 ~ 0.001,
                             origin  < 0.01 ~ 0.01,
                             origin  < 0.05 ~ 0.05,
                                   TRUE ~ origin )
  ) %>% 
  mutate(origin  = as.character(origin )) %>% 
  mutate(origin  = case_when(origin  == "1e-04" ~ "< 0.0001",
                             origin  == "0.001" ~ "< 0.001",
                             origin  == "0.01" ~ "< 0.01",
                             origin  == "0.05" ~ "< 0.05",
                                   TRUE ~ origin )
  ) %>% 
  
  select(Scheme,Trait,unit,origin,fertilization,everything()) %>%
  kableExtra::kable( escape = F,
                     col.names = c("Property","Trait","Unit","pval (Origin)","pval (Fertilization)", 
                                   "Variance explained (fixed)","Variance explained (fixed + random)",
                                   "Mean value in F-","Mean value in F+"
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

