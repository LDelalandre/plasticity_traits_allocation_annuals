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

traits <- c(
  # leaf traits
  "LA", "LDMC","SLA",
  # root traits
  "SRL", "RTD", "RDMC", "diam","BI",
  # nutrient
  "C","N")


FTRAITS <- c(
  # leaf traits
  "log_LA", "LDMC","SLA",
  # root traits
  "SRL", "RTD", "RDMC", "diam","BI",
  # nutrient
  "C","N",
  "RMF","LAR","root_shoot")

# Mixed models on raw data ####
TABLE_PVAL <- NULL
for (ftrait in FTRAITS){
  data_mod <- t2_traits %>% 
    # filter(!code_sp %in% c("BUPLBALD","MYOSRAMO","HORNPETR","FILAPYRA")) %>%
    mutate(log_LA = log(LA),
           RMF = root_dry_mass/plant_dry_mass, # Root mass fraction
           root_shoot = root_dry_mass/(stem_dry_mass+leaf_dry_mass),
           LAR = (SLA * leaf_dry_mass)/plant_dry_mass ) %>% # leaf area ratio 
    mutate(origin = as.factor(origin) ) %>% 
    mutate(fertilization = as.factor(fertilization))
  
  formula0 <- as.formula(paste0(ftrait, " ~ 1 + (1|code_sp)"))
  formula <- as.formula(paste0(ftrait, " ~ fertilization * origin", " + (1|code_sp)"))
  
  mmod0 <- lme4::lmer( formula0 , data = data_mod)
  
  mmod <- lme4::lmer( formula , data = data_mod) # /!\ choose fdata (includes sp just measured in on treatment)
  
  anov <- car::Anova(mmod)
  pval <- anov %>%
    as.data.frame() %>% 
    rename(pval = 'Pr(>Chisq)') %>%
    pull(pval) %>% 
    format(scientific = TRUE, digits = 2) %>%
    as.numeric()
  table_pval <-  data.frame(Trait = ftrait,
                            fertilization = pval[1],
                            origin = pval[2],
                            Interaction = pval[3]
  )
  TABLE_PVAL <- rbind(TABLE_PVAL,table_pval)
  
  # # if( table_pval$Interaction < 0.05 ){
  # posthoc <- multcomp::cld(emmeans::emmeans(mmod, specs = c("fertilization","origin"),  type = "response",
  #                                           adjust = "tuckey"),
  #                          Letters = "abcdefghi", details = T)
  # 
  # # contrasts = différences entre les deux traitements pour annuelles et pérennes
  # contrasts <- posthoc$comparisons %>% 
  #   filter(contrast %in% c("(N- Nat) - (N+ Nat)","(N- Fer) - (N+ Fer)",
  #                          "(N+ Nat) - (N- Nat)","(N+ Fer) - (N- Fer)")) %>%
  #   mutate(estimate = case_when(contrast %in% c("(N- Nat) - (N+ Nat)","(N- Fer) - (N+ Fer)") ~ -estimate,
  #                               TRUE ~ estimate)) %>% 
  #   mutate(contrast = case_when(contrast == "(N- Nat) - (N+ Nat)" ~"(N+ Nat) - (N- Nat)",
  #                               contrast == "(N- Fer) - (N+ Fer)" ~ "(N+ Fer) - (N- Fer)",
  #                               TRUE ~ contrast)) %>% 
  #   mutate(contrast = if_else(contrast == "(N+ Fer) - (N- Fer)", "Fer","Nat")) %>% 
  #   select(contrast,estimate,p.value) %>% 
  #   mutate(estimate = round(estimate,digits =1)) %>% 
  #   mutate(p.value = format(p.value,scientific = TRUE, digits = 2)) %>% 
  #   mutate(est_pval = paste0(estimate, " (",p.value,")")) %>%
  #   select(contrast,est_pval) %>% 
  #   spread(key = contrast,value = est_pval)
  # 
  # table_pval2 <- cbind(table_pval,contrasts)
  # TABLE_PVAL <- rbind(TABLE_PVAL,table_pval2)
}
TABLE_PVAL  %>% arrange(fertilization)





