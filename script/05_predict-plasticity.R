library(tidyverse)
source("script/01_import-data.R")

# Load data --------------------------------------------------------------------
# Traits on which we found significant plasticity
traits_plast <- read.table("output/data/traits-ferti-effect.txt") %>% 
  pull(x)

traits_plast_log <- read.table("output/data/traits-ferti-effect.txt") %>% 
  mutate(x = if_else(x=="plant_dry_mass","log_plant_dry_mass",x)) %>%
  mutate(x = if_else(x=="Hveg","log_Hveg",x)) %>%
  pull(x)

## Saatkamp 2023
species <- read.csv2("data/species_info.csv") %>% 
  dplyr::select(scientificName,code_sp)

saat <- read.csv2("data/Appendix S4 data table v.14.10.2022.FINAL.csv") %>% 
  rename(scientificName = NOM_VALIDE_v12) %>% 
  merge(species,by="scientificName")
saat_N <- saat %>% 
  dplyr::select(code_sp,e.mN,e.sdN, j.mN,j.sdN, l.mN,l.sdN,p.mN,p.sdN)
saat_L <- saat %>% 
  dplyr::select(code_sp,e.mL,e.sdL, j.mL,j.sdL, l.mL,l.sdL,p.mL,p.sdL)





# Interaction species-plasticity------------------------------------------------

library(lmtest)

testAssumptionLinearModel <- function(ftrait) {
  mod <- lm(get(ftrait) ~  code_sp + fertilization  + origin + fertilization:code_sp + origin:code_sp, data = t2_traits)
  
  plot <- F
  if (plot == T) {par(mfrow = c(2,2)) ; plot(mod)}

  sh <- shapiro.test(residuals(mod)) # normality
  bp <- bptest(mod) # homoscedasticity
  # dw <- dwtest(mod) # autocorrelation
  hyp <- data.frame(trait = ftrait,
                    shapiro = sh$p.value,
                    breusch = bp$p.value)
                    # durbin = dw$p.value)
  return(hyp)
}

testAssumptionsAllTraits <- function(traits){
  HYP <- NULL
  for (ftrait in traits_plast_log){
    hyp <- testAssumptionLinearModel(ftrait)
    HYP <- rbind(HYP,hyp)
  }
  return(HYP)
}

testAssumptionsAllTraits(traits_plast_log)

# Species differ on their plasticity and genetic differentiation ?

testSpeciesWithInteraction <- function(mod) {
  # temporaire : post-hoc 
  # pour voir qui sont les espèces qui diffèrent entre les deux
  
  contr <- emmeans::emmeans(mod,pairwise~fertilization*code_sp)
  posthoc  <- contr$contrasts %>%
    as.data.frame() %>%
    mutate(contrast = gsub(x = contrast,pattern = ")", replacement = "") ) %>%
    separate(contrast, into = c("F1", "sp1", "minus","F2","sp2"),sep = " ")

  posthoc %>%
    filter(sp1 == sp2) %>%
    View
}

# if interaction fertilization * species : different species respond differently to fertilization
testInteractionSpFerti <- function(ftrait){
  mod <- lm(get(ftrait) ~  fertilization*pop, data = t2_traits)
  anov <- anova(mod)
  
  whichsp <- F
  if (whichsp == T) {which_species <- testSpeciesWithInteraction(mod)}
  
  c(ftrait,anov$`Pr(>F)`) # 3) centralize the values in a table
}

mod_fix <- lapply(as.list(traits_plast_log), testInteractionSpFerti) %>% 
  rlist::list.rbind() %>% 
  as.data.frame() %>% 
  mutate(trait = V1,species = as.numeric(V2),fertilization=as.numeric(V3),interaction = as.numeric(V4)) %>% 
  dplyr::select(-c(V1,V2,V3,V4,V5) ) 

table_mod_fix <- mod_fix %>% 
  # scientific notation
  mutate(species = scales::scientific(species,digits = 2)) %>%
  mutate(fertilization = scales::scientific(fertilization,digits = 2)) %>% 
  mutate(interaction = scales::scientific(interaction,digits = 2)) %>% 
  kableExtra::kable( escape = F,
                     col.names = c("Trait","Species","Fertilization","Interaction"
                     )) %>%
  kableExtra::kable_styling("hover", full_width = F)



cat(table_mod_fix, file = "draft/05_predict-plasticity_tableS3-interaction-sp-ferti.doc")






# Compute RDPI on all traits----------------------------------------------------


# traits for which plasticity depended on species identity
traits_plast_interaction_sp <- mod_fix %>% 
  filter(interaction < 0.05) %>% 
  mutate(trait = if_else(trait=="log_plant_dry_mass","plant_dry_mass",trait)) %>%
  mutate(trait = if_else(trait=="log_Hveg","Hveg",trait)) %>% 
  pull(trait)

# average trait values per "population
rename_moy <- function(trait){
  paste0(trait,"_moy")
}

trait_moy <- t2_traits %>% 
  group_by(code_sp,origin,pop) %>% 
  dplyr::select(all_of(traits_plast_interaction_sp),"plant_dry_mass") %>% 
  summarize_at(c(traits_plast_interaction_sp,"plant_dry_mass"),mean, na.rm=T) %>% 
  rename_at(c(traits_plast_interaction_sp,"plant_dry_mass"),rename_moy) %>% 
  group_by(pop) %>% 
  dplyr::select(-c(code_sp,origin)) %>% 
  mutate(log_plant_dry_mass_moy = log10(plant_dry_mass_moy))

# By averaging trait values per population
compute_plast_pop <- function(ftrait){
  plast <- traits_pop %>% 
    dplyr::select(pop,fertilization,all_of(ftrait)) %>% 
    spread(key = fertilization,value = ftrait )
  # mutate(plast = (`N+` - `N-`)/`N+` )
  # merge(trait_moy)
  plast[,ftrait] <-  (plast$`N+` - plast$`N-`) / (plast$`N+`) 
  
  plast %>% 
    ungroup() %>% 
    dplyr::select(pop,ftrait)
}

PLAST_pop <- compute_plast_pop(traits_plast[1])
for( i in c(2:length(traits_plast)) ){
  plast <- compute_plast_pop(traits_plast[i])
  PLAST_pop <- merge(PLAST_pop,plast)
}


# Relationship plasticity and strategy ####
PLAST2 <- PLAST_pop %>% 
  separate(pop,into=c("code_sp","origin"),remove = F) %>% 
  group_by(pop,code_sp) %>% 
  dplyr::select(all_of(traits_plast_interaction_sp)) %>% 
  merge(trait_moy) %>% 
  left_join(saat_N) 
  # left_join(species_info %>% dplyr::select(code_sp,N_ellenberg)) %>% 
  # left_join(species_info %>% dplyr::select(code_sp)) %>% 
  # dplyr::filter(!(pop=="ALYSALYS_Nat")) %>% 
  # merge(seed_mass) %>% 
  # mutate(log_SM = log(SeedMass))

trait_title <- data.frame(trait = c("SLA","LDMC",
                                    "N","LMF",
                                    "SMF","RMF",
                                    "plant_dry_mass"),
                          title = c("Specific Leaf Area","Leaf Dry Matter content",
                                    "Plant nitrogen content per mass", "Leaf Mass Fraction",
                                    "Stem Mass Fraction","Root Mass Fraction",
                                    "Plant dry mass"))


plot_rdpi_trait <- function(x_axis){
  # x_axis <- "log_plant_dry_mass_moy"
  # x_axis <- "SLA_moy"
  # x_axis <- "j.mN"

  PLOT <- NULL
  i <- 0
  for (ftrait in traits_plast_interaction_sp){
    i <- i+1
    
    ## model
    form <- as.formula(paste(ftrait, "~", x_axis))
    mod <- lm(form, data = PLAST2)
    anov <- anova(mod)
    summ <- summary(mod)
    
    intercept <- mod$coefficients[1]
    slope <- mod$coefficients[2]
    pval <- anov$`Pr(>F)`[1] 
    r2 <- summ$r.squared%>% round(digits = 2)
    
    if(pval > 0.05){pval <- "n.s."}else{pval <- pval %>% scales::scientific(digits = 2) %>% paste0("p = ",.)}
    
    ## plot
    
    if (!(pval=="n.s.")){
      lab <- grid::textGrob(label = paste0(pval, "\n",
                                           "R² =", r2),
                            x = unit(0.05, "npc"), 
                            y = unit(0.8, "npc"), just = "left",
                            gp = grid::gpar(size = 14, fontface = "bold", fill = "white", alpha = 1))
    }else{
      lab <- grid::textGrob(label = paste0(pval),
                            x = unit(0.05, "npc"), 
                            y = unit(0.8, "npc"), just = "left",
                            gp = grid::gpar(size = 14, fontface = "bold", fill = "black", alpha = 1,col = "black"))
    }
    

    
    plot <- PLAST2 %>% 
      ggplot(aes_string(x = x_axis, y= ftrait)) +
      geom_point()  +
      theme_classic() +
      {if (x_axis == "log_plant_dry_mass_moy") xlab("log(Plant dry mass)")}+
      {if (x_axis == "SLA_moy") xlab("SLA")} +
      {if(x_axis == "j.mN") xlab("Nutrient indicator value") } +
      ggtitle(trait_title %>% filter(trait == ftrait) %>% pull(title)) +
      ylab("RDPI") +
      geom_hline(yintercept = 0,linetype='dashed') +
      {if (!(pval=="n.s."))       geom_abline(slope = slope, intercept = intercept) } +
      annotation_custom(lab,ymin = -0.5) +
      theme_bw()
    plot
    PLOT[[i]] <- plot
  }
  
  plots2 <- ggpubr::ggarrange(plotlist=PLOT, ncol = 1,nrow =6)
  
  if(x_axis == "j.mN"){
    ggpubr::annotate_figure(plots2, top = "Nitrogen requirement")
  }else if (x_axis == "SLA_moy"){
    ggpubr::annotate_figure(plots2, top = "SLA")
  }else if (x_axis == "log_plant_dry_mass_moy"){
    ggpubr::annotate_figure(plots2, top = "Plant mass")
  }
  plots2
}

# plast f mean nutrient ####




# plast f SLA ####
rdpi_sla <- plot_rdpi_trait("SLA_moy")
rdpi_mass <- plot_rdpi_trait("log_plant_dry_mass_moy")
rpdi_sla_mass <- ggpubr::ggarrange(rdpi_mass,rdpi_sla)

# ggsave(paste0("draft/Figure 2.png"), rpdi_sla_mass,width = 6, height = 13)

rdpi_saat_j <- plot_rdpi_trait("j.mN")
rpdi_julve_SLA_mass <- ggpubr::ggarrange(rdpi_saat_j,rdpi_sla,rdpi_mass,ncol = 3)
ggsave(paste0("draft/05_predict-plasticity_Fig2-rdpi-plast.svg"), rpdi_julve_SLA_mass,width = 10, height = 13)







