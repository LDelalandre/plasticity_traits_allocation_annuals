library(tidyverse)
source("script/import_data.R")

# data ####
# Traits on which we found significant plasticity
traits_plast <- c(
  "log_plant_dry_mass",
  "log_LA", "LDMC","SLA",
  "RDMC",
  "N",
  "RMF","LMF","SMF" )


# Interaction species-plasticity ####

# Fixed effects
# hyp mod linéaires

library(lmtest)

ftrait <-  "RMF"
mod <- lm(get(ftrait) ~  code_sp + fertilization  * code_sp , data = t2_traits)
mod <- lm(log_ftrait ~  code_sp + fertilization  + origin + fertilization:code_sp + origin:code_sp, 
          data = t2_traits %>% mutate(log_ftrait = log10(RMF)))
ftrait
par(mfrow = c(2,2)) ; plot(mod)
par(mfrow = c(1,1)) 
# hist(log(t2_traits$C))
shapiro.test(residuals(mod)) # normality
bptest(mod) # homoscedasticity
dwtest(mod) # autocorrelation

HYP <- NULL
for (ftrait in traits_plast){
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
  
  mod <- lm(get(ftrait) ~  fertilization*pop, data = t2_traits)
  # summary(mod)
  anov <- anova(mod)
  
  # temporaire : post-hoc 
  # pour voir qui sont les espèces qui diffèrent entre les deux
  
  # contr <- emmeans::emmeans(mod,pairwise~fertilization*code_sp)
  # posthoc  <- contr$contrasts %>% 
  #   as.data.frame() %>% 
  #   mutate(contrast = gsub(x = contrast,pattern = ")", replacement = "") ) %>% 
  #   # mutate(contrast = gsub(x = contrast,pattern = "(", replacement = "") )
  #   separate(contrast, into = c("F1", "sp1", "minus","F2","sp2"),sep = " ") 
  
  # posthoc %>% 
  #   filter(sp1 == sp2) %>% 
  #   View
  
  
  # 3) centralize the values in a table
  c(ftrait,anov$`Pr(>F)`)
}

mod_fix <- lapply(as.list(traits_plast), interaction_sp_ferti) %>% 
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



cat(table_mod_fix, file = "draft/table_difference_plast_sp.doc")


# Compute RDPI on all traits ####
traits_plast <- c(
  "plant_dry_mass",
  "LA", "LDMC","SLA",
  "RDMC",
  "N",
  "RMF","LMF","SMF" )

# traits for which plasticity depended on species identity
traits_plast_interaction_sp <- c(
  "LDMC","SLA",
  "plant_dry_mass",
  "N",
  "RMF","SMF" )

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
  left_join(info_sp %>% dplyr::select(code_sp,N_ellenberg)) %>% 
  dplyr::filter(!(pop=="ALYSALYS_Nat"))

trait_title <- data.frame(trait = c("SLA","LDMC",
                                    "N","LMF",
                                    "SMF","RMF",
                                    "plant_dry_mass"),
                          title = c("Specific Leaf Area","Leaf Dry Matter content",
                                    "Plant nitrogen content", "Leaf Mass Fraction",
                                    "Stem Mass Fraction","Root Mass Fraction",
                                    "Plant dry mass"))
plot_rdpi_trait <- function(x_axis){
  # x_axis <- "log_plant_dry_mass_moy"
  # x_axis <- "SLA_moy"
  
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
      # geom_smooth(method='lm') +
      theme_classic() +
      {if (x_axis == "log_plant_dry_mass_moy") xlab("log(Plant dry mass)")}+
      {if (x_axis == "SLA_moy") xlab("SLA")} +
      ggtitle(trait_title %>% filter(trait == ftrait) %>% pull(title)) +
      ylab("RDPI") +
      geom_hline(yintercept = 0,linetype='dashed') +
      # geom_smooth(method = "lm")
      {if (!(pval=="n.s."))       geom_abline(slope = slope, intercept = intercept) } +
      annotation_custom(lab) +
      theme_bw()
    plot
    PLOT[[i]] <- plot
  }
  
  plots2 <- ggpubr::ggarrange(plotlist=PLOT, ncol = 1,nrow =6)
  plots2
}

rdpi_ellenberg <- plot_rdpi_trait("N_ellenberg")
ggsave("output/plot/plast_ellenberg.png",rdpi_ellenberg,width = 6,height = 13)

rdpi_sla <- plot_rdpi_trait("SLA_moy")
rdpi_mass <- plot_rdpi_trait("log_plant_dry_mass_moy")
rpdi_sla_mass <- ggpubr::ggarrange(rdpi_mass,rdpi_sla)

ggsave(paste0("draft/RDPI_sla_mass.png"), rpdi_sla_mass,width = 6, height = 13)


# Distribution RDPI ####
PLAST2_with_alys_nat <- PLAST_pop %>% 
  separate(pop,into=c("code_sp","origin"),remove = F) %>% 
  group_by(pop,code_sp) %>% 
  dplyr::select(all_of(traits_plast_interaction_sp)) %>% 
  merge(trait_moy) %>% 
  left_join(info_sp %>% dplyr::select(code_sp,N_ellenberg)) 

plot_plast_dry_mass <- PLAST2_with_alys_nat %>% 
  select(pop,code_sp,plant_dry_mass) %>% 
  ggplot(aes(x = plant_dry_mass)) +
  geom_histogram(binwidth = 0.1,fill="black", col="grey") +
  theme_bw() +
  xlab("Plasticity index of plant dry mass") +
  ggtitle("Plasticity in plant dry mass")

ggsave("draft/plast_dry_mass.png",plot_plast_dry_mass,width = 3, height = 3)
