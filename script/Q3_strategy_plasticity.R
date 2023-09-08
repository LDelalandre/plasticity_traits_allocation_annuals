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

# traits for which plasticity depended on species identity
traits_plast_interaction_sp <- c(
  "N",
  "RMF","SMF" ,
  "LDMC","SLA")


# average trait values per "population
rename_moy <- function(trait){
  paste0(trait,"_moy")
}

trait_moy <- t2_traits %>% 
  group_by(code_sp,origin,pop) %>% 
  select(all_of(traits_plast_interaction_sp),"plant_dry_mass") %>% 
  summarize_at(c(traits_plast_interaction_sp,"plant_dry_mass"),mean, na.rm=T) %>% 
  rename_at(c(traits_plast_interaction_sp,"plant_dry_mass"),rename_moy) %>% 
  group_by(pop) %>% 
  select(-c(code_sp,origin)) %>% 
  mutate(log_plant_dry_mass_moy = log10(plant_dry_mass_moy))


# Compute RDPI on all traits ####

# By averaging trait values per population
compute_plast_pop <- function(ftrait){
  plast <- traits_pop %>% 
    select(pop,fertilization,all_of(ftrait)) %>% 
    spread(key = fertilization,value = ftrait )
  # mutate(plast = (`N+` - `N-`)/`N+` )
  # merge(trait_moy)
  plast[,ftrait] <-  (plast$`N+` - plast$`N-`) / (plast$`N+`) 
  
  plast %>% 
    ungroup() %>% 
    select(pop,ftrait)
}

PLAST_pop <- compute_plast_pop(traits_plast[1])
for( i in c(2:length(traits_plast)) ){
  plast <- compute_plast_pop(traits_plast[i])
  PLAST_pop <- merge(PLAST_pop,plast)
}



# Relationship plasticity and strategy ####
PLAST2 <- PLAST_pop %>% 
  group_by(pop) %>% 
  select(all_of(traits_plast_interaction_sp)) %>% 
  merge(trait_moy)

## models

lm()


## plot
trait_title <- data.frame(trait = c("SLA","LDMC",
                                    "N","LMF",
                                    "SMF","RMF"),
                          title = c("Specific Leaf Area","Leaf Dry Matter content",
                                    "Plant nitrogen content", "Leaf Mass Fraction",
                                    "Stem Mass Fraction","Root Mass Fraction"))
plot_rdpi_trait <- function(x_axis){
  # x_axis <- "log_plant_dry_mass_moy"
  # x_axis <- "SLA_moy"
  
  PLOT <- NULL
  i <- 0
  for (ftrait in c("N","SLA","LDMC","SMF","RMF")){
    i <- i+1
    
    ## model
    form <- as.formula(paste(ftrait, "~", x_axis))
    mod <- lm(form, data = PLAST2)
    anov <- anova(mod)
    sum <- summary(mod)
    
    intercept <- mod$coefficients[1]
    slope <- mod$coefficients[2]
    pval <- anov$`Pr(>F)`[1] 
    r2 <- sum$r.squared%>% round(digits = 2)
    
    if(pval > 0.05){pval <- "n.s."}else{pval <- pval %>% scientific(digits = 2) %>% paste0("p = ",.)}
    
    ## plot
    lab <- grid::textGrob(label = paste0(pval, "\n",
                                         "R² =", r2),
                          x = unit(0.05, "npc"), 
                          y = unit(0.9, "npc"), just = "left",
                          gp = grid::gpar(size = 14, fontface = "bold"))
    
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
      geom_abline(slope = slope, intercept = intercept) +
      annotation_custom(lab) +
      theme_bw()
    PLOT[[i]] <- plot
  }
  
  plots2 <- ggpubr::ggarrange(plotlist=PLOT, ncol = 1,nrow =5)
  plots2
}

rdpi_sla <- plot_rdpi_trait("SLA_moy")
rdpi_mass <- plot_rdpi_trait("log_plant_dry_mass_moy")
rpdi_sla_mass <- ggpubr::ggarrange(rdpi_sla,rdpi_mass)

ggsave(paste0("draft/RDPI_sla_mass.png"), rpdi_sla_mass,width = 6, height = 13)
