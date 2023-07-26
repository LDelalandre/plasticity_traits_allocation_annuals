library(tidyverse)
library(smatr)
library(cowplot)

source("script/import_data.R")

t2_traits <-  t2_traits %>% 
  mutate(log_Ntot = log10(N/1000*plant_dry_mass))# diviser par 1000 pour convertir mg/g en fraction (g/g) et avoir la masse de N en g

## Nitrogen ####
ftrait <- "log_Ntot"

# function to compute elevation and slope of sma with confidence interval for a given trait
# returns it in a data frame
sma_output <- function(ftrait){
  sma_ftrait <- sma(as.formula(paste(ftrait, "~ log_plant_dry_mass + fertilization")), 
                    t2_traits ) 
  
  
  
  coefs_ftrait<- coef(sma_ftrait)
  
  intercept_ftrait_Nm <- sma_ftrait$elevtest[[1]]$a
  intercept_ftrait_Np <- sma_ftrait$elevtest[[2]]$a
  
  if(ftrait %in% c("log_Ntot")){ # traits pour lesquels la pente diffère (erreur dans les sorties de sma...)
    slope_ftrait_Nm <- sma_ftrait$slopetest[[1]]$b #slope in N-
    slope_ftrait_Np <- sma_ftrait$slopetest[[2]]$b #slope in N+
  }else{
    slope_ftrait_Nm <- coefs_ftrait[1,2]
    slope_ftrait_Np <- coefs_ftrait[2,2]
  }
  
  # confidence intervals
  ci_intercept_Nm <- sma_ftrait$elevtest[[1]]$a.ci
  ci_intercept_Np <- sma_ftrait$elevtest[[2]]$a.ci
  
  ci_slope_Nm <- sma_ftrait$slopetest[[1]]$ci
  ci_slope_Np <- sma_ftrait$slopetest[[2]]$ci
  
  data.frame(trait = c(ftrait,ftrait),
             
             fertilization = c("N-","N+"),
             
             elevation = c(intercept_ftrait_Nm,intercept_ftrait_Np),
             ci_elevation_min = c(ci_intercept_Nm[1],ci_intercept_Np[1]),
             ci_elevation_max = c(ci_intercept_Nm[2],ci_intercept_Np[2]),
             
             slope = c(slope_ftrait_Nm,slope_ftrait_Np),
             ci_slope_min = c(ci_slope_Nm[1],ci_slope_Np[1]),
             ci_slope_max = c(ci_slope_Nm[2],ci_slope_Np[2]) 
             )
}



# plot the allometric relationship with the elevation and slope computed with smatr
plot_allom <- function(ftrait){
  sma1 <- sma_output(ftrait)
  intercept_ftrait_Nm <- sma1 %>% filter(fertilization == "N-") %>% pull(elevation)
  intercept_ftrait_Np <- sma1 %>% filter(fertilization == "N+") %>% pull(elevation)
  slope_ftrait_Nm <- sma1 %>% filter(fertilization == "N-") %>% pull(slope)
  slope_ftrait_Np <- sma1 %>% filter(fertilization == "N+") %>% pull(slope)
  
  allom_ftrait <- t2_traits %>% 
    ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait,color = "fertilization")) +
    geom_point()+
    scale_shape_manual(values = c(1,16)) +
    scale_color_brewer(palette = "Set2") +
    theme_classic() + 
    geom_abline(slope =slope_ftrait_Nm,intercept = intercept_ftrait_Nm,colour='#1b9e77') + #linetype=2,
    geom_abline(slope =slope_ftrait_Np,intercept = intercept_ftrait_Np, colour = "#d95f02") +
    theme(legend.position = "none") +
    xlab("log(plant dry mass in g)") +
    ylab(ftrait) 
  
  
  # Add density curves to y and x axis
  xdens <- 
    cowplot::axis_canvas(allom_ftrait, axis = "x") + 
    geom_density(data = t2_traits, aes(x = log_plant_dry_mass, fill = fertilization, colour = fertilization), alpha = 0.3) +
    scale_color_brewer(palette = "Set2")  +
    scale_fill_brewer(palette = "Set2")
  
  ydens_ftrait <-
    cowplot::axis_canvas(allom_ftrait, axis = "y", coord_flip = TRUE) + 
    geom_density(data = t2_traits, aes_string(x = ftrait, fill = "fertilization", colour = "fertilization"), alpha = 0.3) +
    scale_color_brewer(palette = "Set2")  +
    scale_fill_brewer(palette = "Set2") +
    coord_flip()
  
  allom_ftrait2 <- allom_ftrait %>%
    insert_xaxis_grob(xdens, grid::unit(0.5, "in"), position = "top") %>%
    insert_yaxis_grob(ydens_ftrait, grid::unit(0.5, "in"), position = "right") %>%
    ggdraw()
  
  allom_ftrait2
  
}

# Figs allometry ####

## coefs ####
sma_allom <- lapply(list("log_Ntot","log_Hveg","log_leaf_dry_mass",
                    "log_stem_dry_mass","log_root_dry_mass",
                    "log_tot_LA","log_tot_RA"),
                    sma_output) %>% 
  rlist::list.rbind() %>% 
  mutate_if(is.numeric, round,digits = 2)

write.csv2(sma_allom,"output/plot/sma_allom.csv",row.names=F)


## plots ####
plots_allom <- lapply(list("log_Ntot","log_leaf_dry_mass","log_stem_dry_mass","log_root_dry_mass",
                           "log_Hveg","log_tot_LA","log_tot_RA"), 
                      plot_allom)

grand_plot_allom <- ggpubr::ggarrange(plotlist=plots_allom, ncol = 4,nrow = 2)
ggsave("output/plot/fig_allom.png",grand_plot_allom,width = 15,height =8)



# Figs raw values ####
# Charger la fonction 'bp_plast_simple' du fichier "boxplot_plasticity.R"

bps <- lapply(list("N","LMF","SMF","RMF",
                   "Hveg","SLA","SRL"),
              bp_plast_simple)

plots1 <- ggpubr::ggarrange(plotlist=bps, ncol = 4,nrow = 2)
# plots1
ggsave("draft/bp_ferticqs.png", plots1,width = 18, height = 12)



# Allom f(origin) ####
sma_ftrait <- sma(as.formula(paste("log_stem_dry_mass", "~ log_plant_dry_mass + origin")), 
                  t2_traits %>% filter(fertilization == "N+") )

coefs_ftrait<- coef(sma_ftrait)

intercept_ftrait_Nm <- sma_ftrait$elevtest[[1]]$a
intercept_ftrait_Np <- sma_ftrait$elevtest[[2]]$a

slope_ftrait_Nm <- coefs_ftrait[1,2]
slope_ftrait_Np <- coefs_ftrait[2,2]


allom_ftrait <- t2_traits %>%
  filter(fertilization == "N+") %>% 
  ggplot(aes_string(x = "log_plant_dry_mass", y = "log_stem_dry_mass",color = "origin")) +
  geom_point()+
  scale_shape_manual(values = c(1,16)) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() + 
  geom_abline(slope =slope_ftrait_Nm,intercept = intercept_ftrait_Nm,colour='#1b9e77') + #linetype=2,
  geom_abline(slope =slope_ftrait_Np,intercept = intercept_ftrait_Np, colour = "#d95f02") +
  theme(legend.position = "none") +
  xlab("log(plant dry mass in g)") 
