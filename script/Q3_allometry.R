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


## coefs ####
sma_allom <- lapply(list("log_leaf_dry_mass",
                         "log_stem_dry_mass","log_root_dry_mass",
                         "log_Ntot",
                         "log_tot_LA","log_tot_RA","log_Hveg"),
                    sma_output) %>% 
  rlist::list.rbind() %>% 
  mutate_if(is.numeric, round,digits = 2)

write.csv2(sma_allom,"output/plot/sma_allom.csv",row.names=F)

sma_allom2 <- sma_allom %>% 
  filter(trait %in% c("log_leaf_dry_mass","log_stem_dry_mass","log_root_dry_mass","log_Ntot")) %>% 
  mutate(Elevation = paste0(elevation," [",ci_elevation_min,",",ci_elevation_max,"]")) %>% 
  mutate(Slope = paste0(slope," [",ci_slope_min,",",ci_slope_max,"]")) %>% 
  dplyr::select(trait,fertilization,Elevation, Slope)

write.csv2(sma_allom2,"draft/sma_allom.csv",row.names=F)

# Plot ####
FTRAIT <- c("log_leaf_dry_mass","log_stem_dry_mass","log_root_dry_mass","log_Ntot")
ftrait <- FTRAIT[1]

plot_sma <- function(ftrait,sma_allom){
  intercept_ftrait_Nm <- sma_allom %>% 
    filter(trait == ftrait & fertilization == "N-") %>% 
    pull(elevation)
  intercept_ftrait_Np <- sma_allom %>% 
    filter(trait == ftrait & fertilization == "N+") %>% 
    pull(elevation)
  
  slope_ftrait_Nm <- sma_allom %>% 
    filter(trait == ftrait & fertilization == "N-") %>% 
    pull(slope)
  slope_ftrait_Np <- sma_allom %>% 
    filter(trait == ftrait & fertilization == "N+") %>% 
    pull(slope)
  
  allom_ftrait <- t2_traits %>% 
    ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait,color = "fertilization")) +
    geom_point()+
    scale_shape_manual(values = c(1,16)) +
    scale_color_brewer(palette = "Set2") +
    theme_classic() + 
    geom_abline(slope =slope_ftrait_Nm,intercept = intercept_ftrait_Nm,colour='#1b9e77') + #linetype=2,
    geom_abline(slope =slope_ftrait_Np,intercept = intercept_ftrait_Np, colour = "#d95f02") +
    theme(legend.position = "none") +
    # ggtitle(ftrait) +
    xlab("log(plant dry mass)") +
    {if (ftrait == "log_leaf_dry_mass") ylab("log(leaf dry mass)")} +
    {if (ftrait == "log_stem_dry_mass") ylab("log(stem dry mass)")} +
    {if (ftrait == "log_root_dry_mass") ylab("log(root dry mass)")} +
    {if (ftrait == "log_Ntot") ylab("log(plant nitrogen content)")} 
# Units : g
  
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

### Legend ####
allom_leaf_leg <- t2_traits %>% 
  ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait)) +
  geom_point(aes(color = fertilization)) +
  # scale_shape_manual(values = c(1,16)) +
  scale_color_brewer("Fertilization",palette = "Set2") +
  theme_classic() +
  geom_abline(slope =1,intercept = 0) +
  xlab("log(plant dry mass in g)") +
  ylab("log(total leaf area in mm²)") 
# theme(legend.position = c(0.2,0.8))
leg <- get_legend(allom_leaf_leg)
plot(leg)

### Overall plot ####
PLOTS <- lapply(FTRAIT,plot_sma,sma_allom)

fig_allom <- ggpubr::ggarrange(PLOTS[[1]], PLOTS[[2]], PLOTS[[3]], PLOTS[[4]], 
                  labels = c("A. Leaf", "B. Stem","C. Root","D. Nitrogen"), nrow = 2,ncol = 2, common.legend = TRUE, legend = "none")

ggsave("draft/fig_allom.png",fig_allom,width = 7,height =7)
