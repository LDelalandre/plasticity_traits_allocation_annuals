source("script/import_data.R")

library(smatr)

# Surfaces ####
ftrait <- "log_tot_LA"

## Leaf Area ####
sma_leaf <- sma(as.formula(paste(ftrait, "~ log_plant_dry_mass+fertilization")), 
                t2_traits , 
                type = "shift") 
sma_leaf

sma_leaf2 <- sma(as.formula(paste(ftrait, "~ log_plant_dry_mass")), 
                 t2_traits , 
                 type = "shift")
coefs_leaf <- coef(sma_leaf2) 
intercept_leaf <- coefs_leaf[1]
slope_leaf <- coefs_leaf[2]

allom_leaf_leg <- t2_traits %>% 
  ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait)) +
  geom_point(aes(color = fertilization)) +
  # scale_shape_manual(values = c(1,16)) +
  scale_color_brewer("Fertilization",palette = "Set2") +
  theme_classic() +
  geom_abline(slope =slope_leaf,intercept = intercept_leaf) +
  xlab("log(plant dry mass in g)") +
  ylab("log(total leaf area in mm²)") 
# theme(legend.position = c(0.2,0.8))
leg <- get_legend(allom_leaf_leg)

allom_leaf <- allom_leaf_leg + 
  theme(legend.position = "none") +
  ggtitle("A. Whole plant leaf area")

# Add density curves to y and x axis
xdens_leaf <- 
  cowplot::axis_canvas(allom_leaf, axis = "x") + 
  geom_density(data = t2_traits, aes(x = log_plant_dry_mass, fill = fertilization, colour = fertilization), alpha = 0.3) +
  scale_color_brewer(palette = "Set2")  +
  scale_fill_brewer(palette = "Set2")
ydens_leaf <-
  cowplot::axis_canvas(allom_leaf, axis = "y", coord_flip = TRUE) + 
  geom_density(data = t2_traits, aes_string(x = ftrait, fill = "fertilization", colour = "fertilization"), alpha = 0.3) +
  scale_color_brewer(palette = "Set2")  +
  scale_fill_brewer(palette = "Set2") +
  coord_flip()

allom_leaf2 <- allom_leaf %>% 
  insert_xaxis_grob(xdens_leaf, grid::unit(0.5, "in"), position = "top") %>%
  insert_yaxis_grob(ydens_leaf, grid::unit(0.5, "in"), position = "right") %>%
  ggdraw()


# on attend une slope de 0.75, et on a 0.9
sma_enquist_2002 <- sma(log_leaf_dry_mass ~ log_root_dry_mass+fertilization, 
                        t2_traits , 
                        type = "elevation") 
sma_enquist_2002
coef(sma_enquist_2002)

## Root area ####
sma_root <- sma(log_tot_RA ~ log_plant_dry_mass+fertilization, 
                t2_traits , 
                type = "elevation") 
sma_root


coefs_root <- coef(sma_root) 
intercept_root_Nm <- coefs_root[1,1]
intercept_root_Np <- coefs_root[2,1]
slope_root_Nm <- coefs_root[1,2]
slope_root_Np <- coefs_root[2,2]

allom_root <- t2_traits %>% 
  ggplot(aes(x = log_plant_dry_mass, y = log_tot_RA)) +
  geom_point(aes(color = fertilization)) +
  # geom_smooth(method = "lm")+
  scale_shape_manual(values = c(1,16)) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() + 
  geom_abline(slope =slope_root_Nm,intercept = intercept_root_Nm,linetype=2) +
  geom_abline(slope =slope_root_Np,intercept = intercept_root_Np) +
  xlab("log(plant dry mass in g)") +
  ylab("log(total root area in mm²)") + 
  theme(legend.position = "none") +
  ggtitle("B. Whole plant absorptive root area")
allom_root

# Add density curves to y and x axis
xdens_root <- 
  cowplot::axis_canvas(allom_root, axis = "x") + 
  geom_density(data = t2_traits, aes(x = log_plant_dry_mass, fill = fertilization, colour = fertilization), alpha = 0.3) +
  scale_color_brewer(palette = "Set2")  +
  scale_fill_brewer(palette = "Set2")
ydens_root <-
  cowplot::axis_canvas(allom_root, axis = "y", coord_flip = TRUE) + 
  geom_density(data = t2_traits, aes(x = log_tot_RA, fill = fertilization, colour = fertilization), alpha = 0.3) +
  scale_color_brewer(palette = "Set2")  +
  scale_fill_brewer(palette = "Set2") +
  coord_flip()
allom_root2 <- allom_root %>%
  insert_xaxis_grob(xdens_root, grid::unit(0.5, "in"), position = "top") %>%
  insert_yaxis_grob(ydens_root, grid::unit(0.5, "in"), position = "right") %>%
  ggdraw()


## Plot ####
fig_allom <- gridExtra::grid.arrange(grobs = list(allom_leaf2,allom_root2,leg), 
                                     layout_matrix = rbind(rbind(
                                       c(1,1,1,1,2,2,2,2,3))))

ggsave("draft/fig_allom_surfaces.png",fig_allom,width = 10,height =4.5)

