source("script/import_data.R")

library(smatr)
library(cowplot)
t2_traits <-  t2_traits %>% 
  mutate(log_Ntot = log10(N/1000*plant_dry_mass))# diviser par 1000 pour convertir mg/g en fraction (g/g) et avoir la masse de N en g

# Surfaces ####
ftrait <- "log_tot_LA"

## Leaf Area ####
sma_leaf <- sma(as.formula(paste(ftrait, "~ log_plant_dry_mass+fertilization")), 
                t2_traits , 
                type = "elevation") 
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
  # annotate("text", x=-0.5, y=2.5, label= paste(), size=5, parse=TRUE)

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
intercept_root_Nm <- sma_root$elevtest[[1]]$a
intercept_root_Np <- sma_root$elevtest[[2]]$a


slope_root_Nm <- sma_root$slopetest[[1]]$b
slope_root_Np <- sma_root$slopetest[[1]]$b

allom_root <- t2_traits %>% 
  ggplot(aes(x = log_plant_dry_mass, y = log_tot_RA)) +
  geom_point(aes(color = fertilization)) +
  # geom_smooth(method = "lm")+
  scale_shape_manual(values = c(1,16)) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() + 
  geom_abline(slope =slope_root_Nm,intercept = intercept_root_Nm,colour='#1b9e77') + #linetype=2,
  geom_abline(slope =slope_root_Np,intercept = intercept_root_Np, colour = "#d95f02") +
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

## Nitrogen ####
ftrait <- "log_Ntot"

sma_ftrait <- sma(as.formula(paste(ftrait, "~ log_plant_dry_mass + fertilization")), 
                  t2_traits ) 



# Juste dans N+
sma(as.formula(paste(ftrait, "~ log_plant_dry_mass ")), 
                  t2_traits %>% filter(fertilization == "N+"))
# juste dans N-
sma(as.formula(paste(ftrait, "~ log_plant_dry_mass ")), 
                     t2_traits %>% filter(fertilization == "N-")) 
coefs_ftrait<- coef(sma_ftrait) 

# intercept_ftrait_Nm <- coefs_ftrait[1,1]
# intercept_ftrait_Np <- coefs_ftrait[2,1]

intercept_ftrait_Nm <- sma_ftrait$elevtest[[1]]$a
intercept_ftrait_Np <- sma_ftrait$elevtest[[2]]$a

# erreur avec les sorties de pente avec la fonction coef...
slope_ftrait_Nm <- sma_ftrait$slopetest[[1]]$b #slope in N-
slope_ftrait_Np <- sma_ftrait$slopetest[[2]]$b #slope in N+

allom_ftrait <- t2_traits %>% 
  ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait,color = "fertilization")) +
  geom_point()+
  scale_shape_manual(values = c(1,16)) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() + 
  geom_abline(slope =slope_ftrait_Nm,intercept = intercept_ftrait_Nm,colour='#1b9e77') + #linetype=2,
  geom_abline(slope =slope_ftrait_Np,intercept = intercept_ftrait_Np, colour = "#d95f02") +
  theme(legend.position = "none") +
  ggtitle("C. Whole plant nitrogen content") +
  xlab("log(plant dry mass in g)") +
  ylab("log(plant nitrogen content in g)") 
# xlab("log(plant dry mass in g)") +
# ylab("log(total root area in mm²)")  
# theme(legend.position = "none") +
# ggtitle("B. Whole plant absorptive root area")
# allom_ftrait

# Add density curves to y and x axis
ydens_ftrait <-
  cowplot::axis_canvas(allom_ftrait, axis = "y", coord_flip = TRUE) + 
  geom_density(data = t2_traits, aes_string(x = ftrait, fill = "fertilization", colour = "fertilization"), alpha = 0.3) +
  scale_color_brewer(palette = "Set2")  +
  scale_fill_brewer(palette = "Set2") +
  coord_flip()
allom_ftrait2 <- allom_ftrait %>%
  insert_xaxis_grob(xdens_leaf, grid::unit(0.5, "in"), position = "top") %>%
  insert_yaxis_grob(ydens_ftrait, grid::unit(0.5, "in"), position = "right") %>%
  ggdraw()

allom_ftrait2


## Plot ####
fig_allom <- gridExtra::grid.arrange(grobs = list(allom_leaf2,allom_root2,allom_ftrait2,leg), 
                                     layout_matrix = rbind(rbind(
                                       c(1,1,1,1,2,2,2,2,3,3,3,3,43))))

ggsave("draft/fig_allom_surfaces.png",fig_allom,width = 11,height =4)


# Table ####
# for a given trait

ftrait <- "log_Ntot"

output_sma <- function(ftrait){
  sma_ftrait <- sma(as.formula(paste(ftrait, "~ log_plant_dry_mass + fertilization")), 
                    t2_traits ) 

  intercept_ftrait_Nm <- sma_ftrait$elevtest[[1]]$a
  intercept_ftrait_Np <- sma_ftrait$elevtest[[2]]$a
  
  # erreur avec les sorties de pente avec la fonction coef...
  slope_ftrait_Nm <- sma_ftrait$slopetest[[1]]$b #slope in N-
  slope_ftrait_Np <- sma_ftrait$slopetest[[2]]$b #slope in N+
  
  c(intercept_ftrait_Nm,intercept_ftrait_Np,slope_ftrait_Nm,slope_ftrait_Np) 
    
}

table <- rbind (output_sma("log_Ntot"),
                output_sma("log_leaf_dry_mass"),
                output_sma("log_stem_dry_mass"),
                output_sma("log_root_dry_mass")
                )
colnames(table) <- c("F-","F+","F-","F+")
rownames(table) <- c("N content","Leaf mass","Stem mass","Root mass")

table %>%
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

# 
# LA <- data.frame(Trait = c("Leaf area","Root area","Root area","Nitrogen content","Nitrogen content"
#                  Fertilization ="F- and F+",
#                  Intercept = )

# leaf
sma_leaf$elevtest[[1]]$a
sma_leaf$elevtest[[1]]$a.ci
sma_leaf$slopetest[[1]]$b
sma_leaf$slopetest[[1]]$ci

# root
sma_root$elevtest[[1]]
sma_root$elevtest[[2]]

sma_root$slopetest[[1]]

# N
sma_ftrait$elevtest[[1]]
sma_ftrait$elevtest[[2]]

sma_ftrait$slopetest[[1]]
sma_ftrait$slopetest[[2]]


# Other traits ####
 
ftrait <- "log_Ntot"


sma_ftrait <- sma(as.formula(paste(ftrait, "~ log_plant_dry_mass + fertilization")), 
                t2_traits ) 
sma_ftrait
coefs_ftrait<- coef(sma_ftrait) 

intercept_ftrait_Nm <- coefs_ftrait[1,1]
intercept_ftrait_Np <- coefs_ftrait[2,1]
slope_ftrait_Nm <- coefs_ftrait[1,2]
slope_ftrait_Np <- coefs_ftrait[2,2]

allom_ftrait <- t2_traits %>% 
  ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait,color = "fertilization")) +
  geom_point()+
  scale_shape_manual(values = c(1,16)) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() + 
  geom_abline(slope =slope_ftrait_Nm,intercept = intercept_ftrait_Nm,colour='#1b9e77') + #linetype=2,
  geom_abline(slope =slope_ftrait_Np,intercept = intercept_ftrait_Np, colour = "#d95f02") 
  # xlab("log(plant dry mass in g)") +
  # ylab("log(total root area in mm²)")  
  # theme(legend.position = "none") +
  # ggtitle("B. Whole plant absorptive root area")
allom_ftrait

t2_traits %>% 
  ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait,color = "fertilization")) +
  geom_point()+
  # scale_shape_manual(values = c(1,16)) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() +
  facet_wrap(~code_sp) 
  # geom_smooth(method = "lm")


# SMA on SLA per species ####
# in each treatment
fferti <- "N+"
ftrait <- "log_SLA"

sma_ftrait_sp <- t2_traits %>% 
  filter(fertilization == fferti) %>% 
  sma(as.formula(paste(ftrait, "~ log_plant_dry_mass + code_sp")),data = .) 
sma_ftrait_sp
coefs_ftrait<- coef(sma_ftrait_sp)

sp <- rownames(coefs_ftrait)
PLOT <- NULL
for (i in c(1:length(sp))){
  spi <- sp[i]
  plot <- t2_traits %>% 
    # filter(fertilization == fferti) %>% 
    filter(code_sp == spi) %>% 
    ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait)) +
    geom_point(aes(shape = fertilization))+
    theme_classic()+
    geom_abline(slope =coefs_ftrait[i,2],intercept = coefs_ftrait[i,1])+
    ggtitle(spi) +
    theme_bw() +
    theme(legend.position = "none")
  
  PLOT[[i]] <- plot
  # ggsave(paste0("output/plot/sma_sla/",spi,".png"),plot)
}


plot_sla <- ggpubr::ggarrange(plotlist=PLOT, ncol = 3,nrow = 5)
# ggsave(paste0("draft/plot_allom_sla",fferti,".jpg"),plot_sla,height = 15, width = 10)
ggsave(paste0("draft/plot_allom_sla.jpg"),plot_sla,height = 15, width = 10)
