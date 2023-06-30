library(tidyverse)
library(ggpubr)
library(smatr)
library(cowplot())

# Import data ####
t2_traits <- read.csv2("data/t2_traits.csv")%>% 
  mutate(absortive_root_dry_mass = root_dry_mass - pivot_dry_mass) %>% 
  mutate(tot_RL = SRL * absortive_root_dry_mass) %>% # in m (with SRL in m/g and mass in g)
  mutate(log_tot_RL = log10(tot_RL)) %>% 
  
  mutate(tot_RA = tot_RL * 1000 * pi *diam ) %>% # root area, in mm² (convert tot_RL in mm, because diam is is mm)
  mutate(log_tot_RA = log10(tot_RA)) %>%
  
  mutate(tot_LA = SLA * leaf_dry_mass*1000) %>% # convert leaf dry mass into mg so that tot_LA in is mm² (with SLA in mm²/mg) 
  mutate(log_tot_LA = log10(tot_LA)) %>%
  
  mutate(log_plant_dry_mass = log10(plant_dry_mass),
         log_leaf_dry_mass = log10(leaf_dry_mass),
         log_stem_dry_mass = log10(stem_dry_mass),
         log_root_dry_mass = log10(root_dry_mass)) %>% 
  
  mutate(RMF = root_dry_mass/plant_dry_mass,
         SMF = stem_dry_mass/plant_dry_mass,
         LMF = leaf_dry_mass/plant_dry_mass)

sp_fam <- t2_traits %>% 
  select(code_sp,family) %>% 
  unique()

sp_nat = c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO')

traits_pop <- read.csv2("output/data/traits_pop.csv") %>% 
  mutate(log_plant_dry_mass = log(plant_dry_mass)) %>% 
  mutate(log_tot_LA = log10(tot_LA)) %>% 
  mutate(log_tot_RL = log10(tot_RL)) %>% 
  mutate(log_tot_RA = log10(tot_RA)) %>%
  mutate(log_RGRslope = log10(RGRslope)) %>% 
  merge(sp_fam) %>% 
  mutate(log_Hveg = log10(Hveg))

# Plots and analyses ####



# Allométrie ####
# traits_pop %>% 
#   ggplot(aes(x = log_plant_dry_mass,y=log_root_dry_mass,color = fertilization)) +
#   geom_point()

## Mass fraction ####
ftrait <- "log_root_dry_mass"

res_sma <- sma(as.formula(paste(ftrait, "~ log_plant_dry_mass+fertilization")), 
                t2_traits , 
                type = "elevation") 
res_sma

res_sma2 <- sma(as.formula(paste(ftrait, "~ log_plant_dry_mass")), 
                 t2_traits , 
                 type = "shift")
coefs <- coef(res_sma) 
res_intercept_Nm <- coefs[1,1]
res_intercept_Np <- coefs[2,1]
res_slope_Nm <- coefs[1,2]
res_slope_Np <- coefs[2,2]

allom_leg <- t2_traits %>% 
  ggplot(aes_string(x = "log_plant_dry_mass", y = ftrait)) +
  geom_point(aes(color = fertilization)) +
  # scale_shape_manual(values = c(1,16)) +
  scale_color_brewer("Fertilization",palette = "Set2") +
  theme_classic() +
  geom_abline(slope =res_slope_Nm,intercept = res_intercept_Nm,linetype=2) +
  geom_abline(slope =res_slope_Np,intercept = res_intercept_Np,linetype=1) +
  xlab("log(plant dry mass in g)") +
  ylab(ftrait) 
allom_leg
# theme(legend.position = c(0.2,0.8))
leg <- get_legend(allom_leg)

allom <- allom_leg + 
  theme(legend.position = "none")

# Add density curves to y and x axis
xdens <- 
  cowplot::axis_canvas(allom, axis = "x") + 
  geom_density(data = t2_traits, aes(x = log_plant_dry_mass, fill = fertilization, colour = fertilization), alpha = 0.3) +
  scale_color_brewer(palette = "Set2")  +
  scale_fill_brewer(palette = "Set2")
ydens <-
  cowplot::axis_canvas(allom, axis = "y", coord_flip = TRUE) + 
  geom_density(data = t2_traits, aes_string(x = ftrait, fill = "fertilization", colour = "fertilization"), alpha = 0.3) +
  scale_color_brewer(palette = "Set2")  +
  scale_fill_brewer(palette = "Set2") +
  coord_flip()

allom_2 <- allom %>% 
  insert_xaxis_grob(xdens, grid::unit(0.5, "in"), position = "top") %>%
  insert_yaxis_grob(ydens, grid::unit(0.5, "in"), position = "right") %>%
  ggdraw()

# : qualitativement mêmes résultats que niveau individu
# Mais pas même pente et intercept...

## Surfaces ####
ftrait <- "log_tot_LA"

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
  ggtitle("B. Whole plant leaf area")

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
  ggtitle("A. Whole plant absorptive root area")
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


fig_allom <- gridExtra::grid.arrange(grobs = list(allom_leaf2,allom_root2,leg), 
             layout_matrix = rbind(rbind(
               c(1,1,1,1,2,2,2,2,3))))

ggsave("draft/fig_allom_surfaces.png",fig_allom,width = 10,height =4.5)


# avec RGR
traits_pop %>% 
  ggplot(aes(x = log_plant_dry_mass, y = log_RGRslope,color = fertilization)) +
  geom_point() 
  geom_smooth(method = "lm")

sma(log_root_dry_mass ~ log_plant_dry_mass+fertilization, 
    traits_pop , 
    type = "elevation") 

# avec SLA
traits_pop %>% 
  ggplot(aes(x = log_plant_dry_mass, y = log(SLA),color = fertilization)) +
  geom_point() 
  # geom_smooth(method = "lm")



# Boxplot reaction norms ####

make_boxplot <- function(FTRAITS){
  PLOTS <- NULL
  i <- 1
  for (ftrait in FTRAITS){
    A <- traits_pop %>% 
      ggplot(aes_string(x="fertilization", y=ftrait,label = "pop")) + #fill = "fertilization"
      theme_classic() +
      
      geom_boxplot(outlier.shape = NA,
                   aes(fill = fertilization)) +
      geom_point(
        size = 2,
        position = position_dodge(width = .75)) +
      # scale_fill_manual(values = c("grey", "white")) +
      geom_line(aes(group = pop),
                alpha=0.4) + 
      theme(legend.position="none") +
      theme(axis.ticks.x=element_blank() ,
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
      ) +
      ggtitle(ftrait) +
      scale_color_brewer(palette = "Set2")  +
      scale_fill_brewer(palette = "Set2") 
      # scale_shape_manual(values = c(1,16))
    
    PLOTS[[i]] <- A
    i <- i+1
  }
  PLOTS
}



traits_pop %>% 
  ggplot(aes(x=SLA)) +
  geom_density()

traits_pop %>% 
  ggplot(aes(x=RDMC,y=RTD))+
  geom_point()

traits_plant <- c("t0_mass","t1_mass","t2_mass","RGR01","RGR02","RGR12","SAR","N","C")
traits_exchange_surfaces <- c("tot_LA","tot_RL","tot_RA")
traits_allocation <- c("RMF","SMF","LMF")
traits_leaf <- c("log_LA", "LDMC","SLA") 
traits_root <- c("SRL", "RTD", "RDMC", "diam","BI")

p_plant <- make_boxplot(traits_plant)
p_exchange_surfaces <- make_boxplot(traits_exchange_surfaces)
p_allocation <- make_boxplot(traits_allocation)
p_leaf <- make_boxplot(traits_leaf)
p_root <- make_boxplot(traits_root)


library(ggpubr)
boxplot_plant <- ggarrange(plotlist=p_plant)
boxplot_exchange_surfaces <- ggarrange(plotlist = p_exchange_surfaces,ncol = 3, nrow = 1)
boxplot_allocation <- ggarrange(plotlist=p_allocation,ncol = 3,nrow = 2)
boxplot_leaf <- ggarrange(plotlist=p_leaf,ncol = 3,nrow = 2)
boxplot_root <- ggarrange(plotlist=p_root,ncol = 3,nrow = 2)

ggsave("output/plot/boxplot_plant.jpg",boxplot_plant,width = 8, height = 8)
ggsave("output/plot/boxplot_allocation.jpg",boxplot_allocation,width = 8, height = 8)
ggsave("output/plot/boxplot_leaf.jpg",boxplot_leaf,width = 8, height = 8)
ggsave("output/plot/boxplot_root.jpg",boxplot_root,width = 8, height = 8)




bp_tot_LA <- make_boxplot("log_tot_LA")[[1]]
bp_SLA <- make_boxplot("SLA")[[1]]
bp_LMF <- make_boxplot("LMF")[[1]]

ggarrange(bp_tot_LA,allom_leaf, bp_SLA,bp_LMF,ncol = 2,nrow=2)


bp_mass <- make_boxplot("t2_mass")[[1]] +
  ggtitle("Plant dry mass (g)")

bp_SLA <- make_boxplot("SLA")[[1]] +
  ggtitle("SLA (mm²/mg)")
bp_LMF <- make_boxplot("LMF")[[1]] +
  ggtitle("LMF  (%)")

bp_tot_RL <- make_boxplot("log_tot_RL")[[1]]
bp_SRL <- make_boxplot("SRL")[[1]] +
  ggtitle("SRL (m/g)")
bp_RMF <- make_boxplot("RMF")[[1]] +
  ggtitle("RMF (%)")
bp_diam <- make_boxplot("diam")[[1]] +
  ggtitle("Mean diameter (mm)")

fig_bp <- ggarrange(bp_mass, bp_SLA,bp_LMF,bp_diam,bp_SRL,bp_RMF,ncol = 3,nrow=2)
ggsave("draft/fig_bp.png",fig_bp,height = 5,width = 8)



# BIG figure ####
flayout <- rbind(
  c(1,1,1,1,1,1,  2,2,2,2,2,2,  NA),
  c(1,1,1,1,1,1,  2,2,2,2,2,2,   3),
  c(4,4,5,5,NA,NA, 6,6,7,7,8,8, NA)
)

fig_allom_bp <- gridExtra::grid.arrange(
  grobs = list(allom_leaf2,allom_root2,leg,
               bp_LMF,bp_SLA,
               bp_RMF,bp_SRL,bp_diam), 
  layout_matrix = flayout)

ggsave("draft/fig_allom_surfaces_boxplot.png",fig_allom_bp,width = 10,height =6)


flayout <- rbind(
  c(1,1,1,1,  NA,NA,NA,NA,NA,  NA),
  c(1,1,1,1,  NA,4,4,5,5,NA),
  c(1,1,1,1,  NA,4,4,5,5,NA),
  c(1,1,1,1,  3,NA,NA,NA,NA,  NA),
  
  c(2,2,2,2,  3,NA,NA,NA,NA,  NA ),
  c(2,2,2,2,  6,6,7,7,8, 8  ),
  c(2,2,2,2,  6,6,7,7,8, 8  ),
  c(2,2,2,2,  NA,NA,NA,NA,NA,  NA)
)

fig_allom_bp <- gridExtra::grid.arrange(
  grobs = list(allom_leaf2,allom_root2,leg,
               bp_LMF,bp_SLA,
               bp_RMF,bp_SRL,bp_diam), 
  layout_matrix = flayout)

ggsave("draft/fig_allom_surfaces_boxplot.png",fig_allom_bp,width = 10,height =6)



#_________________________________________
# theoretical fig = per population ####
PLOTS <- NULL
i <- 0
ftrait <- "LMF"

for (ftrait in c("RGR01","RMF","SRL","LMF","SLA","SAR")){
  i <- i+1
  fig_pop_plast <- traits_pop %>% 
    filter(!(code_sp  %in% c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO'))) %>% 
    # filter(code_sp=="EROPVERN") %>% 
    mutate(Population = if_else(origin == "Nat","Extensive","Intensive")) %>% 
    
    ggplot(aes_string(x="fertilization", y=ftrait,label = "pop")) +
    theme_classic() +
    geom_point(aes(shape = Population),
               size = 2) +
    scale_fill_manual(values = c("grey", "white")) +
    geom_line(aes(group = pop),
              alpha=0.4) + 
    theme(legend.position="none") +
    theme(axis.ticks.x=element_blank() ,
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    ggtitle(ftrait) +
    facet_wrap(~code_sp,ncol=1) +
    scale_shape_manual(values = c(15,16))

  PLOTS[[i]] <-   fig_pop_plast
}


fig_combi <- ggpubr::ggarrange(PLOTS[[1]],PLOTS[[6]],PLOTS[[2]],PLOTS[[3]],PLOTS[[4]],PLOTS[[5]],nrow = 1)

ggsave("draft/fig_pop_plast.jpg",fig_combi,width = 10, height = 15)


ftrait <- "RGR01"
ftrait2 <- "SAR"
plast_RGR <-traits_pop %>% 
  filter(!(code_sp  %in% c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO'))) %>% 
  mutate(Population = if_else(origin == "Nat","Extensive","Intensive")) %>% 
  
  ggplot(aes_string(x="fertilization", y=ftrait,label = "pop")) +
  theme_classic() +
  geom_point(aes(shape = Population),
             size = 2) +
  scale_fill_manual(values = c("grey", "white")) +
  geom_line(aes(group = pop),alpha=0.4) +
  
  geom_point(aes_string(x = "fertilization",y=ftrait2,shape = "Population"),size = 2,color = "red")+ 
  geom_line(aes_string(x = "fertilization",y=ftrait2,group = "pop"),alpha=0.4, color = "red") +
  # theme(legend.position="none") +
  theme(axis.ticks.x=element_blank() ,
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  ) +
  ggtitle(ftrait) +
  facet_wrap(~code_sp) +
  scale_shape_manual(values = c(15,16))
plast_RGR

ggsave("draft/fig_pop_plast_RGR_SAR.jpg",plast_RGR,width = 7, height = 6)

  # " mass
traits_pop %>% 
  group_by(code_sp,origin,pop,fertilization) %>% 
  select(t0_mass,t1_mass,t2_mass) %>% 
  gather(key = time,value = plant_dry_mass,-c(code_sp,origin,pop,fertilization)) %>% 
  mutate(time = str_sub(time,start = 1,end =2)) %>% 
  mutate(log_plant_dry_mass = log(plant_dry_mass)) %>% 
  mutate(pop_time = paste(pop,time,sep="_"),
         origin_time = paste(origin,time,sep="_")) %>%
  mutate(origin_time = factor(origin_time, levels = c("Fer_t0","Nat_t0","Fer_t1","Nat_t1","Fer_t2","Nat_t2"))) %>% 
  
  filter(!(code_sp  %in% c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO'))) %>% 
  
  ggplot(aes_string(label = "pop")) +
  theme_classic() +
  
  geom_point(aes(x=fertilization, y=log_plant_dry_mass,shape = origin,group = pop,color = time),size = 2) +
  facet_wrap(~code_sp) +
  geom_line(aes(x=fertilization, y=log_plant_dry_mass,group = pop_time),alpha=0.4) +
  
  # theme(legend.position="none") +
  theme(axis.ticks.x=element_blank() ,
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  ) +
  ggtitle("log of plant dry mass") +
  scale_shape_manual(values = c(16,15)) +
  scale_color_brewer(palette = "Set1")


# Theoretical, altogether ####
make_boxplot_wrap_origin <- function(ftrait){
  # color = origin
  # wrap per origin
  # connect with lines same pop (plast)
  # 
  traits_pop %>%
    mutate(origin = factor(origin, levels = c("Nat","Fer"))) %>%
    filter(!(code_sp %in% c("BUPLBALD","MYOSRAMO","FILAPYRA","HORNPETR"))) %>%
    # filter(code_sp %in% c("FILAPYRA","ALYSALYS","ARENSERP","MEDIMINI","GERADISS","BROMHORD","SHERARVE","VEROARVE")) %>%
    #
    # filter(code_sp %in% c("ALYSALYS","ARENSERP","MEDIMINI","BROMHORD","SHERARVE")) %>%
    ggplot(aes_string(x="fertilization", y=ftrait,label = "pop",
                      # shape = "code_sp",
                      color = "origin")) + #fill = "fertilization"
    theme_classic() +
    geom_boxplot(outlier.shape = NA)+
    geom_point(size =3)+
    geom_line(aes(group = pop),alpha=0.4) + # , linetype = factor(origin)
    # theme(legend.position="none") +
    theme(axis.ticks.x=element_blank() ,
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    ggtitle(ftrait) +
    scale_color_brewer(palette = "Set1",direction = -1)  +
    # scale_fill_brewer(palette = "Set2") +
    scale_linetype_manual(values = c("dashed","solid")) +
    scale_shape_manual(values=1:19) +
    # facet_wrap(~origin) +
    theme(legend.position = "none")
  
  # traits_pop %>%
  #   mutate(origin = factor(origin, levels = c("Nat","Fer"))) %>%
  #   filter(!(code_sp %in% c("BUPLBALD","MYOSRAMO","FILAPYRA","HORNPETR"))) %>%
  #   
  #   ggplot(aes_string(x="fertilization", y=ftrait,label = "pop",
  #                     shape = "code_sp",
  #                     color = "origin")) + #fill = "fertilization"
  #   theme_classic() +
  #   # geom_boxplot(outlier.shape = NA)+
  #   geom_point(size =3)+
  #   geom_line(aes(group = pop),alpha=0.4) + # , linetype = factor(origin)
  #   # theme(legend.position="none") +
  #   theme(axis.ticks.x=element_blank() ,
  #         axis.title.y = element_blank(),
  #         axis.title.x = element_blank()
  #   ) +
  #   ggtitle(ftrait) +
  #   scale_color_brewer(palette = "Set1",direction = -1)  +
  #   # scale_fill_brewer(palette = "Set2") +
  #   scale_linetype_manual(values = c("dashed","solid")) +
  #   scale_shape_manual(values=1:19) +
  #   facet_wrap(~code_sp,ncol = 1) +
  #   theme(legend.position = "none")
}





make_boxplot_color_origin <- function(ftrait){
  traits_pop %>% 
    mutate(origin = factor(origin, levels = c("Nat","Fer"))) %>% 
    filter(!(code_sp %in% c("BUPLBALD","MYOSRAMO","FILAPYRA","HORNPETR"))) %>% 
    ggplot(aes_string(x="fertilization", y=ftrait,label = "pop",
                      # shape = "code_sp",
                      color = "origin")) + #fill = "fertilization"
    theme_classic() +
    geom_boxplot(outlier.shape = NA) +
    geom_point(size =1,position = position_dodge(width = .75)) +
    # geom_line(aes(group = pop),alpha=0.4) + # , linetype = factor(origin) 
    # theme(legend.position="none") +
    theme(axis.ticks.x=element_blank() ,
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    ggtitle(ftrait) +
    scale_color_brewer(palette = "Set1",direction = -1)  +
    # scale_fill_brewer(palette = "Set2") +
    scale_linetype_manual(values = c("dashed","solid")) +
    scale_shape_manual(values=1:19) +
    # facet_wrap(~fertilization) +
    theme(legend.position = "none")
}

make_boxplot_wrap_origin("RMF")


traits_manip_bp <- list("N","RMF","SMF","LMF",
                        "log_LA", "LDMC","SLA",
                        "SRL",  "RDMC", "diam")

TBP1 <- traits_manip_bp <- list("SLA","SRL",
                                "LDMC","RDMC",
                                "log_LA","diam")
bps <- lapply(TBP1, make_boxplot_color_origin)
plots1 <- ggarrange(plotlist=bps, ncol = 2,nrow = 3)
ggsave("output/plot/bp_origin_ferti.png", plots1,width = 8, height = 8)

TBP2 <- traits_manip_bp <- list("N","log_Hveg","RMF","LMF")
bps2 <- lapply(TBP2, make_boxplot_color_origin)
plots2 <- ggarrange(plotlist=bps2, ncol = 1,nrow = 4)

ggsave("output/plot/bp_origin_ferti2.png", plots2,width = 4, height = 8)

#_______________________________________________________________________________
# Appendix : Plasticity f(SLA) ####
SLA_moy <- t2_traits %>% 
  group_by(code_sp,origin,pop) %>% 
  summarize(SLA = mean(SLA,na.rm=T))

SLA_Nm <- traits_pop %>% 
  filter(fertilization == "N-") %>% 
  select(pop,SLA)

SLA_Np <- traits_pop %>% 
  filter(fertilization == "N+") %>% 
  select(pop,SLA)

# Plasticité du RGR plus liée à SLA dans le N- que dans le N+
# IL FAUT PRENDRE LES VAL DE SLA DANS LE N-... Pourquoi ?

# ftrait <- "RGRslope"
ftrait <- "RGRslope"
plast <- traits_pop %>% 
  select(pop,fertilization,all_of(ftrait)) %>% 
  spread(key = fertilization,value = ftrait ) %>% 
  mutate(plast = (`N+` - `N-`)/`N-` ) %>% 
  merge(SLA_moy)
  
plast %>% 
  # filter(SLA<28) %>%
  # filter(SLA>18) %>% 
  # filter(!(pop == "ALYSALYS_Nat")) %>%
  ggplot(aes(x = SLA, y = plast,label=pop)) +
  geom_point() +
  geom_abline(slope = 0 , intercept = 0, linetype = "dashed") +
  ylab("[RGR(N+) - RGR(N-)] / RGR(N-)") +
  xlab("SLA moyen")+
  # ggrepel::geom_label_repel()
  geom_smooth(method = "lm")

# Fig de Fichtner & Schultze  
plast %>% 
  select(-c(plast,code_sp,origin,fertilization)) %>% 
  gather(key = fertilization, value = ftrait,-c(SLA,pop)) %>%   
  ggplot(aes(x = SLA, y = ftrait,color = fertilization)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~fertilization)

# Et en prenant les valeurs de SLA dans le traitement où on mesure le RGR
traits_pop %>% 
  ggplot(aes(x = SLA, y = RGRslope, color = fertilization)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~fertilization)



# Même trait en x et en y ####



# RDPI on all traits ####
traits <- c(
  # leaf traits
  "log_LA", "LDMC","SLA",
  # root traits
  "SRL", "RTD", "RDMC", "diam","BI",
  # nutrient
  "C","N",
  "RMF","LMF","SMF",
  # plant_traits
  "tot_RL","tot_RA","tot_LA")

compute_plast <- function(ftrait){
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

PLAST <- compute_plast(traits[1])
for( i in c(2:length(traits)) ){
  plast <- compute_plast(traits[i])
  PLAST <- merge(PLAST,plast)
}

tocorrelate <- PLAST %>% 
  select(-c(tot_RL,tot_RA,tot_LA,C,N)) %>% 
  column_to_rownames("pop")
cor(PLAST %>% select(-c(pop)))

FactoMineR::PCA(tocorrelate) 

library("PerformanceAnalytics")
chart.Correlation(tocorrelate, histogram=TRUE, pch=19)


## rankings ####
abs_PLAST <- PLAST %>% 
  column_to_rownames("pop") %>% 
  abs() %>% 
  rownames_to_column("pop") 
# gather
abs_PLASTg <- abs_PLAST %>% 
  gather(key = trait, value = abs_RDPI,-pop)

abs_PLASTg %>%
  filter(pop%in% c("TRIFCAMP_Nat","CERAGLOM_Nat",#"MYOSRAMO_Nat",#"ARENSERP_Nat",# les plus plastiques sur LA
                   "ALYSALYS_Fer", "VEROARVE_Fer"#,"MINUHYBR_Fer" #,"GERADISS_Nat" # les moins plastiques sur LA))
                   )) %>%  
  mutate(trait = factor(trait, levels = traits )) %>% 
  filter(!(trait %in% c("tot_RL","tot_RA","tot_LA"))) %>% 
  ggplot(aes(x = trait,y=log(abs_RDPI))) +
  geom_point() +
  geom_line(aes(group = pop)) +
  # scale_y_continuous(trans='log10') +
  # facet_wrap(~pop) +
  theme(axis.text.x = element_text(angle = 45))

PLAST %>% 
  ggplot(aes(x = LDMC,SLA )) +
  geom_point() +
  # geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")



## compare rankings ####
rank <- function(ftrait){
  out <- abs_PLAST %>% 
    arrange(get(ftrait),decreasing = T) %>% 
    mutate(rank = c(1:length(abs_PLAST$pop))) %>% 
    select(pop,rank) %>% 
    arrange(pop)
  colnames(out) <- c("pop",ftrait)
  out[,2]
}

rank("LDMC")

RANK <- lapply(traits,rank)
dfRANK <- do.call(cbind, RANK)
colnames(dfRANK) <- traits
dfRANK2 <- cbind(abs_PLAST %>% arrange(pop) %>% select(pop), dfRANK)

GGally::ggparcoord(dfRANK2,
                   columns = 2:14,groupColumn = 1,  
                   scale="globalminmax", 
                   showPoints = TRUE, 
                   title = "Ranking"
) + 
  # Reversed y axis with custom breaks to recreate 1:10 rankings
  scale_y_reverse(breaks = 1:10)

# ranking is not conserved among species

rankcor <- cor(dfRANK2 %>% select(-c("tot_RL","tot_RA","tot_LA")) %>%  column_to_rownames("pop"),method = "spearman")
corrplot::corrplot(rankcor, method = "circle")

## network ####
library(corrr)
PLAST %>% 
  select(-c("tot_RL","tot_RA","tot_LA")) %>% 
  column_to_rownames("pop") %>% 
  correlate(method = "spearman") %>% 
  network_plot(min_cor = 0.4)



#_______________________________________________________________________________
# Appendix: Separating f(origin) ####
mean_traits2 <- mean_traits_pop %>% 
  filter(!code_sp %in% c("BUPLBALD","MYOSRAMO","HORNPETR","FILAPYRA"))

for (ftrait in FTRAITS){
  A <- mean_traits2 %>% 
    filter(origin == "Fer") %>%
    ggplot(aes_string(x="fertilization", y=ftrait,fill = "fertilization")) +
    theme_classic() +
    # theme(axis.title.x=element_blank())+
    # geom_boxplot(aes(color = LifeHistory)) +
    geom_boxplot() +
    geom_point(#aes(color = LifeHistory),
      shape = 19,size = 2,
      position = position_dodge(width = .75)) +
    scale_fill_manual(values = c("grey", "white")) +
    # scale_fill_manual(values = c("grey30", "grey80")) +
    geom_line(aes(group = code_sp),
              alpha=0.4) + #,color=LifeHistory
    ylim(c(miny- 1/10*(maxy-miny),
           maxy)) +
    theme(legend.position="none") +
    # ggtitle ("annuals")
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank() ,
          # axis.title.x = element_blank(),
          axis.title.y = element_blank()
    ) +
    # annotate("text", x = 1, y=maxy, label = comp[1,]$.group)+ 
    # annotate("text", x = 2, y=maxy, label = comp[2,]$.group) + 
    # 
    # annotate("text", x = 1, y=miny - 1/10*(maxy-miny), label = nb1)+ 
    # annotate("text", x = 2, y=miny - 1/10*(maxy-miny), label = nb2) +
    labs(x = "Fer")
  # annotate("text", x = 2, y=maxy + maxy/10, label = trait) 
  
  B <- mean_traits2 %>% 
    filter(origin == "Nat") %>%
    ggplot(aes_string(x="fertilization", y=ftrait,fill = "fertilization")) +
    theme_classic() +
    # theme(axis.title.x=element_blank())+
    # geom_boxplot(aes(color = LifeHistory)) +
    geom_boxplot() +
    geom_point(#aes(color = LifeHistory),
      shape = 19,size = 2,
      position = position_dodge(width = .75)) +
    scale_fill_manual(values = c("grey", "white")) +
    # scale_fill_manual(values = c("grey30", "grey80")) +
    geom_line(aes(group = code_sp),
              alpha=0.4) + #,color=LifeHistory
    ylim(c(miny- 1/10*(maxy-miny),
           maxy)) +
    theme(legend.position="none") +
    # ggtitle ("annuals")
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank() ,
          # axis.title.x = element_blank(),
          axis.title.y = element_blank()
    ) +
    # annotate("text", x = 1, y=maxy, label = comp[1,]$.group)+ 
    # annotate("text", x = 2, y=maxy, label = comp[2,]$.group) + 
    # 
    # annotate("text", x = 1, y=miny - 1/10*(maxy-miny), label = nb1)+ 
    # annotate("text", x = 2, y=miny - 1/10*(maxy-miny), label = nb2) +
    labs(x = "Nat")
  
  
  
  # Create a text grob (for the title = trait name)
  tgrob <- ggpubr::text_grob(FTRAITS[i],size = 10)
  # Draw the text
  plot_0 <- ggpubr::as_ggplot(tgrob) + theme(plot.margin = margin(0,3,0,0, "cm"))
  
  plot <- ggpubr::ggarrange(A, B,
                            ncol = 2,nrow = 1,heights = c(1,5))
  plot2 <- ggpubr::annotate_figure(plot, top = ggpubr::text_grob(FTRAITS[i], 
                                                                 color = "black", 
                                                                 # face = "bold", 
                                                                 size = 12))
  PLOTS[[i]] <- plot2
  i <- i+1
}

## Boxplot ####

# Extract the legend alone, from the data frame of species removal expe
plot <- mean_traits %>% 
  filter(origin == "Fer") %>%
  ggplot(aes_string(x="fertilization", y=ftrait,fill = "fertilization")) +
  theme_classic() +
  # theme(axis.title.x=element_blank())+
  # geom_boxplot(aes(color = LifeHistory)) +
  geom_boxplot() +
  geom_point(#aes(color = LifeHistory),
    shape = 19,size = 2,
    position = position_dodge(width = .75)) +
  scale_fill_manual(values = c("grey", "white")) +
  # scale_fill_manual(values = c("grey30", "grey80")) +
  geom_line(aes(group = code_sp),
            alpha=0.4) + #,color=LifeHistory
  ylim(c(miny- 1/10*(maxy-miny),
         maxy)) +
  # theme(legend.position="none") +
  # ggtitle ("annuals")
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() ,
        # axis.title.x = element_blank(),
        axis.title.y = element_blank()
  ) +
  # annotate("text", x = 1, y=maxy, label = comp[1,]$.group)+ 
  # annotate("text", x = 2, y=maxy, label = comp[2,]$.group) + 
  # 
  # annotate("text", x = 1, y=miny - 1/10*(maxy-miny), label = nb1)+ 
  # annotate("text", x = 2, y=miny - 1/10*(maxy-miny), label = nb2) +
  labs(x = "Fer")

leg <- ggpubr::get_legend(plot)
legend <- ggpubr::as_ggplot(leg)


boxplot_all_traits <- ggpubr::ggarrange(PLOTS[[1]],PLOTS[[2]],PLOTS[[3]],PLOTS[[4]],PLOTS[[5]],PLOTS[[6]],
                                        PLOTS[[7]],PLOTS[[8]],PLOTS[[9]],
                                        PLOTS[[10]],PLOTS[[11]],PLOTS[[12]],legend,
                                        ncol = 3,nrow = 5)

ggsave("output/plot/boxplot_all_traits.jpg",boxplot_all_traits,width = 10, height = 10)
