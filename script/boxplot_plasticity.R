library(tidyverse)
library(ggpubr)
library(smatr)

# Import data ####
t2_traits <- read.csv2("data/t2_traits.csv")%>% 
  mutate(absortive_root_dry_mass = root_dry_mass - pivot_dry_mass) %>% 
  mutate(tot_RL = SRL * absortive_root_dry_mass) %>% 
  mutate(log_tot_RL = log10(tot_RL)) %>%
  
  mutate(tot_RA = SRL * absortive_root_dry_mass * diam ) %>% # root area
  mutate(log_tot_RA = log10(tot_RA)) %>%
  
  mutate(tot_LA = SLA * leaf_dry_mass) %>% 
  mutate(log_tot_LA = log10(tot_LA)) %>%
  
  mutate(log_plant_dry_mass = log10(plant_dry_mass))

sp_nat = c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO')

traits_pop <- read.csv2("output/data/traits_pop.csv") %>% 
  mutate(log_plant_dry_mass = log(plant_dry_mass)) %>% 
  mutate(log_tot_LA = log10(tot_LA)) %>% 
  mutate(log_tot_RL = log10(tot_RL)) %>% 
  mutate(log_tot_RA = log10(tot_RA)) %>%
  mutate(log_RGRslope = log10(RGRslope))

# Plots and analyses ####



# Allométrie niveau pop : qualitativement mêmes résultats que niveau individu
# Mais pas même pente et intercept...
sma_leaf <- sma(log_tot_LA ~ log_plant_dry_mass+fertilization, 
    t2_traits , 
    type = "shift") 
sma_leaf

sma_leaf2 <- sma(log_tot_LA ~ log_plant_dry_mass, 
                t2_traits , 
                type = "shift")
coefs_leaf <- coef(sma_leaf2) 
intercept_leaf <- coefs_leaf[1]
slope_leaf <- coefs_leaf[2]

allom_leaf <- t2_traits %>% 
  ggplot(aes(x = log_plant_dry_mass, y = log_tot_LA)) +
  geom_point(aes(shape = fertilization)) +
  # geom_smooth(method = "lm")+
  scale_shape_manual(values = c(1,16)) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() +
  geom_abline(slope =slope_leaf,intercept = intercept_leaf)
allom_leaf

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
  geom_point(aes(shape = fertilization)) +
  # geom_smooth(method = "lm")+
  scale_shape_manual(values = c(1,16)) +
  scale_color_brewer(palette = "Set2") +
  theme_classic() + 
  geom_abline(slope =slope_root_Nm,intercept = intercept_root_Nm,linetype=2) +
  geom_abline(slope =slope_root_Np,intercept = intercept_root_Np)
allom_root






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
      
      geom_boxplot(outlier.shape = NA) +
      geom_point(
        aes(shape = fertilization),size = 2,
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
      scale_shape_manual(values = c(1,16))
    
    PLOTS[[i]] <- A
    i <- i+1
  }
  PLOTS
}

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


bp_mass <- make_boxplot("t2_mass")[[1]]

bp_tot_LA <- make_boxplot("log_tot_LA")[[1]]
bp_SLA <- make_boxplot("SLA")[[1]]
bp_LMF <- make_boxplot("LMF")[[1]]

ggarrange(bp_tot_LA,allom_leaf, bp_SLA,bp_LMF,ncol = 2,nrow=2)



bp_tot_RA <- make_boxplot("log_tot_RA")[[1]]
bp_tot_RL <- make_boxplot("log_tot_RL")[[1]]
bp_SRL <- make_boxplot("SRL")[[1]]
bp_RMF <- make_boxplot("RMF")[[1]]
bp_diam <- make_boxplot("diam")[[1]]

ggarrange(bp_tot_RL,allom_root, bp_SRL,bp_RMF,ncol = 2,nrow=2)

t2_traits %>% 
  ggplot(aes(x=log_plant_dry_mass,y=SLA)) +
  geom_point() +
  facet_wrap(~fertilization) +
  geom_smooth(method="lm")

ggarrange(allom_leaf,allom_root,ncol = 2,nrow=1)


#_________________________________________
# per population ####
PLOTS <- NULL
i <- 0
ftrait <- "LMF"

for (ftrait in c("RGR01","RMF","SRL","LMF","SLA")){
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


fig_combi <- ggarrange(PLOTS[[1]],PLOTS[[2]],PLOTS[[3]],PLOTS[[4]],PLOTS[[5]],nrow = 1)

ggsave("/fig_pop_plast.jpg",fig_combi,width = 9, height = 15)


ftrait <- "RGR01"
plast_RGR <-traits_pop %>% 
  filter(!(code_sp  %in% c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO'))) %>% 
  mutate(Population = if_else(origin == "Nat","Extensive","Intensive")) %>% 
  
  ggplot(aes_string(x="fertilization", y=ftrait,label = "pop")) +
  theme_classic() +
  geom_point(aes(shape = Population),
             size = 2) +
  scale_fill_manual(values = c("grey", "white")) +
  geom_line(aes(group = pop),
            alpha=0.4) + 
  # theme(legend.position="none") +
  theme(axis.ticks.x=element_blank() ,
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  ) +
  ggtitle(ftrait) +
  facet_wrap(~code_sp) +
  scale_shape_manual(values = c(15,16))

ggsave("draft/fig_pop_plast_RGR.jpg",plast_RGR,width = 7, height = 6)

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
  xlab("SLA moyen")
  # ggrepel::geom_label_repel()
  # geom_smooth(method = "lm")

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
ftrait <- "RTD"

trait_moy <- t2_traits %>% 
  group_by(code_sp,origin,pop) %>% 
  summarize(ftrait = mean(get(ftrait),na.rm=T))

plast <- traits_pop %>% 
  select(pop,fertilization,all_of(ftrait)) %>% 
  spread(key = fertilization,value = ftrait ) %>% 
  mutate(plast = (`N+` - `N-`)/`N+` ) %>% 
  merge(trait_moy)

plast %>% 
  # filter(!(pop == "ALYSALYS_Nat")) %>%
  ggplot(aes(x = ftrait, y = plast,label=pop)) +
  geom_point() +
  geom_abline(slope = 0 , intercept = 0, linetype = "dashed") +
  ylab("[trait(N+) - trait(N-)] / trait(N-)") +
  xlab("trait moyen") +
  ggtitle(ftrait) 
  geom_smooth(method = "lm")

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
