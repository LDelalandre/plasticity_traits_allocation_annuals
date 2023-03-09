library(tidyverse)

# Data ####

## Import ####
t0 <- read.csv2("data/t0_height_dry_mass.csv")
t1_traits <- read.csv2("data/t1_traits.csv")
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

## Process ####
mean_traits_pop <- t2_traits %>%  
  select(code_sp,origin,fertilization,
         all_of(traits), 
         all_of(traits_height_mass)) %>% 
  # ajouter ici les fractions de biomasses voulues!
  mutate(RMF = root_dry_mass/plant_dry_mass, # Root mass fraction
         root_shoot = root_dry_mass/(stem_dry_mass+leaf_dry_mass),
         LAR = (SLA * leaf_dry_mass)/plant_dry_mass # leaf area ratio 
  ) %>%  
  group_by(code_sp,origin,fertilization) %>% 
  summarize_all(.funs = mean,na.rm=T) %>% 
  mutate(log_LA = log(LA),
         RMF = root_dry_mass/plant_dry_mass, # Root mass fraction
         root_shoot = root_dry_mass/(stem_dry_mass+leaf_dry_mass),
         LAR = (SLA * leaf_dry_mass)/plant_dry_mass ) %>% # leaf area ratio 
  mutate(pop = paste(code_sp,origin,sep='_'))


data_rgr_sar <- rbind(
  select(t0,pot,code_sp,origin,fertilization,day_of_year,time,plant_dry_mass) %>% mutate(N = NA),
  select(t1_traits,pot,code_sp,origin,fertilization,day_of_year,time,plant_dry_mass,N),
  select(t2_traits,pot,code_sp,origin,fertilization,day_of_year,time,plant_dry_mass,N)) %>% 
  mutate(pop = paste(code_sp,origin, sep = "_")) %>% 
  filter(!plant_dry_mass == 0 ) %>%  # A faire au propore !!
  mutate(log_plant_dry_mass = log(plant_dry_mass))

data_RGR_pop <- data_rgr_sar %>%
  group_by(fertilization,code_sp,origin) %>% 
  group_modify(.f = ~ broom::tidy(lm(log_plant_dry_mass ~ day_of_year, data = .x), conf.int = F)) %>% 
  filter(term == "day_of_year") %>% 
  dplyr::rename(RGRslope = estimate) %>% 
  select(fertilization,code_sp,RGRslope)

traits_pop <- merge(mean_traits_pop,data_RGR_pop)

FTRAITS <- c(
  "RGRslope","N","C",
  "LAR","RMF","root_shoot",
  "log_LA", "LDMC","SLA",
  "SRL", "RTD", "RDMC", "diam","BI"
)

# Boxplot reaction norms ####
PLOTS <- NULL
i <- 1
for (ftrait in FTRAITS){
  A <- traits_pop %>% 
    ggplot(aes_string(x="fertilization", y=ftrait,fill = "fertilization",label = "pop")) +
    theme_classic() +
    
    geom_boxplot() +
    geom_point(
      shape = 19,size = 2,
      position = position_dodge(width = .75)) +
    scale_fill_manual(values = c("grey", "white")) +
    geom_line(aes(group = pop),
              alpha=0.4) + 
    theme(legend.position="none") +
    theme(axis.ticks.x=element_blank() ,
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    ggtitle(ftrait)
  
  PLOTS[[i]] <- A
  i <- i+1
}

## Boxplot ####

boxplot_all_traits <- ggpubr::ggarrange(PLOTS[[1]],PLOTS[[2]],PLOTS[[3]],PLOTS[[4]],PLOTS[[5]],PLOTS[[6]],
                                        PLOTS[[7]],PLOTS[[8]],PLOTS[[9]],
                                        PLOTS[[10]],PLOTS[[11]],PLOTS[[12]],PLOTS[[13]],PLOTS[[14]],
                                        ncol = 4,nrow = 4)

ggsave("output/plot/boxplot_all_traits_pop.jpg",boxplot_all_traits,width = 10, height = 10)


# Plasticity f(SLA)
SLA_Nm <- traits_pop %>% 
  filter(fertilization == "N-") %>% 
  select(pop,SLA)

SLA_Np <- traits_pop %>% 
  filter(fertilization == "N+") %>% 
  select(pop,SLA)

# Plasticité du RGR plus liée à SLA dans le N- que dans le N+
# IL FAUT PRENDRE LES VAL DE SLA DANS LE N-... Pourquoi ?

ftrait <- "RGRslope"
plast <- traits_pop %>% 
  select(pop,fertilization,all_of(ftrait)) %>% 
  spread(key = fertilization,value = ftrait ) %>% 
  mutate(plast = (`N+` - `N-`)/`N+` ) %>% 
  merge(SLA_Nm)
  
plast %>% 
  # filter(!(pop == "ALYSALYS_Nat")) %>%
  ggplot(aes(x = SLA, y = plast,label=pop)) +
  geom_point() +
  geom_abline(slope = 0 , intercept = 0, linetype = "dashed") 
  # ggrepel::geom_label_repel()
  # geom_smooth(method = "lm")

# Fig de Fichtner & Schultze  
plast %>% 
  select(-plast) %>% 
  gather(key = fertilization, value = ftrait,-c(SLA,pop)) %>%   
  ggplot(aes(x = SLA, y = ftrait,color = fertilization)) +
  geom_point() +
  geom_smooth(method = "lm") 
  # facet_wrap(~fertilization)

# Et en prenant les valeurs de SLA dans le traitement où on mesure le RGR
traits_pop %>% 
  ggplot(aes(x = SLA, y = RGRslope, color = fertilization)) +
  geom_point() +
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
