library(tidyverse)

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
  "SRL", "RTD", "RDMC", "diam",
  # nutrient
  "C","N")


mean_traits <- t2_traits %>%  
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
         LAR = (SLA * leaf_dry_mass)/plant_dry_mass )# leaf area ratio 


FTRAITS <- c(
  # leaf traits
  "log_LA", "LDMC","SLA",
  # root traits
  "SRL", "RTD", "RDMC", "diam",
  # nutrient
  "C","N",
  "RMF","LAR","root_shoot")


# model on raw data rather than averaged

TABLE_PVAL <- NULL
for (ftrait in FTRAITS){
  data_mod <- t2_traits %>% 
    filter(!code_sp %in% c("BUPLBALD","MYOSRAMO","HORNPETR","FILAPYRA")) %>% 
    mutate(log_LA = log(LA),
           RMF = root_dry_mass/plant_dry_mass, # Root mass fraction
           root_shoot = root_dry_mass/(stem_dry_mass+leaf_dry_mass),
           LAR = (SLA * leaf_dry_mass)/plant_dry_mass ) %>% # leaf area ratio 
    mutate(origin = as.factor(origin) ) %>% 
    mutate(fertilization = as.factor(fertilization))
  
  formula0 <- as.formula(paste0(ftrait, " ~ 1 + (1|code_sp)"))
  formula <- as.formula(paste0(ftrait, " ~ fertilization * origin", " + (1|code_sp)"))
  
  mmod0 <- lme4::lmer( formula0 , data = data_mod)
  
  mmod <- lme4::lmer( formula , data = data_mod) # /!\ choose fdata (includes sp just measured in on treatment)
  
  anov <- car::Anova(mmod)
  pval <- anov %>%
    as.data.frame() %>% 
    rename(pval = 'Pr(>Chisq)') %>%
    pull(pval) %>% 
    format(scientific = TRUE, digits = 2) %>%
    as.numeric()
  table_pval <-  data.frame(Trait = ftrait,
                            fertilization = pval[1],
                            origin = pval[2],
                            Interaction = pval[3]
  )
  TABLE_PVAL <- rbind(TABLE_PVAL,table_pval)
}
TABLE_PVAL


TABLE_PVAL <- NULL
PLOTS <- NULL
i <- 1
for (ftrait in FTRAITS){
  ## Linear model ####
  data_mod <- mean_traits %>% 
    mutate(origin = as.factor(origin) ) %>% 
    mutate(fertilization = as.factor(fertilization))
  
  formula0 <- as.formula(paste0(ftrait, " ~ 1 + (1|code_sp)"))
  formula <- as.formula(paste0(ftrait, " ~ fertilization * origin", " + (1|code_sp)"))
  
  mmod0 <- lme4::lmer( formula0 , data = data_mod)
  
  mmod <- lme4::lmer( formula , data = data_mod) # /!\ choose fdata (includes sp just measured in on treatment)

  anov <- car::Anova(mmod)
  pval <- anov %>%
    as.data.frame() %>% 
    rename(pval = 'Pr(>Chisq)') %>%
    pull(pval) %>% 
    format(scientific = TRUE, digits = 2) %>% 
    as.numeric()
  table_pval <-  data.frame(Trait = ftrait,
                            fertilization = pval[1],
                            origin = pval[2],
                            Interaction = pval[3]
  )
  
  # part of variance explained
  # GET CODE FROM SCRIPT PAPER ANNUALS IN SITU
  
  
  # if( table_pval$Interaction < 0.05 ){
  posthoc <- multcomp::cld(emmeans::emmeans(mmod, specs = c("fertilization","origin"),  type = "response",
                                            adjust = "tuckey"),
                           Letters = "abcdefghi", details = T)
  
  # contrasts = différences entre les deux traitements pour annuelles et pérennes
  contrasts <- posthoc$comparisons %>% 
    filter(contrast %in% c("(N- Nat) - (N+ Nat)","(N- Fer) - (N+ Fer)",
                           "(N+ Nat) - (N- Nat)","(N+ Fer) - (N- Fer)")) %>%
    mutate(estimate = case_when(contrast %in% c("(N- Nat) - (N+ Nat)","(N- Fer) - (N+ Fer)") ~ -estimate,
                                TRUE ~ estimate)) %>% 
    mutate(contrast = case_when(contrast == "(N- Nat) - (N+ Nat)" ~"(N+ Nat) - (N- Nat)",
                                contrast == "(N- Fer) - (N+ Fer)" ~ "(N+ Fer) - (N- Fer)",
                                TRUE ~ contrast)) %>% 
    mutate(contrast = if_else(contrast == "(N+ Fer) - (N- Fer)", "Fer","Nat")) %>% 
    select(contrast,estimate,p.value) %>% 
    mutate(estimate = round(estimate,digits =1)) %>% 
    mutate(p.value = format(p.value,scientific = TRUE, digits = 2)) %>% 
    mutate(est_pval = paste0(estimate, " (",p.value,")")) %>%
    select(contrast,est_pval) %>% 
    spread(key = contrast,value = est_pval)
    
    table_pval2 <- cbind(table_pval,contrasts)
    # table_pval2$percent_var_code_sp <- percent_var_code_sp
    TABLE_PVAL <- rbind(TABLE_PVAL,table_pval2)
    
    comp <- as.data.frame(posthoc$emmeans) %>% 
      mutate(fertilization = factor(fertilization,levels = c("N-","N+"))) %>% 
      mutate(origin = factor(origin, levels = c("Fer","Nat"))) %>% 
      arrange(fertilization,origin)
  
  ## plot ####
  miny <- min( mean_traits %>% pull(sym(ftrait)) , na.rm = T)
  maxydata <- max(mean_traits %>% pull(sym(ftrait)), na.rm = T)
  
  maxy <- maxydata + (maxydata - miny)/10
  
  # ICI ####
  A <- mean_traits %>% 
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
  
  B <- mean_traits %>% 
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

ggsave("draft_plasticity/boxplot_all_traits.jpg",boxplot_all_traits,width = 10, height = 10)
