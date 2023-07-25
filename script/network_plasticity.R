source("script/import_data.R")
library(igraph)



# Traits where we evidenced significant plasticity
# (cf. origin_ferti_effect.R)

traits_plast <- c(
  "log_plant_dry_mass",
  # leaf traits
  "log_LA", "LDMC","SLA",
  # root traits
  # "SRL", "RTD",  "diam","BI", # NOT SIGNIFICANT!
  "RDMC",
  # nutrient
  # "C", n.s.
  "N",
  "RMF","LMF","SMF" )
# plant_traits
# "tot_RL","tot_RA","tot_LA")


# Fixed effects ####
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
  
  # temporaire : post-hoc ####
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
  select(-c(V1,V2,V3,V4,V5) ) 

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


# Network plasticity ####
traits_plast <- c(
  "plant_dry_mass",
  # leaf traits
  "LA", "LDMC","SLA",
  # root traits
  # "SRL", "RTD",  "diam","BI", # NOT SIGNIFICANT!
  "RDMC",
  # nutrient
  # "C", n.s.
  "N",
  "RMF","LMF","SMF" )

## Identify traits highly correlated ####
# Mtraits_pop <- traits_pop %>% 
#   mutate(pop_ferti = paste(pop,fertilization,sep="_")) %>% 
#   select(pop_ferti,all_of(traits_plast)) %>% 
#   column_to_rownames("pop_ferti") %>% 
#   as.matrix()
# 
# corrplot::corrplot(cor(Mtraits_pop),method = "circle",type = "upper" , order = "hclust")

## Compute RDPI on all traits ####

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

# By averaging trait values per species
compute_plast_sp <- function(ftrait){
  plast <- traits_sp %>% 
    select(code_sp,fertilization,all_of(ftrait)) %>% 
    spread(key = fertilization,value = ftrait )
  # mutate(plast = (`N+` - `N-`)/`N+` )
  # merge(trait_moy)
  plast[,ftrait] <-  (plast$`N+` - plast$`N-`) / (plast$`N+`) 
  
  plast %>% 
    ungroup() %>% 
    select(code_sp,ftrait)
}

PLAST_sp <- compute_plast_sp(traits_plast[1])
for( i in c(2:length(traits_plast)) ){
  plast_sp <- compute_plast_sp(traits_plast[i])
  PLAST_sp <- merge(PLAST_sp,plast_sp)
}



# traits for which there is a species*fertilization effect
traits_plast_interaction_sp <- c(

  # root traits
  # "SRL", "RTD",  "diam","BI", # NOT SIGNIFICANT!
  # "RDMC",
  # nutrient
  # "C", n.s.
  "N",
  "RMF","SMF" ,
  # leaf traits
  # "LA", 
  "LDMC","SLA") # "LMF"


## Correlations ####
graph_cor_plast <- function(plast_matrix){
  # PLAST_matrix is a matrix with populatons in rows and traits columns, giving values of plasticity for each trait in each pop.
  cor_pval <- plast_matrix %>% 
    Hmisc::rcorr( type = "spearman") 
  
  adjm <- cor_pval$r # adjacency matrix
  pval <- cor_pval$P
  
  
  # correction <- dim(pval)[1] # number of tests performed
  correction <- 1
  signif <- pval < 0.05/correction # Bonferroni
  ns <- pval >= 0.05/correction # Bonferroni
  # ns <- pval >= 0.05
  
  adjm2 <- adjm # backup
  adjm2[ ns ] <- 0
  adjm2[ adjm2 ==1 ] <- 0 # diagonal
  
  
  graph.adjacency(adjm2, weighted=TRUE, mode="lower")
}

plot_cor_plast <- function(graph){
  # E(graph)$weight2 <- E(graph)$weight
  # E(graph)$weight <- abs(E(graph)$weight)
  E(graph)$color <- ifelse(E(graph)$weight2 > 0,'green','red') #You had this as "V3 > 0.6" which I guess works but it is more readable as 0. that way if you decide to lower the correlation threshold you do not have to change this line too.
  
  plot(graph, layout = layout_with_kk, # Kamada Kawai algorithm
       edge.width= 1 + as.integer(cut(abs(E(graph)$weight2), breaks = 5)), # width of edges
       vertex.size = 20,                  # node size
       asp = 1, # aspect ratio (more or less vertical)
       margin = c(0,0,0,0),
       vertex.color = NA,          # node color
       vertex.frame.width = 00,
       # vertex.label = node_labels,        # node labels
       vertex.label.dist = 0,             # node label size
       vertex.label.font = 1,             # node label type (bold)
       vertex.label.cex = 1  # size of vertex labels
       # edge.color = "black"
  ) 
}
# Info : https://igraph.org/r/doc/plot.common.html

# Check assumptions
# PerformanceAnalytics::chart.Correlation(PLAST %>% select(-pop))


## graph ####
# Correlate
PLAST_matrix <- PLAST_pop %>% 
  select(pop,all_of(traits_plast_interaction_sp)) %>% 
  column_to_rownames("pop") %>% 
  as.matrix()

# per species
# PLAST_matrix <- PLAST_sp %>%
#   filter(RMF>-0.8) %>%
#   select(code_sp,all_of(traits_plast_interaction_sp)) %>%
#   column_to_rownames("code_sp") %>%
#   as.matrix()

PLAST_matrix %>% 
  Hmisc::rcorr( type = "spearman") 

PLAST_pop %>% 
  # filter(RMF>-0.8) %>%
  ggplot(aes(x = N, y = RMF)) + 
  geom_point() 
    # geom_smooth(method = "lm")
model <- lm(RMF ~ N, data = PLAST %>%  filter(RMF>-0.8))
summary(model)

t2_traits %>% 
  filter(!is.na(RMF)) %>% 
  group_by(pop,fertilization) %>% 
  summarize(n=n()) 

library(igraph)
g1 <- graph_cor_plast(PLAST_matrix)
E(g1)$weight2 <- E(g1)$weight
E(g1)$weight <- abs(E(g1)$weight)
V(g1)$label.cex = 1

# Clusters (=mdules) ?
wtc <- cluster_walktrap(g1)
modularity(g1,membership(wtc))
par(mfrow=c(1,1))
# pdf("draft/correlation_RDPI.pdf")
png("draft/correlation_RDPI.png",width = 11, height = 11,units = "cm",res = 720)
plot_cor_plast(g1)
dev.off()


#_______________________________________________________________________________

# Link to SLA ####
trait_title <- data.frame(trait = c("SLA","LDMC",
                                    "N","LMF",
                                    "SMF","RMF"),
                          title = c("Specific Leaf Area","Leaf Dry Matter content",
                                    "Plant nitrogen content", "Leaf Mass Fraction",
                                    "Stem Mass Fraction","Root Mass Fraction"))

rename_moy <- function(trait){
  paste0(trait,"_moy")
}

# valeur de traits moyennée par "population"
trait_moy <- t2_traits %>% 
  group_by(code_sp,origin,pop) %>% 
  select(all_of(traits_plast_interaction_sp),"plant_dry_mass") %>% 
  summarize_at(c(traits_plast_interaction_sp,"plant_dry_mass"),mean, na.rm=T) %>% 
  rename_at(c(traits_plast_interaction_sp,"plant_dry_mass"),rename_moy) %>% 
  group_by(pop) %>% 
  select(-c(code_sp,origin)) %>% 
  mutate(log_plant_dry_mass_moy = log10(plant_dry_mass_moy))

PLAST2 <- PLAST_pop %>% 
  group_by(pop) %>% 
  select(all_of(traits_plast_interaction_sp)) %>% 
  merge(trait_moy)


# Aucun pattern, quel que soit le trait

PLOT <- NULL
i <- 0
for (ftrait in c("N","SLA","LDMC","SMF","RMF")){
  i <- i+1
  plot <- PLAST2 %>% 
    ggplot(aes_string(x = "log_plant_dry_mass_moy", y= ftrait)) + 
    geom_point()  +
    # geom_smooth(method='lm') +
    theme_classic() +
    xlab("Mean population size (log") +
    ggtitle(trait_title %>% filter(trait == ftrait) %>% pull(title)) +
    ylab("RDPI") +
    geom_hline(yintercept = 0) +
    geom_smooth(method = "lm")
  PLOT[[i]] <- plot
}

plots2 <- ggpubr::ggarrange(plotlist=PLOT, ncol = 3,nrow = 2)
plots2
ggsave("draft/RDPI-size.png", plots2,width = 10, height = 7)

## N ellenberg and plasticity ####
ellenberg_plast <- PLAST_pop %>% 
  separate(pop,into=c("code_sp","origin")) %>% 
  merge(species_info,by="code_sp") %>% 
  filter(!(nutrient_requirement == "x")) %>% 
  mutate(nutrient_requirement = as.numeric(nutrient_requirement)) %>% 
  filter(!is.na(nutrient_requirement))


ellenberg_trait <- traits_pop %>% 
  merge(species_info,by="code_sp") %>% 
  filter(!(nutrient_requirement == "x")) %>% 
  mutate(nutrient_requirement = as.numeric(nutrient_requirement)) %>% 
  filter(!is.na(nutrient_requirement)) 
  
ellenberg_plast %>% 
  ggplot(aes(x = nutrient_requirement,y= N )) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_classic()
  # geom_smooth(method = "lm")

ellenberg_trait %>% 
  ggplot(aes(x = nutrient_requirement,y= log(plant_dry_mass) )) +
  geom_point() +
  geom_hline(yintercept = 0) +
  facet_wrap(~fertilization) + 
  theme_classic() +
  geom_smooth(method = "lm")

NN <- lm(log_plant_dry_mass ~ nutrient_requirement + fertilization,data = ellenberg_trait )
anova(NN)
plot(NN)



PLOT <- NULL
i <- 0
for (ftrait in c("N","SLA","LDMC","SMF","RMF")){
  i <- i+1
  plot <- ellenberg_plast %>% 
    ggplot(aes_string(x = "nutrient_requirement",y= ftrait )) +
    geom_point() +
    geom_hline(yintercept = 0) +
    theme_classic() +
    xlab("N-number") +
    ggtitle(trait_title %>% filter(trait == ftrait) %>% pull(title)) +
    ylab("RDPI")
  PLOT[[i]] <- plot
}

plots2 <- ggpubr::ggarrange(plotlist=PLOT, ncol = 3,nrow = 2)
plots2
ggsave("draft/bp_ellenberg.png", plots2,width = 10, height = 7)

AN <- lm(N ~ group , data = ellpl)
anova(AN)




## DISCRETISE ####
ellpl <- PLAST_pop %>% #ellenberg plast
  separate(pop,into=c("code_sp","origin")) %>% 
  merge(species_info,by="code_sp") %>% 
  mutate(group = case_when(nutrient_requirement %in% c("2","3","4") ~ "Low",
                           nutrient_requirement %in% c("5","6") ~ "Int.",
                           # nutrient_requirement == "6" ~ "Int.-High",
                           nutrient_requirement =="7" ~ "High",
                           nutrient_requirement == "x" ~ "Ubiq.")) %>% 
  filter(!group=="NA") %>% 
  mutate(group = factor(group,levels = c("Low","Int.","Int.-High","High","Ubiq.")))

PLOT <- NULL
i <- 0
for (ftrait in traits_plast_interaction_sp){
  i <- i+1
  plot <- ellpl %>% 
    ggplot(aes_string(x = "group", y = ftrait)) +
    geom_boxplot(fill = "lightgrey") +
    geom_point() +
    geom_hline(yintercept = 0) +
    theme_classic()
  PLOT[[i]] <- plot
}

AN <- lm(N ~ group , data = ellpl)
anova(AN)

plots2 <- ggpubr::ggarrange(plotlist=PLOT, ncol = 3,nrow = 2)
plots2
ggsave("draft/bp_ellenberg.png", plots2,width = 10, height = 7)

# CSR and plasticity
PLAST %>% 
  separate(pop,into=c("code_sp","origin",sep="_")) %>% 
  merge(species_info,by="code_sp") %>% 
  ggplot(aes(x = CSR,y= LDMC )) +
  geom_boxplot() +
  geom_hline(yintercept = 0) +
  geom_point()


## Which are the most plastic species per group of traits? ####
PLAST_abs <- PLAST_matrix %>% 
  abs() %>%
  as.data.frame() %>% 
  rownames_to_column("pop")
# plasticity on RTD, LDMC, RMF (3 "modules")

PLAST_matrix %>% 
  as.data.frame() %>%  
  rownames_to_column("pop") %>% 
  ggplot(aes(x = SLA, y = LDMC, label = pop)) + 
  geom_point() +
  # ggrepel::geom_text_repel() +
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_vline(xintercept = 0,linetype = "dashed")




traits <- c(
  # leaf traits
  "log_LA", "LDMC","SLA",
  # root traits
  "SRL", "RTD",  "diam","BI","RDMC",
  # nutrient
  "C","N",
  "RMF","LMF","SMF" )
subs_traits_pop <- traits_pop %>% 
  select(pop,all_of(traits))

traits_and_plast <- PLAST %>% 
  rename_at(vars(-pop),function(x) paste0("p",x)) %>% 
  merge(subs_traits_pop,by="pop") %>% 
  select(-pop)

# par(mfrow=c(1,1))
traits_and_plast %>% 
  corrr::correlate() %>% 
  column_to_rownames("term") %>% 
  as.matrix() %>% 
  corrplot::corrplot(method = "ellipse")

