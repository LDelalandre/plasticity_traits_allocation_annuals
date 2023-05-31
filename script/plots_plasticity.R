library(tidyverse)
library(ggpubr)
library(smatr)
library(cowplot)
library(igraph)

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


# Boxplot of the three traits that move ####
bp_plast_simple <- function(ftrait){
  traits_pop %>%
    mutate(origin = factor(origin, levels = c("Fer","Nat"))) %>%
    mutate(fertilization = factor(fertilization, levels = c("N+","N-"))) %>%
    ggplot(aes_string(x="fertilization", y=ftrait,label = "pop"
                      # shape = "code_sp",
    )) + #fill = "fertilization"
    theme_classic() +
    geom_boxplot(outlier.shape = NA)+
    
    geom_line(aes(group = pop),alpha=0.4,color = "black") + 
    geom_point(size = 2,aes(color = fertilization),)+
    theme(legend.position="none") +
    theme(axis.ticks.x=element_blank() ,
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    ggtitle(ftrait) +
    scale_color_brewer(palette = "Set2",direction = -1) + 
    theme(text=element_text(size=13))
}
bps <- lapply(list("N","RMF","log_plant_dry_mass"), bp_plast_simple)
plots1 <- ggpubr::ggarrange(plotlist=bps, ncol = 3,nrow = 1)
ggsave("draft/bp_ferti.png", plots1,width = 8, height = 3)


# Theoretical plot population and ferti ####
# - Pour le papier : shape = origin et color = ferti avec   # scale_color_brewer(palette = "Set1")
# - Pour l'oral de thèse : color = origin (pour coller avec le papier JVS)

ftrait <- "LDMC"
plot_th_ftrait <- traits_pop %>% 
  group_by(code_sp,origin,pop,fertilization) %>% 
  mutate(fertilization = factor(fertilization, levels = c("N+","N-"))) %>% 

  filter(!(code_sp  %in% c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO'))) %>% 
  
  ggplot(aes_string(label = "pop")) +
  theme_classic() +
  
  geom_point(aes_string(x="fertilization", y=ftrait,color = "origin"),size = 2) + #,shape = "origin"

  geom_line(aes_string(x="fertilization", y=ftrait,group = "pop"),alpha=0.4) +
  
  theme(legend.position="none") +
  theme(axis.ticks.x=element_blank() ,
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
  ) +
  ggtitle(ftrait)+
  scale_shape_manual(values = c(16,15)) +
  facet_wrap(~code_sp)
ggsave("output/plot/plot_th_ftrait.png",plot_th_ftrait)


# Network plasticity ####

## Compute RDPI on all traits ####
# No, on traits where we evidenced significant plasticity
traits_plast <- c(
  # leaf traits
  "log_LA", "LDMC","SLA",
  # root traits
  # "SRL", "RTD",  "diam","BI", # NOT SIGNIFICANT!
  "RDMC",
  # nutrient
  "C","N",
  "RMF","LMF","SMF" )
  # plant_traits
  # "tot_RL","tot_RA","tot_LA")

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

PLAST <- compute_plast(traits_plast[1])
for( i in c(2:length(traits_plast)) ){
  plast <- compute_plast(traits_plast[i])
  PLAST <- merge(PLAST,plast)
}

## Correlations ####

graph_cor_plast <- function(plast_matrix){
  # PLAST_matrix is a matrix with populatons in rows and traits columns, giving values of plasticity for each trait in each pop.
  cor_pval <- plast_matrix %>% 
    Hmisc::rcorr( type = "pearson") 
  
  adjm <- cor_pval$r # adjacency matrix
  pval <- cor_pval$P
  
  
  correction <- dim(pval)[1] # number of tests performed
  # correction <- 1
  signif <- pval < 0.05/correction # Bonferroni
  ns <- pval >= 0.05/correction # Bonferroni
  # ns <- pval >= 0.05
  
  adjm2 <- adjm # backup
  adjm2[ ns ] <- 0
  adjm2[ adjm2 ==1 ] <- 0 # diagonal
  
  
  graph.adjacency(adjm2, weighted=TRUE, mode="lower")
}

plot_cor_plast <- function(graph){
  E(graph)$weight2 <- E(graph)$weight
  E(graph)$weight <- abs(E(graph)$weight)
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

# Correlate
PLAST_matrix <- PLAST %>% 
  column_to_rownames("pop") %>% 
  as.matrix()

g1 <- graph_cor_plast(PLAST_matrix)
V(g1)$label.cex = 1


# pdf("draft/correlation_RDPI.pdf")
png("draft/correlation_RDPI.png",width = 11, height = 11,units = "cm",res = 720)
plot_cor_plast(g1)
dev.off()

## Randomization analysis ####
# randomize species - plasticity association, independently for each trait
# measure the number of statistically significant edges in the randomized datasets
rand_nb_edges <- c()
rand_gr <- NULL
j <- 0
for (i in c(1:100)){
  rand_PLAST_matrix <- apply(PLAST_matrix, 2, sample)
  gr <- graph_cor_plast(rand_PLAST_matrix)
  rand_nb_edges <- c(rand_nb_edges,length(E(gr)))
  
  if (length(E(gr)) > 0) {# if there is at least one significant correlation in the randomized dataset
    gr <- graph_cor_plast(rand_PLAST_matrix)
    j <- j+1
    rand_gr[[j]] <- gr
  }
}
rand_nb_edges

rand_gr[[1]] %>% 
  plot_cor_plast()


## Which are the most plastic species per group of traits? ####
PLAST_abs <- PLAST_matrix %>% 
  abs() %>%
  as.data.frame() %>% 
  rownames_to_column("pop")
# plasticity on RTD, LDMC, RMF (3 "modules")

PLAST_matrix %>% 
  as.data.frame() %>%  
  rownames_to_column("pop") %>% 
  ggplot(aes(x = RTD, y = SRL, label = pop)) + 
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

traits_and_plast %>% 
  corrr::correlate() %>% 
  column_to_rownames("term") %>% 
  as.matrix() %>% 
  corrplot::corrplot(method = "ellipse")
