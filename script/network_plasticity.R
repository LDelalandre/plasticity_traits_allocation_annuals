source("script/import_data.R")
library(igraph)

# Network plasticity ####

# Traits where we evidenced significant plasticity
# (cf. origin_ferti_effect.R)

traits_plast <- c(
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

## Identify traits highly correlated ####
Mtraits_pop <- traits_pop %>% 
  mutate(pop_ferti = paste(pop,fertilization,sep="_")) %>% 
  select(pop_ferti,all_of(traits_plast)) %>% 
  column_to_rownames("pop_ferti") %>% 
  as.matrix()

corrplot::corrplot(cor(Mtraits_pop),method = "circle",type = "upper" , order = "hclust")

## Compute RDPI on all traits ####
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

## Species differ on their plasticity and genetic differentiation ? ####
# if interaction fertilization * species : different species respond differently to fertilization
interaction_sp_ferti <- function(ftrait){
  mod <- lm(get(ftrait) ~  code_sp + fertilization  + origin + fertilization:code_sp + origin:code_sp, data = t2_traits)
  # summary(mod)
  anov <- anova(mod)
  c(ftrait,anov$`Pr(>F)`)
}

mod_fix <- lapply(as.list(FTRAITS), interaction_sp_ferti) %>% 
  rlist::list.rbind() %>% 
  as.data.frame() %>% 
  mutate(trait = V1,species = as.numeric(V2),fertilization=as.numeric(V3),origin = as.numeric(V4),
         species_fertilization = as.numeric(V5),species_origin = as.numeric (V6)) %>% 
  select(-c(V1,V2,V3,V4,V5,V6,V7) ) %>%   
  mutate(species = scales::scientific(species,digits = 2)) %>%
  mutate(fertilization = scales::scientific(fertilization,digits = 2)) %>% 
  mutate(origin = scales::scientific(origin,digits = 2)) %>%
  mutate(species_fertilization  = scales::scientific(species_fertilization ,digits = 2)) %>%
  mutate(species_origin = scales::scientific(species_origin,digits = 2))

table_mod_fix <- mod_fix %>% 
  kableExtra::kable( escape = F,
                     col.names = c("Trait","Species","Fertilization","Origin","Species_Fertilization","Species_Origin"
                     )) %>%
  kableExtra::kable_styling("hover", full_width = F)



cat(table_mod_fix, file = "draft/table_difference_plast_sp.doc")


# traits for which there is a species*fertilization effect
traits_plast_interaction_sp <- c(
  # leaf traits
  "log_LA", "LDMC","SLA",
  # root traits
  # "SRL", "RTD",  "diam","BI", # NOT SIGNIFICANT!
  # "RDMC",
  # nutrient
  # "C", n.s.
  "N",
  "RMF","SMF" ) # "LMF"


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
PLAST_matrix <- PLAST %>% 
  select(pop,all_of(traits_plast_interaction_sp)) %>% 
  column_to_rownames("pop") %>% 
  as.matrix()

g1 <- graph_cor_plast(PLAST_matrix)
E(g1)$weight2 <- E(g1)$weight
E(g1)$weight <- abs(E(g1)$weight)
V(g1)$label.cex = 1

# Clusters (=mdules) ?
wtc <- cluster_walktrap(g1)
modularity(g1,membership(wtc))

# pdf("draft/correlation_RDPI.pdf")
png("draft/correlation_RDPI.png",width = 11, height = 11,units = "cm",res = 720)
plot_cor_plast(g1)
dev.off()


#_______________________________________________________________________________

## Link to SLA ####
rename_moy <- function(trait){
  paste0(trait,"_moy")
}

trait_moy <- t2_traits %>% 
  group_by(code_sp,origin,pop) %>% 
  select(all_of(traits_plast_interaction_sp)) %>% 
  summarize_at(traits_plast_interaction_sp,mean, na.rm=T) %>% 
  rename_at(traits_plast_interaction_sp,rename_moy) %>% 
  group_by(pop) %>% 
  select(-c(code_sp,origin))

PLAST2 <- PLAST %>% 
  group_by(pop) %>% 
  select(all_of(traits_plast_interaction_sp)) %>% 
  merge(trait_moy)

PLAST2 %>% 
  ggplot(aes(x = SLA_moy, y= LDMC_moy)) +
  geom_point() + geom_smooth(method='lm')
# Aucun pattern, quel que soit le trait






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

traits_and_plast %>% 
  corrr::correlate() %>% 
  column_to_rownames("term") %>% 
  as.matrix() %>% 
  corrplot::corrplot(method = "ellipse")
