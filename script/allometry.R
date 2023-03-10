library(tidyverse)
library(plyr)
library(tidyverse)
library(broom)
library(smatr)
library(GGally)
library(viridis)
library(cowplot)

t0 <- read.csv2("data/t0_height_dry_mass.csv")
t1_traits <- read.csv2("data/t1_traits.csv",fileEncoding = "Latin1")  
t2_traits <- read.csv2("data/t2_traits.csv")

t1_dry_mass <- t1_traits %>% 
  mutate(pop = paste(code_sp,origin,sep="_")) %>% 
  select(code_sp,pot,origin,pop,fertilization,time,
         "plant_dry_mass", "stem_dry_mass","leaf_dry_mass","root_dry_mass") 


t2_dry_mass <- t2_traits %>% 
  mutate(pop = paste(code_sp,origin,sep="_")) %>% 
  select(code_sp,pot,origin,pop,fertilization,time,
         "plant_dry_mass", "stem_dry_mass","leaf_dry_mass","root_dry_mass") 



t1_t2_dry_mass <- rbind(t1_dry_mass,t2_dry_mass) %>% 
  mutate(shoot_dry_mass = stem_dry_mass + leaf_dry_mass) %>%
  mutate(log_plant_dry_mass = log10(plant_dry_mass),
         log_stem_dry_mass = log10(stem_dry_mass),
         log_leaf_dry_mass = log10(leaf_dry_mass),
         log_root_dry_mass = log10(root_dry_mass),
         log_shoot_dry_mass = log10(shoot_dry_mass)) %>% 
  mutate(root_shoot = root_dry_mass/shoot_dry_mass) %>% 
  mutate(log_root_shoot = log(root_shoot)) %>% 
  mutate(pop_fer = paste(pop,fertilization,sep ="_")) %>% 
  mutate(sp_fer = paste(code_sp,fertilization,sep ="_"))

list_sp = sort(unique(t1_dry_mass$code_sp)) #liste de toutes les espèces
list_sp_nat = c('BUPLBALD','HORNPETR','FILAPYRA','MYOSRAMO') #liste des espèces qui sont uniquement dans le milieu natif
list_sp_no_nat = list_sp[!(list_sp %in% list_sp_nat)]
list_pop = sort(unique(t1_dry_mass$sp_or))
list_pop_fer = sort(unique(t1_dry_mass$sp_or_fer))
list_sp_fer = sort(unique(t1_dry_mass$sp_fer))
list_rel_allom = c('root:shoot','root:leaf','stem:leaf','stem:root','root:all')


## _____________________________________________________________________________
## Comparaison allométrie à t1 et t2 ####
## On compare la pente globale à t1 et à t2 pour voir si on a une modification de l'allométrie avec l'ontogénie

## Sans séparer par population

t1_t2_dry_mass %>% 
  # filter(time=="t1") %>% 
  ggplot(aes(x = log_leaf_dry_mass, y = log_stem_dry_mass, color = time,label = pot))+
  geom_point()+
  geom_smooth(method = 'lm') 


sma(log_root_dry_mass ~ log_shoot_dry_mass*time,t1_t2_dry_mass) #pas de diff entre t1 et t2
sma(log_root_dry_mass ~ log_leaf_dry_mass*time,t1_t2_dry_mass) #pas de diff entre t1 et t2
sma(log_leaf_dry_mass ~ log_stem_dry_mass*time,t1_t2_dry_mass) #pour le leaf:stem les pentes sont tres différentes entre t1 et t2


## En séparant par population 

t1_t2_dry_mass %>% 
  ggplot(aes(x = log_leaf_dry_mass, y = log_stem_dry_mass,shape = fertilization))+
  geom_point(aes(color = time))+
  # geom_smooth(method = 'lm')+
  facet_wrap(~pop)

sma_t1_t2 <- function(pop){ #fonction qui calcule la pvalue de la diff de pente entre t1 et t2 pour la sma de root = f(shoot) pour une pop donnée
  a <- sma(log_root_dry_mass ~log_stem_dry_mass*time,t1_t2_dry_mass[t1_t2_dry_mass$pop == pop,])
  b <- a$commoncoef[[2]]
  b
}

list_pop <- t1_t2_dry_mass %>% pull(pop) %>% unique()

diff_t1_t2 <- data.frame(pop = list_pop,
                         pval_diff_t1_t2 = unlist(lapply(lapply(list_pop,sma_t1_t2),as.data.frame)))

#Y a 4 populations pour lesquelles on a une différence significative de pente entre t1 et t2
#Pour MYOSRAMO_Nat à t2 on a un r2 de 0,035 et une pvalue de 0,6
#Pour MINUHYBR_Fer on a clairement une diff, des pvalue OK et r2 entre 0,5 et 0,7
#Pour GERADISS_Nat on  a un mauvais r2 et une mauvaise p value à t1 
#Pour SHERARVE_Fer pvalue et r2 pas trop mauvaises 





## _____________________________________________________________________________
## Allométrie pour chaque paire d'organes ####
## Etude des relations allométriques en combinant les données à t1 et t2
## On étudie 5 relations : root:shoot ; leaf:stem ; leaf:root ; root:stem ; root:masse_tot


# Plot rapide des relations allométriques pour chaque espèce selon la fertilisation
t1_t2_dry_mass %>% 
  ggplot(aes(x = log_plant_dry_mass,y = log_root_dry_mass,color = fertilization)) +
  geom_point() +
  facet_wrap(~pop) +
  geom_smooth(method = 'lm') +
  scale_color_hue(h = c(180, 300))

t1_t2_dry_mass %>% 
  ggplot(aes(x = log_plant_dry_mass,y = log_root_dry_mass,color = fertilization)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_color_hue(h = c(180, 300))


#_____________________________________________


# Pour traiter chaque relation allométrique séparément :

# root = f(shoot)
t1_t2_dry_mass_rsh <- t1_t2_dry_mass %>% 
  select(pot,code_sp,fertilization,origin,sp_fer,log_shoot_dry_mass,log_root_dry_mass,pop_fer,pop)
colnames(t1_t2_dry_mass_rsh)[colnames(t1_t2_dry_mass_rsh) == "log_shoot_dry_mass"] <- "abs"
colnames(t1_t2_dry_mass_rsh)[colnames(t1_t2_dry_mass_rsh) == "log_root_dry_mass"] <- "ord"

# root = f(leaf)
t1_t2_dry_mass_rl <- t1_t2_dry_mass %>% 
  select(pot,code_sp,fertilization,origin,sp_fer,log_leaf_dry_mass,log_root_dry_mass,pop_fer,pop)
colnames(t1_t2_dry_mass_rl)[colnames(t1_t2_dry_mass_rl) == "log_leaf_dry_mass"] <- "abs"
colnames(t1_t2_dry_mass_rl)[colnames(t1_t2_dry_mass_rl) == "log_root_dry_mass"] <- "ord"

# stem = f(leaf)
t1_t2_dry_mass_sl <- t1_t2_dry_mass %>% 
  select(pot,code_sp,fertilization,origin,sp_fer,log_leaf_dry_mass,log_stem_dry_mass,pop_fer,pop)
colnames(t1_t2_dry_mass_sl)[colnames(t1_t2_dry_mass_sl) == "log_leaf_dry_mass"] <- "abs"
colnames(t1_t2_dry_mass_sl)[colnames(t1_t2_dry_mass_sl) == "log_stem_dry_mass"] <- "ord"

# stem = f(root)
t1_t2_dry_mass_str <- t1_t2_dry_mass %>% 
  select(pot,code_sp,fertilization,origin,sp_fer,log_root_dry_mass,log_stem_dry_mass,pop_fer,pop)
colnames(t1_t2_dry_mass_str)[colnames(t1_t2_dry_mass_str) == "log_root_dry_mass"] <- "abs"
colnames(t1_t2_dry_mass_str)[colnames(t1_t2_dry_mass_str) == "log_stem_dry_mass"] <- "ord"

# root = f(plant)
t1_t2_dry_mass_rall <- t1_t2_dry_mass %>% 
  select(pot,code_sp,fertilization,origin,sp_fer,log_root_dry_mass,log_plant_dry_mass,pop_fer,pop)
colnames(t1_t2_dry_mass_rall)[colnames(t1_t2_dry_mass_rall) == "log_plant_dry_mass"] <- "abs"
colnames(t1_t2_dry_mass_rall)[colnames(t1_t2_dry_mass_rall) == "log_root_dry_mass"] <- "ord"

# choix de la relation qui nous intéressera ####
frelationship <- 1 # 1 pour root shoot, 5 pour RMF


# SMA per pop*trt ####
# chose the relationship to study
fdata <- t1_t2_dry_mass_rall # root to all
fdata <- t1_t2_dry_mass_rsh # root to shoot

# perform SMA to relate the two biomasses
tmp <- with(t1_t2_dry_mass_rall,
            by(t1_t2_dry_mass_rall, pop_fer,
               function(x) sma(ord ~ abs, data = x)))

# extract coefs (intercept and slope), pvalue and r2
get_pval <- function(sma_output){
  sma_output$pval[[1]]
}
get_r2 <- function(sma_output){
  sma_output$r2[[1]]
}

coefs <- sapply(tmp, coef)  %>% 
  t() %>%
  as.data.frame() %>% 
  rownames_to_column("pop_fer") %>% 
  separate(pop_fer,into = c("code_sp","origin","fertilization"),sep="_") %>% 
  mutate(pop = paste (code_sp,origin,sep="_"))

pval <- sapply(tmp, get_pval) %>% 
  as.data.frame() %>% 
  rownames_to_column("pop_fer") %>% 
  separate(pop_fer,into = c("code_sp","origin","fertilization"),sep="_") %>% 
  mutate(pop = paste (code_sp,origin,sep="_"))
colnames(pval)[4] <- "pval"

r2 <- sapply(tmp,get_r2)%>% 
  as.data.frame() %>% 
  rownames_to_column("pop_fer") %>% 
  separate(pop_fer,into = c("code_sp","origin","fertilization"),sep="_") %>% 
  mutate(pop = paste (code_sp,origin,sep="_"))
colnames(r2)[4] <- "r2"

sma_pop <- merge(coefs,pval) %>% merge(r2)
sma_pop_nonat <- sma_pop %>% filter(!code_sp %in% list_sp_nat)

write.csv2(sma_pop,'output/data/sma_dry_mass_t1_t2_rs.csv',row.names=F)


# graphes pour voir
sma_pop %>% 
  ggplot(aes(x = fertilization, y = elevation)) +
  geom_boxplot() 

# Pas les mêmes résultats que Lila
# - Je ne travaille pas exactement sur les mêmes données ? Mais comment ça pourrait différer tant pour quelques points en plus ou en moins ?
# - Il y a un problème qqpart dans mes scripts (ou les siens) ?
# - ... "elevation" ne veut pas dire "intercept" ?

sma_pop_intercept <- sma_pop %>% 
  select(-c(pval,r2,slope)) %>% 
  spread(key = fertilization,value = elevation)

## _____________________________________________________________________________
## Plot et test des différences de pentes et intercepts #### 

## En groupant toutes les populations  : ----


# Pour apparier les populations (pour faire les tests de Student) 
# (Et pour avoir le plot avec les points reliés pour la pente en N+ et en N- pour chaque espèce)
# = Avoir pour chaque pop la pente et l'intercept du N- et du N+ sur la même ligne
# (Au lieu d'avoir une ligne pour pop_N+ et une ligne pour pop_N-)

pop_appariees <- function(df){ 
  #fonction qui renvoie un df : pour chaque pop_fer on a la pente du N+, du N-, intercept du N+ et du N- pour une relation allométrique donnée)
  pop_appar <- df
  pop_appar <- pop_appar %>%
    mutate(slope_fer = if_else(fertilization =='N+',
                               'slope_Np',
                               'slope_Nm'),
           intercept_fer = if_else(fertilization == 'N+',
                                   'intercept_Np',
                                   'intercept_Nm')) %>%
    
    pivot_wider(names_from = slope_fer, values_from = slope) %>% 
    pivot_wider(names_from = intercept_fer, values_from = intercept)
  
  pop_appar_2 <- select(pop_appar[is.na(pop_appar$slope_Np) == F,],pop, slope_Np,intercept_Np,code_sp)
  pop_appar_3 <- select(pop_appar[is.na(pop_appar$slope_Np) == T,],pop, slope_Nm,intercept_Nm,code_sp)
  pop_appar_4 <- merge(pop_appar_2,pop_appar_3)
  
  pop_appar_4
}


list_coef_sma_Np_Nm <- lapply(sma_dry_mass_t1_t2,pop_appariees) 
list_coef_sma_Np_Nm_no_nat <- lapply(sma_dry_mass_t1_t2_no_nat,pop_appariees) 

write.csv2(list_coef_sma_Np_Nm[[frelationship]], "output/data/coef_sma_Np_Nm.csv")
# on exporte le dataframe de la relation root:shoot pour pouvoir l'utiliser dans d'autres scripts


i = frelationship
#i va de 1 à 5 selon la relation allométrique qu'on veut 
# Rappel de l'ordre : 'root:shoot','root:leaf','stem:leaf','stem:root','root:all'
# Le stem:root est intéressant car pas de diff significative --> intéressant en terme d'équilibre fonctionnel
# car l'équilibre se fait entre racines et feuilles ou feuilles et tiges mais pas racines et tiges


## -------------------- Pour les pentes ---------------------

#Boxplot global sans séparer les espèces
sma_dry_mass_t1_t2_no_nat[[i]] %>%
  ggplot(aes(x = fertilization,y = slope,color = fertilization))+
  geom_boxplot()+
  ggtitle(list_rel_allom[i])+
  scale_color_hue(h = c(180, 300))

# facet_wrap(~code_sp)"

#Plot qui relie les pentes du N+ et N- pour chaque espèce
list_coef_sma_Np_Nm[[i]] %>% 
  ggparcoord(columns = c(5,3),
             groupColumn = 1,
             showPoints = T,
             scale = "globalminmax")+
  scale_color_viridis(discrete=TRUE)+
  ggtitle(list_rel_allom[i])


t.test(list_coef_sma_Np_Nm[[i]]$slope_Np,list_coef_sma_Np_Nm[[i]]$slope_Nm, alternative = "greater")
#différence de pente non significative (on compare uniquemen tles distributions des pentes, pas les pentes population par population)


## -------------------- Pour les intercepts --------------------- 
#Boxplot global sans séparer les espèces
sma_pop %>%
  ggplot(aes(x = fertilization,y = elevation,color = fertilization))+
  geom_boxplot()+
  geom_point()+
  scale_color_hue(h = c(180, 300))
  ggtitle(list_rel_allom[i])

# facet_wrap(~code_sp)"

#Plot qui relie les intercepts dans le N+ et le N- pour chaque espèce
list_coef_sma_Np_Nm[[i]] %>% 
  ggparcoord(columns = c(6,4),
             groupColumn = 1,
             showPoints = T,
             scale = "globalminmax")+
  scale_color_viridis(discrete=TRUE)


t.test(list_coef_sma_Np_Nm[[i]]$intercept_Np,list_coef_sma_Np_Nm[[i]]$intercept_Nm)
# Différence significative de distribution des intercepts entre le N- et le N+



## En regardant population par population ----
## Etude population par population de si la pente est différente de 1 et différentre entre N+ et N-


# 1) Pente significativement différente de 1 ? 

pval_sma_one <- function(pop_fer,datafr){
  a <- sma(ord~abs,slope.test = 1,datafr[datafr$pop_fer == pop_fer,])$slopetest[[1]]$p
  a
}

pval_allom_one <- function(datafr){
  df <- data.frame(pop_fer = list_pop_fer)
  df <- group_by(df,pop_fer)
  df <- mutate(df, pval = pval_sma_one(pop_fer,datafr))
  colnames(df)[2] <- paste('pval',unlist(str_split(deparse(substitute(datafr)),'_'))[5],sep = '_')
  df
}

test_slope_one <- list(pval_allom_one(t1_t2_dry_mass_rsh), 
                       pval_allom_one(t1_t2_dry_mass_rl),
                       pval_allom_one(t1_t2_dry_mass_sl),
                       pval_allom_one(t1_t2_dry_mass_str),
                       pval_allom_one(t1_t2_dry_mass_rall))

test_slope_one <- test_slope_one %>% 
  reduce(full_join, by='pop_fer')

test_slope_one <- mutate(test_slope_one,fertilization = unlist(strsplit(pop_fer,'_'))[[3]])



# 2) Pente et intercept significativement différents entre le N+ et le N- ? 


pval_sma_N <- function(pop,datafr){
  pslope <- sma(ord ~ abs * fertilization,datafr[datafr$pop == pop,])
  pintercept <- sma(ord ~ abs + fertilization,datafr[datafr$pop == pop,], type = "elevation")
  pshift <- sma(ord ~ abs + fertilization,datafr[datafr$pop == pop,], type = "shift")
  list(pslope$commoncoef[[2]],pintercept$gtr$p,pshift$gtr$p)
}

pval_allom_N <- function(datafr){
  df <- data.frame(pop = list_pop)
  df <- df %>% 
    group_by(pop) %>% 
    mutate(pval_slope = pval_sma_N(pop,datafr)[1],
           pval_intercept = pval_sma_N(pop,datafr)[2],
           pval_shift = pval_sma_N(pop,datafr)[3]) %>% 
    ungroup()
  colnames(df)[2] <- paste('pval_slope',unlist(str_split(deparse(substitute(datafr)),'_'))[5],sep = '_')
  colnames(df)[3] <- paste('pval_intercept',unlist(str_split(deparse(substitute(datafr)),'_'))[5],sep = '_')
  colnames(df)[4] <- paste('pval_shift',unlist(str_split(deparse(substitute(datafr)),'_'))[5],sep = '_')
  df
}

df_rsh <- pval_allom_N(t1_t2_dry_mass_rsh)
df_rl <- pval_allom_N(t1_t2_dry_mass_rl)
df_sl <- pval_allom_N(t1_t2_dry_mass_sl)
df_str <- pval_allom_N(t1_t2_dry_mass_str)
df_rall <- pval_allom_N(t1_t2_dry_mass_rall)

test_slope_N_rsh <- merge(df_str,merge(df_rall,merge(df_sl,merge(df_rsh,df_rl))))
# Pour avoir toutes les pvalue, pop parp op, des différences de pente, d'intercet et de shift entre le N- et le N+ 
# 5 dataframe = un par relation allométrique
