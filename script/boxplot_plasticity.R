source("script/import_data.R")

TABLE_PVAL <- read.csv2("output/data/table_origin_ferti_effect")

traits_ferti_effect <- TABLE_PVAL %>% 
  filter(fertilization < 0.05) %>% 
  pull(Trait)

# Boxplot of the three traits that move ####
bp_plast_simple <- function(ftrait){
  traits_pop %>%
    # filter(pop=="CERAPUMI_Fer") %>%
    mutate(origin = factor(origin, levels = c("Fer","Nat"))) %>%
    mutate(fertilization = factor(fertilization, levels = c("N-","N+"))) %>%
    ggplot(aes_string(x="fertilization", y=ftrait,label = "pop"
                      # shape = "code_sp",
    )) + #fill = "fertilization"
    theme_classic() +
    geom_boxplot(outlier.shape = NA)+
    
    {if(ftrait %in% traits_ferti_effect) geom_line(aes(group = pop),alpha=0.4,color = "black")} + 
    {if( !(ftrait %in% traits_ferti_effect)) geom_line(aes(group = pop),alpha=0.4,color = "grey")} + 

    geom_point(size = 2,aes(color = fertilization),)+
    theme(legend.position="none") +
    theme(axis.ticks.x=element_blank() ,
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    ggtitle(ftrait) +
    scale_color_brewer(palette = "Set2",direction = 1) + 
    theme(text=element_text(size=13))
}
bps <- lapply(list("log_plant_dry_mass","log_Hveg",
                   "log_LA","SLA","LDMC",
                   "LMF","SMF",
                   "SRL", "RTD", "RDMC", 
                   "RMF","N",
                   "diam","BI"
                   ), 
              bp_plast_simple)
plots1 <- ggpubr::ggarrange(plotlist=bps, ncol = 5,nrow = 3)
# plots1
ggsave("draft/bp_ferti.png", plots1,width = 12, height = 9)
ggsave("draft/bp_ferti.svg", plots1,width = 18, height = 13)


# https://stackoverflow.com/questions/64184348/change-line-type-for-certain-categories-in-ggplot
# ligne différente selon espèce