source("script/import_data.R")

TABLE_PVAL <- read.csv2("output/data/table_origin_ferti_effect")

traits_ferti_effect <- TABLE_PVAL %>% 
  filter(fertilization < 0.05) %>% 
  pull(Trait)

traits_ferti_effect <- c("plant_dry_mass",
                         "SLA",
                         "SMF","RMF",
                         "N","LMF","RDMC")


# Boxplot of the three traits that move ####
bp_plast_simple <- function(ftrait){
  
  trait_title <- data.frame(trait = c("plant_dry_mass","Hveg",
                                      "LA","SLA","LDMC",
                                      "N","LMF",
                                      "SRL", "RTD", "RDMC", 
                                      "SMF","RMF",
                                      "diam","BI"),
                            title = c("Plant dry mass (g)", "Vegetative height (cm)", 
                                      "Leaf Area (cm²)","Specific Leaf Area (mm²/mg)","Leaf Dry Matter content (mg/g)",
                                      "Plant nitrogen content (mg/g)", "Leaf Mass Fraction (g/g)",
                                      "Specific Root Length (m/g)", "Root Tissue Density (g/cm3)","Root Dry Matter Content (mg/g)",
                                      "Stem Mass Fraction (g/g)","Root Mass Fraction(g/g)",
                                      "Mean root diameter (mm)","Branching intensity (cm-1)"))
  
  traits_pop %>%
    mutate(fertilization = if_else(fertilization == "N+", "F+","F-")) %>% 
    # filter(pop=="CERAPUMI_Fer") %>%
    mutate(origin = factor(origin, levels = c("Fer","Nat"))) %>%
    mutate(fertilization = factor(fertilization, levels = c("F-","F+"))) %>%
    ggplot(aes_string(x="fertilization", y=ftrait,label = "pop"
                      # shape = "code_sp",
    )) + #fill = "fertilization"
    theme_classic() +
    geom_boxplot(outlier.shape = NA)+
    
    {if(ftrait %in% c(traits_ferti_effect,"plant_dry_mass")) geom_line(aes(group = pop),alpha=0.4,color = "black")} + 
    {if( !(ftrait %in% traits_ferti_effect)) geom_line(aes(group = pop),alpha=0.4,color = "grey")} + 

    geom_point(size = 2,aes(color = fertilization),)+
    theme(legend.position="none") +
    theme(axis.ticks.x=element_blank() ,
          axis.title.y = element_blank(),
          axis.title.x = element_blank()
    ) +
    ggtitle(trait_title %>% filter(trait == ftrait) %>% pull(title)) +
    scale_color_brewer(palette = "Set2",direction = 1) + 
    theme(text=element_text(size=13)) +
    {if (ftrait %in% c("plant_dry_mass","Hveg","LA")) scale_y_continuous(trans='log10')} 
}
bps <- lapply(list("plant_dry_mass","Hveg",
                   "LA","SLA","LDMC",
                   "N","LMF",
                   "diam","SRL", "RTD",  
                   "SMF","RMF",
                   "RDMC","BI"
                   ), 
              bp_plast_simple)
plots1 <- ggpubr::ggarrange(plotlist=bps, ncol = 5,nrow = 3)
# plots1
# ggsave("draft/bp_ferti.png", plots1,width = 18, height = 12)
# ggsave("draft/bp_ferti.svg", plots1,width = 18, height = 13)

# autre tentative
# Whole plant
WP1 <- lapply(list("plant_dry_mass","Hveg",
                   "N","LMF",
                   "SMF","RMF"), 
bp_plast_simple)
WP2 <- ggpubr::ggarrange(plotlist=WP1, ncol = 2,nrow = 3,
                         align = "hv") 

# Leaf
L1 <- lapply(list("LA","SLA","LDMC"), 
              bp_plast_simple)
L2 <- ggpubr::ggarrange(plotlist=L1, ncol = 3,nrow = 1,align = "hv") 

# Root
R1 <- lapply(list("diam","SRL", "RTD","RDMC","BI"), 
             bp_plast_simple)
R2 <- ggpubr::ggarrange(plotlist=R1, ncol = 3,nrow = 2) 


correct_margin <- 0.04
bps_traits <- cowplot::ggdraw() +
  cowplot::draw_plot(WP2, x = 0, y = 0, width = 2/5 - correct_margin , height = 1 - correct_margin) +
  cowplot::draw_plot(L2, x = 2/5, y = 2/3, width = 3/5  - correct_margin, height = 1/3  - correct_margin) +
  cowplot::draw_plot(R2 , x = 2/5, y = 0, width = 3/5  - correct_margin, height = 2/3  - correct_margin) +
  cowplot::draw_plot_label(label = c("A. Whole-plant traits", "B. Leaf traits","C. Root traits"), 
                  size = 24, x = c(0,2/5,2/5), y = c(1, 1,2/3))

# ggsave("draft/bp_traits.png", bps_traits,width = 18, height = 12)

# https://stackoverflow.com/questions/64184348/change-line-type-for-certain-categories-in-ggplot
# ligne différente selon espèce


# par catégorie (scheme) ####
# autre tentative
# Leaf
L1 <- lapply(list("Hveg","LA","SLA","LDMC"), 
             bp_plast_simple)
L2 <- ggpubr::ggarrange(plotlist=L1, ncol = 5,nrow = 1,align = "hv") 

# Whole plant
WP1 <- lapply(list("plant_dry_mass",
                   "N","LMF",
                   "SMF","RMF"), 
              bp_plast_simple)
WP2 <- ggpubr::ggarrange(plotlist=WP1, ncol = 5,nrow = 1,
                         align = "hv") 



# Root
R1 <- lapply(list("diam","SRL", "RTD","RDMC","BI"), 
             bp_plast_simple)
R2 <- ggpubr::ggarrange(plotlist=R1, ncol = 5,nrow = 1) 


correct_margin <- 0.04
bps_traits <- cowplot::ggdraw() +
  cowplot::draw_plot(L2, x = 0, y = 2/3, width = 1, height = 1/3  - correct_margin) +
  cowplot::draw_plot(R2 , x = 0, y = 1/3, width = 1  , height = 1/3  - correct_margin) +
  cowplot::draw_plot(WP2, x = 0, y = 0, width = 1 , height = 1/3 - correct_margin) +

  cowplot::draw_plot_label(label = c("A. Ecological strategies", "B. Nutrient absorption","C. Whole-plant integration"), 
                  size = 24, x = c(0,0,0), y = c(1, 2/3, 1/3))

ggsave("draft/Figure 1.png", bps_traits,width = 18, height = 12)



# PhD Defense ####

bp_oral<-function(ftrait){
  bp_plast_simple(ftrait)+
    theme(text=element_text(size=50)) +
    theme(axis.text.x=element_blank())+
    geom_point(size = 4,aes(color = fertilization))+
    theme( plot.title=element_blank())
    
}


bps <- lapply(list("Hveg","LA","SLA","LDMC",
                   "diam","SRL", "RTD","RDMC","BI",
                   "plant_dry_mass",
                   "N","LMF",
                   "SMF","RMF"), bp_oral)

plots1 <- ggpubr::ggarrange(plotlist=bps, ncol = 5,nrow = 3)


ggsave("output/plot/phd_defence.png",plots1,width =20,height = 12)
