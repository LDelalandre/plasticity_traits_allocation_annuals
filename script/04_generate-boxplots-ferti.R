
# Functions --------------------------------------------------------------------


getTraitsSignifFerti <- function(TABLE_PVAL) {
  traits_ferti_effect <- TABLE_PVAL %>% 
    filter(fertilization < 0.05) %>% 
    dplyr::select(Trait) %>% 
    rename(name=Trait) %>% 
    merge(trait_name) %>% 
    pull(trait)
}



getCompleteTraitNameUnits <- function() {
    data.frame(trait = c("plant_dry_mass","Hveg",
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
}





generateBbPlast <- function(ftrait,trait_title,traits_ferti_effect,traits_pop) {
  
  traits_pop %>%
    mutate(fertilization = if_else(fertilization == "N+", "F+","F-")) %>% 
    mutate(origin = factor(origin, levels = c("Fer","Nat"))) %>%
    mutate(fertilization = factor(fertilization, levels = c("F-","F+"))) %>%
    ggplot(aes_string(x="fertilization", y=ftrait,label = "pop"
    )) +
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





# Load data --------------------------------------------------------------------
source("script/01_import-data.R") # load traits_pop

TABLE_PVAL <- read.csv2("output/data/table_origin_ferti_effect")

trait_title <- getCompleteTraitNameUnits()

traits_ferti_effect <- getTraitsSignifFerti(TABLE_PVAL)



# Generate figure --------------------------------------------------------------

# Boxplots for leaf traits
L1 <- lapply(list("Hveg","LA","SLA","LDMC"), 
             generateBbPlast,
             trait_title = trait_title,
             traits_ferti_effect,
             traits_pop = traits_pop)

L2 <- ggpubr::ggarrange(plotlist=L1, ncol = 5,nrow = 1,align = "hv") 

# For whole plant traits
WP1 <- lapply(list("plant_dry_mass",
                   "N","LMF",
                   "SMF","RMF"), 
              generateBbPlast,
              trait_title = trait_title,
              traits_ferti_effect,
              traits_pop = traits_pop)

WP2 <- ggpubr::ggarrange(plotlist=WP1, ncol = 5,nrow = 1,
                         align = "hv") 



# For root traitss
R1 <- lapply(list("diam","SRL", "RTD","RDMC","BI"), 
             generateBbPlast,
             trait_title = trait_title,
             traits_ferti_effect,
             traits_pop = traits_pop)

R2 <- ggpubr::ggarrange(plotlist=R1, ncol = 5,nrow = 1) 


# Combining all
correct_margin <- 0.04
bps_traits <- cowplot::ggdraw() +
  cowplot::draw_plot(L2, x = 0, y = 2/3, width = 1, height = 1/3  - correct_margin) +
  cowplot::draw_plot(R2 , x = 0, y = 1/3, width = 1  , height = 1/3  - correct_margin) +
  cowplot::draw_plot(WP2, x = 0, y = 0, width = 1 , height = 1/3 - correct_margin) +
  cowplot::draw_plot_label(
    label = c("A. Ecological strategies", "B. Nutrient absorption","C. Whole-plant integration"), 
    size = 24, 
    x = c(0,0,0), 
    y = c(1, 2/3, 1/3))



# Export -----------------------------------------------------------------------

ggsave("draft/04_generate-boxplots-ferti_Figure1.png", 
       bps_traits,
       width = 18, height = 12,bg="white")
