source("script/import_data.R")

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
