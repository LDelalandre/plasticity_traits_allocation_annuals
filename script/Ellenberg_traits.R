source("script/import_data.R")

TSP <- t2_traits %>% 
  full_join(species_info)

TSP %>% 
  ggplot(aes(x = nutrient_requirement, y =SLA)) +
  geom_boxplot()+
  geom_point() +
  facet_wrap(~fertilization)
