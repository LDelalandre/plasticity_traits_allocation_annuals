library(tidyverse)


info_sp %>% 
  group_by(pollinisation) %>% 
  summarize(n=n())


info_sp_traits <- info_sp %>% 
  merge(fMEAN)
  # filter(!code_sp == "FILAPYRA")
  

# CSR and SLA
info_sp_traits %>% 
  ggplot(aes(x = OrigValueStr, y = SLA, label = code_sp)) +
  geom_boxplot() +
  # geom_label(aes(color = treatment)) +
  geom_point() 
 
  
mod <- lm(SLA~OrigValueStr,data = info_sp_traits)
anova(mod)
