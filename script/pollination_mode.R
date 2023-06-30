library(tidyverse)

info_sp <- read.csv2("data/info_species.csv") %>% 
  mutate(OrigValueStr = if_else(code_sp == "BUPLBALD", "sr", OrigValueStr))
info_sp %>% 
  group_by(pollinisation) %>% 
  summarize(n=n())


info_sp_traits <- info_sp %>% 
  merge(fMEAN)

# SCR and SLA
info_sp_traits %>% 
  ggplot(aes(x = OrigValueStr, y = LDMC, label = code_sp)) +
  geom_boxplot() +
  geom_label(aes(color = treatment)) 
  
