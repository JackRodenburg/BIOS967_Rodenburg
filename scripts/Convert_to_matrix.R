library(tidyverse)

dat=read.csv("data/Platte_Fish_Cleaned.csv")

dat %>% select(-X, -Length, -Weight) %>%
  group_by(Segment, Year, Month, Day, Species) %>%
  summarise(count_use=sum(Count)) %>%
  mutate(sample_id=paste(Segment, Year, Month, Day, sep="-")) %>%
  pivot_wider(id_cols=sample_id, names_from=Species, values_from=count_use)

#dat$Count
dat[482,]
dat