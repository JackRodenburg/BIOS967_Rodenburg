#install.packages("tidyberse")
#install.packages("wbstats")

library(tidyverse)
library(wbstats)

#using the billboard
billboard
?billboard

#filter the data by band
filter(billboard, artist == "Backstreet Boys, The")
#filter by band sing tidyverse
billboard %>% filter(artist=="Backstreet Boys, The")

#use verticle line | to mean or, & means and
billboard %>% filter(artist=="Backstreet Boys, The" | artist =="N'Sync")

#show me backstreet songs that come in under rank 50 in week 1
billboard %>% filter(artist == "Backstreet Boys, The" & wk1<50)

#Backstreet Boys and N'Sync songs taht came in top 50 in week 1'
billboard %>% filter(artist == "Backstreet Boys, The" | artist == "N'Sync") %>% filter(wk1<50)


#SELECT BY COLUMNS
billboard %>% select(artist, track, date.entered, wk1)
#UNselect columns by negative sign "-"
billboard %>% select(-artist, -date.entered)

##shifting from "WIDE" format to "LONG" format using pivot_longer()
billboard %>% pivot_longer(cols=starts_with("wk"), names_to = "week", values_to="rank")

#cols= assigns the columns you want to post
#values_to = assigns the variable name for the values in those columns
#names_to assigns the variable name from what you'

bb.summary= billboard %>%

  pivot_longer(cols=starts_with("wk"), names_to = "week", values_to="rank") %>%
  drop_na() %>%
  group_by(artist) %>%
  summarise(artist.best=min(rank), n.weeks=n())
  #summarise(n.weeks=n())

  #bb.summmary
  ggplot(bb.summary, aes(x=n.weeks, y=artist.best)) + geom_point()


#create a new variable using mutate()
#calculate rank change between week 1 and week 2 for each song
billboard %>%
    mutate(rank.diff = wk2-wk1) %>%
    select(artist, track, wk1, wk2, rank.diff)