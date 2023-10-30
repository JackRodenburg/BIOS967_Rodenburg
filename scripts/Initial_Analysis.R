library(dplyr)
#platte_fish <- read.csv("/Users/robin/Documents/GitHub/R_Project_2023/BIOS967_Rodenburg/data/Platte_Fish_Data.csv")

library(readr)
#platte_fish <- read_csv("/Users/robin/Documents/GitHub/R_Project_2023/BIOS967_Rodenburg/data/Platte_Fish_Data.csv")

#^^these didn't work properly

library(readxl)
platte_fish <- read_excel("~/Documents/Biology/Biology Research/Platte River/Platte River fish data.xlsx")
View(platte_fish)


platte_fish
#cleaning the No Fish, and Net did not Fish from the data (999 and 998)
platte_fish_cleaned <- platte_fish %>%
  select(Year, Month, Day, Species, Count) %>% #might add in Size
  filter(!Species %in% c(999,998))


#separating out all the differet types of sturegeon
sturgeon <- platte_fish_cleaned %>%
  filter(Species %in% c(20,24,26,27,28))

#separates just the endangered pallid sturgeon
pallid_sturgeon <- platte_fish_cleaned %>%
  filter(Species == 26)

#separates just the catfish
catfish <- platte_fish_cleaned %>%
  filter(Species %in% c(300,350,355,360,365,370))

#separates just the invasive carp (grass carp 176, common carp 178, silver carp 180, "Unidentified Asian Carp" 181, Bidhead carp 182)
carp <- platte_fish_cleaned %>%
  filter(Species %in% c(176,178,180,181,182))

year_range <- range(platte_fish_cleaned$Year)
year_range


unique_species <- unique(platte_fish$Species)
unique_months <- unique(platte_fish$Month)
unique_years <- unique(platte_fish$Year)


write.csv(platte_fish_cleaned, "/Users/robin/Documents/GitHub/R_Project_2023/BIOS967_Rodenburg/data/Platte_Fish_Cleaned.csv")


