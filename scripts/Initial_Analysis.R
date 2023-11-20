library(dplyr)
#platte_fish <- read.csv("/Users/robin/Documents/GitHub/R_Project_2023/BIOS967_Rodenburg/data/Platte_Fish_Data.csv")

library(readr)
#platte_fish <- read_csv("/Users/robin/Documents/GitHub/R_Project_2023/BIOS967_Rodenburg/data/Platte_Fish_Data.csv")

#^^these didn't work properly

library(readxl)
platte_fish <- read_excel("~/Documents/Biology/Biology Research/Platte River/Platte River fish data.xlsx")


#cleaning the No Fish, and Net did not Fish from the data (999 and 998)
platte_fish_cleaned <- platte_fish %>%
  select(Segment, Year, Month, Day, Length, Weight, Species, Count, Gear) %>% #might add in Size
  filter(!Species %in% c(999,998,997,NA)) %>%
  mutate(Count = as.numeric(as.character(Count))) %>%
  mutate(Length = as.numeric(as.character(Length))) %>%
  mutate(Weight = as.numeric(as.character(Weight)))

platte_fish_extended <- platte_fish_cleaned %>%
  mutate(
    Origin = case_when(
      Species %in% c(176, 178, 180, 181, 182) ~ "Non",  # Assign "Non" to the specified species codes
      TRUE ~ "Nat"  # Assign "Nat" to all other species
    )
  )


LLP_fish <- platte_fish_extended %>%
  filter(Segment == "LLP")

CP_fish <- platte_fish_extended %>%
  filter(Segment == "CP")

ULP_fish <- platte_fish_extended %>%
  filter(Segment == "ULP")

ELK_fish <- platte_fish_extended %>%
  filter(Segment == "ELK")

PMC_fish <- platte_fish_extended %>%
  filter(Segment == "PMC")

# Creating the dataset for native fish
native_fish <- platte_fish_extended %>%
  filter(Origin == "Nat") %>%
  filter(Segment != "NA")

# Creating the dataset for non-native fish
nonnative_fish <- platte_fish_extended %>%
  filter(Origin == "Non") %>%
  filter(Segment != "NA")

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


write.csv(platte_fish_cleaned, "data/Platte_Fish_Cleaned.csv", row.names=F) #removes proname column



