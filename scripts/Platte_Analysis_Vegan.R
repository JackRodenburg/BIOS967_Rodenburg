library(tidyverse)
library(vegan)
library(ggplot2)

#View(platte_fish_cleaned)
dat <- read.csv("data/Platte_Fish_Cleaned.csv")

fish_vegan <- dat %>% select(-X, -Length, -Weight) %>%
  group_by(Segment, Year, Month, Day, Species) %>%
  summarise(count_use=sum(Count)) %>%
  mutate(sample_id=paste(Segment, Year, Month, Day, sep="-")) %>%
  pivot_wider(id_cols=sample_id, names_from=Species, values_from=count_use)

fish_vegan[is.na(fish_vegan)] <- 0 #converts NA's to 0s
#View(fish_vegan)

numeric_data <- fish_vegan %>% select(-sample_id) #remove non-integer column
#numeric_data <- fish_vegan %>% select_if(is.numeric) #also removes any non-integer columns

## CALCULATES THE SHANNON DIVERSITY
shannon_div <- diversity(numeric_data, index = "shannon")

#PLOTTING A HISTOGRAM OF ALL SHANNON DIVERSITY NUMBERS IN shannon_div
ggplot(data.frame(Shannon_Diversity = shannon_div), aes(x = Shannon_Diversity)) +
  geom_histogram(binwidth = 0.1, fill = "dodgerblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Shannon Diversity", x = "Shannon Diversity", y = "Number of Samples")

#pairs shannon diversity index with sample id
shannon_data <- data.frame(sample_id = fish_vegan$sample_id, Shannon_Diversity = shannon_div)

#creates separate columns with segment and date as separate columns
shannon_data_extend <- shannon_data %>%
  separate(sample_id, into = c("Segment", "Year", "Month", "Day"), sep = "-") %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d"))

#Calculates each unique shannon diversity for each time point
shannon_avg_time <- shannon_data_extend %>%
  group_by(Date) %>%
  summarize(Avg_Shannon_Diversity = mean(Shannon_Diversity))

#shannon_avg_time

ggplot(shannon_avg_time, aes(x = Date, y = Avg_Shannon_Diversity)) +
  geom_line(color = "black") +
  #geom_point(color = "blue") +
  labs(title = "Shannon Diversity Over Time",
       x = "Date",
       y = "Average Shannon Diversity") +
  theme_minimal()

#CREATES PLOT 1



#calculates each shannon diversity for each unique location
shannon_avg_location <- shannon_data_extend %>%
  group_by(Segment) %>%
  summarize(Avg_Shannon_Diversity = mean(Shannon_Diversity))

#plots Shannon diversity for each location
ggplot(shannon_avg_location, aes(x = Segment, y = Avg_Shannon_Diversity)) +
  geom_line(color = "black") +
  geom_point(color = "blue") +
  labs(title = "Shannon Diversity Across Locations",
       x = "Segment",
       y = "Average Shannon Diversity") +
  theme_minimal()

shannon_avg_location


#####
# Prepare the data with Year and Shannon Diversity
shannon_yearly_max <- shannon_data_extend %>%
  group_by(Year) %>%
  summarize(Max_Shannon_Diversity = max(Shannon_Diversity)) %>%
  ungroup()  # to remove the grouping

shannon_yearly_max

# Plot the data
ggplot(shannon_yearly_max, aes(x = as.numeric(Year), y = Max_Shannon_Diversity)) +
  geom_line(group = 1, color = "blue") +  # Line plot
  geom_point(color = "red") +  # Add points
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 10), minor_breaks = NULL) +  # Pretty breaks for years
  labs(title = "Maximum Shannon Diversity Per Year",
       x = "Year",
       y = "Maximum Shannon Diversity") +
  theme_minimal()


# Prepare the data with Year and Shannon Diversity
shannon_yearly_max <- shannon_data_extend %>%
  group_by(Year) %>%
  summarize(Max_Shannon_Diversity = max(Shannon_Diversity)) %>%
  ungroup()  # to remove the grouping

shannon_yearly_min <- shannon_data_extend %>%
  group_by(Year) %>%
  summarize(Min_Shannon_Diversity = min(Shannon_Diversity)) %>%
  ungroup()  # to remove the grouping
#not really useful, all

# Prepare the data with Year and Shannon Diversity
shannon_yearly_avg <- shannon_data_extend %>%
  group_by(Year) %>%
  summarize(Average_Shannon_Diversity = avg(Shannon_Diversity)) %>%
  ungroup()  # to remove the grouping





# Assuming 'shannon_data_extend' contains columns 'Year', 'Segment', and 'Shannon_Diversity'
shannon_location_year <- shannon_data_extend %>%
  group_by(Segment, Year) %>%
  summarize(Avg_Shannon_Diversity = mean(Shannon_Diversity)) %>%
  ungroup()  # to remove the grouping after summarization

ggplot(shannon_location_year, aes(x = Segment, y = Avg_Shannon_Diversity, group = Year, color = as.factor(Year))) +
  geom_line() +  # Draw lines
  geom_point() +  # Add points
  scale_color_manual(values = rainbow(length(unique(shannon_location_year$Year)))) +  # Color lines uniquely
  labs(title = "Shannon Diversity Across Locations and Years",
       x = "Segment",
       y = "Average Shannon Diversity") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))  # Adjust legend text size





#separate out by date, make separate matrices
