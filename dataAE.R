setwd("/Users/shimonaseth/Desktop/rproject/data_AECalifornia")
#loading packages required to clean data
library(tidyverse)
library(purrr)
pacman::p_load(
  rio,        # Importing data  
  here,       # Relative file pathways  
  janitor,    # Data cleaning and tables
  lubridate,  # Working with dates
  matchmaker, # Dictionary-based cleaning
  epikit,     # age_categories() function
  tidyverse,  # Data management and visualization
  forecast,
  styler
)

#loading the csv file
data <- read.csv("adverse_events.csv")
data

#check data set
str(data)

# Remove commas and convert "Count" and "Population" to numeric
data$Count <- as.numeric(gsub(",", "", data$Count))
data$Population <- as.numeric(gsub(",", "", data$Population))

# Convert "Year" to Date format
data$Year <- as.Date(as.character(data$Year), format="%Y")

# Check for missing values
missing_values <- colSums(is.na(data))
print(missing_values[missing_values > 0])

#Viewing data
summary(data)
View(data)
head(data)
glimpse(data)

#Create plot

# Load necessary libraries
library(ggplot2)
library(RColorBrewer)


#Aggregate data by Year to get the total count
total_count_data <- data %>%
  group_by(Year) %>%
  summarise(total_count = sum(Count)) #to summarise stats for each group using sum function
glimpse(total_count_data)

# Create a visually appealing line plot for the total count over the years
ggplot(total_count_data, aes(x = Year, y = total_count)) +
  geom_line(color = "#0072B2", size = 1.5) +  # Line color and size
  geom_point(color = "#0072B2", size = 3) +  # Point color and size
  labs(title = "Total Count Trend Over the Years", y = "Total Count", x = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),  # Remove major gridlines
        panel.grid.minor = element_blank(),  # Remove minor gridlines
        panel.border = element_blank(),  # Remove plot border
        axis.line = element_line(color = "black"),  # Set axis line color
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Title styling
        axis.title = element_text(size = 14, face = "bold"))  # Axis label styling

pacman::p_load("gridExtra")
plot(tableGrob(total_count_data))



# Group by County, calculate the total count
county_summary <- data %>%
  group_by(County) %>%
  summarise(total_count = sum(ObsRate)) %>%
  arrange(desc(total_count)) %>%
  top_n(5, total_count)

county_summary <- data %>%
  group_by(County) %>%
  summarise(total_count = sum(ObsRate)) %>%
  arrange(desc(total_count)) %>%
  top_n(5, total_count)

# Use a darker shade of blue from the "Blues" palette
blue_palette <- colorRampPalette(rev(brewer.pal(9, "Blues")))(5)

# Create a bar plot with a range of blue shades(reorders the levels of a factor based on a numeric or character variable, -total_count: The numeric variable by which to reorder the factor levels. The negative sign (-) is used to sort in descending order.)
ggplot(county_summary, aes(x = reorder(County, -total_count), y = total_count, fill = as.factor(County))) +
  geom_bar(stat = "identity", color = "white") + 
  scale_fill_manual(values = blue_palette) +  # Use the defined blue color palette
  labs(title = "Top 5 Counties by Total Count of Adverse Events", y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.line = element_line(color = "black"),  # Set axis line color
        panel.grid.major.y = element_line(color = "darkgray"),  # Darken grid lines
        panel.border = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Bold title
        axis.title = element_text(size = 14, face = "bold"))  # Axis label styling

#For top 10 counties
county_summary <- data %>%
  group_by(County) %>%
  summarise(total_count = sum(ObsRate)) %>%
  arrange(desc(total_count)) %>%
  top_n(10, total_count)

# Use a darker shade of blue from the "Blues" palette
blue_palette <- colorRampPalette(rev(brewer.pal(9, "Blues")))(10)

# Create a bar plot with a range of blue shades
ggplot(county_summary, aes(x = reorder(County, -total_count), y = total_count, fill = as.factor(County))) +
  geom_bar(stat = "identity", color = "white") + 
  scale_fill_manual(values = blue_palette) +  # Use the defined blue color palette
  labs(title = "Top 10 Counties by Total Count of Adverse Events", y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.line = element_line(color = "black"),  # Set axis line color
        panel.grid.major.y = element_line(color = "darkgray"),  # Darken grid lines
        panel.border = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Bold title
        axis.title = element_text(size = 14, face = "bold"))  # Axis label styling

# Filter data for the statewide county
statewide_data <- data %>%
  filter(County == "STATEWIDE")

# Group by Year and count the occurrences
statewide_counts <- aggregate(County ~ Year, data = statewide_data, length)

# Rename the columns for clarity
colnames(statewide_counts) <- c('Year', 'Event_Counts')

# Filter data for the years 2005-2015
filtered_data <- data %>%
  filter(Year >= 2005 & Year <= 2015)

# Calculate ObsRate for each year and PSIDescription
result <- filtered_data %>%
  group_by(Year, PSIDescription) %>%
  summarise(ObsRate = sum(ObsRate) / n())

# Print the result
print(result)

# Plot the result using ggplot2
ggplot(result, aes(x = as.factor(Year), y = ObsRate, fill = PSIDescription)) +    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "ObsRate by PSIDescription from year 2005-2015",
         x = "Year",
         y = "ObsRate",
         fill = "PSIDescription",
         legend.title = "PSIDescription") +
    theme_minimal()

# Filter data for the years 2005-2015
filtered_data <- data %>%
  filter(Year >= 2005 & Year <= 2015)

# Calculate the percentage of each PSIDescription(PROBLEM in PLOT)
result <- filtered_data %>%
  group_by(PSIDescription) %>%
  summarise(Percentage = sum(ObsRate) / sum(filtered_data$ObsRate) * 100)

# Print the result
print(result)

# Plot the result using a pie chart
pie(result$Percentage, labels = paste(result$PSIDescription, "(", round(result$Percentage, 2), "%)"), main = "Percentage of Each PSIDescription", col = rainbow(length(result$Percentage), start = 0.1, end = 0.9))







# Create a line plot for the average ObsRate over the years
ggplot(agg_data, aes(x = Year, y = avg_ObsRate)) +
  geom_line(color = "#0072B2", size = 1.5) +
  labs(title = "Average ObsRate Trend Over the Years (STATEWIDE)",
       x = "Year",
       y = "Average ObsRate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"))
























































































  
  
  
  