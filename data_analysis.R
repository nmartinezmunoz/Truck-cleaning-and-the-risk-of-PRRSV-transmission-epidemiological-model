#load packages
library(viridis)
library(tidyverse)
library(ggpubr)
####load dataset and filter
#setting the work directory
setwd("C:/Users/.....") 
#load the data
data <- read.csv("cleandb.csv") #have only data 2019-2021
str(data)
#####data cleaning#####
data$entry <- as.POSIXct(data$entry, format = "%m/%d/%Y %H:%M") #dates for the entry column
data$exit <- as.POSIXct(data$exit, format = "%m/%d/%Y %H:%M")
data <- data[data$class != "UNCLASSIFIED" & data$class != "", ] #delete the unclassified
# Sort the data by 'truck' and 'entry' in ascending order
data <- data %>% arrange(truck, entry)
# Keep only the rows where the farm is different from the previous row's farm for each truck
data1 <- data %>%
  mutate(day = floor_date(entry, unit = "day")) %>%
  arrange(truck, day)%>%
  group_by(truck) %>%
  filter(farm != lag(farm, default = "")) %>%
  ungroup()
####sumary statistics####
#statistics for total min
summary_stats <- merged_data %>%
  group_by(class_to) %>%
  summarise(
    mean_time = mean(totalmin),
    median_time = median(totalmin),
    min_time = min(totalmin),
    max_time = max(totalmin),
    sd_time = sd(totalmin)
  )

#summary statistics for the data from october 2020 to september 2021
startdate <- as.Date("2020-10-01")
enddate <- as.Date("2021-09-30")
filtered_data <- data1 %>%
  filter(entry >= startdate, entry <= enddate)
summary_stats <- filtered_data %>%
  group_by(class) %>%
  summarise(
    mean_time = mean(totalmin),
    median_time = median(totalmin),
    min_time = min(totalmin),
    max_time = max(totalmin),
    sd_time = sd(totalmin)
  )

nrow(filtered_data)
length(unique(filtered_data$farm))
unique(filtered_data$truck)
farms <- farms %>%
  distinct(farm, .keep_all = TRUE)

# Calculate monthly statistics
monthly_stats <- merged_data %>%
  group_by(class_to, month = format(entry, "%Y-%m")) %>%
  summarise(
    mean_movements = n()
  )
monthly_stats <- merged_data %>%
  group_by(class_to) %>%
  summarise(
    mean_movements = median(totalmin)
  )
# Calculate daily statistics (mean, Q1, Q2, Q3 for number of movements)
daily_stats <- merged_data %>%
  group_by(truck, date = as.Date(entry)) %>%
  summarise(
    mean_movements = n(),
    q1_movements = quantile(n(), 0.25),
    median_movements = quantile(n(), 0.5),
    q3_movements = quantile(n(), 0.75)
  )
#####fig 1######
merged_data%>%
  group_by(class_to)%>%
  summarise(n=n())

merged_data %>%
  group_by(class_to) %>%
  summarise(n = n_distinct(to))

result <- merged_data %>%
  group_by(class_from, class_to) %>%
  summarise(n = n()) %>%
  group_by(class_from) %>%
  mutate(proportion = (n / 1687)*100)

write.csv(result,"Fig1.csv")#save fig 1 dataset
#####Figure 2#####
#first graph total number of movements per farm or site category
graph1 <- read.csv("total number of movements per category.csv")

plot1 <- merged_data %>%
  mutate(day = floor_date(entry, unit = "month")) %>%
  arrange(truck, day) %>%
  group_by(class_to, day) %>%
  summarise(n = n(), .groups = 'drop')
write.csv(plot1, "Figure2A.csv") #save fig 1 dataset

# Create a line plot with ggplot2
plot1$day <- as.Date(plot1$day)

#plot
plot1 <- ggplot(data = plot1, aes(x = day, y = n, color = class_to)) +
  geom_line(size=1) +
  labs(
    x = "Date",
    y = "Movements") +
  scale_color_manual(values = color_mapping, name = "Premise type") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_date(labels = scales::date_format("%b %Y"), breaks = scales::date_breaks("1 month")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
        axis.text.y =  element_text(size = 15),
        text = element_text(size = 15)) +
  geom_vline(xintercept = as.Date(c("2020-12-17", "2020-12-29", "2021-05-24", "2021-07-16")), 
             color = "brown3", linetype = "dashed", size=1)

#second plot of monthly movement per truck

graph2 <- merged_data %>%
  mutate(day = floor_date(entry, unit = "month")) %>%
  arrange(truck, day) %>%
  group_by(truck, day) %>%
  summarise(n = n(), .groups = 'drop')
write.csv(graph2, "Fig2B.csv")
mean(graph2$n)

graph2 %>%
  group_by(truck)%>%
  summarise(mean = mean(n))
monthly_sum <- graph2 %>%
  group_by(truck, year_month) %>%
  summarise(total_sum = sum(X1))

graph2$day <- as.Date(graph2$day)

# create vector 
truck <- c(rep("01", 10),
           rep("02", 10),
           rep("03", 10),
           rep("04", 10),
           rep("05", 10),
           rep("06", 8),
           rep("07", 10),
           rep("08", 8),
           rep("09", 10),
           rep("10", 10))

graph2$Vehicle <- truck #add the data to dataset

# Create a line plot
my_palette <- viridis_pal()(length(unique(graph2$Vehicle)))
graph2$day <- as.Date(graph2$day)
#plot 2
plot2 <- ggplot(data = graph2, aes(x = day, y = n, color = Vehicle)) +
  geom_line(size = 1) +
  labs(
    x = "Date",
    y = "Movements"
  ) +
  scale_color_manual(values = my_palette, name = "Truck Id") +  # Use viridis colors
  theme_bw() +
  scale_y_continuous(limits = c(0, 55)) +
  scale_x_date(labels = scales::date_format("%b %Y"), breaks = "1 month") +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 15),
    axis.text.y = element_text(size = 15),
    text = element_text(size = 15)) +
  geom_vline(
    xintercept = as.Date(c("2020-12-17", "2020-12-29", "2021-05-24", "2021-07-16")),
    color = "brown3",
    linetype = "dashed",
    size = 1
  )

# Print the plot
print(plot2)

# Combine both plots with the letter A and B
plot1y2 <- ggarrange(plot1, plot2, labels= c("A", "B"), ncol=1, nrow=2)
plot1y2

# Count the total number of unique values in the column 
freq_trucks <- filtered_data %>%
  mutate(day = floor_date(entry, unit = "month"))%>%
  group_by(truck, day) %>%
  summarize(sum = sum(n()), .groups = "drop",
  )
# Calculate the median distance
median_distance <- freq_trucks %>%
  summarise(
    median_distance = median(sum, na.rm = TRUE)
  )

# Calculate the IQR
iqr_distance <- freq_trucks %>%
  summarise(
    lower_iqr = quantile(sum, 0.25, na.rm = TRUE),
    upper_iqr = quantile(sum, 0.75, na.rm = TRUE)
  )

# Calculate the maximum distance
max_distance <- freq_trucks %>%
  summarise(
    max_distance = max(sum, na.rm = TRUE)
  )

# Extract the values
median_val <- median_distance$median_distance
lower_iqr_val <- iqr_distance$lower_iqr
upper_iqr_val <- iqr_distance$upper_iqr
max_val <- max_distance$max_distance

freq_trucks %>%
  summarize(
    min = min(sum),
    max = max(sum),
    median = median(sum),
    sd = sd(sum),
    mean = mean(sum)
  )
freq_trucks <- filtered_data %>%
  mutate(day = floor_date(entry, unit = "day"))%>%
  group_by(truck, day) %>%
  summarize(sum = sum(n()), .groups = "drop",
  )
freq_trucks %>%
  summarize(
    min = min(sum),
    max = max(sum),
    median = median(sum),
    sd = sd(sum),
    mean = mean(sum)
  )

freq_farm <- filtered_data %>%
  group_by(farm) %>%
  summarize(sum = sum(n()), .groups = "drop")
frequ_class <- filtered_data %>%
  group_by(class) %>%
  summarize(sum = sum(n()), .groups = "drop")

sum(frequ_class$sum)

#######FILTERING DATA######
#filter data before outbreak 1: 

dataset1 <- subset(data_cleaned, entry >= "" & entry <= "") 
unique(dataset1$truck[dataset1$farm == "XX"]) #filter the only farm that went to the farm affected
Adataset1 <- subset(dataset1, (truck %in% c("XX")) 

#fuction to add wash station column
add_wash_station_column <- function(data) {
  result <- data %>%
    group_by(truck)%>%
    arrange(truck, entry)%>%
    mutate(wash_station = ifelse(lag(farm, default = first(farm)) == "Truck Wash", 1, 0))%>%
    filter(farm != "Truck Wash")
  return(result)
}

Adataset1 <- add_wash_station_column(Adataset1)
write.csv(Adataset1, file="Adataset1.csv", row.names = FALSE)


# Repeat the same for other datasets


##############

#codes for farms
library(dplyr)
unique_farms <- data1 %>%
  distinct(farm, .keep_all = TRUE) %>%
  dplyr::select(farm, class)%>%
  arrange(farm)

unique_farms <- unique_farms %>%
  mutate(farm_number = row_number())

write.csv(unique_farms, "unique_farms.csv", row.names = FALSE)
#final dataset


#######Distances####
#####data cleaning every network

# Define a function for data cleaning and transformation

# Data cleaning to get the edges
clean_and_transform <- function(data1) {
  # Data cleaning to get the edges
  data_cleaned <- data1 %>%
    group_by(truck) %>%
    mutate(from = lag(farm)) %>% 
    slice(-1) %>% 
    ungroup()
  
  # Select relevant columns and rename them
  data_selected <- data_cleaned[, c(7, 10)]
  colnames(data_selected) <- c("to", "from")
  
  # Reorder columns
  data_selected <- data_selected[, c(2, 1)]
  
  # Convert columns to factors
  data_selected$from <- as.factor(data_selected$from)
  data_selected$to <- as.factor(data_selected$to)
  
  # Create frequency column
  data_with_freq <- data_selected %>%
    group_by(from, to) %>%
    tally(name = "freq") %>%
    ungroup()
  
  return(data_with_freq)
}

# Apply the function to your datasets
DS1 <- clean_and_transform(dataset1)
###apply the fuction for every dataset

####### creating the function to calculate distances
calculate_distances <- function(data, file1) {
  # Extract 'farm' columns
  file2 <- data_selected[, 1]
  colnames(file2) <- "farm"
  file3 <- data_selected[, 2]
  colnames(file3) <- "farm"
  
  # Join with 'file1' to get 'from' and 'to' coordinates
  source <- inner_join(file2, file1, by = "farm")
  source <- left_join(file2, file1, by = "farm")
  colnames(source) <- c("from", "lat1", "lon1", "type_from")
  dest <- left_join(file3, file1, by = "farm")
  colnames(dest) <- c("to", "lat2", "lon2","type_to")
  
  # Combine 'source' and 'dest' into 'tog'
  tog <- bind_cols(source, dest)
  colnames(tog) <- c("from", "lat1", "lon1","type_from", "to", "lat2", "lon2", "type_to")
  
  # Prepare 'source' and 'dest' for osrm
  source <- as.data.frame(source[, c("lon1", "lat1")])
  dest <- as.data.frame(dest[, c("lon2", "lat2")])
  
  # Calculate distances using osrmRoute for car
  route <- list()
  for (i in 1:nrow(tog)) {
    route[[i]] <- osrmRoute(
      src = c(source$lon1[i], source$lat1[i]),
      dst = c(dest$lon2[i], dest$lat2[i]),
      overview = FALSE,
      osrm.profile = "car"
    )
  }
  
  # Extract distances
  distances <- sapply(route, function(x) x["distance"])
  
  # Create a data frame
  distance_df <- data.frame(Distance = distances)
  
  # Add distances in kilometers to 'tog' data frame
  tog$distance <- distance_df$Distance
  return(tog)
}


distances <- calculate_distances(data_selected, file1)

##### add the distances to my data set data_cleaned#####

# Left join the datasets based on the "from" and "to" columns
merged_data <- left_join(data_cleaned, distances, by = c("from", "to"))

# Display the merged dataset
print(merged_data)

# Calculate the median distance
median_distance <- all_distances2 %>%
  summarise(
    median_distance = weighted.mean(distance, freq)
  )
############
median_distance <- median(merged_data$distance, na.rm = TRUE)
# Calculate the lower quartile (Q1)
q1_distance <- quantile(merged_data$distance, probs = 0.25, na.rm = TRUE)
# Calculate the upper quartile (Q3)
q3_distance <- quantile(merged_data$distance, probs = 0.75, na.rm = TRUE)
# Filter the rows where "class" contains "sow"
filtered_data <- merged_data[merged_data$type_to == "F", ]

# Calculate Q1, median (Q2), and Q3 for the "distance" column
quantile(filtered_data$distance, probs = 0.25, na.rm = TRUE)
quantile(filtered_data$distance, probs = 0.50, na.rm = TRUE)
quantile(filtered_data$distance, probs = 0.75, na.rm = TRUE)

hist(merged_data$distance)

# Calculate the IQR
iqr_distance <- merged_data %>%
  summarise(
    lower_iqr = quantile(distance, 0.25, weights = freq),
    upper_iqr = quantile(distance, 0.75, weights = freq)
  )

# Calculate the maximum distance
max_distance <- all_distances2 %>%
  summarise(
    max_distance = max(distance, na.rm = TRUE)
  )

# Extract the values
median_val <- median_distance$median_distance
lower_iqr_val <- iqr_distance$lower_iqr
upper_iqr_val <- iqr_distance$upper_iqr
max_val <- max_distance$max_distance


# Join for the "from" farm
all_distances2 <- all_distances2 %>%
  left_join(farms, by = c("from" = "farm"))

# Rename the added class column
all_distances2 <- all_distances2 %>%
  rename(from_class = class)

# Join for the "to" farm
all_distances2 <- all_distances2 %>%
  left_join(farms, by = c("to" = "farm"))

# Rename the added class column
all_distances2 <- all_distances2 %>%
  rename(to_class = class)




