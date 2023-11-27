#load packages
library(igraph)
library(viridis)
library(tidyverse)
####load dataset and filter
#setting the work directory
setwd("C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data") 

data <- read.csv("cleandb.csv") #have only data 2019-2021
str(data)
#####data cleaning#####
data$entry <- as.POSIXct(data$entry, format = "%m/%d/%Y %H:%M") #dates for the entry column
data$exit <- as.POSIXct(data$exit, format = "%m/%d/%Y %H:%M")
data <- data[data$class != "UNCLASSIFIED" & data$class != "", ] #delete the unclassified and ASF as we agreed, 7706 obs, "TRUCK WASH""SOW""WEAN-TO-FINISH""NURSERY""GILT ISOLATION""FINISHER" 
#data <- data[data$totalmin >= 5, ] #delete all the rows with values under 5.

# Sort the data by 'truck' and 'entry' in ascending order
data <- data %>% arrange(truck, entry)

# Keep only the rows where the farm is different from the previous row's farm for each truck 
data1 <- data %>%
  mutate(day = floor_date(entry, unit = "day")) %>%
  arrange(truck, day)%>%
  group_by(truck) %>%
  filter(farm != lag(farm, default = "")) %>%
  ungroup()
data1[, 9] <- NULL
write.csv(data1, file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/data1.csv") #6667 observ

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

write.csv(result,"Fig1.csv")
#####Figure 2#####
#first graph total number of movements per farm or site category
graph1 <- read.csv("total number of movements per category.csv")

plot1 <- merged_data %>%
  mutate(day = floor_date(entry, unit = "month")) %>%
  arrange(truck, day) %>%
  group_by(class_to, day) %>%
  summarise(n = n(), .groups = 'drop')
write.csv(plot1, "Figure2A.csv")


# Create a line plot with ggplot2
plot1$day <- as.Date(plot1$day)

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

# Crear el vector "truck" con las repeticiones
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
library(ggpubr)
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
# how many farms each truck visit a day
farmsperday <- merged_data %>%
  mutate(entry_date = as.Date(entry)) %>%  # Extract the date from entry timestamp
  group_by(truck, entry_date) %>%
  summarise(num_farms_visited = n_distinct(to), .groups ="drop")
washstation <- merged_data %>%
  arrange(truck, entry)%>%
  filter(class_to == "Truck wash") %>%
  group_by(truck) %>%
  summarise(num_wash_visits = n(), .groups ="drop")


# Filter for rows where class is not "TRUCK WASH"
farms_before_wash <- merged_data %>%
  filter(type_to != "Truck wash") %>%
  mutate(entry_date = as.Date(entry)) %>%
  group_by(truck, entry_date) %>%
  summarise(num_farms_visited = n_distinct(to), .groups = 'drop')

median(farms_before_wash$num_farms_visited)
# Calculate the difference in days between consecutive entries for each truck
movement_frequency <- merged_data %>%
  filter(type_to != "Truck wash") %>%
  group_by(truck) %>%
  arrange(entry) %>%
  mutate(entry_date = as.Date(entry)) %>%
  summarise(movement_frequency = mean(diff(entry_date, units = "days")))

mean(movement_frequency$movement_frequency)

#filter data before outbreak 1: Tilney

NAtilney <- subset(data_cleaned, entry >= "2020-10-17 00:00:00" & entry <= "2020-12-17 23:59:59") #399 obs 10 variables

unique(NAtilney$truck[NAtilney$farm == "Tilney"]) #filter the only farm that went to the farm affected

Atilney <- subset(NAtilney, (truck %in% c("Tilney")) ) #solo 34 obs de mov del truck

unique(Atilney$farm) #8 different farms

add_wash_station_column <- function(data) {
  result <- data %>%
    group_by(truck)%>%
    arrange(truck, entry)%>%
    mutate(wash_station = ifelse(lag(farm, default = first(farm)) == "Truck Wash Springfield", 1, 0))%>%
    filter(farm != "Truck Wash Springfield")
  return(result)
}


Atilney <- add_wash_station_column(Atilney)
write.csv(Atilney, file="Atilney.csv", row.names = FALSE)
#filter data after outbreak 1:tilney

NBtilney <- subset(data_cleaned, entry >= "2020-12-17 00:00:00" & entry <= "2021-02-17 23:59:59") #395 obs

unique(NBtilney$truck[NBtilney$farm == "Tilney"]) #"Cull 1TON","Lance Semi","Sundlee Semi","Tilney" fue a la granja

Btilney <- subset(NBtilney, !(truck %in% c("Willow Trailer", "Kaylor Trailer", "Brewster", 
                          "Site 2", "Huiras","Cliff Trailer")) ) #138 obs
unique(Btilney$farm) #15 farms 
Btilney <- add_wash_station_column(Btilney)
write.csv(Btilney, file="Btilney.csv", row.names = FALSE)

#filter data before outbreak 2:huiras

NAhuiras <- subset(data_cleaned, entry >= "2020-10-29 00:00:00" & entry <= "2020-12-29 23:59:59") #431 obs

unique(NAhuiras$truck[NAhuiras$farm == "Huiras"]) #camiones que fueron a huiras:"Cull 1TON","Huiras","Lance Semi","Sundlee Semi","Tilney" 

Ahuiras <- subset(NAhuiras, (truck %in% c("Cull 1TON","Huiras","Lance Semi","Sundlee Semi","Tilney")) ) # 187 obs for all the trucks

unique(Ahuiras$farm) #15 farms 

Ahuiras <- add_wash_station_column(Ahuiras)
write.csv(Ahuiras, file="Ahuiras.csv", row.names = FALSE)

#filter data after outbreak 2: huiras

NBhuiras <- subset(data_cleaned, entry >= "2020-12-29 00:00:00" & entry <= "2021-02-28 23:59:59") #383 obs

unique(NBhuiras$truck[NBhuiras$farm == "Huiras"]) #camiones que fueron a huiras:"Cull 1TON","Huiras","Lance Semi","Sundlee Semi" 

Bhuiras <- subset(NBhuiras, (truck %in% c("Cull 1TON","Huiras","Lance Semi","Sundlee Semi" )) ) # 199 obs for all the trucks
unique(Bhuiras$farm) #16 farms 
Bhuiras <- add_wash_station_column(Bhuiras)
write.csv(Bhuiras, file="Bhuiras.csv", row.names = FALSE)

#filter data before outbreak 3: brewster

NAbrewster <- subset(data_cleaned, entry >= "2021-03-24 00:00:00" & entry <= "2021-05-24 23:59:59") #174 obs

unique(NAbrewster$truck[NAbrewster$farm == "Brewster"]) #camiones que fueron a brewster:"Brewster","Lance Semi","Sundlee Semi"

Abrewster <- subset(NAbrewster, (truck %in% c("Brewster","Lance Semi","Sundlee Semi")) ) # 56 obs for all the trucks

unique(Abrewster$farm) #8 farms 

Abrewster <- add_wash_station_column(Abrewster)
write.csv(Abrewster, file="Abrewster.csv", row.names = FALSE)

#filter data after outbreak 3: brewster
NBbrewster <- subset(data_cleaned, entry >= "2021-05-24 00:00:00" & entry <= "2021-07-24 23:59:59") #412 obs

unique(NBbrewster $truck[NBbrewster$farm == "Brewster"]) #camiones que fueron a huiras:"Brewster","Cull 1TON","Lance Semi"

Bbrewster <- subset(NBbrewster , (truck %in% c("Brewster","Cull 1TON","Lance Semi")) ) # 96 obs for all the trucks

unique(Bbrewster$farm) #16 farms 
Bbrewster <- add_wash_station_column(Bbrewster)
write.csv(Bbrewster, file="Bbrewster.csv", row.names = FALSE)

#filter data before outbreak 4:  kaylor

NAkaylor <- subset(data_cleaned, entry >= "2021-05-21 00:00:00" & entry <= "2021-07-21 23:59:59") #397 obs
unique(NAkaylor$truck[NAkaylor$farm == "Kaylor"]) #camiones que fueron a brewster:"Kaylor Trailer","Lance Semi"  

Akaylor <- subset(NAkaylor, (truck %in% c("Kaylor Trailer","Lance Semi")) ) # 93 obs for all the trucks
unique(Akaylor$farm) #13 farms 
Akaylor <- add_wash_station_column(Akaylor)
write.csv(Akaylor, file="Akaylor.csv", row.names = FALSE)


#filter data after outbreak 4:  kaylor
NBkaylor <- subset(data_cleaned, entry >= "2021-07-21 00:00:00" & entry <= "2021-09-21 23:59:59") #388 obs

unique(NBkaylor$truck[NBkaylor$farm == "Kaylor"]) #camiones que fueron a kaylor:"Kaylor Trailer"

Bkaylor <- subset(NBkaylor, (truck %in% c("Kaylor Trailer" )) ) # 30 obs for all the trucks
unique(Bkaylor$farm) #4 farms 
Bkaylor <- add_wash_station_column(Bkaylor)
write.csv(Bkaylor, file="Bkaylor.csv", row.names = FALSE)


##### Define a function for running the infection probability analysis######
nSim <- 1000
probInf <- 0.428


# Your doSim function
doSim <- function(mov, wash_station) {
  infections <- rep(0, length(mov))
  for (i in 1:length(mov)) {
    closest_tilney_idx <- which(mov == "Tilney" & seq_along(mov) > i)[1]
    if (!is.na(closest_tilney_idx)) {
      if (1 %in% wash_station[i:(closest_tilney_idx - 1)]) {
        infections[i] <- rbinom(1, 1, probInf * (1 - 0.9))
      } else {
        infections[i] <- rbinom(1, 1, probInf)
      }
    } else {
      infections[i] <- rbinom(1, 1, probInf * (1 - 0.9 * wash_station[i]))
    }
  }
  return(infections)
}

# Generate res matrix
mov <- Btilney$farm
UniqueFarm <- unique(mov)
wash_station <- Btilney$wash_station
res <- replicate(nSim, doSim(mov, wash_station))

# Create a data frame to store the results
results_Btilney <- data.frame(Farm = UniqueFarm)

# Calculate the mean and uncertainty for each farm
for (i in 1:length(UniqueFarm)) {
  farm_name <- UniqueFarm[i]
  farm_rows <- res[which(mov == farm_name), ]
  
  # Calculate the mean for the current farm
  mean_infections <- mean(farm_rows)
  
  # Custom bootstrapping function for binary data
  bootstrap_function <- function(data) {
    # Sample with replacement to create a bootstrap sample
    bootstrap_sample <- sample(data, replace = TRUE)
    # Calculate the mean for the bootstrap sample
    mean_infections <- mean(bootstrap_sample)
    return(mean_infections)
  }
  
  # Number of bootstrap samples
  n_bootstrap <- 1000
  
  # Initialize a vector to store bootstrap means
  bootstrap_means <- numeric(n_bootstrap)
  
  # Perform bootstrapping
  for (j in 1:n_bootstrap) {
    # Apply the custom bootstrapping function
    bootstrap_means[j] <- bootstrap_function(farm_rows)
  }
  
  # Calculate quantiles for the bootstrap means
  bootstrap_mean_quantiles <- quantile(bootstrap_means, c(0.025, 0.975))
  
  # Add results to the data frame
  results_Btilney[i, "Mean"] <- mean_infections
  results_Btilney[i, "Lower_CI"] <- bootstrap_mean_quantiles[1]
  results_Btilney[i, "Upper_CI"] <- bootstrap_mean_quantiles[2]
}

# Print the results
print(results_Btilney)


# Repeat the same for other datasets (Btilney, Ahuiras, Bhuiras, Abrewster, Bbrewster, Akaylor, Bkaylor)


# Create a histogram of the infection probabilities
#plot(infectionProbs)


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

#Tilney
final_tilney <- read.csv("tilney_final.csv")#first outbreak
   
   library(viridis)
 # Plot with reversed order of columns in facet_wrap
final_tilney$code <- as.character(final_tilney$code)
final_tilney$Status <- factor(final_tilney$Status,
                              levels = c("Before", "After"))
order_til <- c("F1", "F5", "F8", "F9", "F26", "F28", "F35", "F53", "F55", "F56", "F69", "F74", "F75", "F77", "F83", "F84")

gtilney <- ggplot(data = final_tilney, aes(x = factor(code, levels = order_til), y = Probability, fill = Class.farm)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Gilt Isolation" = "#472D7BFF",
                               "Nursery" = "#2A788EFF",
                               "Sow" = "#35B779FF",
                               "Wean-to-finish" = "#FDE725FF")) +
  facet_wrap(~ Status, ncol = 2) +
  labs(x = "Farm", y = "Probability", fill = "Farm class") +
  geom_text(data = subset(final_tilney, code == "F74"), aes(label = "*"), vjust = -1, color = "black")

gtilney
library(ggplot2)


#######PLOT_PROBS#######

#HUIRAS

final_huiras <- read.csv("huiras_final.csv")#second outbreak 
final_huiras$code <- as.character(final_huiras$code)
final_huiras$Status <- factor(final_huiras$Status,
                               levels = c("Before", "After"))  
order_hui <- c("F1", "F5", "F8", "F9", "F25", "F26", "F28", "F29", "F35", "F53", "F55", "F56", "F62", "F65", "F69", "F74","F75", "F77","F83", "F84")
ghuiras <- ggplot(data = final_huiras, aes(x = factor(code,levels=order_hui), y = Probability, fill = Class.farm)) +
   geom_bar(stat="identity") +
   scale_fill_manual(values = c("Finisher" = "#440154FF" ,
                                "Gilt isolation"= "#472D7BFF",
                                "Nursery"= "#2A788EFF" ,
                                "Sow"= "#35B779FF",
                                "Wean-to-finish"= "#FDE725FF" ))+
   facet_wrap(~Status, ncol = 2) +
   labs(x = "Farm", y = "Probability", fill = "Farm class")+
   geom_text(data = subset(final_huiras, code == "F26"), aes(label = "*"), vjust = -1.5, color = "black")
  

#BREWSTER
final_brewster <- read.csv("brewster_final.csv")#third outbreak 
final_brewster$code <- as.character(final_brewster$code)
final_brewster$status <- factor(final_brewster$status,
                              levels = c("Before", "After"))  
order_bre <- c("F1", "F5", "F8", "F15", "F18", "F26", "F30", "F36", "F43", "F55", "F56", "F72", "F74","F77","F79", "F83")
gbrewster <- ggplot(data = final_brewster, aes(x = factor(code,levels=order_bre), y = Probability, fill = Class.farm)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("Finisher" = "#440154FF" ,
                               "Gilt isolation"= "#472D7BFF",
                               "Nursery"= "#2A788EFF" ,
                               "Sow"= "#35B779FF",
                               "Wean-to-finish"= "#FDE725FF" ))+
  facet_wrap(~status, ncol = 2) +
  labs(x = "Farm", y = "Probability", fill = "Farm class")+
  geom_text(data = subset(final_brewster, code == "F5"), aes(label = "*"), vjust = -1.5, color = "black") 

#KAYLOR
  
final_kaylor <- read.csv("kaylor_final.csv")#second outbreak 
final_kaylor$code <- as.character(final_kaylor$code)
final_kaylor$Status <- factor(final_kaylor$status,
                              levels = c("Before", "After"))  
order_kay <- c("F1","F3", "F5", "F8", "F16", "F26", "F30", "F31", "F39", "F40", "F49", "F55", "F56", "F72", "F77","F79","F83")
gkaylor <- ggplot(data = final_kaylor, aes(x = factor(code,levels=order_kay), y = Probability, fill = Class.farm)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c(
                               "Gilt Isolation"= "#472D7BFF",
                               "Nursery"= "#2A788EFF" ,
                               "Sow"= "#35B779FF",
                               "Wean-to-finish"= "#FDE725FF" ))+
  facet_wrap(~status, ncol = 2) +
  labs(x = "Farm", y = "Probability", fill = "Farm class")+
  geom_text(data = subset(final_kaylor, code == "F30"), aes(label = "*"), vjust = -1.5, color = "black")


 
##################network analysis#####

library(igraph)

#outbreak1
#add a column wit outbreak
NAbrewster$movementcode <- "NAbrewster"
NAhuiras$movementcode <- "NAhuiras"
NAkaylor$movementcode <- "NAkaylor"
NAtilney$movementcode <- "NAtilney"
NBbrewster$movementcode <- "NBbrewster"
NBhuiras$movementcode <- "NBhuiras"
NBkaylor$movementcode <- "NBkaylor"
NBtilney$movementcode <- "NBtilney"

totalnetwork <- rbind(NAbrewster, NAhuiras, NAkaylor, NAtilney, NBbrewster, NBhuiras, NBkaylor, NBtilney)
str(totalnetwork)

write.csv(totalnetwork, file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/totalnetwork.csv")

edges <- read.csv(file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/networkanalysis/edges.csv")

edges <- data.table(edges)

ed <- ddply(edges, .(edges$source, edges$target, edges$movementcode), nrow)
names(ed) <- c("source", "target", "db", "total")
# Rearrange the columns
ed <- ed[,c(1,2,4,3)]
write.csv(ed,file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/ed.csv" )
nodes <- read.csv(file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/networkanalysis/totalnodes.csv")
ed <- read.csv(file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/networkanalysis/ed.csv")
# How many node IDs (sources and targets) were involved in the trades?
ed %>% filter(db == "NAtilney")%>%
  tally(n_distinct(c_across(cols = c("source","target"))))
ed %>% filter(db == "NBtilney")%>%
  tally(n_distinct(c_across(cols = c("source","target"))))
ed %>% filter(db == "NAhuiras")%>%
  tally(n_distinct(c_across(cols = c("source","target"))))
ed %>% filter(db == "NBhuiras")%>%
  tally(n_distinct(c_across(cols = c("source","target"))))
ed %>% filter(db == "NAbrewster")%>%
  tally(n_distinct(c_across(cols = c("source","target"))))
ed %>% filter(db == "NBbrewster")%>%
  tally(n_distinct(c_across(cols = c("source","target"))))
ed %>% filter(db == "NAkaylor")%>%
  tally(n_distinct(c_across(cols = c("source","target"))))
ed %>% filter(db == "NBkaylor")%>%
  tally(n_distinct(c_across(cols = c("source","target"))))
#How many movements were recorded in each timestamp?
ed %>% filter(db == "NAtilney")%>%
  tally(nrow(.))
ed %>% filter(db == "NBtilney")%>%
  tally(nrow(.))
ed %>% filter(db == "NAhuiras")%>%
  tally(nrow(.))
ed %>% filter(db == "NBhuiras")%>%
  tally(nrow(.))
ed %>% filter(db == "NAbrewster")%>%
  tally(nrow(.))
ed %>% filter(db == "NBbrewster")%>%
  tally(nrow(.))
ed %>% filter(db == "NAkaylor")%>%
  tally(nrow(.))
ed %>% filter(db == "NBkaylor")%>%
  tally(nrow(.))
  
#test igraph

a1 <- as.data.frame(unique(tilney1$to))
a2 <- as.data.frame(a1[order(a1$`unique(tilney1$to)`),])
a2$id <- 1:40
colnames(a2) <- c("farm", "id")
colnames(a3) <- c("farm", "id","class")
a3 <- merge(a2, unique_farms, by = "farm", all.x = TRUE)
a3 <- a3[,-c(3,5)]#codes and class of all the nodes for the 8 networks (84 nodes)

#Tilney1 / vertexg1 and edgesg1
nod<- as.data.frame(unique(tilney1$from))
colnames(nod)<- "farm"
vertexg1 <- join(nod, a3, by= "farm")

edgesg1 <- tilney1
edgesg1 <- merge(edgesg1, vertexg1, by.x = "from", by.y = "farm", all.x = TRUE )
edgesg1 <- merge(edgesg1, vertexg1, by.x = "to", by.y = "farm", all.x = TRUE ) 
colnames(edgesg1) <- c("target", "source", "freq", "dist","from", "class","to","cass")
edgesg1 <- edgesg1[,-c(1, 2, 6,8)] #final with 398 mov
edgesg1 <- edgesg1[,c(3,4,1,2)]#rearrange


#tilney2 /vertexg2 and edgesg2
nod<- as.data.frame(unique(tilney2$from))
colnames(nod)<- "farm"
vertexg2 <- join(nod, a3, by= "farm")

edgesg2 <- tilney2
edgesg2 <- merge(edgesg2, vertexg1, by.x = "from", by.y = "farm", all.x = TRUE )
edgesg2 <- merge(edgesg2, vertexg1, by.x = "to", by.y = "farm", all.x = TRUE ) 
colnames(edgesg2) <- c("target", "source", "freq", "dist","from", "class","to","cass")
edgesg2 <- edgesg2[,-c(1, 2, 6,8)] #final with 398 mov
edgesg2 <- edgesg2[,c(3,4,1,2)]#rearrange

#huiras1 /vertexg3 and edgesg3
nod<- as.data.frame(unique(tilney2$from))
colnames(nod)<- "farm"
vertexg2 <- join(nod, a3, by= "farm")

edgesg2 <- tilney2
edgesg2 <- merge(edgesg2, vertexg1, by.x = "from", by.y = "farm", all.x = TRUE )
edgesg2 <- merge(edgesg2, vertexg1, by.x = "to", by.y = "farm", all.x = TRUE ) 
colnames(edgesg2) <- c("target", "source", "freq", "dist","from", "class","to","cass")
edgesg2 <- edgesg2[,-c(1, 2, 6,8)] #final with 398 mov
edgesg2 <- edgesg2[,c(3,4,1,2)]#rearrange

#huiras2 /vertexg4 and edgesg4
nod<- as.data.frame(unique(tilney2$from))
colnames(nod)<- "farm"
vertexg2 <- join(nod, a3, by= "farm")

edgesg2 <- tilney2
edgesg2 <- merge(edgesg2, vertexg1, by.x = "from", by.y = "farm", all.x = TRUE )
edgesg2 <- merge(edgesg2, vertexg1, by.x = "to", by.y = "farm", all.x = TRUE ) 
colnames(edgesg2) <- c("target", "source", "freq", "dist","from", "class","to","cass")
edgesg2 <- edgesg2[,-c(1, 2, 6,8)] #final with 398 mov
edgesg2 <- edgesg2[,c(3,4,1,2)]#rearrange
#brewster1 /vertexg5 and edgesg5
nod<- as.data.frame(unique(tilney2$from))
colnames(nod)<- "farm"
vertexg2 <- join(nod, a3, by= "farm")

edgesg2 <- tilney2
edgesg2 <- merge(edgesg2, vertexg1, by.x = "from", by.y = "farm", all.x = TRUE )
edgesg2 <- merge(edgesg2, vertexg1, by.x = "to", by.y = "farm", all.x = TRUE ) 
colnames(edgesg2) <- c("target", "source", "freq", "dist","from", "class","to","cass")
edgesg2 <- edgesg2[,-c(1, 2, 6,8)] #final with 398 mov
edgesg2 <- edgesg2[,c(3,4,1,2)]#rearrange

#brewster2 /vertexg6 and edgesg6
nod<- as.data.frame(unique(tilney2$from))
colnames(nod)<- "farm"
vertexg2 <- join(nod, a3, by= "farm")

edgesg2 <- tilney2
edgesg2 <- merge(edgesg2, vertexg1, by.x = "from", by.y = "farm", all.x = TRUE )
edgesg2 <- merge(edgesg2, vertexg1, by.x = "to", by.y = "farm", all.x = TRUE ) 
colnames(edgesg2) <- c("target", "source", "freq", "dist","from", "class","to","cass")
edgesg2 <- edgesg2[,-c(1, 2, 6,8)] #final with 398 mov
edgesg2 <- edgesg2[,c(3,4,1,2)]#rearrange

#kaylor1 /vertexg7 and edgesg7
nod<- as.data.frame(unique(tilney2$from))
colnames(nod)<- "farm"
vertexg2 <- join(nod, a3, by= "farm")

edgesg2 <- tilney2
edgesg2 <- merge(edgesg2, vertexg1, by.x = "from", by.y = "farm", all.x = TRUE )
edgesg2 <- merge(edgesg2, vertexg1, by.x = "to", by.y = "farm", all.x = TRUE ) 
colnames(edgesg2) <- c("target", "source", "freq", "dist","from", "class","to","cass")
edgesg2 <- edgesg2[,-c(1, 2, 6,8)] #final with 398 mov
edgesg2 <- edgesg2[,c(3,4,1,2)]#rearrange

#kaylor2 /vertexg8 and edgesg8
nod<- as.data.frame(unique(tilney2$from))
colnames(nod)<- "farm"
vertexg2 <- join(nod, a3, by= "farm")

edgesg2 <- tilney2
edgesg2 <- merge(edgesg2, vertexg1, by.x = "from", by.y = "farm", all.x = TRUE )
edgesg2 <- merge(edgesg2, vertexg1, by.x = "to", by.y = "farm", all.x = TRUE ) 
colnames(edgesg2) <- c("target", "source", "freq", "dist","from", "class","to","cass")
edgesg2 <- edgesg2[,-c(1, 2, 6,8)] #final with 398 mov
edgesg2 <- edgesg2[,c(3,4,1,2)]#rearrange

library(igraph)

vertexg1 <- vertexg1[, -1]
g1 <- graph_from_data_frame(d=edgesg1,vertices=vertexg1, directed = TRUE)

# Basic Network Properties
nodes_number <- vcount(g1)
edges_number <- ecount(g1)
edge_density <- edge_density(g1)
diameter <- diameter(g1)
avg_path_length <- mean_distance(g1, directed = TRUE)
transitivity_value <- transitivity(g1, type = "global")

# Mean, Min, Max of Degree
degree_values <- degree(g1)
mean_degree <- mean(degree_values)
min_degree <- min(degree_values)
max_degree <- max(degree_values)

# In-degree and Out-degree
in_degree_values <- degree(g1, mode = "in")
out_degree_values <- degree(g1, mode = "out")
mean_in_degree <- mean(in_degree_values)
mean_out_degree <- mean(out_degree_values)
min_degree <- min(in_degree_values)
min_degree <- min(out_degree_values)
max(in_degree_values)
max(out_degree_values)
# Closeness Centrality
closeness_values <- closeness(g1, mode = "all")
mean_closeness <- mean(closeness_values)
mean_closeness_in <- mean(closeness(g1, mode = "in"))
mean_closeness_out <- mean(closeness(g1, mode = "out"))

# Betweenness Centrality
betweenness_values <- betweenness(g1, directed = TRUE)
mean_betweenness <- mean(betweenness_values)

# Clustering Coefficient
clustering_coefficient_values <- transitivity(g1, type = "local")
clustering_coefficient_values <- transitivity(g1, type = "global")
mean_clustering_coefficient <- mean(clustering_coefficient_values)

# Weak Components
weak_components <- decompose(g1, mode = "weak")
weak_components_number <- length(weak_components)
weak_component_largest_size <- max(sapply(weak_components, vcount))

# Strong Components
strong_components <- decompose(g1, mode = "strong")
strong_components_number <- length(strong_components)
strong_component_largest_size <- max(sapply(strong_components, vcount))






# Node degree
node_degrees <- degree(g1, mode = "in")  # "in" for in-degree, "out" for out-degree
# Centrality measures
g1betweenness_centrality <- betweenness(g1)
g1closeness_centrality <- closeness(g1, mode = "in")  # "in" for in-closeness, "out" for out-closeness
g1eigenvector_centrality <- eigen_centrality(g1, directed = TRUE)$vector

# Step 5: Visualize the graph
plot(g1, edge.arrow.size = 0.5, edge.width = E(g1)$weight, edge.label = E(g1)$dist,
     vertex.color = V(g1)$class, vertex.label = V(g1)$id,
     vertex.size = node_degrees)

# Step 6: Perform community detection (optional)
communities <- cluster_louvain(g1)
membership <- membership(communities)

median(edgesg1$dist)
median(edgesg2$dist)
median(edgesg3$dist)
median(edgesg4$dist)
median(edgesg5$dist)
median(edgesg6$dist)
median(edgesg7$dist)
median(edgesg8$dist)
g1

#show all the edges to or from "Tilney" id: 74
incident(g1, "74", mode = c("all"))
#relationships between farms
neighbors(g1, "74", mode=c("all"))
#network density
graph.density(g1)
#important or influential vertices
#degree
degree<- degree(g1, mode=c("out"))
mdegree <- median(degree)


NWg1<-as_long_data_frame(g1)
write.csv(NWg1, file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/NWg1.csv")

#G2
g2 <- graph_from_data_frame(edgesg2, directed = TRUE, vertices = vertexg2)
g2.2 <- graph_from_data_frame(edgesg2, directed = FALSE, vertices = vertexg2)
E(g2)$weight <- E(g2)$Freq
#checking igraph objects
is.directed(g2)
is.weighted(g2)
#show all the edges to or from "Tilney" id: 74
incident(g2, "74", mode = c("all"))
#relationships between farms
neighbors(g2, "74", mode=c("all"))
#network density
graph.density(g2)
#important or influential vertices
#degree
degree<- degree(g2, mode=c("all"))
mdegree <- median(degree)

betweenness(g2, directed =TRUE)
betweenness(g2, directed=TRUE, normalized=(TRUE))
g2
#identifying neighbors in common
x <- neighbors(g2, "74", mode=c("all"))
y <- neighbors(g2, "26", mode=c("all"))
intersection(x,y)
#important or influential vertices


NWg2<-as_long_data_frame(g2)
write.csv(NWg2, file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/NWg2.csv")

g3 <- graph_from_data_frame(edgesg3, directed = TRUE, vertices = vertexg3)
g3.2 <- graph_from_data_frame(edgesg3, directed = FALSE, vertices = vertexg3)
E(g3)$weight <- E(g3)$Freq
g3

#checking igraph objects
is.directed(g3)
is.weighted(g3)
#show all the edges to or from "Tilney" id: 74
incident(g3, "26", mode = c("all"))
#relationships between farms
neighbors(g3, "26", mode=c("all"))
#network density
graph.density(g3)
#important or influential vertices
#degree
degree<- degree(g3, mode=c("all"))
mdegree <- median(degree)

betweenness(g3, directed =TRUE)
betweenness(g3, directed=TRUE, normalized=(TRUE))
NWg3<-as_long_data_frame(g3)
write.csv(NWg3, file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/NWg3.csv")

g4 <- graph_from_data_frame(edgesg4, directed = TRUE, vertices = vertexg4)
g4.2 <- graph_from_data_frame(edgesg4, directed = FALSE, vertices = vertexg4)
E(g1)$weight <- E(g4)$Freq
g4
E(g1)$weight <- E(g1)$Freq
g1
#checking igraph objects
is.directed(g4)
is.weighted(g4)
#show all the edges to or from "Tilney" id: 74
incident(g4, "26", mode = c("all"))
#relationships between farms
neighbors(g4, "26", mode=c("all"))
#network density
graph.density(g4)
#important or influential vertices
#degree
degree<- degree(g4, mode=c("all"))
mdegree <- median(degree)

betweenness(g4, directed =TRUE)
betweenness(g4, directed=TRUE, normalized=(TRUE))
NWg4<-as_long_data_frame(g4)
write.csv(NWg4, file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/NWg4.csv")

#G5 BRewster ID 5
g5 <- graph_from_data_frame(edgesg5, directed = TRUE, vertices = vertexg5)
g5.2 <- graph_from_data_frame(edgesg5, directed = FALSE, vertices = vertexg5)
E(g5)$weight <- E(g5)$Freq
g5
#checking igraph objects
is.directed(g5)
is.weighted(g5)
#show all the edges to or from "Tilney" id: 74
incident(g5, "5", mode = c("all"))
#relationships between farms
neighbors(g5, "5", mode=c("all"))
#network density
graph.density(g5)
#important or influential vertices
#degree
degree<- degree(g5, mode=c("all"))
mdegree <- median(degree)

betweenness(g5, directed =TRUE)
betweenness(g5, directed=TRUE, normalized=(TRUE))
NWg5<-as_long_data_frame(g5)
write.csv(NWg5, file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/NWg5.csv")
#g6 brewster id 5
g6 <- graph_from_data_frame(edgesg6, directed = TRUE, vertices = vertexg6)
g6.2 <- graph_from_data_frame(edgesg6, directed = FALSE, vertices = vertexg6)
E(g6)$weight <- E(g6)$Freq
g6

#checking igraph objects
is.directed(g6)
is.weighted(g6)
#show all the edges to or from "Tilney" id: 74
incident(g6, "5", mode = c("all"))
#relationships between farms
neighbors(g6, "5", mode=c("all"))
#network density
graph.density(g6)
#important or influential vertices
#degree
degree<- degree(g6, mode=c("all"))
mdegree <- median(degree)

betweenness(g6, directed =TRUE)
betweenness(g6, directed=TRUE, normalized=(TRUE))
NWg6<-as_long_data_frame(g6)
write.csv(NWg6, file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/NWg6.csv")

#g7 kaylor preoutbreak id 30
g7 <- graph_from_data_frame(edgesg7, directed = TRUE, vertices = vertexg7)
g7.2 <- graph_from_data_frame(edgesg7, directed = FALSE, vertices = vertexg7)
E(g7)$weight <- E(g7)$Freq
g7

#checking igraph objects
is.directed(g7)
is.weighted(g7)
#show all the edges to or from "Tilney" id: 74
incident(g7, "30", mode = c("all"))
#relationships between farms
neighbors(g7, "30", mode=c("all"))
#network density
graph.density(g7)
#important or influential vertices
#degree
degree<- degree(g7, mode=c("all"))
mdegree <- median(degree)

betweenness(g7, directed =TRUE)
betweenness(g7, directed=TRUE, normalized=(TRUE))
NWg7<-as_long_data_frame(g7)
write.csv(NWg7, file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/NWg7.csv")

#g8 postoutbreak kaylor id=30
g8 <- graph_from_data_frame(edgesg8, directed = TRUE, vertices = vertexg8)
g8.2 <- graph_from_data_frame(edgesg8, directed = FALSE, vertices = vertexg8)
E(g8)$weight <- E(g8)$Freq
g8

#checking igraph objects
is.directed(g8)
is.weighted(g8)
#show all the edges to or from "Tilney" id: 74
incident(g8, "74", mode = c("all"))
#relationships between farms
neighbors(g8, "74", mode=c("all"))
#network density
graph.density(g8)
#important or influential vertices
#degree
degree<- degree(g8, mode=c("all"))
mdegree <- median(degree)

betweenness(g8, directed =TRUE)
betweenness(g8, directed=TRUE, normalized=(TRUE))
NWg8<-as_long_data_frame(g8)
write.csv(NWg8, file = "C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/NWg8.csv")
#1. igraph summary
g1
gsize(g1) #number of edges
gorder(g1) #number of vertex

#2. Nodelist
V(g1)

#3. Edgelist
E(g1) 

#4. Attributes
V(g1)$class

#5. Adjacency matrix
g1[c(1:10),c(1:10)]

#==================================================================#
#===================== Measuring Centrality =======================#
#==================================================================#
# In this section, you will measure the centrality of the igraph   #
# object, "g1". You will be able to see how the theoretical   #
# concept of each centrality such as degree, eigenvector, and      #
# betweenness centrality is measured by the igraph.                #
#==================================================================#

#1. Degree centrality
g1_deg<-degree(g1,mode=c("All"))
V(g1)$degree<-g1_deg
V(g1)$degree
which.max(g1_deg) #answer was #3 truck wash

#2. Eigenvector centrality
g1_eig <- evcent(g1)$vector
V(g1)$Eigen<-g1_eig
V(g1)$Eigen
which.max(g1_eig) #again #3 truck wash, 5.609889e-02

#3. Betweenness centrality
g1_bw<-betweenness(g1, directed = TRUE)
g1.2_bw<-betweenness(g1.2, directed = FALSE)
V(g1)$betweenness<-g1_bw
V(g1)$betweenness
which.max(g1_bw)#number 3

NWtilney<-as_long_data_frame(g1) #ideas with this info I can do another statistics analysis


#==================================================================#
#================== Measuring Network Structure ===================#
#==================================================================#
# In this section, you will measure the indicators of the network  #
# structure such as network density, assortativity.                #
#==================================================================#

#1. Network Density
edge_density(g1) # Global density
SOW<-induced_subgraph(g1, V(g1)[class=="SOW"], impl=c("auto")) # Subgraphing into each class
edge_density(SOW) # Class level density
NURSERY <-induced_subgraph(g1, V(g1)[class=="NURSERY"], impl=c("auto")) # Subgraphing into each class
edge_density(NURSERY) # Class level density
#2. Assortativity
values <- as.numeric(factor(V(g1)$class))
assortativity_nominal(g1, types=values) #low assortativity by class

#2.1. Calculate the observed assortativity
observed.assortativity <- assortativity_nominal(g1, types=values)
results <- vector('list', 1000)
for(i in 1:1000){results[[i]] <- assortativity_nominal(g1, sample(values))}

#2.2.  Plot the distribution of assortativity values and add a red vertical line at the original observed value
hist(unlist(results), xlim = c(0,0.4))
abline(v = observed.assortativity,col = "red", lty = 3, lwd=2) #most of the score is below 0, means they tend to
#connect with different class

#==================================================================#
#===================== Network Visualization ======================#
#==================================================================#

#1. Plotting a network with the degree centrality

set.seed(1001)
library(RColorBrewer) # This is the color library
pal<-brewer.pal(length(unique(V(g1.2)$class)), "Set3") # Vertex color assigned per each class number
plot(g1.2,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(g1, "class")))],
     vertex.size = sqrt(g1_deg)/3, edge.width=sqrt(E(g1)$weight/800),
     layout = layout.fruchterman.reingold)

#1. Plotting a network with the eigenvector centrality

set.seed(1001)
plot(g1,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(g1, "class")))],
     vertex.size = sqrt(g1_eig)*10, edge.width=sqrt(E(g1)$weight/800),
     layout = layout.fruchterman.reingold)

#2. Plotting a network with the betweenness centrality

set.seed(1001)
plot(g1 ,edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(g1, "Class")))],
     vertex.size = sqrt(g1_bw)/3, edge.width=sqrt(E(g1)$weight/800),
     layout = layout.fruchterman.reingold)

#3. Plotting a scatter plot to see the correlation

#3.1. between degree and betweenness centrality

plot(V(g1)$degree, V(g1)$betweenness)
cor(V(g1)$degree, V(g1)$betweenness)  #correlation 0.8826591
#3.2. between degree and eigenvector centrality

plot(V(g1)$degree, V(g1)$Eigen)
cor(V(g1)$degree, V(g1)$Eigen)
#==================================================================#
#====================== Community Detection =======================#
#==================================================================#

#1. Louvain clustering
lc <- cluster_louvain(g1.2) # Create a cluster based on the Louvain method only for undirected!!
communities(lc) # You can check which vertices belongs to which clusters.

#2. Plotting the Betweenness Centrality network with the community detection

set.seed(1001) # To duplicate the computer process and create exactly the same network repetitively you should set the seed.
plot(lc, g1.2, edge.color = 'black',vertex.label.cex =0.5,
     vertex.color=pal[as.numeric(as.factor(vertex_attr(g1.2, "class")))],
     vertex.size = sqrt(g1.2_bw)/3, edge.width=sqrt(E(g1.2)$weight/800),
     layout = layout.fruchterman.reingold)



#test youtube Epimodel
library(Epimodel)
nodes$node <- c(1:84)

nw <- network_initialize(n=84)
nw <- set_vertex_attribute(nw, nodes$class)




## Create an igraph object 
g <- graph_from_data_frame(edges_state, 
                           directed = TRUE, # the network is directed
                           vertices = NULL)
# Select only connected nodes by removing isolated nodes
# Isolated node: node without any edge.
Isolated = which(igraph::degree(g)==0)
g = delete.vertices(g, Isolated)

# is the graph connected?
is.connected(g, mode = "strong")
# Yes. The graph, in which nodes are aggregated at federal state level, is strongly connected.

## is the graph simple?
is.simple(g)

# The graph is a multi graph 
# Convert into a simple graph 
g_simple <- g

# Assign weight = 1 for each movement
# Therefore weight is the trade frequency (i.e. number of movements over a certain period of time)
E(g_simple)$weight <- 1
g_simple <- igraph::simplify(g_simple, 
                             remove.loops = FALSE) 
# Do not remove loops, this represent the intra-state movements

is.weighted(g_simple) # TRUE

## Turn into data frame
edges_df <- igraph::as_data_frame(g_simple)

# Renaming the columns' names
edges_df <- edges_df[,c("from","to","weight")]

# Check how many federal states receive pigs
table(edges_df$to)

## Plotting the circular plot / edge bundle plot
# parameters for circular plot
circos.clear()
circos.par(start.degree = 90, gap.degree = 6, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# Plot 
chordDiagram(edges_df, 
             grid.col = mycolor, 
             scale = TRUE, # this specifies whether the percentages appear or not
             directional = 1,
             direction.type = c("arrows", "diffHeight"),
             diffHeight  = -0.04,
             link.arr.type = "big.arrow")
install.packages("tnet")
library(tnet)
colnames(Ahuiras)[3] <- "timestamp"


# Create an empty data frame for network data
network_data <- data.frame(source = character(), target = character(), timestamp = as.POSIXct(character()), totalmin = numeric())

# Get unique truck values in the dataset
unique_trucks <- unique(Ahuiras$truck)

# Iterate over each truck
for (truck in unique_trucks) {
  # Subset the data for the current truck
  truck_data <- Ahuiras[Ahuiras$truck == truck, ]
  
  # Iterate over the rows of the truck data
  for (i in 1:(nrow(truck_data)-1)) {
    if (!is.na(truck_data$farm[i]) && !is.na(truck_data$farm[i+1])) {
      row <- data.frame(source = truck_data$farm[i], target = truck_data$farm[i+1], timestamp = truck_data$timestamp[i+1], totalmin = truck_data$totalmin[i])
      network_data <- rbind(network_data, row)
    }
  }
}

# Reset row names in the network data
row.names(network_data) <- NULL

# Print the resulting network data
print(network_data)



# Create a new dataframe with source and target columns
edges <- data.frame(source, target)


# Convert network_data to an igraph object

ig_temp_net <- graph_from_data_frame(network_data, directed = TRUE)

# Perform temporal path analysis
start_node <- "Tilney"  # Replace with the actual start node value
end_node <- "Huiras"    # Replace with the actual end node value

# Calculate the temporal shortest path
temp_path <- get.shortest.paths(ig_temp_net, from = start_node, to = end_node, mode = "out", output = "epath")
paths <- temporal_paths

# Print the temporal path
print(temp_path)
plot(ig_temp_net)

#analysis of the paths
library(tnet)

# Create an empty temporal network object
temp_net <- tnet()

# Add edges to the temporal network
for (i in 1:nrow(network_data)) {
  source <- network_data$source[i]
  target <- network_data$target[i]
  timestamp <- network_data$timestamp[i]
  
  # Add the edge to the temporal network
  temp_net <- tnet.add.edges(temp_net, from = source, to = target, t = timestamp)
}

# Perform temporal path analysis
start_node <- "Tilney"  # Replace with the actual start node value
end_node <- "Huiras"    # Replace with the actual end node value

# Calculate the temporal shortest path
temp_path <- temporal_shortest_path(temp_net, start = start_node, end = end_node, mode = "out")

# Print the temporal path
print(temp_path)

#work with the distances
b1 <- read.csv("Ubic_farms.csv")
b2 <- merge(edges, b1, by = "target")

median(tilney1$dist)
median(tilney2$dist)
median(huiras1$dist)
median(huiras2$dist)
median(brewster1$dist)
median(brewster2$dist)
median(kaylor1$dist)
median(kaylor2$dist)

summary(tilney1$dist)
summary(tilney2$dist)
summary(huiras1$dist)
summary(huiras2$dist)
summary(brewster1$dist)
summary(brewster2$dist)
summary(kaylor1$dist)
summary(kaylor2$dist)


# Load the necessary packages
library(dplyr)
library(igraph)

library(dplyr)
library(igraph)

# Assuming you have loaded the required libraries and have your data in filtered_data

# Group data by truck and class
grouped_data <- filtered_data %>%
  group_by(truck, class, from) %>%
  arrange(entry) %>% # Arrange by entry to ensure order
  mutate(from = lag(class)) %>%
  slice(-1) %>% # Remove the first row in each group
  ungroup()

# Loop through each truck and create directed networks
unique_trucks <- unique(grouped_data$truck)

for (truck_id in unique_trucks) {
  # Create a subdataset for the current truck
  subdataset <- grouped_data %>%
    filter(truck == truck_id)
  
  # Get distinct class nodes and their connections
  truck_nodes <- distinct(subdataset, class, from)
  
  # Create a directed network
  truck_network <- graph_from_data_frame(truck_nodes, directed = TRUE)
  
  # Visualize the directed network
  plot(truck_network, main = paste("Truck:", truck_id))
}

library(dplyr)
library(igraph)
library(rgexf)  # Load the rgexf library for GraphML export

# Assuming you have loaded the required libraries and have your data in filtered_data

# Group data by truck and class
grouped_data <- filtered_data %>%
  group_by(truck) %>%
  arrange(entry) %>% # Arrange by entry to ensure order
  mutate(source = lag(class)) %>%
  slice(-1) %>% # Remove the first row in each group
  ungroup()
View(grouped_data)
library(dplyr)

grouped_data <- grouped_data %>%
  rename(target = Target)
# Select two columns
data_for_plot1 <- grouped_data %>%
  select(source, target)

# Count the frequency of combinations
data_for_plot1 <- data_for_plot1 %>%
  group_by(source, target) %>%
  summarize(freq = n()) %>%
  ungroup()

# Calculate the proportion
data_for_plot1 <- data_for_plot1 %>%
  mutate(proportion = freq / sum(freq))

data_for_plot1$proportion <- round(data_for_plot1$proportion, 2)
View(data_for_plot1)
# Loop through each truck and create directed networks
unique_trucks <- unique(grouped_data$truck)

for (truck_id in unique_trucks) {
  # Create a subdataset for the current truck
  subdataset <- grouped_data %>%
    filter(truck == truck_id)
  
  # Get distinct class nodes and their connections
  truck_nodes <- distinct(subdataset, class, from)
  
  # Create a directed network
  truck_network <- graph_from_data_frame(truck_nodes, directed = TRUE)
  
  # Export the network to GraphML format
  graphml_file <- paste("truck_", truck_id, ".graphml", sep = "")
  write.graph(truck_network, graphml_file, format = "graphml")
  
  # Optional: You can also load the exported GraphML files into Gephi for visualization.
}
View(filtered_data)

filtered_data$source_farm <- lag(filtered_data$farm, default = NA)
filtered_data$destination_farm <- lead(filtered_data$farm, default = NA)
filtered_data$source_class <- lag(filtered_data$class, default = NA)
filtered_data$destination_class <- lead(filtered_data$class, default = NA)

# Aggregate data to calculate the number of movements
movement_summary <- table(
  #Source_Farm = filtered_data$source_farm,
  Source_Class = filtered_data$source_class,
  #Destination_Farm = filtered_data$destination_farm,
  Destination_Class = filtered_data$destination_class
)


# Calculate total number of movements for each source farm and class
total_movements <- rowSums(movement_summary)

# Calculate proportions
proportions <- movement_summary / total_movements

# Convert the table to a data frame
proportions_df <- as.data.frame(as.table(movement_summary))

# Create the plot
library(ggplot2)
ggplot(data = proportions_df, aes(x = Source_Class, y = Destination_Class, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Source class", y = "Destination class", fill = "Proportion") +
  theme_minimal()
# Assuming your data is in a data frame called 'your_data'
write.csv(proportions_df, file = "proportions_.csv", row.names = FALSE)


#######Distances####
#####data cleaning every network

# Define a function for data cleaning and transformation

# Data cleaning to get the edges
data_cleaned <- filtered_data %>%
  filter(!(farm %in% c("VanRavenswaay Hogs - Gilt", "Lendt Wean-Finish", "Hoff Finishing")))

str(data_cleaned)  

data_cleaned <- data_cleaned %>%
  group_by(truck) %>%
  mutate(from = lag(farm)) %>% 
  slice(-1) %>% 
  ungroup()

# Select relevant columns and rename them
data_selected <- data_cleaned[, c(6, 7)]


# Convert columns to factors
data_selected$from <- as.factor(data_selected$from)
data_selected$to <- as.factor(data_selected$to)

# Create frequency column
data_with_freq <- data_selected %>%
  group_by(from, to) %>%
  tally(name = "freq") %>%
  ungroup()



# Apply the function to your datasets
tilney1 <- clean_and_transform(NAtilney)
tilney2 <- clean_and_transform(NBtilney)
huiras1 <- clean_and_transform(NAhuiras)
huiras2 <- clean_and_transform(NBhuiras)
brewster1 <- clean_and_transform(NAbrewster)
brewster2 <- clean_and_transform(NBbrewster)
kaylor1 <- clean_and_transform(NAkaylor)
kaylor2 <- clean_and_transform(NBkaylor)

####### creating the function to calculate distances

any(is.na(tog$lat1))
file1 <- distinct(file1)

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

# Load your datasets (tilney1, tilney2, etc.)
# Make sure to replace 'your_file1.csv', 'your_file2.csv', etc. with actual file paths
file1 <- read.csv("C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/ubicaciones_farm.csv")
file1 <- file1[,c(2,9,10,11)]
file1 <- file1 %>%
  arrange(Site.Name)
file1$Site.Name[322] <- "Al VanRavenswaay"
file1$Site.Name[167] <- "Kruger Nursery"
file1$Site.Name[192] <- "Lusk"
file1$Site.Name[226] <- "Oldfather Nursery"
file1$Site.Name[263] <- "Ruden Site 1"
file1$Site.Name[317] <- "Truck Wash Springfield"
file1$Site.Name[330] <- "Wally West Finisher"
file1 <- file1[,-1]
write.csv(file1, "ubicaciones_farm.csv")
file1 <- read.csv("C:/Users/nm01097/OneDrive - University of Surrey/Desktop/Chapter3. Corzo data/ubicaciones_farm.csv")
file1 <- file1[,-1]
# Calculate distances and save results for each dataset
unique_farms <- unique(merged_data$to)

# Replace the farm names in file1 using the mapping
# Create a logical condition to match farm names
condition <- file1$Site.Name %in% unique_farms

# Use the condition to subset the rows in file1
filtered_file1 <- file1[condition, ]

file1 <- filtered_file1

row <- data.frame(farm="Sobota Site 1", Latitude=41.546729, Longitud=-97.176776,type ="WF")
file1 <- rbind(file1, row)
colnames(file1) <- c("farm", "Latitude", "Longitud", "type")

#calculate distances


distances <- calculate_distances(data_selected, file1)


result_tilney1 <- calculate_distances(tilney1, file1)
write.csv(result_tilney1, "distancestilney1.csv", row.names = FALSE)

result_tilney2 <- calculate_distances(tilney2, file1)
write.csv(result_tilney2, "distancestilney2.csv", row.names = FALSE)

result_huiras1 <- calculate_distances(huiras1, file1)
write.csv(result_huiras1, "distanceshuiras1.csv", row.names = FALSE)

result_huiras2 <- calculate_distances(huiras2, file1)
write.csv(result_huiras2, "distancehuiras2.csv", row.names = FALSE)

result_brewster1 <- calculate_distances(brewster1, file1)
write.csv(result_brewster1, "distancesbrewster1.csv", row.names = FALSE)

result_brewster2 <- calculate_distances(brewster2, file1)
write.csv(result_brewster2, "distancesbrewster2.csv", row.names = FALSE)

result_kaylor1 <- calculate_distances(kaylor1, file1)
write.csv(result_kaylor1, "distanceskaylor1.csv", row.names = FALSE)

result_kaylor2 <- calculate_distances(kaylor2, file1)
write.csv(result_kaylor2, "distanceskaylor2.csv", row.names = FALSE)
#add distances to edges datasets
tilney1$distance <- result_tilney1$distance
tilney2$distance <- result_tilney2$distance
huiras1$distance <- result_huiras1$distance
huiras2$distance <- result_huiras2$distance
brewster1$distance <- result_brewster1$distance
brewster2$distance <- result_brewster2$distance
kaylor1$distance <- result_kaylor1$distance
kaylor2$distance <- result_kaylor2$distance

all_distances1 <- bind_rows(tilney1, tilney2, huiras1, huiras2, brewster1, brewster2, kaylor1, kaylor2)

# Remove duplicated combinations of origin and destination
all_distances2 <- all_distances1 %>%
  distinct(from, to, .keep_all = TRUE)

all_distances3 <- bind_rows(NAtilney, NBtilney, NAhuiras, NBhuiras, NAbrewster, NBbrewster, NAkaylor, NBkaylor)
farms <- all_distances3[,c(7,8)]
farms <- farms %>%
  distinct(farm, .keep_all = TRUE)

##### add the distances to my data set data_cleaned#####

# Left join the datasets based on the "from" and "to" columns
merged_data <- left_join(data_cleaned, distances, by = c("from", "to"))

# Display the merged dataset
print(merged_data)
merged_data <- distinct(merged_data)
# Display the merged dataset
print(merged_data)

write.csv(merged_data, "final_gooddataset.csv", row.names = FALSE)

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

# adding class to the dataset to get the values per production type

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

#statistics per category 
summary_stats <- all_distances2 %>%
  group_by(from_class) %>%
  summarise(
    median_distance = weighted.mean(distance, freq),
    q1_distance = quantile(distance, 0.25, weights = freq),
    q3_distance = quantile(distance, 0.75, weights = freq),
    max_distance = max(distance)
  ) %>%
  ungroup()


library(ggplot2)

# Increase the size of legends in ggplot plots
custom_theme <- theme(
  legend.text = element_text(size = 20),  # Adjust the text size for legends
  legend.title = element_text(size = 20)  # Adjust the title size for legends
)
# Create a filtered dataset excluding "Finisher" category
filtered_distances <- all_distances2 %>%
  filter(from_class != "FINISHER")
plot(filtere)
# Create the histogram for total distances with 25 km binwidth

hist(merged_data$distance)
hist_plot <- ggplot(merged_data, aes(x = distance)) +
  geom_histogram(binwidth = 25, fill = "lightblue", color = "black") +
  labs(
    x = "Distance (km)",
    y = "Frequency",
    title = "A"
  ) +
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 14),   # Set the X-axis label size
    axis.title.y = element_text(size = 14)    # Set the Y-axis label size
  )




# Create the eCDF plot for the filtered dataset
ecdf_plot <- ggplot(merged_data, aes(x = distance, color = class_to)) +
  stat_ecdf(geom = "step") +
  labs(
    x = "Distance (km)",
    y = "Cumulative Probability",
    title = "B", color= "Premise Type"
  ) +
  scale_color_manual(values = color_mapping) +
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 14),   # Set the X-axis label size
    axis.title.y = element_text(size = 14)  
    # Set the Y-axis label size
  )

write.csv(merged_data, "Fig3AandB.csv")
# Combine both plots with the letter A and B

grid.arrange(hist_plot, ecdf_plot) 

# Display the combined plot
print(combined_plots)

###### analysis of distances by every dataset
list_distances <- list(tilney1, tilney2, huiras1, huiras2, brewster1, brewster2, kaylor1, kaylor2)


# Define a function to summarize and plot one dataset
calculate_summary_stats <- function(dataset) {
  # Calculate summary statistics
  median_val <- sum(dataset$distance * dataset$freq) / sum(dataset$freq)
  iqr_25 <- quantile(dataset$distance, probs = 0.25, weights = dataset$freq)
  iqr_75 <- quantile(dataset$distance, probs = 0.75, weights = dataset$freq)
  max_val <- max(dataset$distance)
  
  
  # Create a summary data frame
  summary_df <- data.frame(
    Median = median_val,
    IQR_25 = iqr_25,
    IQR_75 = iqr_75,
    Max = max_val
  )
  
  return(summary_df)
}

summary_stats_list <- lapply(list_distances, calculate_summary_stats)


library(gridExtra)

# Define a function to create histograms
create_histogram <- function(dataset) {
  ggplot(dataset, aes(x = distance, weight = freq)) +
    geom_histogram(binwidth = 25, fill = "#728b77" , color = "black") +
    labs(
      x = "Distance (km)",
      y = "Frequency"
    ) +
    theme_bw() 
}

hist_plot <- ggplot(merged_data, aes(x = distance)) +
  geom_histogram(binwidth = 25, fill = "lightblue", color = "black") +
  labs(
    x = "Distance (km)",
    y = "Frequency",
    title = "A"
  ) +
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 14),   # Set the X-axis label size
    axis.title.y = element_text(size = 14)    # Set the Y-axis label size
  )



# Define the time interval
start_date <- as.POSIXct("2020-10-17 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2020-12-17 23:59:59", format = "%Y-%m-%d %H:%M:%S")
tilney1 <- merged_data %>%
  filter(to == "Tilney")
#filter(to == "Tilney" & entry >= start_date & entry <= end_date)
# Define the common limits
x_limit <- c(0, 400)
y_limit <- c(0, 160)

# Create your individual plots
p0 <- ggplot(merged_data, aes(x = distance, fill = class_from)) +
  geom_histogram(binwidth = 10) +
  scale_fill_manual(values = color_mapping) +
  labs(
    x = "Distance (km)",
    y = "Frequency", fill = "Premise type", title = "A"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = 'top', legend.direction = "horizontal",
    legend.text = element_text(size=12),
    legend.title = element_text(face ="bold"),
    text = element_text(size = 12),
    
  ) +
  coord_cartesian(xlim = x_limit, ylim = y_limit) +
  geom_histogram(binwidth = 10, na.rm = TRUE) 


tilney2 %>%
  group_by(class_to)%>%
  summarise(n = n(),
            median_d = median(distance),
            q1 = quantile(distance, 0.25, na.rm = TRUE),
            q2= quantile(distance, 0.75, na.rm = TRUE) )


P1 <- ggplot(tilney1, aes(x = distance, fill = class_from)) +
  geom_histogram(binwidth = 10) +
  scale_fill_manual(values = color_mapping) +
  labs(
    x = "Distance (km)",
    y = "Frequency", fill = "Premise type", title = "A"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    text = element_text(size = 12),legend.position = 'none'
  ) +
  coord_cartesian(xlim = x_limit, ylim= y_limit) +
  geom_histogram(binwidth = 10, na.rm = TRUE) 


# Define the time interval
start_date <- as.POSIXct("2020-12-17 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2021-02-17 23:59:59", format = "%Y-%m-%d %H:%M:%S")
tilney2 <- merged_data %>%
  filter(from == "Tilney")

P2 <- ggplot(tilney2, aes(x = distance, fill = class_to)) +
  geom_histogram(binwidth=10) +
  scale_fill_manual(values = color_mapping) +
  labs(
    x = "Distance (km)",
    y = "Frequency", fill = "Premise type", title="B"
  ) +
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),   # Set the X-axis label size
    axis.title.y = element_text(size = 12), legend.position="none"    # Set the Y-axis label size
  )+
  coord_cartesian(xlim = x_limit, ylim= y_limit) +
  geom_histogram(binwidth = 10, na.rm = TRUE) 


# Define the time interval
start_date <- as.POSIXct("2020-10-29 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2020-12-29 23:59:59", format = "%Y-%m-%d %H:%M:%S")
hui1 <- merged_data %>%
  filter(to == "Huiras")


P3 <-ggplot(hui1, aes(x = distance, y = n, fill = class_from)) +
  geom_bar(stat = "identity") +  # Use stat = "identity" to specify that 'n' represents the actual values
  scale_fill_manual(values = color_mapping) +
  labs(
    x = "Distance (km)",
    y = "Frequency",
    fill = "Premise type",
    title = "C"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "none"
  ) +
  coord_cartesian() 



# Define the time interval
start_date <- as.POSIXct("2020-12-29 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2021-02-29 23:59:59", format = "%Y-%m-%d %H:%M:%S")
hui2 <- merged_data %>%
  filter(from == "Huiras")

P4 <- ggplot(hui2, aes(x = distance, fill = class_to)) +
  geom_histogram(binwidth=10) +
  scale_fill_manual(values = color_mapping) +
  labs(
    x = "Distance (km)",
    y = "Frequency", fill = "Premise type", title="D"
  ) +
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),   # Set the X-axis label size
    axis.title.y = element_text(size = 12), legend.position="none"   # Set the Y-axis label size
  )+
  coord_cartesian(xlim = x_limit, ylim= y_limit) +
  geom_histogram(binwidth=10, na.rm = TRUE) 

unique(hui1$from)
hui1 %>%
  group_by(class_from)%>%
  summarise(n = n(),
            median_d = median(distance),
            q1 = quantile(distance, 0.25, na.rm = TRUE),
            q2= quantile(distance, 0.75, na.rm = TRUE) )
unique(hui2$to)
hui2 %>%
  group_by(class_to)%>%
  summarise(n = n(),
            median_d = median(distance),
            q1 = quantile(distance, 0.25, na.rm = TRUE),
            q2= quantile(distance, 0.75, na.rm = TRUE) )

# Define the time interval
start_date <- as.POSIXct("2021-03-24 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2021-05-24 23:59:59", format = "%Y-%m-%d %H:%M:%S")
Bre1 <- merged_data %>%
  filter(to == "Brewster")
P5 <- ggplot(Bre1, aes(x = distance, fill = class_from)) +
  geom_histogram(binwidth=10) +
  scale_fill_manual(values = color_mapping) +
  labs(
    x = "Distance (km)",
    y = "Frequency", fill = "Premise type", title="E"
  ) +
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),   # Set the X-axis label size
    axis.title.y = element_text(size = 12), legend.position="none"   # Set the Y-axis label size
  )+
  coord_cartesian(xlim = x_limit, ylim= y_limit) +
  geom_histogram(binwidth=10, na.rm = TRUE) 



# Define the time interval
start_date <- as.POSIXct("2021-05-24 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2021-07-24 23:59:59", format = "%Y-%m-%d %H:%M:%S")
Bre2 <- merged_data %>%
  filter(from == "Brewster")
P6 <- ggplot(Bre2, aes(x = distance, fill = class_to)) +
  geom_histogram(binwidth=10) +
  scale_fill_manual(values = color_mapping) +  # Use the color mapping
  labs(
    x = "Distance (km)",
    y = "Frequency", fill = "Premise type", title="F")+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),   # Set the X-axis label size
    axis.title.y = element_text(size = 12), legend.position="none"    # Set the Y-axis label size
  )+
  coord_cartesian(xlim = x_limit, ylim= y_limit) +
  geom_histogram(binwidth=10, na.rm = TRUE) 
#description results
unique(Bre1$from)
Bre1 %>%
  group_by(class_from)%>%
  summarise(n = n(),
            median_d = median(distance),
            q1 = quantile(distance, 0.25, na.rm = TRUE),
            q2= quantile(distance, 0.75, na.rm = TRUE) )
unique(Bre2$to)
Bre2 %>%
  group_by(class_to)%>%
  summarise(n = n(),
            median_d = median(distance),
            q1 = quantile(distance, 0.25, na.rm = TRUE),
            q2= quantile(distance, 0.75, na.rm = TRUE) )

# Define the time interval
start_date <- as.POSIXct("2021-05-16 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2021-07-16 23:59:59", format = "%Y-%m-%d %H:%M:%S")
kay1 <- merged_data %>%
  filter(to == "Kaylor")
P7 <- ggplot(kay1, aes(x = distance, fill = class_from)) +
  geom_histogram(binwidth=10) +
  scale_fill_manual(values = color_mapping) +  # Use the color mapping
  labs(
    x = "Distance (km)",
    y = "Frequency", fill = "Premise type", title="G")+
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),   # Set the X-axis label size
    axis.title.y = element_text(size = 12), legend.position="none"    # Set the Y-axis label size
  )+
  coord_cartesian(xlim = x_limit, ylim= y_limit) +
  geom_histogram(binwidth=10, na.rm = TRUE) 


# Define the time interval
start_date <- as.POSIXct("2021-07-16 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end_date <- as.POSIXct("2021-09-16 23:59:59", format = "%Y-%m-%d %H:%M:%S")
kay2 <- merged_data %>%
  filter(from == "Kaylor")
P8 <- ggplot(kay2, aes(x = distance, fill = class_to)) +
  geom_histogram(binwidth=10) +
  scale_fill_manual(values = color_mapping) +  # Use the color mapping
  labs(
    x = "Distance (km)",
    y = "Frequency", fill = "Premise type", title="H") +
  theme_bw()+
  theme(
    axis.title.x = element_text(size = 12),   # Set the X-axis label size
    axis.title.y = element_text(size = 12), legend.position="none"    # Set the Y-axis label size
  )+
  coord_cartesian(xlim = x_limit, ylim= y_limit) +
  geom_histogram(binwidth=10, na.rm = TRUE) 
#description results
unique(kay1$from)
kay1 %>%
  group_by(class_from)%>%
  summarise(n = n(),
            median_d = median(distance),
            q1 = quantile(distance, 0.25, na.rm = TRUE),
            q2= quantile(distance, 0.75, na.rm = TRUE) )
unique(Bre2$to)
Bre2 %>%
  group_by(class_to)%>%
  summarise(n = n(),
            median_d = median(distance),
            q1 = quantile(distance, 0.25, na.rm = TRUE),
            q2= quantile(distance, 0.75, na.rm = TRUE) )
library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


legend <- get_legend(p0)

grid.arrange(legend, arrangeGrob(P1, P2, P3, P4, ncol = 2),
             arrangeGrob(P5, P6, P7, P8, ncol = 2),
             ncol = 1, heights = c(0.5, 2.5, 2.5))



# Asignar colores a las categorías
categorias <- c("Nursery", "Truck Wash", "Finisher", "Wean-to-finisher", "Gilt isolation", "Sow")
colores <- pal[1:length(categorias)]

# Visualizar la paleta de colores
pie(rep(1, length(categorias)), labels = categorias, col = colores)


merged_data <- merged_data %>%
  mutate(class_from = case_when(
    type_from == "N" ~ "Nursery",
    type_from == "TW" ~ "Truck Wash",
    type_from == "F" ~ "Finisher",
    type_from == "WF" ~ "Wean-to-finisher",
    type_from == "GI" ~ "Gilt isolation",
    type_from == "S" ~ "Sow"
  ))


library(RColorBrewer)

# Build a color palette, 5 colors coming from the BuPu color palette"
coul <- viridis(n = 6)
# Build a pie chart with it
pie(rep(1, length(coul)), col = coul , main="") 


coul <- c("R62G74B137", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1")

color_mapping <- setNames(coul, categorias)

scale_color_viridis(discrete=TRUE, option="viridis")


