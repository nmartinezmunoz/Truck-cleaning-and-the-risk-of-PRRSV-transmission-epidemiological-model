
####cleaning DS##### 
#deleting the repeated farms, the farms that have different name but are the same (VanVanRavenswaay and the Lendt Wean-Finish, and nursery)

# Remove specific rows from Btilney
Btilney <- Btilney[-c(1:5, 23:34, 50:55, 14, 15, 25, 29, 34, 41, 44, 47, 48), ] # 42 rows

# Remove specific rows from Ahuiras
Ahuiras <- Ahuiras[-c(11, 96, 97, 82, 83, 86, 87, 93, 119), ]#110 rows

# Remove specific rows from Bhuiras
Bhuiras <- Bhuiras[-c(1:6, 10, 11, 21, 62:69, 72, 80:82, 85, 86, 91, 92, 96), ] #84 rows

# Remove specific rows from Abrewster
Abrewster <- Abrewster[-c(10, 25, 19), ] #22 rows

# Remove specific rows from Bbrewster
Bbrewster <- Bbrewster[-c(24:30, 33:44, 49: 51, 53:57), ] #30rows

# Remove specific rows from Akaylor
Akaylor <- Akaylor[-c(23, 26, 29, 32, 35, 38, 41, 44, 60:74), ]#51 rows


#####simulations####
write.csv(Atilney, "Atilney.csv")
write.csv(Btilney, "Btilney.csv")
write.csv(Ahuiras, "Ahuiras.csv")
write.csv(Bhuiras, "Bhuiras.csv")
write.csv(Abrewster, "Abrewster.csv")
write.csv(Bbrewster, "Bbrewster.csv")
write.csv(Akaylor, "Akaylor.csv")
write.csv(Bkaylor, "Bkaylor.csv")


Atilney <- read.csv("Atilney.csv")
write.csv(Btilney, "Btilney.csv")
write.csv(Ahuiras, "Ahuiras.csv")
write.csv(Bhuiras, "Bhuiras.csv")
write.csv(Abrewster, "Abrewster.csv")
write.csv(Bbrewster, "Bbrewster.csv")
write.csv(Akaylor, "Akaylor.csv")
write.csv(Bkaylor, "Bkaylor.csv")
######Tilney A####
#parameters
nSim <- 1000
probInf <- 0.428
mov <- Atilney$farm
UniqueFarm <- unique(mov)
wash_station <- Atilney$wash_station

# doSim function
doSim <- function(mov, wash_station) {
  infections <- rep(0, length(mov))
  for (i in 1:length(mov)) {
    closest_farm_idx <- which(mov == "Tilney" & seq_along(mov) > i)[1]
    if (!is.na(closest_farm_idx)) {
      if (1 %in% wash_station[i:(closest_farm_idx - 1)]) {
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

res <- replicate(nSim, doSim(mov, wash_station))

# Create a data frame to store the results
results_Atilney <- data.frame(farm = UniqueFarm)

# Define the number of bootstrap samples
nBootstrapSamples <- 100  # You can adjust this number as needed

# Initialize a list to store means for each farm
farm_means <- list()

# Loop through each unique farm (i in UniqueFarm)
for (i in UniqueFarm) {
  farm_values <- numeric(nBootstrapSamples)  # Initialize a vector to store bootstrap means
  
  for (j in 1:nBootstrapSamples) {
    # Generate a new set of simulations (assuming `doSim` generates simulations)
    sim_results <- replicate(nSim, doSim(mov, wash_station))
    
    if (length(dim((sim_results[which(mov == i), ])) > 0)) {
      tmp <- colSums(sim_results[which(mov == i), ])
    } else {
      tmp <- sim_results[which(mov == i), ]
    }
    
    # Calculate the mean for this set of simulations
    value <- ifelse(length(tmp > 0), sum(tmp > 0) / nSim, 0)
    farm_values[j] <- value
  }
  
  # Store the means for the current farm (i)
  farm_means[[i]] <- farm_values
  
  # Calculate quantiles for the bootstrap means
  quantiles <- quantile(farm_means[[i]], c(0.025, 0.975))
  
  # Add results to the data frame
  results_Atilney$Mean[results_Atilney$farm == i] <- mean(farm_means[[i]])
  results_Atilney$Lower_CI[results_Atilney$farm == i] <- quantiles[1]
  results_Atilney$Upper_CI[results_Atilney$farm == i] <- quantiles[2]
}

# View the resulting data frame
print(results_Atilney)

#####Tilney B####
#parameters
nSim <- 1000
probInf <- 0.428
mov <- Btilney$farm
UniqueFarm <- unique(mov)
wash_station <- Btilney$wash_station

# doSim function
doSim <- function(mov, wash_station) {
  infections <- rep(0, length(mov))
  for (i in 1:length(mov)) {
    closest_farm_idx <- which(mov == "Tilney" & seq_along(mov) > i)[1]
    if (!is.na(closest_farm_idx)) {
      if (1 %in% wash_station[i:(closest_farm_idx - 1)]) {
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

res <- replicate(nSim, doSim(mov, wash_station))

# Create a data frame to store the results
results_Btilney <- data.frame(farm = UniqueFarm)

# Define the number of bootstrap samples
nBootstrapSamples <- 100  # You can adjust this number as needed

# Initialize a list to store means for each farm
farm_means <- list()

# Loop through each unique farm (i in UniqueFarm)
for (i in UniqueFarm) {
  farm_values <- numeric(nBootstrapSamples)  # Initialize a vector to store bootstrap means
  
  for (j in 1:nBootstrapSamples) {
    # Generate a new set of simulations (assuming `doSim` generates simulations)
    sim_results <- replicate(nSim, doSim(mov, wash_station))
    
    if (length(dim((sim_results[which(mov == i), ])) > 0)) {
      tmp <- colSums(sim_results[which(mov == i), ])
    } else {
      tmp <- sim_results[which(mov == i), ]
    }
    
    # Calculate the mean for this set of simulations
    value <- ifelse(length(tmp > 0), sum(tmp > 0) / nSim, 0)
    farm_values[j] <- value
  }
  
  # Store the means for the current farm (i)
  farm_means[[i]] <- farm_values
  
  # Calculate quantiles for the bootstrap means
  quantiles <- quantile(farm_means[[i]], c(0.025, 0.975))
  
  # Add results to the data frame
  results_Btilney$Mean[results_Btilney$farm == i] <- mean(farm_means[[i]])
  results_Btilney$Lower_CI[results_Btilney$farm == i] <- quantiles[1]
  results_Btilney$Upper_CI[results_Btilney$farm == i] <- quantiles[2]
}

# View the resulting data frame
print(results_Btilney)

#####Huiras A####
#parameters
nSim <- 1000
probInf <- 0.428
mov <- Ahuiras$farm
UniqueFarm <- unique(mov)
wash_station <- Ahuiras$wash_station

# doSim function
doSim <- function(mov, wash_station) {
  infections <- rep(0, length(mov))
  for (i in 1:length(mov)) {
    closest_farm_idx <- which(mov == "Huiras" & seq_along(mov) > i)[1]
    if (!is.na(closest_farm_idx)) {
      if (1 %in% wash_station[i:(closest_farm_idx - 1)]) {
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

res <- replicate(nSim, doSim(mov, wash_station))

# Create a data frame to store the results
results_Ahuiras <- data.frame(farm = UniqueFarm)

# Define the number of bootstrap samples
nBootstrapSamples <- 100  # You can adjust this number as needed

# Initialize a list to store means for each farm
farm_means <- list()

# Loop through each unique farm (i in UniqueFarm)
for (i in UniqueFarm) {
  farm_values <- numeric(nBootstrapSamples)  # Initialize a vector to store bootstrap means
  
  for (j in 1:nBootstrapSamples) {
    # Generate a new set of simulations (assuming `doSim` generates simulations)
    sim_results <- replicate(nSim, doSim(mov, wash_station))
    
    if (length(dim((sim_results[which(mov == i), ])) > 0)) {
      tmp <- colSums(sim_results[which(mov == i), ])
    } else {
      tmp <- sim_results[which(mov == i), ]
    }
    
    # Calculate the mean for this set of simulations
    value <- ifelse(length(tmp > 0), sum(tmp > 0) / nSim, 0)
    farm_values[j] <- value
  }
  
  # Store the means for the current farm (i)
  farm_means[[i]] <- farm_values
  
  # Calculate quantiles for the bootstrap means
  quantiles <- quantile(farm_means[[i]], c(0.025, 0.975))
  
  # Add results to the data frame
  results_Ahuiras$Mean[results_Ahuiras$farm == i] <- mean(farm_means[[i]])
  results_Ahuiras$Lower_CI[results_Ahuiras$farm == i] <- quantiles[1]
  results_Ahuiras$Upper_CI[results_Ahuiras$farm == i] <- quantiles[2]
}

# View the resulting data frame
print(results_Ahuiras)

#####Huiras B####
#parameters
nSim <- 1000
probInf <- 0.428
mov <- Bhuiras$farm
UniqueFarm <- unique(mov)
wash_station <- Bhuiras$wash_station

# doSim function
doSim <- function(mov, wash_station) {
  infections <- rep(0, length(mov))
  for (i in 1:length(mov)) {
    closest_farm_idx <- which(mov == "Huiras" & seq_along(mov) > i)[1]
    if (!is.na(closest_farm_idx)) {
      if (1 %in% wash_station[i:(closest_farm_idx - 1)]) {
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

res <- replicate(nSim, doSim(mov, wash_station))

# Create a data frame to store the results
results_Bhuiras <- data.frame(farm = UniqueFarm)

# Define the number of bootstrap samples
nBootstrapSamples <- 100  # You can adjust this number as needed

# Initialize a list to store means for each farm
farm_means <- list()

# Loop through each unique farm (i in UniqueFarm)
for (i in UniqueFarm) {
  farm_values <- numeric(nBootstrapSamples)  # Initialize a vector to store bootstrap means
  
  for (j in 1:nBootstrapSamples) {
    # Generate a new set of simulations (assuming `doSim` generates simulations)
    sim_results <- replicate(nSim, doSim(mov, wash_station))
    
    if (length(dim((sim_results[which(mov == i), ])) > 0)) {
      tmp <- colSums(sim_results[which(mov == i), ])
    } else {
      tmp <- sim_results[which(mov == i), ]
    }
    
    # Calculate the mean for this set of simulations
    value <- ifelse(length(tmp > 0), sum(tmp > 0) / nSim, 0)
    farm_values[j] <- value
  }
  
  # Store the means for the current farm (i)
  farm_means[[i]] <- farm_values
  
  # Calculate quantiles for the bootstrap means
  quantiles <- quantile(farm_means[[i]], c(0.025, 0.975))
  
  # Add results to the data frame
  results_Bhuiras$Mean[results_Bhuiras$farm == i] <- mean(farm_means[[i]])
  results_Bhuiras$Lower_CI[results_Bhuiras$farm == i] <- quantiles[1]
  results_Bhuiras$Upper_CI[results_Bhuiras$farm == i] <- quantiles[2]
}

# View the resulting data frame
print(results_Bhuiras)

#####Brewster A####
#parameters
nSim <- 1000
probInf <- 0.428
mov <- Abrewster$farm
UniqueFarm <- unique(mov)
wash_station <- Abrewster$wash_station

# doSim function
doSim <- function(mov, wash_station) {
  infections <- rep(0, length(mov))
  for (i in 1:length(mov)) {
    closest_farm_idx <- which(mov == "Brewster" & seq_along(mov) > i)[1]
    if (!is.na(closest_farm_idx)) {
      if (1 %in% wash_station[i:(closest_farm_idx - 1)]) {
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

res <- replicate(nSim, doSim(mov, wash_station))

# Create a data frame to store the results
results_Abrewster <- data.frame(farm = UniqueFarm)

# Define the number of bootstrap samples
nBootstrapSamples <- 100  # You can adjust this number as needed

# Initialize a list to store means for each farm
farm_means <- list()

# Loop through each unique farm (i in UniqueFarm)
for (i in UniqueFarm) {
  farm_values <- numeric(nBootstrapSamples)  # Initialize a vector to store bootstrap means
  
  for (j in 1:nBootstrapSamples) {
    # Generate a new set of simulations (assuming `doSim` generates simulations)
    sim_results <- replicate(nSim, doSim(mov, wash_station))
    
    if (length(dim((sim_results[which(mov == i), ])) > 0)) {
      tmp <- colSums(sim_results[which(mov == i), ])
    } else {
      tmp <- sim_results[which(mov == i), ]
    }
    
    # Calculate the mean for this set of simulations
    value <- ifelse(length(tmp > 0), sum(tmp > 0) / nSim, 0)
    farm_values[j] <- value
  }
  
  # Store the means for the current farm (i)
  farm_means[[i]] <- farm_values
  
  # Calculate quantiles for the bootstrap means
  quantiles <- quantile(farm_means[[i]], c(0.025, 0.975))
  
  # Add results to the data frame
  results_Abrewster$Mean[results_Abrewster$farm == i] <- mean(farm_means[[i]])
  results_Abrewster$Lower_CI[results_Abrewster$farm == i] <- quantiles[1]
  results_Abrewster$Upper_CI[results_Abrewster$farm == i] <- quantiles[2]
}

# View the resulting data frame
print(results_Abrewster)


#####Brewster B####
#parameters
nSim <- 1000
probInf <- 0.428
mov <- Bbrewster$farm
UniqueFarm <- unique(mov)
wash_station <- Bbrewster$wash_station

# doSim function
doSim <- function(mov, wash_station) {
  infections <- rep(0, length(mov))
  for (i in 1:length(mov)) {
    closest_farm_idx <- which(mov == "Brewster" & seq_along(mov) > i)[1]
    if (!is.na(closest_farm_idx)) {
      if (1 %in% wash_station[i:(closest_farm_idx - 1)]) {
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

res <- replicate(nSim, doSim(mov, wash_station))

# Create a data frame to store the results
results_Bbrewster <- data.frame(farm = UniqueFarm)

# Define the number of bootstrap samples
nBootstrapSamples <- 100  # You can adjust this number as needed

# Initialize a list to store means for each farm
farm_means <- list()

# Loop through each unique farm (i in UniqueFarm)
for (i in UniqueFarm) {
  farm_values <- numeric(nBootstrapSamples)  # Initialize a vector to store bootstrap means
  
  for (j in 1:nBootstrapSamples) {
    # Generate a new set of simulations (assuming `doSim` generates simulations)
    sim_results <- replicate(nSim, doSim(mov, wash_station))
    
    if (length(dim((sim_results[which(mov == i), ])) > 0)) {
      tmp <- colSums(sim_results[which(mov == i), ])
    } else {
      tmp <- sim_results[which(mov == i), ]
    }
    
    # Calculate the mean for this set of simulations
    value <- ifelse(length(tmp > 0), sum(tmp > 0) / nSim, 0)
    farm_values[j] <- value
  }
  
  # Store the means for the current farm (i)
  farm_means[[i]] <- farm_values
  
  # Calculate quantiles for the bootstrap means
  quantiles <- quantile(farm_means[[i]], c(0.025, 0.975))
  
  # Add results to the data frame
  results_Bbrewster$Mean[results_Bbrewster$farm == i] <- mean(farm_means[[i]])
  results_Bbrewster$Lower_CI[results_Bbrewster$farm == i] <- quantiles[1]
  results_Bbrewster$Upper_CI[results_Bbrewster$farm == i] <- quantiles[2]
}

# View the resulting data frame
print(results_Bbrewster)

#####Kaylor A#####
#parameters
nSim <- 1000
probInf <- 0.428
mov <- Akaylor$farm
UniqueFarm <- unique(mov)
wash_station <- Akaylor$wash_station

# doSim function
doSim <- function(mov, wash_station) {
  infections <- rep(0, length(mov))
  for (i in 1:length(mov)) {
    closest_farm_idx <- which(mov == "Kaylor" & seq_along(mov) > i)[1]
    if (!is.na(closest_farm_idx)) {
      if (1 %in% wash_station[i:(closest_farm_idx - 1)]) {
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

res <- replicate(nSim, doSim(mov, wash_station))

# Create a data frame to store the results
results_Akaylor <- data.frame(farm = UniqueFarm)

# Define the number of bootstrap samples
nBootstrapSamples <- 100  # You can adjust this number as needed

# Initialize a list to store means for each farm
farm_means <- list()

# Loop through each unique farm (i in UniqueFarm)
for (i in UniqueFarm) {
  farm_values <- numeric(nBootstrapSamples)  # Initialize a vector to store bootstrap means
  
  for (j in 1:nBootstrapSamples) {
    # Generate a new set of simulations (assuming `doSim` generates simulations)
    sim_results <- replicate(nSim, doSim(mov, wash_station))
    
    if (length(dim((sim_results[which(mov == i), ])) > 0)) {
      tmp <- colSums(sim_results[which(mov == i), ])
    } else {
      tmp <- sim_results[which(mov == i), ]
    }
    
    # Calculate the mean for this set of simulations
    value <- ifelse(length(tmp > 0), sum(tmp > 0) / nSim, 0)
    farm_values[j] <- value
  }
  
  # Store the means for the current farm (i)
  farm_means[[i]] <- farm_values
  
  # Calculate quantiles for the bootstrap means
  quantiles <- quantile(farm_means[[i]], c(0.025, 0.975))
  
  # Add results to the data frame
  results_Akaylor$Mean[results_Akaylor$farm == i] <- mean(farm_means[[i]])
  results_Akaylor$Lower_CI[results_Akaylor$farm == i] <- quantiles[1]
  results_Akaylor$Upper_CI[results_Akaylor$farm == i] <- quantiles[2]
}

# View the resulting data frame
print(results_Akaylor)
#####Kaylor B####
#parameters
nSim <- 1000
probInf <- 0.428
mov <- Bkaylor$farm
UniqueFarm <- unique(mov)
wash_station <- Bkaylor$wash_station

# doSim function
doSim <- function(mov, wash_station) {
  infections <- rep(0, length(mov))
  for (i in 1:length(mov)) {
    closest_farm_idx <- which(mov == "Kaylor" & seq_along(mov) > i)[1]
    if (!is.na(closest_farm_idx)) {
      if (1 %in% wash_station[i:(closest_farm_idx - 1)]) {
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

res <- replicate(nSim, doSim(mov, wash_station))

# Create a data frame to store the results
results_Bkaylor <- data.frame(farm = UniqueFarm)

# Define the number of bootstrap samples
nBootstrapSamples <- 100  # You can adjust this number as needed

# Initialize a list to store means for each farm
farm_means <- list()

# Loop through each unique farm (i in UniqueFarm)
for (i in UniqueFarm) {
  farm_values <- numeric(nBootstrapSamples)  # Initialize a vector to store bootstrap means
  
  for (j in 1:nBootstrapSamples) {
    # Generate a new set of simulations (assuming `doSim` generates simulations)
    sim_results <- replicate(nSim, doSim(mov, wash_station))
    
    if (length(dim((sim_results[which(mov == i), ])) > 0)) {
      tmp <- colSums(sim_results[which(mov == i), ])
    } else {
      tmp <- sim_results[which(mov == i), ]
    }
    
    # Calculate the mean for this set of simulations
    value <- ifelse(length(tmp > 0), sum(tmp > 0) / nSim, 0)
    farm_values[j] <- value
  }
  
  # Store the means for the current farm (i)
  farm_means[[i]] <- farm_values
  
  # Calculate quantiles for the bootstrap means
  quantiles <- quantile(farm_means[[i]], c(0.025, 0.975))
  
  # Add results to the data frame
  results_Bkaylor$Mean[results_Bkaylor$farm == i] <- mean(farm_means[[i]])
  results_Bkaylor$Lower_CI[results_Bkaylor$farm == i] <- quantiles[1]
  results_Bkaylor$Upper_CI[results_Bkaylor$farm == i] <- quantiles[2]
}

# View the resulting data frame
print(results_Bkaylor)



######farm index in results#####
# Extract the unique farm names
combined <- list(Atilney, Btilney, Abrewster, Bbrewster, Ahuiras, Bhuiras, Akaylor, Bkaylor)
combined <- bind_rows(combined)
selected_columns <- lapply(combined, function(df) {
  df %>% 
    select(farm, class)
})

# Bind the selected columns together
combined <- bind_rows(selected_columns)

# Create a unique index in the "Farm X" format
unique_farms <- unique(combined)
farm_index <- data.frame(farm = unique_farms$farm,class = unique_farms$class, stringsAsFactors = FALSE)


farm_index$Index <- sprintf("%02d", seq_along(farm_index$farm))
farm_index<- farm_index %>%
  mutate(class = case_when(
    class == "NURSERY" ~ "Nursery",
    class == "FINISHER" ~ "Finisher",
    class == "WEAN-TO-FINISH" ~ "Wean-to-finisher",
    class == "GILT ISOLATION" ~ "Gilt isolation",
    class == "SOW" ~ "Sow"
  ))

write.csv(farm_index,"farm_index.csv")


#####plots####
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
write.csv(results, file = "result_Akaylor.csv", row.names = FALSE)

# Create a list of your datasets
dataset_list <- list(
  results_Atilney = results_Atilney,
  results_Btilney = results_Btilney,
  results_Ahuiras = results_Ahuiras,
  results_Bhuiras = results_Bhuiras,
  results_Abrewster = results_Abrewster,
  results_Bbrewster = results_Bbrewster,
  results_Akaylor = results_Akaylor,
  results_Bkaylor = results_Bkaylor
)
# Add a "Source" column with the list names to each data frame
for (i in 1:length(dataset_list)) {
  dataset_list[[i]]$Source <- names(dataset_list)[i]
}
Fig5 <- do.call(rbind, dataset_list)
Fig5 <- merge(Fig5, farm_index, by = "farm", all.x = TRUE)

write.csv(Fig5, "Fig5.csv")

# Combine all datasets into one and add a dataset column

Fig5 <- Fig5 %>%
  arrange(Source, Index)


final_plot <- Fig5 %>%
  mutate(Upper_CI = Upper_CI * 100, 
         Lower_CI=Lower_CI *100,
         Mean=Mean*100)


final_plot <- final_plot %>%
  mutate(Source = case_when(
    Source == "results_Atilney" ~ "Potential sources of infection-case № 1",
    Source == "results_Btilney"  ~ "Spread risk of infection-case № 1",
    Source == "results_Ahuiras" ~ "Potential sources of infection-case № 2",
    Source == "results_Bhuiras" ~ "Spread risk of infection-case № 2",
    Source == "results_Abrewster" ~ "Potential sources of infection-case № 3",
    Source == "results_Bbrewster" ~ "Spread risk of infection-case № 3",
    Source == "results_Akaylor" ~ "Potential source of infection-case № 4",
    Source == "results_Bkaylor" ~ "Spread risk of infection-case № 4",
    TRUE ~ as.character(Source)  # Default to the original value
  ))

final_plot$Source <- factor(final_plot$Source, levels = c("Potential sources of infection-case № 1", "Spread risk of infection-case № 1", 
                                                            "Potential sources of infection-case № 2", "Spread risk of infection-case № 2",
                                                          "Potential sources of infection-case № 3", "Spread risk of infection-case № 3",
                                                          "Potential source of infection-case № 4","Spread risk of infection-case № 4"))





plot <- ggplot(final_plot, aes(x = `Index`, y = Mean, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_mapping) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ Source, ncol = 2) +
  labs(x = "Farm Index", y = "Probability (%)", fill="Premise Type") +
  theme_bw() +
  theme(legend.position = "top")

# Print the plot
print(plot)
plot+ geom_point(data = final_plot[final_plot$Index %in% c(3,5,8,20), ], aes(size = 3, bold) )

                