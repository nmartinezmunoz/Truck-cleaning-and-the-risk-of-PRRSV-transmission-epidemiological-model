# Epidemiological Model
# Packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Parameters
numSimulations <- 1000  # Adjust as needed
probInf <- 0.428  # Based on relevant paper (Galvis et al., 2022)
mov <- dataset$farm
UniqueFarm <- unique(mov)
wash_station <- dataset$wash_station  # Binomial values: 1 for washed, 0 for not washed

# doSim Function
# Simulates disease spread based on farm movements and previous truck wash.
doSim <- function(mov, wash_station) {
  infections <- rep(0, length(mov))
  for (i in 1:length(mov)) {
    closest_farm_idx <- which(mov == "farm" & seq_along(mov) > i)[1]
    if (!is.na(closest_farm_idx)) {
      if (1 %in% wash_station[i:(closest_farm_idx - 1)]) {
        infections[i] <- rbinom(1, 1, probInf * (1 - 0.9))  # Effectiveness of wash: 90%
      } else {
        infections[i] <- rbinom(1, 1, probInf)
      }
    } else {
      infections[i] <- rbinom(1, 1, probInf * (1 - 0.9 * wash_station[i]))
    }
  }
  return(infections)
}

# Generate Simulation Results
res <- replicate(numSimulations, doSim(mov, wash_station))

# Create a Data Frame to Store Results
results_simulation <- data.frame(farm = UniqueFarm)

# Define the Number of Bootstrap Samples
nBootstrapSamples <- 100  # Adjust as needed

# Initialize a List to Store Means for Each Farm
farm_means <- list()

# Loop Through Each Unique Farm (i in UniqueFarm)
for (i in UniqueFarm) {
  farm_values <- numeric(nBootstrapSamples)  # Initialize a vector to store bootstrap means
  
  for (j in 1:nBootstrapSamples) {
    # Generate a New Set of Simulations
    sim_results <- replicate(numSimulations, doSim(mov, wash_station))
    
    if (length(dim((sim_results[which(mov == i), ])) > 0)) {
      tmp <- colSums(sim_results[which(mov == i), ])
    } else {
      tmp <- sim_results[which(mov == i), ]
    }
    
    # Calculate the Mean for This Set of Simulations
    value <- ifelse(length(tmp > 0), sum(tmp > 0) / numSimulations, 0)
    farm_values[j] <- value
  }
  
  # Store the Means for the Current Farm (i)
  farm_means[[i]] <- farm_values
  
  # Calculate Quantiles for the Bootstrap Means
  quantiles <- quantile(farm_means[[i]], c(0.025, 0.975))
  
  # Add Results to the Data Frame
  results_simulation$Mean[results_simulation$farm == i] <- mean(farm_means[[i]])
  results_simulation$Lower_CI[results_simulation$farm == i] <- quantiles[1]
  results_simulation$Upper_CI[results_simulation$farm == i] <- quantiles[2]
}

# View the Resulting Data Frame
print(results_simulation)

######farm index in results#####
# Extract the Unique Farm Names
combined <- list()  # Replace with your actual datasets
combined <- bind_rows(combined)
selected_columns <- lapply(combined, function(df) {
  df %>% 
    select(farm, class)
})

# Bind the Selected Columns Together
combined <- bind_rows(selected_columns)

# Create a Unique Index in the "Farm X" Format
unique_farms <- unique(combined)
farm_index <- data.frame(farm = unique_farms$farm, class = unique_farms$class, stringsAsFactors = FALSE)

farm_index$Index <- sprintf("%02d", seq_along(farm_index$farm))
farm_index <- farm_index %>%
  mutate(class = case_when(
    class == "NURSERY" ~ "Nursery",
    class == "FINISHER" ~ "Finisher",
    class == "WEAN-TO-FINISH" ~ "Wean-to-finisher",
    class == "GILT ISOLATION" ~ "Gilt isolation",
    class == "SOW" ~ "Sow"
  ))

write.csv(farm_index, "farm_index.csv")

#####plots####
# Create a List of Your Datasets
dataset_list <- list(
  results_A = results_,  # Replace with your actual datasets
  results_B = results_B, etc
)

# Add a "Source" Column with the List Names to Each Data Frame
for (i in 1:length(dataset_list)) {
  dataset_list[[i]]$Source <- names(dataset_list)[i]
}

Fig5 <- do.call(rbind, dataset_list)
Fig5 <- merge(Fig5, farm_index, by = "farm", all.x = TRUE)

write.csv(Fig5, "Fig5.csv")

# Combine All Datasets into One and Add a Dataset Column
Fig5 <- Fig5 %>%
  arrange(Source, Index)
final_plot <- Fig5 %>%
  mutate(Upper_CI = Upper_CI * 100, 
         Lower_CI = Lower_CI * 100,
         Mean = Mean * 100)

final_plot$Source <- factor(final_plot$Source, levels = c("Potential sources of infection-case № 1", "Spread risk of infection-case № 1", 
                                                            "Potential sources of infection-case № 2", "Spread risk of infection-case № 2",
                                                            "Potential sources of infection-case № 3", "Spread risk of infection-case № 3",
                                                            "Potential source of infection-case № 4", "Spread risk of infection-case № 4"))

# Plot
plot <- ggplot(final_plot, aes(x = `Index`, y = Mean, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_mapping) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ Source, ncol = 2) +
  labs(x = "Farm Index", y = "Probability (%)", fill = "Premise Type") +
  theme_bw() +
  theme(legend.position = "top")

# Print the Plot
print(plot)
plot+ geom_point(data = final_plot[final_plot$Index %in% c(3,5,8,20), ], aes(size = 3, bold) )
