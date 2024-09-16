# Epidemiological Model
# Define the function to evaluate infection risk
evaluateInfectionRisk <- function(data, wash_col, vehicle_col, farm_col, target_farm, eff_range, probInf) {
  # Extract relevant columns
  mov <- data[[farm_col]]
  wash_station <- data[[wash_col]]
  vehicle <- data[[vehicle_col]]
  
  # Initialize the infections vector to all zeros
  infections <- rep(0, length(mov))
  
  # Identify all indices where the movement is to the target farm
  target_indices <- which(mov == target_farm)
  
  # Print the target indices for debugging
  print(paste("Target farm indices:", toString(target_indices)))
  
  # Get unique vehicles
  unique_vehicles <- unique(vehicle)
  
  # Loop through each vehicle
  for (veh in unique_vehicles) {
    # Get the indices for the current vehicle
    vehicle_indices <- which(vehicle == veh) #check
    
    # Filter target farm indices for the current vehicle
    vehicle_target_indices <- target_indices[target_indices %in% vehicle_indices]#this 
    #not only consider the target, check that the farm target indices are inside the first vehicle indice??? 
    
    # Print vehicle and target indices for debugging
    print(paste("Processing vehicle:", veh))
    print(paste("Target indices for vehicle:", toString(vehicle_target_indices)))
    
    # Loop through each target farm index to define segments within the vehicle
    for (i in 1:length(vehicle_target_indices)) {
      # Define the current target farm index
      target_idx <- vehicle_target_indices[i]
      
      # Define the starting index for the current segment
      if (i == 1) {
        start_idx <- min(vehicle_indices)
      } else {
        start_idx <- vehicle_target_indices[i-1] + 1
      }
      
      # Print the segment being processed
      print(paste("Processing segment from", start_idx, "to", target_idx))
      
      # Variable to keep track if the current segment is infected
      segment_infected <- FALSE
      
      # Loop through each movement in the current segment
      for (j in start_idx:target_idx) {
        # Sample wash effectiveness for the current movement
        wash_effectiveness <- runif(1, min = eff_range[1], max = eff_range[2])
        
        if (j == start_idx) {
          # Determine infection probability for the first movement in the segment
          infections[j] <- rbinom(1, 1, probInf * (1 - wash_effectiveness * wash_station[j])) #output 0 or 1
          
          # Print infection status for the first movement
          print(paste("Movement", j, "infection status:", infections[j]))
          if (infections[j] == 1) {
            segment_infected <- TRUE
          }
        } else if (segment_infected) {
          # Determine infection probability for subsequent movements if the segment is infected
          infections[j] <- rbinom(1, 1, probInf * (1 - wash_effectiveness * wash_station[j]))
          # Print infection status for subsequent movements
          print(paste("Movement", j, "infection status:", infections[j]))
          if (infections[j] == 1) {
            segment_infected <- TRUE
          } else {
            segment_infected <- FALSE
          }
        }
        
        # Stop transmission if infection is not propagated
        if (infections[j] == 0) {
          print(paste("Stopping transmission at movement", j))
          break
        }
      }
    }
  }
  
  # Return the infections vector
  return(infections)
}


# Define the function to evaluate spread risk
evaluateSpreadRisk <- function(data, wash_col, vehicle_col, farm_col, target_farm, eff_range, probInf) {
  # Extract relevant columns
  mov <- data[[farm_col]]
  wash_station <- data[[wash_col]]
  vehicle <- data[[vehicle_col]]
  
  # Initialize the infections vector to all zeros
  infections <- rep(0, length(mov))
  
  # Identify all indices where the movement is to the target farm
  target_indices <- which(mov == target_farm)
  
  # Print the target indices for debugging
  print(paste("Target farm indices:", toString(target_indices)))
  
  # Get unique vehicles
  unique_vehicles <- unique(vehicle)
  
  # Loop through each vehicle
  for (veh in unique_vehicles) {
    # Get the indices for the current vehicle
    vehicle_indices <- which(vehicle == veh)
    
    # Filter target farm indices for the current vehicle
    vehicle_target_indices <- target_indices[target_indices %in% vehicle_indices]
    
    # Print vehicle and target indices for debugging
    print(paste("Processing vehicle:", veh))
    print(paste("Target indices for vehicle:", toString(vehicle_target_indices)))
    
    # Loop through each target farm index to define segments within the vehicle
    for (i in 1:length(vehicle_target_indices)) {
      # Define the current target farm index
      target_idx <- vehicle_target_indices[i]
      
      # Define the ending index for the current segment
      if (i == length(vehicle_target_indices)) {
        end_idx <- max(vehicle_indices)
      } else {
        end_idx <- vehicle_target_indices[i+1] - 1
      }
      
      # Print the segment being processed
      print(paste("Processing segment from", target_idx, "to", end_idx))
      
      # Variable to keep track if the current segment is infected
      segment_infected <- TRUE
      
      # Loop through each movement in the current segment starting from the target farm index
      for (j in target_idx:end_idx) {
        # Sample wash effectiveness for the current movement
        wash_effectiveness <- runif(1, min = eff_range[1], max = eff_range[2])
        
        if (j == target_idx) {
          # Set the first movement in the segment as infected
          infections[j] <- 1
          print(paste("Starting infection at movement", j))
        } else if (segment_infected) {
          # Determine infection probability for subsequent movements if the segment is infected
          infections[j] <- rbinom(1, 1, probInf * (1 - wash_effectiveness * wash_station[j]))
          # Print infection status for subsequent movements
          print(paste("Movement", j, "infection status:", infections[j]))
          if (infections[j] == 1) {
            segment_infected <- TRUE
          } else {
            segment_infected <- FALSE
          }
        }
        
        # Stop transmission if infection is not propagated
        if (infections[j] == 0) {
          print(paste("Stopping transmission at movement", j))
          break
        }
      }
    }
  }
  
  # Return the infections vector
  return(infections)
}





