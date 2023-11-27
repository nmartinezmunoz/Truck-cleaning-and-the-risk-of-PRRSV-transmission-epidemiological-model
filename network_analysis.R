# Load necessary libraries
library(igraph)
#creation of the network
g1 <- graph_from_data_frame(d=dataset[, c("from", "to")],vertices=node, directed = TRUE)
#etc.......

E(g1)$distance <- dataset$distance
E(g1)$type_from <- dataset$type_from
E(g1)$type_to <- dataset$type_to
E(g1)$weight <- dataset$freq

# Function to analyze clusters in a graph
analyze_clusters <- function(graph, graph_name) {
  # Community detection
  communities <- edge.betweenness.community(graph)
  
  # Access community membership
  community_membership <- membership(communities)
  
  # Loop through communities and count members and record their types
  for (i in unique(community_membership)) {
    nodes_in_community <- which(community_membership == i)
    types_in_community <- V(graph)$type_to[nodes_in_community]
    
    # Create a data frame for community information
    community_info_df <- data.frame(
      Graph = graph_name,
      Community = i,
      Number_of_Members = length(nodes_in_community),
      Types = paste(types_in_community, collapse = ", ")
    )
    
    # Append the community information data frame to the results data frame
    results_df <<- rbind(results_df, community_info_df)
  }
}

# Analysis of multiple graphs
graph_list <- list(g1, g2, g3, g4, g5, g6, g7, g8)
results_df <- data.frame()

# Loop through each graph in graph_list and analyze clusters
for (i in 1:length(graph_list)) {
  cat("Analysis for Graph", i, "\n")
  
  # Analyze clusters for the current graph
  analyze_clusters(graph_list[[i]], paste("G", i))
  
  cat("\n")
}

# Print and inspect the results data frame
print(results_df)

# Save results to a CSV file
write.csv(results_df, "results_networks.csv")

# Calculate network parameters for each graph
for (i in 1:length(graph_list)) {
  graph <- graph_list[[i]]
  
  cat("Network Parameters for Graph", i, "\n")
  
  # Calculate parameters
  density_val <- graph.density(graph)
  median_degree <- median(degree(graph))
  edge_density <- ecount(graph) / (vcount(graph) * (vcount(graph) - 1))
  avg_path_length <- average.path.length(graph)
  transitivity_val <- transitivity(graph)
  in_degree <- degree(graph, mode = "in")
  out_degree <- degree(graph, mode = "out")
  in_closeness <- closeness(graph, mode = "in")
  out_closeness <- closeness(graph, mode = "out")
  betweenness_val <- betweenness(graph)
  
  # Print results
  cat("Density:", density_val, "\n")
  cat("Median Degree:", median_degree, "\n")
  cat("Edge Density:", edge_density, "\n")
  cat("Average Path Length:", avg_path_length, "\n")
  cat("Transitivity:", transitivity_val, "\n")
  cat("In-Degree:", mean(in_degree), "\n")
  cat("Out-Degree:", mean(out_degree), "\n")
  cat("In-Closeness:", mean(in_closeness), "\n")
  cat("Out-Closeness:", mean(out_closeness), "\n")
  cat("Betweenness:", mean(betweenness_val), "\n\n")
}
