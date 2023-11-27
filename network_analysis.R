library(igraph)

kaylor2 <- NBkaylor %>%
  group_by(from, to, distance, type_to, type_from) %>%
  tally(name = "freq") %>%
  ungroup()




g1 <- graph_from_data_frame(d=tilney1[, c("from", "to")],vertices=node, directed = TRUE)
node <- unique(Atilney[, c("from", "type_from")])
g1.1 <- graph_from_data_frame(d=Atilney[, c("from", "to")],vertices=node, directed = TRUE)
plot(g1.1,edge.arrow.size=0.1)

g2 <- graph_from_data_frame(tilney2[, c("from", "to")], directed = TRUE)
node <- unique(Btilney[, c("from", "type_from")])
g2.1 <- graph_from_data_frame(d=Btilney[, c("from", "to")],vertices=node, directed = TRUE)
plot(g2.1,edge.arrow.size=0.1)

g3 <- graph_from_data_frame(huiras1[, c("from", "to")], directed = TRUE)
node <- unique(Ahuiras[, c("from", "type_from")])
g3.1 <- graph_from_data_frame(d=Ahuiras[, c("from", "to")],vertices=node, directed = TRUE)
plot(g3.1,edge.arrow.size=0.1)
g4 <- graph_from_data_frame(huiras2[, c("from", "to")], directed = TRUE)
node <- unique(Bhuiras[, c("from", "type_from")])
g4.1 <- graph_from_data_frame(d=Bhuiras[, c("from", "to")],vertices=node, directed = TRUE)
plot(g4.1,edge.arrow.size=0.1)
g5 <- graph_from_data_frame(brewster1[, c("from", "to")], directed = TRUE)
node <- unique(Abrewster[, c("from", "type_from")])
g5.1 <- graph_from_data_frame(d=Abrewster[, c("from", "to")],vertices=node, directed = TRUE)
plot(g5.1,edge.arrow.size=0.1)
g6 <- graph_from_data_frame(brewster2[, c("from", "to")], directed = TRUE)
node <- unique(Bbrewster[, c("from", "type_from")])
g6.1 <- graph_from_data_frame(d=Bbrewster[, c("from", "to")],vertices=node, directed = TRUE)
plot(g6.1,edge.arrow.size=0.1)
g7 <- graph_from_data_frame(kaylor1[, c("from", "to")], directed = TRUE)
node <- unique(Akaylor[, c("from", "type_from")])http://127.0.0.1:34405/graphics/plot_zoom_png?width=1163&height=598
g7.1 <- graph_from_data_frame(d=Akaylor[, c("from", "to")],vertices=node, directed = TRUE)
plot(g7.1,edge.arrow.size=0.1)
g8 <- graph_from_data_frame(kaylor2[, c("from", "to")], directed = TRUE)
node <- unique(Bkaylor[, c("from", "type_from")])
g8.1 <- graph_from_data_frame(d=Bkaylor[, c("from", "to")],vertices=node, directed = TRUE)
plot(g8.1,edge.arrow.size=0.1)

# Set the edge weights to the FREQ column
plot(g1, vertex.color=my_color)
library(RColorBrewer)

node <- unique(Atilney[, c("from", "type_from")])

# Add node ID as vertex attribute
V(g3)$node_id <- node$from

# Add class as vertex attribute
V(g3)$class <- node$type_from

# Plot the graph with colors based on the class
# values is the color mapping for each type_to
values <- c("N" = "#440154FF", "TW" = "#414487FF", "F" = "#2A788EFF", "WF" = "#22A884FF", "GI" = "#7AD151FF", "S" = "#FDE725FF" )

# Create a new attribute for color
V(g1.1)$color <- values[as.character(V(g1.1)$type_to)]

# Plot the graph with vertex colors
plot(g1, vertex.color = V(g1)$color)




# Calculate the total number of edges
total_edges <- ecount(g8)

# Get the production system for each edge
production_systems_from <- E(g8)$type_from
production_systems_to <- E(g8)$type_to

# Calculate the number of edges between different production systems
edges_different_systems <- sum(production_systems_from != production_systems_to)

# Calculate the percentage of connections between different types of farms
percentage_different_systems <- (edges_different_systems / total_edges) * 100

# Print the result
cat(sprintf("%.2f%% of all connections were between farms of different production systems.\n", percentage_different_systems))

E(g1)$distance <- tilney1$distance
E(g1)$type_from <- tilney1$type_from
E(g1)$type_to <- tilney1$type_to
E(g1)$weight <- tilney1$freq


E(g2)$distance <- tilney2$distance
E(g2)$type_from <- tilney2$type_from
E(g2)$type_to <- tilney2$type_to
E(g2)$weight <- tilney2$freq

E(g3)$distance <- huiras1$distance
E(g3)$type_from <- huiras1$type_from
E(g3)$type_to <- huiras1$type_to
E(g3)$weight <- huiras1$freq

E(g4)$distance <- huiras2$distance
E(g4)$type_from <- huiras2$type_from
E(g4)$type_to <- huiras2$type_to
E(g4)$weight <- huiras2$freq

E(g5)$distance <- brewster1$distance
E(g5)$type_from <- brewster1$type_from
E(g5)$type_to <- brewster1$type_to
E(g5)$weight <- brewster1$freq

E(g6)$distance <- brewster2$distance
E(g6)$type_from <- brewster2$type_from
E(g6)$type_to <- brewster2$type_to
E(g6)$weight <- brewster2$freq

E(g7)$distance <- kaylor1$distance
E(g7)$type_from <- kaylor1$type_from
E(g7)$type_to <- kaylor1$type_to
E(g7)$weight <- kaylor1$freq

E(g8)$distance <- kaylor2$distance
E(g8)$type_from <- kaylor2$type_from
E(g8)$type_to <- kaylor2$type_to
E(g8)$weight <- kaylor2$freq



node_degrees <- degree(g2)

vcount(g8)#nodes
ecount(g8)#edges
diameter(g8)#


graph.density(g1)
graph.density(g2)
graph.density(g3)
graph.density(g4)
graph.density(g5)
graph.density(g6)
graph.density(g7)
graph.density(g8)
#degree
degree(g1)
median(degree(g1))
max(degree(g1))
degree(g2)
median(degree(g2))
degree(g3)
median(degree(g3))
degree(g4)
median(degree(g4))
degree(g5)
median(degree(g5))
degree(g6)
median(degree(g6))
degree(g7)
median(degree(g7))
degree(g8)
median(degree(g8))

#The edges density is the ratio of the number of edges to the total number of possible edges.
edges_density <- ecount(g) / (vcount(g) * (vcount(g) - 1))
edges_density <- ecount(g) / (vcount(g) * (vcount(g) - 1))
edges_density <- ecount(g) / (vcount(g) * (vcount(g) - 1))
edges_density <- ecount(g) / (vcount(g) * (vcount(g) - 1))
edges_density <- ecount(g) / (vcount(g) * (vcount(g) - 1))
edges_density <- ecount(g) / (vcount(g) * (vcount(g) - 1))
edges_density <- ecount(g) / (vcount(g) * (vcount(g) - 1))
edges_density <- ecount(g) / (vcount(g) * (vcount(g) - 1))

#The average path length is the average length of the shortest paths between all pairs of nodes.
average.path.length(g1)
average.path.length(g2)
average.path.length(g3)
average.path.length(g4)
average.path.length(g5)
average.path.length(g6)
average.path.length(g7)
average.path.length(g8)




#Transitivity measures the likelihood that two nodes that are connected to the same node are also connected to each other.
transitivity <- transitivity(g1)
mean(transitivity)
# Calculate the mean shortest path length
mean(average.path.length(g8))
transitivity(g8)
indegree <- degree(g1, mode= "in")
outdegree <- degree(g1, mode= "out")
in_closeness <- closeness(g1, mode="in")
out_closeness <- closeness(g1, mode="out")
betweenness <- betweenness(g1)

# Calculate the mean, min, and max value of each metric
mean(indegree)
min(indegree)
max(indegree)

mean(outdegree)
min(outdegree)
max(outdegree)

mean(in_closeness)
min(in_closeness)
max(in_closeness)

mean(out_closeness)
min(out_closeness)
max(out_closeness)

mean(betweenness)
min(betweenness)
max(betweenness)

edge_density(g8)

plot(g1)
communities <- cluster_louvain(g1)
plot(g2)
plot(g3)
plot(g4)
plot(g5)
plot(g6)
plot(g7)
plot(g8)

#general ingoing and outgoing
# Create a list to store these graphs

graph_list <- list(g1, g2, g3, g4, g5, g6, g7, g8)
clusters <- cluster_fast_greedy(combined_graph, directed = TRUE)

# Print the community structure
print(clusters)

# Plot the graph with communities
plot(g1, vertex.color = membership(clusters))


# Assuming you have nodes with unique IDs in all graphs
# Combine all graphs into one
combined_graph <- graph_list[[1]]
combined_graph <- do.call(union, graph_list)

for (i in 2:length(graph_list)) {
  combined_graph <- combined_graph + graph_list[[i]]
}

# Calculate in-degree and out-degree for each node
in_degrees <- degree(combined_graph, mode = "in")
out_degrees <- degree(combined_graph, mode = "out")

# Calculate total nodes in the combined graph
total_nodes <- vcount(combined_graph)

# Calculate in-going and outgoing loyalty for each node
in_going_loyalty <- data.frame(in_degrees / total_nodes * 100)
outgoing_loyalty <- data.frame(out_degrees / total_nodes * 100)

# Print in-going and outgoing loyalty for the first few nodes
degree <- head(data.frame(Node = V(combined_graph)$name, InGoingLoyalty = in_going_loyalty, OutgoingLoyalty = outgoing_loyalty))

##conection between farms
# Assuming 'ProductionSystem' is an attribute of each node indicating the production system

graph_list <- list(g1, g2, g3, g4, g5, g6, g7, g8)
graph_names <- c("g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8")

# Initialize an empty dataframe to store results
results_df <- data.frame(Graph = character(),
                         Density = numeric(),
                         Median_Degree = numeric(),
                         Edge_Density = numeric(),
                         Avg_Path_Length = numeric(),
                         Transitivity = numeric(),
                         In_Degree = numeric(),
                         Min_idegree =numeric(),
                         Max_idegree=numeric(),
                         Out_Degree = numeric(),
                         Min_odegree =numeric(),
                         Max_odegree=numeric(),
                         In_Closeness = numeric(),
                         Min_icloseness =numeric(),
                         Max_icloseness=numeric(),
                         Out_Closeness = numeric(),
                         Min_ocloseness =numeric(),
                         Max_ocloseness=numeric(),
                         Betweenness = numeric(),
                         Min_Betweenness =numeric(),
                         Max_Betweenness=numeric(),
                         stringsAsFactors = FALSE)

# Loop through each graph
for (i in seq_along(graph_list)) {
  g <- graph_list[[i]]
  
  # Calculate network metrics
  density_val <- graph.density(g)
  median_degree <- median(degree(g))
  edge_density <- ecount(g) / (vcount(g) * (vcount(g) - 1))
  avg_path_length <- average.path.length(g)
  transitivity_val <- transitivity(g)
  in_degree <- degree(g, mode = "in")
  out_degree <- degree(g, mode = "out")
  in_closeness <- closeness(g, mode = "in")
  out_closeness <- closeness(g, mode = "out")
  betweenness_val <- betweenness(g)
  
  
  # Append results to the dataframe
  results_df <- rbind(results_df, data.frame(Graph = graph_names[i],
                                             Density = density_val,
                                             Median_Degree = median_degree,
                                             Edge_Density = edge_density,
                                             Avg_Path_Length = avg_path_length,
                                             Transitivity = transitivity_val,
                                             In_Degree = mean(in_degree),
                                             Min_idegree <- min(in_degree),
                                             Max_idegree <- max(in_degree),
                                             Out_Degree = mean(out_degree),
                                             Min_odegree =min(out_degree),
                                             Max_odegree =max(out_degree),
                                             In_Closeness = mean(in_closeness),
                                             Min_icloseness = min(in_closeness),
                                             Max_icloseness = max(in_closeness),
                                             Out_Closeness = mean(out_closeness),
                                             Min_ocloseness = min(out_closeness),
                                             Max_ocloseness = max(out_closeness),
                                             Betweenness = mean(betweenness_val),
                                             Min_Betweenness = min(betweenness_val),
                                             Max_Betweenness = max(betweenness_val)))
}

# Print the results dataframe
print(results_df)
write.csv(results_df, "results_networks.csv")

######comunities
graph_list <- list(g1, g2, g3, g4, g5, g6, g7, g8)

# Loop through each graph, perform community detection, and visualize the communities
for (i in 1:length(graph_list)) {
  # Calculate communities using edge betweenness
  communities <- edge.betweenness.community(graph_list[[i]])
  
  # Access community membership
  community_membership <- membership(communities)
  
  # Plot the graph with community colors
  plot(graph_list[[i]], vertex.color = community_membership, vertex.size = 10, main = paste("Community Detection - G", i))
}
# Assuming you have already loaded the necessary libraries and have your graphs in graph_list
library(igraph)

# Initialize an empty data frame to store the results
results_df <- data.frame()

# Create a function to analyze clusters in a graph
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

# Loop through each graph in graph_list and analyze clusters
for (i in 1:length(graph_list)) {
  cat("Analysis for Graph", i, "\n")
  analyze_clusters(graph_list[[i]], paste("G", i))
  cat("\n")
}

# Print and inspect the results data frame
print(results_df)

# Assuming you have already loaded the necessary libraries and have your graphs in graph_list




