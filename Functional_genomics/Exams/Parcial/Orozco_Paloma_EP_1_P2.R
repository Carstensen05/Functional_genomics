#################################
###### FUNCTIONAL GENOMICS ######
# ANA PALOMA OROZCO CARSTENSEN ##
######### PARCIAL 1_2 ###########
#################################

### Libraries
library (igraph) # Load library
library (igraphdata) # Load library

### P2. Network: karate

## Data

data ("karate") # Load data

## 1. Find the 3 persons more connected 

Degree_centrality_K <- degree (karate) # Calculate the degree of the network
Degree_centrality_K # Print
sort (Degree_centrality_K, decreasing = TRUE) [1:3] # Order the in degree, and only the 3 first ones

# Discussion: John A is the person more connected, with 17 connections, and then Mr Hi, with 16 & Actor 33, with 12.


## 2. The graph of the connectivity distribution 

degree (karate) # Check the degrees
plot (degree_distribution (karate), main = "Degree distribution", xlab = "Connections", ylab = "Frequency") # Plot
hist (degree_distribution (karate), main = "Degree distribution", xlab = "Connections", ylab = "Frequency") # Plot the histogram

# Discussion: It is a long tail network. 


## 3. The diameter of the network

Diameter_K <- diameter (karate) # Obtain the diameter
Diameter_K # Print


## 4. The clustering coefficient of each of the 3 more connected persons

Clustering_coefficient <- transitivity (karate, type = "local", vids = c ("John A", "Mr Hi", "Actor 33")) # Calculate the clustering coefficient locally
Clustering_coefficient # Print


## 5. Find, if possible, the nodes with clustering coefficient of 1. 

Transitivity <- transitivity (karate, type = "local") # Obtain the clustering coefficient
any (Transitivity = 1) # Any has a value = 1? 

# Discussion: Having a clustering coefficient = 1 means that the neighbors of that node (person), are all linked to each other


## 6. The percentage of connections of the total

Length <- length (degree (karate)) # Find the length of the complete data
Length # Print

Sum <- sum (degree (karate)) # Obtain the total of connections
Sum # Print

Proportion <- (Length * 100) / Sum  # Calculate the proportion
Proportion # Print


## 7. The mean of connectivities 

mean (degree (karate)) # Calculate the mean of connectivities


## 8. Find who are the 3 more important persons with 3 different methods

# 1. By degree centrality 
Degree_centrality_K <- degree (karate) # Calculate the degree of the network
Degree_centrality_K # Print
sort (Degree_centrality_K, decreasing = TRUE) [1:3] # Order the in degree, and only the 3 first ones

# Discussion: John A, Mr Hi & Actor 33 are the most important nodes, because they are the ones with more connections

# 2. By losness centrality 
Closness_centrality_K <- closeness (karate, mode = "all") # Calculate the closness centrality of the network
Closness_centrality_K # Print
sort (Closness_centrality_K, decreasing = TRUE) [1:3] # Order the in degree, and only the 3 first ones

# Discussion: Mr Hi, John A and Actor 20 are the most important nodes, because they is the closest to other persons.

# 3. By betweenness centrality 
Betweenness_Centrality_K <- betweenness (karate, directed = TRUE) # Calculate the betweenness centrality of the network 
Betweenness_Centrality_K # Print
sort (Betweenness_Centrality_K, decreasing = TRUE) [1:3] # Order the in degree, and only the 3 first ones

# Discussion: Mr Hi, John A and Actor 20 are the most important nodes, because the they are the one with more passes through 


## 9. Find the path between the more far nodes

Longest_Path <- get_diameter (karate) # Obtain the longest path
Longest_Path # Print


## 10. Cluster the network with 4 different methods

# 1. Cluster based on edge betweenness

Cluster_EB <- cluster_edge_betweenness (karate, directed = FALSE) # Create the cluster
Cluster_EB # Print

membership (Cluster_EB) # Obtain which member are in which clusters
table (membership (Cluster_EB)) # Obtain the number of members by clusters

plot (Cluster_EB, karate) # Plot by clusters

# 2. Cluster by community strucure via short random walks

Cluster_W <- cluster_walktrap (karate) # Create the cluster
Cluster_W # Print

membership (Cluster_W) # Obtain which member are in which clusters
table (membership (Cluster_W)) # Obtain the number of members by clusters

plot (Cluster_W, karate) # Plot by clusters

# 3. Cluster by finding communities in graphs based on statistical meachanics

Cluster_S <- cluster_spinglass (karate) # Create the cluster
Cluster_S # Print

membership (Cluster_S) # Obtain which member are in which clusters
table (membership (Cluster_S)) # Obtain the number of members by clusters

plot (Cluster_S, karate) # Plot by clusters

# 4. Cluster by optimal community structure

Cluster_O <- cluster_optimal (karate) # Create the cluster
Cluster_O # Print

membership (Cluster_O) # Obtain which member are in which clusters
table (membership (Cluster_O)) # Obtain the number of members by clusters

plot (Cluster_O, karate) # Plot by clusters

# Discussion: It looks like there are 2 principal clusters in all the 4 methods, and smaller clusters are inside them.

