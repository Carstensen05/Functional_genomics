#################################
###### FUNCTIONAL GENOMICS ######
# ANA PALOMA OROZCO CARSTENSEN ##
######### PARCIAL 1_1 ###########
#################################

### Libraries
library (igraph) # Load library
library (igraphdata) # Load library

### P1. Network: Friends

## Data
Friends <- read.csv("Data/Friends_NW.csv") # Load dataset


## Fix the dataset
row.names (Friends) <- Friends [ , 1] # Take the first column and make it the row names
Friends <- Friends [ , -1] # Take first row


## Matrix 

Friends <- as.matrix (Friends) # Save it as matrix
class (Friends) # Check class
Friends # Print


## Network 

Network <- graph_from_adjacency_matrix (Friends, mode = "directed") # Create network
Network # Print


## 1. Graph the network 

plot (Network)


## 2. Determine the 3 persons with more friends

In_degree <- degree (Network, mode = "in") # Check the in degree
In_degree # Print
sort (In_degree, decreasing = TRUE) [1:3] # Order the in degree

# Discussion: Carolina, Mayela and Mafer Mtz. are the ones with more friends


## 3. Determine the 3 persons who considere more friends 

Out_degree <- degree (Network, mode = "out") # Check the out degree
Out_degree # Print
sort (Out_degree, decreasing = TRUE) [1:3] # Order the in degree

# Discussion: Enrique, Julián and Mafer Mtz. are the ones that considere more friends


## 4. The 3 persons more important by 3 centrality meassures 

# 1. By degree centrality 

Degree_centrality <- degree (Network) # Calculate the degree of the network
Degree_centrality # Print
sort (Degree_centrality, decreasing = TRUE) [1:3] # Order the in degree, and only the 3 first ones

# Discussion: The most important person is Mafer Mtz., with a degree value of 24. It is because she is the one with more connections.

# 2. By losness centrality 
Closness_centrality <- closeness (Network, mode = "all") # Calculate the closness centrality of the network
Closness_centrality # Print
sort (Closness_centrality, decreasing = TRUE) [1:3] # Order the in degree, and only the 3 first ones

# Discussion: The most important person is the Enrique, with a closness centrality value of 0.04545455. It is because he is the closest to other persons.

# 3. By betweenness centrality 
Betweenness_Centrality <- betweenness (Network, directed = TRUE) # Calculate the betweenness centrality of the network 
Betweenness_Centrality # Print
sort (Betweenness_Centrality, decreasing = TRUE) [1:3] # Order the in degree, and only the 3 first ones

# Discussion: The most important person is Mayela, with a betweenness value of 30.75090. It is because the she is the one with more passes through 


## 5. Cluster the network with at least 2 methods and determine which are the clusters

# 1. Cluster by optimal community structure

Cluster_O <- cluster_optimal (Network) # Create the cluster
Cluster_O # Print

membership (Cluster_O) # Obtain which members are in which clusters
groups (Cluster_O) # Separate by clusters

plot (Cluster_O, Network) # Plot by clusters

# Discussion: I would give it an 9, just because i think Isabel and Sara should be in a different cluster

# 2. Cluster by community strucure via short random walks

Cluster_W <- cluster_walktrap (Network) # Create the cluster
Cluster_W # Print

membership (Cluster_W) # Obtain which members are in which clusters
groups (Cluster_W) # Separate by clusters

plot (Cluster_W, Network) # Plot by clusters

# Discussion: I would give it an 10, hahaha. 


## 6. Calculate the diameter

Diameter <- diameter (Network) # Calculate the diameter
Diameter # Print

## 7. Distance matrix and a heatmap 

Adjacent_matrix <- as.matrix (get.adjacency (Network)) # Create the adjacent matrix
Adjacent_matrix # Print

heatmap (Adjacent_matrix, Rowv = NA, Colv = "Rowv") # Plot a heatmap from the matrix 