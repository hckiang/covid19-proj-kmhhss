# set up data
cluster_data <- read.csv("./data/clustering_distance_datasets/cluster_data.csv",
  row.names = 1)

# import library
library(RMaCzek)

# compute Czek matrix
czek_matrix_out <- czek_matrix(cluster_data, scale_data = FALSE, n_classes = 9)
ordering <- attr(czek_matrix_out, "order")

# visualise
par(mar=c(8,8,4,1))
plot(czek_matrix_out)

## stepwise dendrogram - reference: Müllner, D., 2011. Modern hierarchical, agglomerative clustering algorithms. arXiv preprint arXiv:1109.2378

# initialise
S <- ordering
N <- length(S) # number of nodes
size <- rep(1, N)
dendrogram <- matrix(nrow = nrow(cluster_data) - 1, ncol = 4) 

# NB: technically, the strict total order preservation criteria 
# along with the specification of the distance function should 
# make a single column dendrogram suffice, but for the sake of 
# compatibility with Python (Scipy) function for downstream 
# clustering task, 4 columns are being maintained

n <- N

# compute distance matrix (vector); distance between each (node, next node) pair; "next" defined as per ordering
d <- sapply(1:(N-1), function(x)
 sqrt(sum((cluster_data[S[x], ]-cluster_data[S[x + 1], ])^2)))

# update dendrogram matrix
for (i in 1:nrow(dendrogram)) {
  
  # compute one pair of nodes with minimal distance
  min_dist_ind = which(d == min(d))
  
  if (length(min_dist_ind) > 1) {
    
    # randomly break ties: may be superfluous
    min_dist_ind = sample(min_dist_ind, 1)
    
  }
  
  # create new node label
  n = n + 1
  
  # update size vector
  size[n] <- size[S[min_dist_ind]] + size[S[min_dist_ind + 1]]
  
  # update dendrogram matrix
  dendrogram[i, ] = 
    c(S[min_dist_ind], S[min_dist_ind + 1], d[min_dist_ind], size[n])
  
  # update ordering
  S[min_dist_ind] = n
  S = S[-(min_dist_ind + 1)]
  
  # update distance matrix (vector)
  d = d[-min_dist_ind]

  # update number of nodes
  N <- length(S)
  
}

# write dendrogram matrix to file
write.csv(dendrogram, row.names = F,
          file = "./data/clustering_distance_datasets/czekanowski_dendrogram.csv")