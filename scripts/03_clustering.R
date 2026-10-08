
# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 03_clustering.R
# =====================================================


# 1. LOAD DATA ---------------------------------------

# Load TF-IDF matrix from Script 02.
posts.matrix = readRDS(
  "data/processed/bluesky_tfidf.rds"
)

# Load matching posts.
posts = read.csv(
  "data/processed/bluesky_text_analysis_posts.csv"
)

# Check data dimensions.
dim(posts.matrix)
nrow(posts)

# Ensure the documents match.
stopifnot(nrow(posts.matrix) == nrow(posts))


# 2. REMOVE EMPTY DOCUMENTS --------------------------

# Identify documents with zero TF-IDF weights.
empties = rowSums(posts.matrix^2) == 0

sum(empties)

# Remove empty documents from both datasets.
posts.matrix = posts.matrix[!empties, , drop = FALSE]
posts = posts[!empties, , drop = FALSE]

# Check final dimensions.
dim(posts.matrix)


# 3. NORMALISE DOCUMENT VECTORS ----------------------

# Normalise TF-IDF vectors to unit length.
norm.posts.matrix = diag(
  1 / sqrt(rowSums(posts.matrix^2))
) %*% posts.matrix

# Check normalised vector lengths.
head(sqrt(rowSums(norm.posts.matrix^2)))


# 4. CALCULATE COSINE DISTANCE -----------------------

# Calculate cosine distance using normalised vectors.
D = dist(
  norm.posts.matrix,
  method = "euclidean"
)^2 / 2

# Check distance matrix dimensions.
dim(as.matrix(D))


# 5. MULTIDIMENSIONAL SCALING ------------------------

# Represent documents using 100 MDS dimensions.
N = nrow(posts.matrix)

mds.posts.matrix = cmdscale(
  D,
  k = min(100, N - 2)
)

dim(mds.posts.matrix)


# 6. ELBOW METHOD ------------------------------------

# Compare within-cluster sum of squares for k = 1 to 15.
n = min(15, N - 1)

SSW = rep(0, n)

for (a in 1:n) {
  
  set.seed(123)
  
  K = kmeans(
    mds.posts.matrix,
    a,
    nstart = 20,
    iter.max = 100
  )
  
  SSW[a] = K$tot.withinss
}

# Display elbow results.
round(SSW, 3)

# Plot the elbow curve.

plot(
  1:n,
  SSW,
  type = "b",
  col = "#2563EB",
  pch = 19,
  lwd = 2,
  main = "Elbow Method for AI Discussion Clusters",
  xlab = "Number of Clusters",
  ylab = "Within-Cluster Sum of Squares"
)


# 7. K-MEANS CLUSTERING ------------------------------

# Select seven clusters based on comparison.
number.of.clusters = 7

set.seed(123)

K = kmeans(
  mds.posts.matrix,
  number.of.clusters,
  nstart = 20,
  iter.max = 100
)

# Inspect clustering results.
K$size
K$tot.withinss
K$betweenss
K$totss

# Calculate proportion of between-cluster variation.
K$betweenss / K$totss


# 8. VISUALISE CLUSTERS ------------------------------

# Project documents into two MDS dimensions.
mds2.posts.matrix = cmdscale(
  D,
  k = 2
)

# Plot the clusters.

# Blue palette for seven clusters.
cluster.colors = c(
  "#082F49",
  "#0369A1",
  "#0284C7",
  "#0EA5E9",
  "#38BDF8",
  "#60A5FA",
  "#93C5FD"
)

plot(
  mds2.posts.matrix,
  col = cluster.colors[K$cluster],
  pch = 19,
  main = "AI Discussion Clusters on Bluesky",
  xlab = "MDS Dimension 1",
  ylab = "MDS Dimension 2"
)

legend(
  "topright",
  legend = paste("Cluster", 1:number.of.clusters),
  col = cluster.colors,
  pch = 19
)



# 9. CLUSTER SIZES -----------------------------------

# Add cluster labels to posts.
posts$cluster = K$cluster

# Count posts in each cluster.
table(posts$cluster)

# Calculate cluster percentages.
round(
  prop.table(table(posts$cluster)) * 100,
  2
)

# Visualise cluster sizes.

barplot(
  table(posts$cluster),
  col = cluster.colors,
  border = NA,
  main = "Number of Posts in Each Cluster",
  xlab = "Cluster",
  ylab = "Number of Posts"
)


# 10. IDENTIFY CLUSTER TERMS -------------------------

# Identify the top ten TF-IDF terms per cluster.
for (a in 1:number.of.clusters) {
  
  print(paste("Cluster", a))
  
  # Select posts in the cluster.
  cluster.matrix = posts.matrix[
    K$cluster == a, ,
    drop = FALSE
  ]
  
  # Calculate average TF-IDF weights.
  w = colMeans(cluster.matrix)
  
  # Rank terms by weight.
  o = order(w, decreasing = TRUE)[1:10]
  
  # Display top terms and weights.
  print(colnames(posts.matrix)[o])
  print(round(w[o], 3))
}


# 11A. INSPECT EXAMPLE POSTS --------------------------

# Examine five example posts per cluster.
for (a in 1:number.of.clusters) {
  
  print(paste("Cluster", a))
  
  cluster.posts = posts$text[
    posts$cluster == a
  ]
  
  print(head(cluster.posts, 5))
}


# 11B. CLUSTER SIMILARITY ----------------------------

# Calculate the average TF-IDF profile of each cluster.
cluster.profiles = matrix(
  0,
  nrow = number.of.clusters,
  ncol = ncol(posts.matrix)
)

for (a in 1:number.of.clusters) {
  
  cluster.profiles[a, ] = colMeans(
    posts.matrix[K$cluster == a, , drop = FALSE]
  )
}

# Normalise the cluster profiles.
cluster.profiles = cluster.profiles /
  sqrt(rowSums(cluster.profiles^2))

# Calculate cosine similarity between clusters.
cluster.similarity = cluster.profiles %*%
  t(cluster.profiles)

rownames(cluster.similarity) = paste(
  "Cluster", 1:number.of.clusters
)

colnames(cluster.similarity) = paste(
  "Cluster", 1:number.of.clusters
)

round(cluster.similarity, 3)
# 12. SAVE RESULTS -----------------------------------

# Save posts with cluster assignments.
write.csv(
  posts,
  "data/processed/bluesky_clustered_posts.csv",
  row.names = FALSE
)

# Save K-means results.
saveRDS(
  K,
  "data/processed/bluesky_kmeans.rds"
)
