# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 03_clustering.R
#
# RQ1: What are the main topics in AI-related posts?
# K-means with cosine distance, following Module 6.
# =====================================================


# 1. LOAD DATA ---------------------------------------

# TF-IDF matrix and matching posts from Script 02.
posts.matrix = readRDS("data/processed/bluesky_tfidf.rds")
posts = read.csv("data/processed/bluesky_text_posts.csv")

# Each row of the matrix must match one post.
dim(posts.matrix)
nrow(posts)


# 2. COSINE DISTANCE ---------------------------------

# Normalise every post vector to length 1, so long posts
# do not look different only because they have more words.
norm.posts.matrix = diag(1 / sqrt(rowSums(posts.matrix^2))) %*% posts.matrix

# For unit vectors, squared Euclidean distance / 2
# equals cosine distance.
D = dist(norm.posts.matrix, method = "euclidean")^2 / 2


# 3. PROJECT INTO EUCLIDEAN SPACE (MDS) --------------

# k-means only works with Euclidean distance, so MDS
# places the posts in 100 dimensions that preserve the
# cosine distances as closely as possible.
mds.posts.matrix = cmdscale(D, k = 100)
dim(mds.posts.matrix)


# 4. ELBOW METHOD: CHOOSE THE NUMBER OF CLUSTERS -----

# Within-cluster sum of squares (SSW) for k = 1 to 15.
n = 15
SSW = rep(0, n)

for (a in 1:n) {
  set.seed(123)
  K = kmeans(mds.posts.matrix, a, nstart = 20)
  SSW[a] = K$tot.withinss
}

round(SSW, 2)

plot(1:n, SSW, type = "b", pch = 19,
     main = "Elbow Method", xlab = "Number of clusters (k)",
     ylab = "Within-cluster sum of squares")

# Save the elbow values for the report.
write.csv(data.frame(k = 1:n, SSW = SSW),
          "data/processed/clustering_elbow_values.csv", row.names = FALSE)


# 5. K-MEANS CLUSTERING ------------------------------

# Number of clusters chosen from the elbow plot and
# from how interpretable the cluster terms are.
number.of.clusters = 7

# nstart = 20 runs k-means from 20 random starts and
# keeps the best result; set.seed() makes it repeatable.
set.seed(123)
K = kmeans(mds.posts.matrix, number.of.clusters, nstart = 20)

# Number of posts in each cluster.
table(K$cluster)

# Proportion of total variation explained by the clusters.
K$betweenss / K$totss

# Store the cluster of each post.
posts$cluster = K$cluster


# 6. VISUALISE THE CLUSTERS --------------------------

# A separate 2-dimensional MDS is used only for plotting;
# colours show the clusters found in 100 dimensions.
mds2.posts.matrix = cmdscale(D, k = 2)

plot(mds2.posts.matrix, col = K$cluster, pch = 19, cex = 0.6,
     main = "AI Discussion Clusters (2D MDS)",
     xlab = "MDS dimension 1", ylab = "MDS dimension 2")
legend("topright", legend = paste("Cluster", 1:number.of.clusters),
       col = 1:number.of.clusters, pch = 19, cex = 0.7)

barplot(table(K$cluster), col = 1:number.of.clusters,
        main = "Number of Posts in Each Cluster",
        xlab = "Cluster", ylab = "Number of posts")


# 7. DESCRIBE EACH CLUSTER ---------------------------

# For each cluster, average the TF-IDF weight of every term
# over the cluster's posts. The highest averages describe
# the cluster (Module 6).
top.terms = NULL

for (a in 1:number.of.clusters) {

  # Rows (posts) that belong to cluster a.
  clusterpostsId = which(K$cluster == a)
  clusterposts = posts.matrix[clusterpostsId, , drop = FALSE]

  # Average weight of each term in this cluster.
  clusterTermWeight = colMeans(clusterposts)

  # The 10 highest-weighted terms.
  top10 = sort(clusterTermWeight, decreasing = TRUE)[1:10]
  print(paste("Cluster", a))
  print(round(top10, 3))

  # Keep the terms for the report.
  top.terms = rbind(top.terms, data.frame(cluster = a, term = names(top10)))
}

write.csv(top.terms, "data/processed/clustering_top_terms.csv", row.names = FALSE)

# Read example posts from each cluster to name the themes.
for (a in 1:number.of.clusters) {
  print(paste("Cluster", a))
  print(head(posts$text[posts$cluster == a], 5))
}


# 8. SAVE RESULTS ------------------------------------

write.csv(posts, "data/processed/bluesky_clustered_posts.csv", row.names = FALSE)
