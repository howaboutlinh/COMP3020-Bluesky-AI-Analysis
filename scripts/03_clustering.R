
# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 03_clustering.R
# =====================================================

# Research Question 1:
# What are the main topics in AI-related
# discussions on Bluesky?

# 1. LOAD DATA ---------------------------------------

# Load the TF-IDF matrix from Script 02.
tfidf = readRDS("data/processed/bluesky_tfidf.rds")

# Load the matching posts.
posts = read.csv(
  "data/processed/bluesky_text_analysis_posts.csv"
)

# Check the data.
dim(tfidf)
nrow(posts)


# 2. ELBOW METHOD ------------------------------------

# The Elbow Method helps us choose the
# number of clusters using within-cluster
# sum of squares (WSS).

set.seed(3020)

k.values = 2:10
wss = numeric(length(k.values))

for (i in 1:length(k.values)) {
  result = kmeans(tfidf, centers = k.values[i],
                  nstart = 10)
  wss[i] = result$tot.withinss
}

# Plot the Elbow Method.
plot(
  k.values, wss,
  type = "b",
  xlab = "Number of Clusters (k)",
  ylab = "Within-Cluster Sum of Squares",
  main = "Elbow Method"
)


# 3. K-MEANS CLUSTERING ------------------------------

# Use seven clusters for the analysis.
# The elbow is not very clear, so k = 7
# is an exploratory choice.

set.seed(3020)

model = kmeans(
  tfidf,
  centers = 7,
  nstart = 10
)

# Number of posts in each cluster.
table(model$cluster)

# Proportion of variation explained.
model$betweenss / model$totss


# 4. IDENTIFY CLUSTER TOPICS -------------------------

# Find the most important terms in each cluster.
# Use average TF-IDF scores to describe topics.

for (i in 1:7) {
  
  cluster.posts = tfidf[model$cluster == i, ,
                        drop = FALSE]
  
  scores = colMeans(cluster.posts)
  
  top.words = head(
    sort(scores, decreasing = TRUE), 10
  )
  
  print(i)
  print(top.words)
}


# 5. SAVE CLUSTER RESULTS ----------------------------

# Add cluster membership to the posts.
posts$cluster = model$cluster

# Save the posts and their assigned clusters.
write.csv(
  posts,
  "data/processed/bluesky_clustered_posts.csv",
  row.names = FALSE
)

# Save the K-means model.
saveRDS(
  model,
  "data/processed/bluesky_kmeans_model.rds"
)


# 6. VISUALISE CLUSTER SIZES -------------------------

# Compare the number of posts in each cluster.
barplot(
  table(posts$cluster),
  main = "Number of Posts by Topic Cluster",
  xlab = "Cluster",
  ylab = "Number of Posts"
)
