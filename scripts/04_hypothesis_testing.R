
# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 04_hypothesis_testing.R
# =====================================================


# 1. LOAD DATA ---------------------------------------

# Load posts with cluster labels from Script 03.
posts = read.csv(
  "data/processed/bluesky_clustered_posts.csv"
)

# Check the data.
dim(posts)
names(posts)


# 2. PREPARE ENGAGEMENT DATA -------------------------

# Check required columns.
stopifnot(
  all(c(
    "cluster",
    "like_count",
    "repost_count",
    "reply_count"
  ) %in% names(posts))
)

# Remove posts with missing engagement values.
posts = posts[
  complete.cases(
    posts[, c("like_count", "repost_count", "reply_count")]
  ),
]

# Calculate total engagement.
posts$engagement = posts$like_count +
  posts$repost_count +
  posts$reply_count

# Check engagement values.
summary(posts$engagement)


# 3. SELECT TWO GROUPS -------------------------------

# Cluster 1: Broad AI and ChatGPT discussions.
cluster1 = posts$engagement[
  posts$cluster == 1
]

# Cluster 5: AI Art / Stable Diffusion (verify with current cluster terms).
cluster5 = posts$engagement[
  posts$cluster == 5
]

# Check sample sizes.
length(cluster1)
length(cluster5)


# 4. DIFFERENCE IN MEANS -----------------------------

# Calculate mean engagement for each group.
mean(cluster1)
mean(cluster5)

# Calculate observed difference.
meanDiff = mean(cluster5) - mean(cluster1)

print(meanDiff)


# 5. SHUFFLING THE OBSERVATIONS ----------------------

# Shuffle two samples without changing their sizes.
shuffleMean = function(cluster1, cluster5) {
  
  combined = c(cluster1, cluster5)
  
  chosen = sample(
    length(combined),
    size = length(cluster1),
    replace = FALSE
  )
  
  shuffledBefore = combined[chosen]
  shuffledAfter = combined[-chosen]
  
  mean(shuffledAfter) - mean(shuffledBefore)
}


# 6. RANDOMISATION DISTRIBUTION ----------------------

# Generate the randomisation distribution.
set.seed(123)

randDist = replicate(
  1000,
  shuffleMean(cluster1, cluster5)
)

# Plot the randomisation distribution.
hist(
  randDist,
  col = "#93C5FD",
  main = "Randomisation Distribution",
  xlab = "Difference in Mean Engagement"
)

# Mark the observed difference.
abline(
  v = meanDiff,
  col = "#2563EB",
  lwd = 2
)

# Calculate the two-sided p-value.
pVal = mean(
  abs(randDist) >= abs(meanDiff)
)

print(pVal)


# 7. T-TEST ------------------------------------------

# Perform the two-sample t-test.
tt = t.test(cluster1, cluster5)

tt
tt$statistic
tt$p.value
