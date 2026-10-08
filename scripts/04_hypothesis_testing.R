
# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 04_hypothesis_testing.R
# =====================================================

# Research Question 2:
# Does user engagement differ across
# AI discussion topics?


# 1. LOAD DATA ---------------------------------------

# Load posts and topic clusters from Script 03.
posts = read.csv(
  "data/processed/bluesky_clustered_posts.csv"
)

# Check the dataset.
dim(posts)
names(posts)
head(posts)


# 2. CALCULATE ENGAGEMENT ----------------------------

# Total engagement is the sum of four interactions:
# likes, reposts, replies and quotes.

posts$engagement = posts$like_count +
  posts$repost_count +
  posts$reply_count +
  posts$quote_count

# Examine engagement distribution.
summary(posts$engagement)

# Count posts with zero engagement.
sum(posts$engagement == 0, na.rm = TRUE)

# View the engagement distribution.
hist(
  posts$engagement,
  main = "Distribution of Post Engagement",
  xlab = "Total Engagement"
)


# 3. COMPARE TOPIC CLUSTERS --------------------------

# Treat cluster numbers as categories.
posts$cluster = as.factor(posts$cluster)

# Number of posts in each cluster.
table(posts$cluster)

# Median engagement for each cluster.
aggregate(
  engagement ~ cluster,
  data = posts,
  FUN = median
)

# Mean engagement for each cluster.
aggregate(
  engagement ~ cluster,
  data = posts,
  FUN = mean
)


# 4. HYPOTHESIS TEST ---------------------------------

# H0: Engagement distributions are the same
#     across all topic clusters.
#
# H1: At least one cluster has a different
#     engagement distribution.
#
# Kruskal-Wallis compares engagement ranks
# across more than two independent groups.

test = kruskal.test(
  engagement ~ cluster,
  data = posts
)

# Display the test result.
test


# 5. EFFECT SIZE -------------------------------------

# Epsilon-squared estimates the strength
# of the difference between topic clusters.

# H = Kruskal-Wallis test statistic.
H = as.numeric(test$statistic)

# n = number of posts used in the test.
n = sum(complete.cases(
  posts[, c("engagement", "cluster")]
))

# k = number of topic clusters.
k = nlevels(posts$cluster)

# Calculate epsilon-squared.
epsilon.squared = (H - k + 1) / (n - k)

epsilon.squared


# 6. VISUALISE ENGAGEMENT ----------------------------

# Use log(1 + engagement) to make the
# skewed distribution easier to visualise.
# The statistical test uses original values.

boxplot(
  log1p(engagement) ~ cluster,
  data = posts,
  main = "Engagement Across AI Topic Clusters",
  xlab = "Topic Cluster",
  ylab = "Log(1 + Engagement)"
)


# 7. SAVE RESULTS ------------------------------------

# Save engagement data with topic clusters.
write.csv(
  posts,
  "data/processed/bluesky_engagement_analysis.csv",
  row.names = FALSE
)
