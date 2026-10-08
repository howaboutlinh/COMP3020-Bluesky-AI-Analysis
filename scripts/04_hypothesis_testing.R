# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 04_hypothesis_testing.R
#
# RQ2: Does engagement differ between AI topics?
# Difference in means with a randomisation test and a
# t-test, following Module 10.
# =====================================================


# 1. LOAD DATA ---------------------------------------

# Posts with cluster labels from Script 03.
posts = read.csv("data/processed/bluesky_clustered_posts.csv")


# 2. ENGAGEMENT --------------------------------------

# Engagement of a post = likes + reposts + replies.
posts$engagement = posts$like_count + posts$repost_count + posts$reply_count
summary(posts$engagement)


# 3. SELECT THE TWO TOPICS TO COMPARE ----------------

# Cluster 6: AI news and link sharing.
# Cluster 5: personal opinions and discussion about using AI.
# Chosen because they are the two largest clusters.
cluster.A = 6
cluster.B = 5

groupA = posts$engagement[posts$cluster == cluster.A]
groupB = posts$engagement[posts$cluster == cluster.B]

length(groupA)
length(groupB)

# Hypotheses (two-sided, significance level 0.05):
# H0: mean engagement of news posts = mean engagement of opinion posts
# HA: mean engagement of news posts != mean engagement of opinion posts


# 4. OBSERVED DIFFERENCE IN MEANS --------------------

mean(groupA)
mean(groupB)

meanDiff = mean(groupB) - mean(groupA)
print(meanDiff)


# 5. RANDOMISATION TEST (Module 10) ------------------

# If H0 is true, the topic label does not matter, so we
# shuffle the labels and recompute the difference.
shuffleMean = function(groupA, groupB) {
  # Put both groups together.
  combined = c(groupA, groupB)
  # Randomly choose which posts form the new group A.
  chosen = sample(length(combined), size = length(groupA), replace = FALSE)
  shuffledA = combined[chosen]
  shuffledB = combined[-chosen]
  # Difference in means for the shuffled groups.
  mean(shuffledB) - mean(shuffledA)
}

# Repeat the shuffle 1000 times to get the distribution
# of differences we would expect under H0.
set.seed(123)
randDist = replicate(1000, shuffleMean(groupA, groupB))

hist(randDist, col = "lightblue",
     main = "Randomisation Distribution",
     xlab = "Difference in mean engagement (opinion - news)")
abline(v = meanDiff, col = "red", lwd = 2)

# Two-sided p-value: proportion of shuffled differences at
# least as extreme as the observed one (abs() = both sides).
pVal = mean(abs(randDist) > abs(meanDiff))
print(pVal)


# 6. T-TEST ------------------------------------------

# Welch two-sample t-test as a comparison.
tt = t.test(groupA, groupB)
tt
tt$p.value


# 7. MEAN ENGAGEMENT BY TOPIC ------------------------

# Mean engagement of each cluster.
cluster.means = rep(0, 7)
for (a in 1:7) {
  cluster.means[a] = mean(posts$engagement[posts$cluster == a])
}
round(cluster.means, 2)

# Bar chart of the means; the two tested clusters are highlighted.
barplot(cluster.means, names.arg = 1:7,
        col = c("grey", "grey", "grey", "grey", "lightgreen", "lightblue", "grey"),
        main = "Mean Engagement by Topic Cluster",
        xlab = "Cluster", ylab = "Mean likes + reposts + replies")


# 8. ALL TOPICS TOGETHER: ONE-WAY ANOVA --------------

# The randomisation test above compares only two topics.
# To check RQ2 across ALL topics we use ANOVA, which the
# Module 10 lab runs with aov() (one factor here: cluster).
#
# H0: all topics have the same mean engagement
# HA: at least one topic has a different mean engagement
#
# factor() tells R that cluster numbers are group labels,
# not numeric values.
model = aov(engagement ~ factor(cluster), data = posts)
summary(model)

# Caution: ANOVA assumes roughly normal values with similar
# spread in each group. Engagement is very skewed, so the
# result is interpreted together with the randomisation test.
