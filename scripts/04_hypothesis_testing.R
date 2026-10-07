# COMP3020 - AI Discussions on Bluesky
# 04_hypothesis_testing.R
# RQ2: Does user engagement differ across AI discussion topics?

# The seven canonical RQ1 k-means clusters are used as reproducible lexical
# topic proxies. They are not silently collapsed into broader themes.


# 1. PATHS AND SETTINGS

posts_path <- "data/processed/bluesky_text_posts.csv"
clustered_path <- "data/processed/bluesky_clustered_posts.csv"
duplicate_log_path <- "data/processed/bluesky_text_duplicate_log.csv"

analysis_output_path <- "data/processed/rq2_engagement_by_topic.csv"
distribution_output_path <-
  "data/processed/rq2_engagement_distribution.csv"
test_output_path <- "data/processed/rq2_test_results.csv"
posthoc_output_path <- "data/processed/rq2_posthoc_comparisons.csv"
audit_output_path <- "data/processed/rq2_sample_audit.csv"
figure_output_path <- "figures/rq2_engagement_by_topic.png"

alpha <- 0.05
expected_clusters <- 1:7
engagement_components <- c(
  "like_count", "repost_count", "reply_count", "quote_count"
)


# 2. LOAD AND VALIDATE RQ1 OUTPUTS

input_paths <- c(posts_path, clustered_path, duplicate_log_path)
missing_files <- input_paths[!file.exists(input_paths)]

if (length(missing_files) > 0) {
  stop("Missing required input files: ", paste(missing_files, collapse = ", "))
}

posts <- read.csv(
  posts_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)
clustered <- read.csv(
  clustered_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)
duplicate_log <- read.csv(
  duplicate_log_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)

required_post_columns <- c("uri", engagement_components)
required_cluster_columns <- c("uri", "cluster", "cluster_label")
required_duplicate_columns <- c("duplicate_uri", "retained_uri")

check_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(label, " is missing columns: ", paste(missing, collapse = ", "))
  }
}

check_columns(posts, required_post_columns, "Canonical text-post data")
check_columns(clustered, required_cluster_columns, "Clustered-post data")
check_columns(duplicate_log, required_duplicate_columns, "Duplicate log")

if (anyDuplicated(posts$uri) > 0 || anyDuplicated(clustered$uri) > 0) {
  stop("Post URIs must be unique in both canonical input datasets.")
}
if (anyDuplicated(duplicate_log$duplicate_uri) > 0) {
  stop("The duplicate log contains repeated duplicate_uri values.")
}
if (anyNA(posts$uri) || anyNA(clustered$uri) || anyNA(duplicate_log$duplicate_uri) ||
    anyNA(duplicate_log$retained_uri)) {
  stop("URI fields used for RQ2 joins must not be missing.")
}
if (!all(clustered$uri %in% posts$uri)) {
  stop("Some clustered URIs are absent from the canonical text-post data.")
}
if (!all(duplicate_log$duplicate_uri %in% posts$uri)) {
  stop("Some duplicate-log URIs are absent from the canonical text-post data.")
}
if (!all(duplicate_log$retained_uri %in% posts$uri)) {
  stop("Some retained URIs are absent from the canonical text-post data.")
}
if (!setequal(sort(unique(clustered$cluster)), expected_clusters)) {
  stop("Canonical clustered posts must contain exactly clusters 1 through 7.")
}

# Counts must be finite, whole, non-negative post-level interaction counts.
for (component in engagement_components) {
  values <- posts[[component]]
  if (!is.numeric(values)) {
    stop(component, " is not numeric.")
  }
  if (anyNA(values) || any(!is.finite(values))) {
    stop(component, " contains missing or non-finite values.")
  }
  if (any(values < 0) || any(values != floor(values))) {
    stop(component, " contains negative or non-integer values.")
  }
}


# 3. RESTORE POST-LEVEL TOPIC ASSIGNMENTS

# Exact duplicate text was removed only while fitting RQ1. A removed post gets
# the retained identical-text post's cluster when that retained post was among
# the 855 clusterable documents. All joins are URI based.
canonical_match <- match(posts$uri, clustered$uri)
duplicate_match <- match(posts$uri, duplicate_log$duplicate_uri)
retained_uri_for_post <- duplicate_log$retained_uri[duplicate_match]
retained_cluster_match <- match(retained_uri_for_post, clustered$uri)

assigned_cluster <- clustered$cluster[canonical_match]
assigned_label <- clustered$cluster_label[canonical_match]
mapping_status <- rep("unassigned_no_cluster", nrow(posts))
mapping_status[!is.na(canonical_match)] <- "canonical_unique_text"

mappable_duplicate <- is.na(canonical_match) &
  !is.na(duplicate_match) &
  !is.na(retained_cluster_match)

assigned_cluster[mappable_duplicate] <-
  clustered$cluster[retained_cluster_match[mappable_duplicate]]
assigned_label[mappable_duplicate] <-
  clustered$cluster_label[retained_cluster_match[mappable_duplicate]]
mapping_status[mappable_duplicate] <- "mapped_duplicate_text"

included <- !is.na(assigned_cluster)

rq2 <- data.frame(
  uri = posts$uri[included],
  cluster = as.integer(assigned_cluster[included]),
  cluster_label = assigned_label[included],
  duplicate_mapping_status = mapping_status[included],
  like_count = posts$like_count[included],
  repost_count = posts$repost_count[included],
  reply_count = posts$reply_count[included],
  quote_count = posts$quote_count[included],
  stringsAsFactors = FALSE
)

# Total engagement gives each available interaction one count and no implicit
# weighting: likes + reposts + replies + quotes.
rq2$total_engagement <- rowSums(rq2[, engagement_components, drop = FALSE])

if (anyDuplicated(rq2$uri) > 0) {
  stop("The final RQ2 post-level dataset contains duplicate URIs.")
}
if (anyNA(rq2$cluster) || !all(rq2$cluster %in% expected_clusters)) {
  stop("Every included RQ2 post must have a valid canonical cluster.")
}
if (!identical(
  rq2$total_engagement,
  rowSums(rq2[, engagement_components, drop = FALSE])
)) {
  stop("total_engagement does not equal the sum of its four components.")
}

# Direct cluster assignments must exactly agree with RQ1.
canonical_rows <- rq2$duplicate_mapping_status == "canonical_unique_text"
agreement_match <- match(rq2$uri[canonical_rows], clustered$uri)
if (!identical(
  rq2$cluster[canonical_rows],
  as.integer(clustered$cluster[agreement_match])
)) {
  stop("Direct RQ2 cluster assignments disagree with canonical RQ1 output.")
}

# Mapped duplicates must exactly inherit their retained post's cluster.
mapped_rows <- rq2$duplicate_mapping_status == "mapped_duplicate_text"
mapped_log_match <- match(rq2$uri[mapped_rows], duplicate_log$duplicate_uri)
mapped_retained_match <- match(
  duplicate_log$retained_uri[mapped_log_match], clustered$uri
)
if (anyNA(mapped_retained_match) || !identical(
  rq2$cluster[mapped_rows],
  as.integer(clustered$cluster[mapped_retained_match])
)) {
  stop("A mapped duplicate did not inherit the retained post's cluster.")
}


# 4. SAMPLE AUDIT AND DISTRIBUTION AUDIT

canonical_usable_posts <- nrow(posts)
unique_text_clustered_posts <- nrow(clustered)
mapped_duplicate_posts <- sum(mappable_duplicate)
unassignable_duplicate_posts <- sum(
  !is.na(duplicate_match) & !mappable_duplicate
)
included_posts <- nrow(rq2)
excluded_posts <- sum(!included)

sample_audit <- data.frame(
  measure = c(
    "canonical_usable_text_posts",
    "unique_text_clustered_posts",
    "duplicate_posts_in_log",
    "duplicate_posts_mapped_to_cluster",
    "duplicate_posts_not_assignable",
    "primary_posts_included",
    "posts_excluded_no_cluster"
  ),
  value = c(
    canonical_usable_posts,
    unique_text_clustered_posts,
    nrow(duplicate_log),
    mapped_duplicate_posts,
    unassignable_duplicate_posts,
    included_posts,
    excluded_posts
  ),
  stringsAsFactors = FALSE
)

summarise_measure <- function(values, cluster_number, measure_name) {
  data.frame(
    cluster = cluster_number,
    cluster_label = paste("Cluster", cluster_number),
    measure = measure_name,
    n = length(values),
    mean = mean(values),
    median = median(values),
    standard_deviation = sd(values),
    iqr = IQR(values),
    minimum = min(values),
    q1 = unname(quantile(values, 0.25, type = 7)),
    q3 = unname(quantile(values, 0.75, type = 7)),
    maximum = max(values),
    proportion_zero = mean(values == 0),
    stringsAsFactors = FALSE
  )
}

distribution_measures <- c("total_engagement", engagement_components)
distribution_rows <- list()
row_number <- 1

for (measure_name in distribution_measures) {
  for (cluster_number in expected_clusters) {
    values <- rq2[[measure_name]][rq2$cluster == cluster_number]
    distribution_rows[[row_number]] <- summarise_measure(
      values, cluster_number, measure_name
    )
    row_number <- row_number + 1
  }
}

engagement_distribution <- do.call(rbind, distribution_rows)


# 5. PRIMARY OMNIBUS TEST AND EFFECT SIZE

# ANOVA is not the primary test because the observed outcome is a highly
# right-skewed count with many zeros, extreme observations, unequal group
# sizes, and visibly different spreads. Kruskal-Wallis is a course-appropriate
# rank-based omnibus comparison that does not require normal residuals. With
# distributions of different shapes, it is interpreted as a test of equal
# distributions/rank locations, not solely as a test of means or medians.
#
# H0: Total-engagement distributions are the same across the seven canonical
#     lexical clusters.
# H1: At least one canonical lexical cluster differs in total engagement.

primary_test <- kruskal.test(
  total_engagement ~ factor(cluster),
  data = rq2
)

kruskal_epsilon_squared <- function(test, n, groups) {
  # Epsilon-squared_H = (H - k + 1) / (n - k). Sampling variation can yield a
  # negative estimate near zero, so the reported magnitude is bounded at zero.
  max(0, (unname(test$statistic) - groups + 1) / (n - groups))
}

primary_effect <- kruskal_epsilon_squared(
  primary_test, nrow(rq2), length(expected_clusters)
)


# 6. CONDITIONAL POST-HOC COMPARISONS

empty_posthoc <- data.frame(
  cluster_1 = integer(),
  cluster_2 = integer(),
  n_cluster_1 = integer(),
  n_cluster_2 = integer(),
  median_cluster_1 = numeric(),
  median_cluster_2 = numeric(),
  mean_rank_cluster_1 = numeric(),
  mean_rank_cluster_2 = numeric(),
  higher_mean_rank_cluster = character(),
  raw_p_value = numeric(),
  adjusted_p_value_holm = numeric(),
  significant_after_holm = logical(),
  stringsAsFactors = FALSE
)

posthoc <- empty_posthoc

if (primary_test$p.value < alpha) {
  pairs <- combn(expected_clusters, 2)
  pair_rows <- vector("list", ncol(pairs))

  for (i in seq_len(ncol(pairs))) {
    cluster_1 <- pairs[1, i]
    cluster_2 <- pairs[2, i]
    values_1 <- rq2$total_engagement[rq2$cluster == cluster_1]
    values_2 <- rq2$total_engagement[rq2$cluster == cluster_2]
    pair_ranks <- rank(c(values_1, values_2), ties.method = "average")
    rank_1 <- mean(head(pair_ranks, length(values_1)))
    rank_2 <- mean(tail(pair_ranks, length(values_2)))

    comparison <- suppressWarnings(wilcox.test(
      values_1,
      values_2,
      alternative = "two.sided",
      exact = FALSE,
      correct = TRUE
    ))

    pair_rows[[i]] <- data.frame(
      cluster_1 = cluster_1,
      cluster_2 = cluster_2,
      n_cluster_1 = length(values_1),
      n_cluster_2 = length(values_2),
      median_cluster_1 = median(values_1),
      median_cluster_2 = median(values_2),
      mean_rank_cluster_1 = rank_1,
      mean_rank_cluster_2 = rank_2,
      higher_mean_rank_cluster = if (rank_1 > rank_2) {
        paste("Cluster", cluster_1)
      } else if (rank_2 > rank_1) {
        paste("Cluster", cluster_2)
      } else {
        "Tie"
      },
      raw_p_value = comparison$p.value,
      stringsAsFactors = FALSE
    )
  }

  posthoc <- do.call(rbind, pair_rows)
  posthoc$adjusted_p_value_holm <- p.adjust(
    posthoc$raw_p_value, method = "holm"
  )
  posthoc$significant_after_holm <-
    posthoc$adjusted_p_value_holm < alpha
}


# 7. UNIQUE-TEXT SENSITIVITY ANALYSIS

sensitivity <- rq2[canonical_rows, , drop = FALSE]

if (nrow(sensitivity) != nrow(clustered) ||
    !setequal(sensitivity$uri, clustered$uri)) {
  stop("Sensitivity data are not exactly the canonical clustered posts.")
}

sensitivity_test <- kruskal.test(
  total_engagement ~ factor(cluster),
  data = sensitivity
)
sensitivity_effect <- kruskal_epsilon_squared(
  sensitivity_test,
  nrow(sensitivity),
  length(expected_clusters)
)

primary_significant <- primary_test$p.value < alpha
sensitivity_significant <- sensitivity_test$p.value < alpha
sensitivity_conclusion <- if (identical(
  primary_significant, sensitivity_significant
)) {
  "Statistical-significance conclusion unchanged"
} else {
  "Statistical-significance conclusion changed"
}

test_results <- data.frame(
  analysis = c(
    "Primary: all topic-assignable posts",
    "Sensitivity: canonical unique-text clustered posts"
  ),
  outcome = "total_engagement",
  test = "Kruskal-Wallis rank sum test",
  n = c(nrow(rq2), nrow(sensitivity)),
  groups = length(expected_clusters),
  statistic_h = c(
    unname(primary_test$statistic),
    unname(sensitivity_test$statistic)
  ),
  degrees_of_freedom = c(
    unname(primary_test$parameter),
    unname(sensitivity_test$parameter)
  ),
  p_value = c(primary_test$p.value, sensitivity_test$p.value),
  effect_size = "epsilon_squared_H",
  effect_size_value = c(primary_effect, sensitivity_effect),
  alpha = alpha,
  significant = c(primary_significant, sensitivity_significant),
  stringsAsFactors = FALSE
)

if (any(!is.finite(unlist(test_results[, c(
  "statistic_h", "degrees_of_freedom", "p_value", "effect_size_value"
)])))) {
  stop("Statistical results contain NA, NaN, or infinite values.")
}
if (nrow(posthoc) > 0 && any(!is.finite(c(
  posthoc$raw_p_value, posthoc$adjusted_p_value_holm
)))) {
  stop("Post-hoc results contain NA, NaN, or infinite values.")
}


# 8. EXPORT DATA AND FIGURE

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

write.csv(rq2, analysis_output_path, row.names = FALSE, na = "")
write.csv(
  engagement_distribution,
  distribution_output_path,
  row.names = FALSE,
  na = ""
)
write.csv(test_results, test_output_path, row.names = FALSE, na = "")
write.csv(posthoc, posthoc_output_path, row.names = FALSE, na = "")
write.csv(sample_audit, audit_output_path, row.names = FALSE, na = "")

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required to create the RQ2 figure.")
}

plot_data <- rq2
plot_data$cluster_label <- factor(
  plot_data$cluster_label,
  levels = paste("Cluster", expected_clusters)
)

engagement_plot <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = cluster_label, y = total_engagement)
) +
  ggplot2::geom_boxplot(
    width = 0.68,
    fill = "#7BA7C9",
    colour = "#234A66",
    outlier.colour = "#C94C4C",
    outlier.alpha = 0.55,
    outlier.size = 1.4
  ) +
  ggplot2::scale_y_continuous(
    trans = "log1p",
    breaks = c(0, 1, 2, 5, 10, 25, 100, 500, 1500),
    expand = ggplot2::expansion(mult = c(0.02, 0.08))
  ) +
  ggplot2::labs(
    x = "Canonical lexical cluster",
    y = "Total engagement (log1p scale)"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
    plot.margin = ggplot2::margin(12, 18, 12, 12)
  )

ggplot2::ggsave(
  figure_output_path,
  plot = engagement_plot,
  width = 9,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


# 9. CONCISE REPRODUCIBILITY SUMMARY

cat("\nRQ2 HYPOTHESIS TESTING COMPLETED\n")
cat("Total RQ2 posts included:", included_posts, "\n")
cat("Mapped duplicate posts:", mapped_duplicate_posts, "\n")
cat("Excluded/unassigned posts:", excluded_posts, "\n")
cat(
  "Engagement formula: likes + reposts + replies + quotes\n"
)
cat("Selected omnibus test: Kruskal-Wallis rank sum test\n")
cat("H statistic:", format(unname(primary_test$statistic), digits = 7), "\n")
cat("Degrees of freedom:", unname(primary_test$parameter), "\n")
cat("P-value:", format.pval(primary_test$p.value, digits = 5), "\n")
cat("Epsilon-squared:", format(primary_effect, digits = 5), "\n")
cat(
  "Post-hoc testing required:",
  if (primary_significant) "yes" else "no", "\n"
)
cat("Sensitivity analysis:", sensitivity_conclusion, "\n")

cat("\nFILES CREATED\n")
cat(analysis_output_path, "\n")
cat(distribution_output_path, "\n")
cat(test_output_path, "\n")
cat(posthoc_output_path, "\n")
cat(audit_output_path, "\n")
cat(figure_output_path, "\n")
