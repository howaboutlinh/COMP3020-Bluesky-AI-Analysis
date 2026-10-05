# COMP3020 - AI Discussions on Bluesky
# 03_clustering.R
# RQ1: What are the main topics in AI-related discussions on Bluesky?

# Topic names are not assigned automatically. The script exports characteristic
# terms and representative posts so the team can interpret each cluster.


# 1. PATHS AND SETTINGS

analysis_path <- "data/processed/bluesky_text_analysis.csv"
representation_path <-
  "data/processed/bluesky_text_representation.rds"
clustered_path <- "data/processed/bluesky_clustered_posts.csv"
elbow_values_path <- "data/processed/clustering_elbow_values.csv"
top_terms_path <- "data/processed/clustering_top_terms.csv"
representative_posts_path <-
  "data/processed/clustering_representative_posts.csv"
elbow_figure_path <- "figures/rq1_elbow_plot.png"
cluster_size_figure_path <- "figures/rq1_cluster_sizes.png"

random_seed <- 3020
tested_k <- 2:10
elbow_nstart <- 20
final_nstart <- 50
maximum_iterations <- 100
top_terms_per_cluster <- 12
representative_posts_per_cluster <- 3

# The elbow is gradual rather than sharp. Inspection of WSS reductions, cluster
# sizes, characteristic terms, and representative posts showed that k = 7 is a
# useful stopping point. Improvements become smaller after k = 7, while k = 7
# separates seven interpretable lexical groupings without using semantic labels
# in the machine-readable output.
chosen_k <- 7


# 2. LOAD AND VALIDATE VERIFIED STEP 02 OUTPUTS

if (!file.exists(analysis_path)) {
  stop("Missing Step 02 analysis data: ", analysis_path)
}

if (!file.exists(representation_path)) {
  stop("Missing Step 02 text representation: ", representation_path)
}

text_analysis <- read.csv(
  analysis_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)

text_representation <- readRDS(representation_path)

required_columns <- c(
  "uri",
  "text",
  "cleaned_text",
  "author_did",
  "author_handle",
  "search_keyword",
  "like_count",
  "repost_count",
  "reply_count",
  "quote_count",
  "retained_term_count"
)

missing_columns <- setdiff(required_columns, names(text_analysis))

if (length(missing_columns) > 0) {
  stop(
    "Step 02 analysis data is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

required_representation_objects <- c(
  "uri",
  "document_term_matrix",
  "tf_idf_matrix",
  "vocabulary"
)

missing_objects <- setdiff(
  required_representation_objects,
  names(text_representation)
)

if (length(missing_objects) > 0) {
  stop(
    "Step 02 representation is missing: ",
    paste(missing_objects, collapse = ", ")
  )
}

if (!identical(text_representation$uri, text_analysis$uri)) {
  stop("URI alignment failed between Step 02 CSV and RDS outputs.")
}

tf_idf <- text_representation$tf_idf_matrix
document_term_matrix <- text_representation$document_term_matrix

if (!identical(rownames(tf_idf), text_analysis$uri)) {
  stop("TF-IDF row names do not match the analysis URI order.")
}

if (!identical(rownames(document_term_matrix), text_analysis$uri)) {
  stop("DTM row names do not match the analysis URI order.")
}

if (!identical(colnames(tf_idf), text_representation$vocabulary)) {
  stop("TF-IDF columns do not match the Step 02 vocabulary.")
}

if (any(!is.finite(tf_idf))) {
  stop("TF-IDF contains NA, NaN, or infinite values.")
}


# 3. EXCLUDE ZERO-TERM DOCUMENTS FROM DISTANCE-BASED CLUSTERING

total_text_posts <- nrow(text_analysis)
clusterable <- text_analysis$retained_term_count > 0
matrix_nonzero <- rowSums(tf_idf^2) > 0

if (!identical(unname(clusterable), unname(matrix_nonzero))) {
  stop(
    "retained_term_count does not agree with nonzero TF-IDF rows."
  )
}

zero_term_posts <- sum(!clusterable)
clusterable_posts <- text_analysis[clusterable, , drop = FALSE]
clusterable_tfidf <- tf_idf[clusterable, , drop = FALSE]

if (!identical(rownames(clusterable_tfidf), clusterable_posts$uri)) {
  stop("URI alignment failed after selecting clusterable documents.")
}

if (nrow(clusterable_posts) <= max(tested_k)) {
  stop("Not enough clusterable documents for the requested k range.")
}


# 4. ELBOW METHOD

wss <- numeric(length(tested_k))

for (i in seq_along(tested_k)) {
  set.seed(random_seed)
  elbow_model <- kmeans(
    clusterable_tfidf,
    centers = tested_k[[i]],
    nstart = elbow_nstart,
    iter.max = maximum_iterations
  )
  wss[[i]] <- elbow_model$tot.withinss
}

wss_reduction <- c(NA_real_, head(wss, -1) - tail(wss, -1))
percentage_reduction <- c(
  NA_real_,
  wss_reduction[-1] / head(wss, -1) * 100
)

elbow_values <- data.frame(
  k = tested_k,
  total_within_cluster_ss = wss,
  reduction_from_previous_k = wss_reduction,
  percentage_reduction = percentage_reduction,
  selected = tested_k == chosen_k
)


# 5. FINAL K-MEANS MODEL

set.seed(random_seed)
final_model <- kmeans(
  clusterable_tfidf,
  centers = chosen_k,
  nstart = final_nstart,
  iter.max = maximum_iterations
)

cluster_id <- final_model$cluster
cluster_label <- paste("Cluster", cluster_id)

if (length(cluster_id) != nrow(clusterable_posts)) {
  stop("Not every clusterable post received a cluster ID.")
}

if (anyNA(cluster_id)) {
  stop("Missing cluster IDs were produced.")
}


# 6. CHARACTERISTIC TERMS

top_terms <- do.call(
  rbind,
  lapply(seq_len(chosen_k), function(cluster_number) {
    cluster_rows <- cluster_id == cluster_number
    average_tfidf <- colMeans(
      clusterable_tfidf[cluster_rows, , drop = FALSE]
    )
    term_order <- order(average_tfidf, decreasing = TRUE)
    selected_terms <- head(term_order, top_terms_per_cluster)

    data.frame(
      cluster = cluster_number,
      cluster_label = paste("Cluster", cluster_number),
      term_rank = seq_along(selected_terms),
      term = names(average_tfidf)[selected_terms],
      average_tfidf = average_tfidf[selected_terms],
      stringsAsFactors = FALSE
    )
  })
)


# 7. REPRESENTATIVE POSTS CLOSEST TO EACH CENTROID

representative_posts <- do.call(
  rbind,
  lapply(seq_len(chosen_k), function(cluster_number) {
    cluster_rows <- which(cluster_id == cluster_number)
    centroid <- final_model$centers[cluster_number, ]
    centroid_matrix <- matrix(
      centroid,
      nrow = length(cluster_rows),
      ncol = length(centroid),
      byrow = TRUE
    )
    squared_distance <- rowSums(
      (clusterable_tfidf[cluster_rows, , drop = FALSE] -
         centroid_matrix)^2
    )
    selected_within_cluster <- head(
      order(squared_distance),
      representative_posts_per_cluster
    )
    selected_rows <- cluster_rows[selected_within_cluster]

    data.frame(
      cluster = cluster_number,
      cluster_label = paste("Cluster", cluster_number),
      representative_rank = seq_along(selected_rows),
      uri = clusterable_posts$uri[selected_rows],
      text = clusterable_posts$text[selected_rows],
      squared_distance_to_centroid =
        squared_distance[selected_within_cluster],
      stringsAsFactors = FALSE
    )
  })
)


# 8. POST-LEVEL CLUSTER OUTPUT FOR FUTURE RQ2

clustered_posts <- clusterable_posts
clustered_posts$cluster <- cluster_id
clustered_posts$cluster_label <- cluster_label

if (anyDuplicated(clustered_posts$uri) > 0) {
  stop("Duplicate URIs were produced in the clustered output.")
}

if (any(text_analysis$uri[!clusterable] %in% clustered_posts$uri)) {
  stop("A zero-term URI was incorrectly included in clustering.")
}


# 9. EXPORT TABLES AND USEFUL FIGURES

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

write.csv(clustered_posts, clustered_path, row.names = FALSE, na = "")
write.csv(elbow_values, elbow_values_path, row.names = FALSE, na = "")
write.csv(top_terms, top_terms_path, row.names = FALSE, na = "")
write.csv(
  representative_posts,
  representative_posts_path,
  row.names = FALSE,
  na = ""
)

png(elbow_figure_path, width = 1200, height = 800, res = 140)
plot(
  tested_k,
  wss,
  type = "b",
  pch = 19,
  xlab = "Number of clusters (k)",
  ylab = "Total within-cluster sum of squares",
  main = "Elbow method for Bluesky topic clusters"
)
abline(v = chosen_k, col = "#D55E00", lty = 2, lwd = 2)
legend(
  "topright",
  legend = paste("Selected k =", chosen_k),
  col = "#D55E00",
  lty = 2,
  bty = "n"
)
dev.off()

cluster_sizes <- table(
  factor(cluster_id, levels = seq_len(chosen_k))
)
names(cluster_sizes) <- paste("Cluster", seq_len(chosen_k))

png(cluster_size_figure_path, width = 1200, height = 800, res = 140)
barplot(
  cluster_sizes,
  col = "#4472C4",
  xlab = "Cluster",
  ylab = "Number of posts",
  main = "Cluster sizes for Bluesky AI discussions"
)
dev.off()


# 10. CONSOLE SUMMARY

cat("\nRQ1 CLUSTERING COMPLETED\n")
cat("Total text-analysis posts:", total_text_posts, "\n")
cat("Zero-term posts excluded:", zero_term_posts, "\n")
cat("Posts used for clustering:", nrow(clustered_posts), "\n")
cat("URI alignment after subsetting: TRUE\n")

cat("\nELBOW VALUES\n")
print(elbow_values, row.names = FALSE)

cat("\nFINAL MODEL\n")
cat("Chosen k:", chosen_k, "\n")
cat("Total within-cluster SS:", final_model$tot.withinss, "\n")
cat("Between-cluster SS:", final_model$betweenss, "\n")
cat("Total SS:", final_model$totss, "\n")
cat("Between/total SS:", final_model$betweenss / final_model$totss, "\n")

cat("\nCLUSTER SIZES\n")
print(cluster_sizes)

cat("\nTOP TERMS\n")
for (cluster_number in seq_len(chosen_k)) {
  terms <- top_terms$term[top_terms$cluster == cluster_number]
  cat(
    "Cluster ", cluster_number, ": ",
    paste(terms, collapse = ", "), "\n",
    sep = ""
  )
}

cat("\nFILES CREATED\n")
cat(clustered_path, "\n")
cat(elbow_values_path, "\n")
cat(top_terms_path, "\n")
cat(representative_posts_path, "\n")
cat(elbow_figure_path, "\n")
cat(cluster_size_figure_path, "\n")
