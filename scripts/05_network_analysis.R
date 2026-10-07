# COMP3020 - AI Discussions on Bluesky
# 05_network_analysis.R
# RQ3: How are users connected through AI-related discussions, and which
# users occupy central positions?

# This is an observed reply/interaction network, not a follower network.
# A directed edge source -> target means that source authored a collected post
# replying to target. Edge weight is the number of observed replies.


# 1. PATHS AND SETTINGS

edge_path <- "data/processed/bluesky_network_edges.csv"
post_path <- "data/processed/bluesky_posts.csv"
reply_path <- "data/processed/bluesky_reply_edges.csv"

network_summary_path <- "data/processed/rq3_network_summary.csv"
centrality_path <- "data/processed/rq3_user_centrality.csv"
component_path <- "data/processed/rq3_component_summary.csv"
figure_path <- "figures/rq3_reply_network.png"

layout_seed <- 3020
maximum_figure_labels <- 5


# 2. LOAD AND AUDIT THE CANONICAL INPUT

required_files <- c(edge_path, post_path)
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop("Missing required input files: ", paste(missing_files, collapse = ", "))
}
if (!requireNamespace("igraph", quietly = TRUE)) {
  stop("Package 'igraph' is required for RQ3 network analysis.")
}

edges_raw <- read.csv(
  edge_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)
posts <- read.csv(
  post_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)

required_edge_columns <- c("from", "to", "weight")
required_post_columns <- c("author_did", "author_handle")

missing_edge_columns <- setdiff(required_edge_columns, names(edges_raw))
missing_post_columns <- setdiff(required_post_columns, names(posts))

if (length(missing_edge_columns) > 0) {
  stop(
    "Canonical edge data is missing columns: ",
    paste(missing_edge_columns, collapse = ", ")
  )
}
if (length(missing_post_columns) > 0) {
  stop(
    "Canonical post data is missing columns: ",
    paste(missing_post_columns, collapse = ", ")
  )
}

missing_source <- is.na(edges_raw$from) |
  !nzchar(trimws(edges_raw$from))
missing_target <- is.na(edges_raw$to) |
  !nzchar(trimws(edges_raw$to))
missing_weight <- is.na(edges_raw$weight)
invalid_weight <- !missing_weight & (
  !is.numeric(edges_raw$weight) |
    !is.finite(edges_raw$weight) |
    edges_raw$weight <= 0 |
    edges_raw$weight != floor(edges_raw$weight)
)

if (any(missing_source) || any(missing_target)) {
  stop("Canonical edge data contains missing source or target IDs.")
}
if (!is.numeric(edges_raw$weight) || any(missing_weight) || any(invalid_weight)) {
  stop("Edge weights must be finite, positive, integer-valued numeric counts.")
}

self_loop_count <- sum(edges_raw$from == edges_raw$to)
if (self_loop_count > 0) {
  stop(
    "Canonical edge data unexpectedly contains ", self_loop_count,
    " self-loop(s); inspect Step 01 rather than silently removing them."
  )
}

pair_key <- paste(edges_raw$from, edges_raw$to, sep = "\r")
duplicate_pair_rows <- sum(duplicated(pair_key))

# Duplicate pairs are safely aggregated if present, while their presence is
# reported. This preserves the observed reply total and graph semantics.
edges <- aggregate(
  weight ~ from + to,
  data = edges_raw,
  FUN = sum
)
edges <- edges[order(edges$from, edges$to), , drop = FALSE]
row.names(edges) <- NULL

if (any(edges$from == edges$to)) {
  stop("Self-loops remain after edge aggregation.")
}

# When the canonical reply-level file is available, independently verify that
# aggregating its observed interactions exactly reproduces the edge file.
reply_level_verified <- NA
if (file.exists(reply_path)) {
  replies <- read.csv(
    reply_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = ""
  )
  required_reply_columns <- c("from", "to")
  missing_reply_columns <- setdiff(required_reply_columns, names(replies))
  if (length(missing_reply_columns) > 0) {
    stop("Reply-level data lacks from/to columns required for verification.")
  }
  if (anyNA(replies$from) || anyNA(replies$to)) {
    stop("Reply-level verification data contains missing endpoint IDs.")
  }
  reply_key <- paste(replies$from, replies$to, sep = "\r")
  reply_counts <- table(reply_key)
  aggregated_key <- paste(edges$from, edges$to, sep = "\r")
  reply_level_verified <- setequal(names(reply_counts), aggregated_key) &&
    all(as.integer(reply_counts[aggregated_key]) == edges$weight)
  if (!reply_level_verified) {
    stop("Canonical edge weights do not reproduce the reply-level records.")
  }
}

unique_sources <- length(unique(edges$from))
unique_targets <- length(unique(edges$to))
network_user_ids <- sort(unique(c(edges$from, edges$to)))
total_observed_replies <- sum(edges$weight)


# 3. AUDIT AND CREATE DID-TO-HANDLE MAPPING

handle_rows <- posts[
  !is.na(posts$author_did) & nzchar(trimws(posts$author_did)) &
    !is.na(posts$author_handle) & nzchar(trimws(posts$author_handle)),
  c("author_did", "author_handle"),
  drop = FALSE
]
handle_rows <- unique(handle_rows)

handles_by_did <- split(handle_rows$author_handle, handle_rows$author_did)
handle_options <- lapply(handles_by_did, unique)
ambiguous_dids <- names(handle_options)[lengths(handle_options) > 1]
unambiguous_handles <- vapply(
  handle_options[lengths(handle_options) == 1],
  function(x) x[[1]],
  character(1)
)

author_handle <- unname(unambiguous_handles[network_user_ids])
mapped_handle_count <- sum(!is.na(author_handle))
unmapped_handle_count <- sum(is.na(author_handle))
ambiguous_network_count <- sum(network_user_ids %in% ambiguous_dids)

# Stable safe labels make figures readable without exposing long raw DIDs.
unmapped_sequence <- cumsum(is.na(author_handle))
display_label <- ifelse(
  !is.na(author_handle),
  author_handle,
  paste("Unmapped user", unmapped_sequence)
)

vertices <- data.frame(
  name = network_user_ids,
  author_handle = author_handle,
  display_label = display_label,
  stringsAsFactors = FALSE
)


# 4. BUILD THE DIRECTED WEIGHTED OBSERVED REPLY GRAPH

graph <- igraph::graph_from_data_frame(
  edges,
  directed = TRUE,
  vertices = vertices
)
igraph::E(graph)$weight <- edges$weight

if (igraph::vcount(graph) != length(network_user_ids)) {
  stop("Graph vertex count does not match unique source/target IDs.")
}
if (igraph::ecount(graph) != nrow(edges)) {
  stop("Graph edge count does not match unique directed pairs.")
}
if (sum(igraph::E(graph)$weight) != total_observed_replies) {
  stop("Graph weights do not sum to the observed reply total.")
}
if (!igraph::is_directed(graph)) {
  stop("RQ3 graph must be directed.")
}


# 5. NETWORK-LEVEL STATISTICS AND COMPONENTS

weak_components <- igraph::components(graph, mode = "weak")
strong_components <- igraph::components(graph, mode = "strong")
weak_sizes <- weak_components$csize
largest_weak_id <- which.max(weak_sizes)
largest_weak_size <- unname(weak_sizes[largest_weak_id])
largest_weak_proportion <- largest_weak_size / igraph::vcount(graph)

unweighted_degree <- igraph::degree(graph, mode = "all", loops = FALSE)
weighted_degree <- igraph::strength(
  graph,
  mode = "all",
  loops = FALSE,
  weights = igraph::E(graph)$weight
)
isolate_count <- sum(unweighted_degree == 0)

# With an edge-list-only graph, isolates cannot appear: users with no observed
# reply edge are absent. Statistics describe the 240 reply-network users, not
# all authors in the collected posts.
network_summary <- data.frame(
  metric = c(
    "input_edge_rows",
    "input_missing_source_ids",
    "input_missing_target_ids",
    "input_missing_weights",
    "input_invalid_weights",
    "input_self_loops",
    "input_duplicate_directed_pair_rows",
    "unique_source_users",
    "unique_target_users",
    "vertices",
    "directed_edges",
    "total_reply_interactions",
    "density",
    "reciprocity",
    "weak_components",
    "strong_components",
    "largest_weak_component_size",
    "largest_weak_component_proportion",
    "isolates_in_edge_list_graph",
    "isolate_proportion_in_edge_list_graph",
    "mean_unweighted_degree",
    "median_unweighted_degree",
    "mean_weighted_degree_strength",
    "median_weighted_degree_strength",
    "mapped_network_users",
    "unmapped_network_users",
    "ambiguous_network_handle_mappings"
  ),
  value = c(
    nrow(edges_raw),
    sum(missing_source),
    sum(missing_target),
    sum(missing_weight),
    sum(invalid_weight),
    self_loop_count,
    duplicate_pair_rows,
    unique_sources,
    unique_targets,
    igraph::vcount(graph),
    igraph::ecount(graph),
    total_observed_replies,
    igraph::edge_density(graph, loops = FALSE),
    igraph::reciprocity(graph, ignore.loops = TRUE, mode = "ratio"),
    weak_components$no,
    strong_components$no,
    largest_weak_size,
    largest_weak_proportion,
    isolate_count,
    isolate_count / igraph::vcount(graph),
    mean(unweighted_degree),
    median(unweighted_degree),
    mean(weighted_degree),
    median(weighted_degree),
    mapped_handle_count,
    unmapped_handle_count,
    ambiguous_network_count
  ),
  stringsAsFactors = FALSE
)

component_rows <- vector("list", weak_components$no)
for (component_id in seq_len(weak_components$no)) {
  member_names <- names(weak_components$membership)[
    weak_components$membership == component_id
  ]
  component_graph <- igraph::induced_subgraph(graph, vids = member_names)
  component_rows[[component_id]] <- data.frame(
    weak_component_id = component_id,
    vertices = igraph::vcount(component_graph),
    directed_edges = igraph::ecount(component_graph),
    reply_interactions = sum(igraph::E(component_graph)$weight),
    proportion_of_network_vertices =
      igraph::vcount(component_graph) / igraph::vcount(graph),
    is_largest = component_id == largest_weak_id,
    stringsAsFactors = FALSE
  )
}
component_summary <- do.call(rbind, component_rows)
component_summary <- component_summary[
  order(
    -component_summary$vertices,
    -component_summary$directed_edges,
    component_summary$weak_component_id
  ),
  ,
  drop = FALSE
]
component_summary$size_rank <- rank(
  -component_summary$vertices,
  ties.method = "min"
)
row.names(component_summary) <- NULL


# 6. USER-LEVEL CENTRALITY

in_degree <- igraph::degree(graph, mode = "in", loops = FALSE)
out_degree <- igraph::degree(graph, mode = "out", loops = FALSE)
in_strength <- igraph::strength(
  graph,
  mode = "in",
  loops = FALSE,
  weights = igraph::E(graph)$weight
)
out_strength <- igraph::strength(
  graph,
  mode = "out",
  loops = FALSE,
  weights = igraph::E(graph)$weight
)

# Weighted directed PageRank treats more frequently observed replies as larger
# transition weights. Directed betweenness is deliberately unweighted because
# reply frequency is interaction strength, whereas igraph shortest-path
# weights represent distance (larger means farther).
pagerank <- igraph::page_rank(
  graph,
  directed = TRUE,
  weights = igraph::E(graph)$weight
)$vector
betweenness <- igraph::betweenness(
  graph,
  directed = TRUE,
  weights = NA,
  normalized = FALSE
)

centrality <- data.frame(
  user_id = igraph::V(graph)$name,
  author_handle = igraph::vertex_attr(graph, "author_handle"),
  display_label = igraph::vertex_attr(graph, "display_label"),
  in_degree = as.numeric(in_degree),
  out_degree = as.numeric(out_degree),
  total_degree = as.numeric(in_degree + out_degree),
  in_strength = as.numeric(in_strength),
  out_strength = as.numeric(out_strength),
  total_strength = as.numeric(in_strength + out_strength),
  pagerank = as.numeric(pagerank),
  betweenness = as.numeric(betweenness),
  stringsAsFactors = FALSE
)

rank_descending <- function(values) {
  rank(-values, ties.method = "min")
}
centrality$in_degree_rank <- rank_descending(centrality$in_degree)
centrality$in_strength_rank <- rank_descending(centrality$in_strength)
centrality$pagerank_rank <- rank_descending(centrality$pagerank)
centrality$betweenness_rank <- rank_descending(centrality$betweenness)
centrality$weak_component_id <- unname(
  weak_components$membership[centrality$user_id]
)
centrality$in_largest_weak_component <-
  centrality$weak_component_id == largest_weak_id

centrality <- centrality[order(centrality$user_id), , drop = FALSE]
row.names(centrality) <- NULL


# 7. INTERNAL VALIDATION

if (!identical(centrality$in_degree, as.numeric(igraph::degree(
  graph, mode = "in", loops = FALSE
)))) {
  stop("Exported in-degree does not match igraph.")
}
if (!identical(centrality$out_degree, as.numeric(igraph::degree(
  graph, mode = "out", loops = FALSE
)))) {
  stop("Exported out-degree does not match igraph.")
}
if (!identical(centrality$in_strength, as.numeric(igraph::strength(
  graph, mode = "in", loops = FALSE, weights = igraph::E(graph)$weight
)))) {
  stop("Exported in-strength does not match edge weights.")
}
if (!identical(centrality$out_strength, as.numeric(igraph::strength(
  graph, mode = "out", loops = FALSE, weights = igraph::E(graph)$weight
)))) {
  stop("Exported out-strength does not match edge weights.")
}
if (any(!is.finite(centrality$pagerank)) ||
    abs(sum(centrality$pagerank) - 1) > 1e-10) {
  stop("PageRank must be finite and sum to one.")
}
if (any(!is.finite(centrality$betweenness)) ||
    any(centrality$betweenness < 0)) {
  stop("Betweenness must be finite and non-negative.")
}
if (length(weak_components$membership) != igraph::vcount(graph) ||
    anyNA(weak_components$membership)) {
  stop("Weak-component membership is incomplete.")
}
if (sum(component_summary$vertices) != igraph::vcount(graph) ||
    max(component_summary$vertices) != largest_weak_size) {
  stop("Component summary does not reproduce graph membership.")
}


# 8. EXPORT TABLES

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

write.csv(network_summary, network_summary_path, row.names = FALSE, na = "")
write.csv(centrality, centrality_path, row.names = FALSE, na = "")
write.csv(component_summary, component_path, row.names = FALSE, na = "")


# 9. FIGURE: LARGEST WEAKLY CONNECTED COMPONENT

largest_vertex_names <- names(weak_components$membership)[
  weak_components$membership == largest_weak_id
]
largest_graph <- igraph::induced_subgraph(graph, vids = largest_vertex_names)

set.seed(layout_seed)
layout <- igraph::layout_with_fr(largest_graph, weights = NA)

largest_pagerank <- pagerank[igraph::V(largest_graph)$name]
if (diff(range(largest_pagerank)) == 0) {
  vertex_size <- rep(20, igraph::vcount(largest_graph))
} else {
  vertex_size <- 14 + 14 * (
    largest_pagerank - min(largest_pagerank)
  ) / diff(range(largest_pagerank))
}

label_order <- order(
  -largest_pagerank,
  igraph::V(largest_graph)$name
)
label_vertices <- head(label_order, maximum_figure_labels)
vertex_labels <- rep(NA_character_, igraph::vcount(largest_graph))
vertex_labels[label_vertices] <- igraph::vertex_attr(
  largest_graph, "display_label"
)[label_vertices]

edge_width <- 1.2 + 1.2 * (igraph::E(largest_graph)$weight - 1)

png(figure_path, width = 1800, height = 1300, res = 200)
par(mar = c(1, 1, 5, 1))
plot(
  largest_graph,
  layout = layout,
  vertex.size = vertex_size,
  vertex.color = "#7BA7C9",
  vertex.frame.color = "#234A66",
  vertex.label = vertex_labels,
  vertex.label.color = "#1F2933",
  vertex.label.cex = 0.82,
  vertex.label.dist = 1.15,
  edge.color = grDevices::adjustcolor("#566573", alpha.f = 0.70),
  edge.width = edge_width,
  edge.arrow.size = 0.65,
  edge.curved = 0.08,
  main = "Observed AI-discussion reply network\nLargest weakly connected component",
  sub = paste0(
    largest_weak_size, " of ", igraph::vcount(graph),
    " users in the observed edge-list network"
  )
)
dev.off()


# 10. CONCISE CONSOLE SUMMARY

top_labels <- function(values, n = 5) {
  if (length(unique(values)) == 1) {
    return(paste0("all ", length(values), " users tied at ", values[[1]]))
  }
  order_index <- order(-values, centrality$user_id)
  paste(
    paste0(
      centrality$display_label[head(order_index, n)],
      " (", format(values[head(order_index, n)], digits = 5), ")"
    ),
    collapse = "; "
  )
}

cat("\nRQ3 OBSERVED REPLY-NETWORK ANALYSIS COMPLETED\n")
cat("Users in observed reply network:", igraph::vcount(graph), "\n")
cat("Unique directed user pairs:", igraph::ecount(graph), "\n")
cat("Total observed replies:", total_observed_replies, "\n")
cat("Density:", format(igraph::edge_density(graph), digits = 6), "\n")
cat(
  "Reciprocity:",
  format(igraph::reciprocity(graph, mode = "ratio"), digits = 6), "\n"
)
cat("Weak components:", weak_components$no, "\n")
cat(
  "Largest weak component:", largest_weak_size, "users (",
  format(100 * largest_weak_proportion, digits = 4), "%)\n",
  sep = ""
)
cat("Handle mappings:", mapped_handle_count, "mapped;",
    unmapped_handle_count, "unmapped;", ambiguous_network_count,
    "ambiguous\n")
cat("Top by in-degree:", top_labels(centrality$in_degree), "\n")
cat("Top by PageRank:", top_labels(centrality$pagerank), "\n")
cat("Top by betweenness:", top_labels(centrality$betweenness), "\n")
cat(
  "Community detection: omitted because 111 tiny weak components and no ",
  "reciprocal edges make a community partition substantively unhelpful.\n",
  sep = ""
)
cat(
  "Limitation: edge-list construction excludes authors with no observed ",
  "reply relationship; this is not the full Bluesky social graph.\n",
  sep = ""
)

cat("\nFILES CREATED\n")
cat(network_summary_path, "\n")
cat(centrality_path, "\n")
cat(component_path, "\n")
cat(figure_path, "\n")
