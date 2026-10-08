
# ============================================================
# COMP3020 - SOCIAL WEB ANALYTICS
# 05_network_analysis.R
#
# RQ3: How are users connected through AI-related
# discussions, and which users occupy central positions?
#
# Based on Module 7 - Network Analysis
# ============================================================


# 1. LOAD LIBRARY AND DATA --------------------------------

library(igraph)

# Load the dataset of AI-related posts.
posts <- read.csv("data/processed/bluesky_clustered_posts.csv")

# Identify unique authors of AI-related posts.
ai_users <- unique(posts$author_handle)
ai_users <- ai_users[!is.na(ai_users) & ai_users != ""]

length(ai_users)


# 2. LOAD FOLLOW RELATIONSHIPS ----------------------------

# Follow relationships were collected using get_follows().
# Each edge A -> B means account A follows account B.

edges <- read.csv("data/processed/ai_follow_edges.csv")

# Keep the source and target accounts.
el <- as.matrix(edges[, c("from", "to")])

# Remove duplicate, missing and invalid relationships.
el <- unique(el)

el <- el[
  !is.na(el[, 1]) &
    !is.na(el[, 2]) &
    el[, 1] != "" &
    el[, 2] != "" &
    el[, 1] != el[, 2] &
    el[, 1] != "handle.invalid" &
    el[, 2] != "handle.invalid",
  ,
  drop = FALSE
]

dim(el)
head(el)


# 3. CREATE THE FOLLOW NETWORK ----------------------------

# A directed graph is used because following is not
# necessarily reciprocal.

g_all <- graph_from_edgelist(
  el,
  directed = TRUE
)

vcount(g_all)
ecount(g_all)


# 4. CREATE THE AI AUTHOR NETWORK -------------------------

# Keep only accounts that posted about AI.
# This makes the network directly relevant to RQ3.

g <- induced_subgraph(
  g_all,
  V(g_all)[name %in% ai_users]
)

# Include AI authors without observed follow relationships.
# These accounts are represented as isolated nodes.

missing_users <- ai_users[!ai_users %in% V(g)$name]

g <- add_vertices(
  g,
  length(missing_users),
  name = missing_users
)

vcount(g)
ecount(g)


# 5. NETWORK PROPERTIES -----------------------------------

# Number of AI authors in the network.
vcount(g)

# Number of observed follow relationships.
ecount(g)

# Proportion of possible directed edges observed.
edge_density(g)

# Weak components ignore the direction of edges.
comp <- components(g, mode = "weak")

# Number of weakly connected components.
comp$no

# Size of the largest weakly connected component.
max(comp$csize)

# Degree distribution of the full AI-author network.
plot(
  degree_distribution(g),
  type = "h",
  xlab = "Degree",
  ylab = "Proportion",
  main = "AI Author Degree Distribution"
)


# 6. CREATE THE LARGEST COMPONENT -------------------------

# Select the largest weakly connected component.
# This subgraph contains the largest connected group
# of AI discussion authors in the observed network.

largest_component <- which.max(comp$csize)

g2 <- induced_subgraph(
  g,
  V(g)[comp$membership == largest_component]
)

vcount(g2)
ecount(g2)
edge_density(g2)


# 7. VISUALISE THE FULL NETWORK ---------------------------

# Fruchterman-Reingold layout is used in Module 7.
# Hide labels to avoid overlapping account names.

set.seed(3020)

plot(
  g,
  layout = layout_with_fr(g),
  vertex.size = 4,
  vertex.label = NA,
  edge.arrow.size = 0.2,
  main = "AI Author Follow Network"
)


# 8. VISUALISE THE LARGEST COMPONENT ----------------------

# Node size represents in-degree centrality.
# Larger nodes are followed by more AI authors.

set.seed(3020)

plot(
  g2,
  layout = layout_with_kk(g2),
  vertex.size = 3 + degree(g2, mode = "in") * 1.5,
  vertex.label = NA,
  vertex.color = "skyblue",
  edge.color = "grey75",
  edge.arrow.size = 0.2,
  main = "Largest Connected AI Author Group"
)

# 9. DEGREE CENTRALITY ------------------------------------

# In-degree measures how many AI authors in our
# dataset follow each account.

in_degree <- degree(g, mode = "in")

# Out-degree measures how many other AI authors
# each account follows in the observed network.

out_degree <- degree(g, mode = "out")

# Top 5 accounts by in-degree.
top_in_degree <- head(
  sort(in_degree, decreasing = TRUE),
  5
)

# Top 5 accounts by out-degree.
top_out_degree <- head(
  sort(out_degree, decreasing = TRUE),
  5
)

top_in_degree
top_out_degree


# 10. BETWEENNESS CENTRALITY ------------------------------

# Betweenness measures how often an account lies
# on shortest directed paths between other accounts.
# High betweenness can indicate a bridging position.

betweenness_scores <- betweenness(
  g,
  directed = TRUE
)

# Top 5 accounts by betweenness.
top_betweenness <- head(
  sort(betweenness_scores, decreasing = TRUE),
  5
)

top_betweenness


# 11. LIMITATIONS -----------------------------------------

# The network contains authors from the collected AI posts.
#
# Only observed follow relationships are included.
# Missing edges do not prove that users are unconnected.
#
# The get_follows() collection limit may omit relationships.
#
# Following another account does not necessarily mean
# interacting with that account about AI.
#
# Centrality scores describe this observed network,
# not influence across the entire Bluesky platform.
#
# Disconnected components limit the interpretation
# of shortest-path-based centrality measures.


# ============================================================
# END OF SCRIPT 05
# ============================================================

# Data provenance: ai_follow_edges.csv was collected separately using
# atrrr::get_follows(). This script uses the existing collected snapshot.
# It does not call the API or create additional relationships.
