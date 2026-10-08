# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 05_network_analysis.R
#
# RQ3: How are the authors of AI posts connected, and
# which authors are central? Follows Module 7 Part 6
# and Module 8 (PageRank).
#
# Network definition:
#   node = an author of at least one collected AI post
#   edge A -> B = author A follows author B
# The graph is DIRECTED because following is one-way.
# =====================================================

library(igraph)


# 1. LOAD DATA ---------------------------------------

posts = read.csv("data/processed/bluesky_clustered_posts.csv")
edges = read.csv("data/raw/bluesky_follow_edges.csv")

# Authors of the AI posts used in the analysis.
ai_users = unique(posts$author_handle)
length(ai_users)


# 2. BUILD THE EDGE LIST -----------------------------

# Each row: "from" follows "to".
el = as.matrix(edges[, c("from", "to")])

# Remove repeated edges and self-follows.
el = unique(el)
el = el[el[, "from"] != el[, "to"], ]

# Keep only follows BETWEEN two AI-post authors.
el = el[el[, "from"] %in% ai_users & el[, "to"] %in% ai_users, ]
dim(el)


# 3. CREATE THE NETWORK ------------------------------

g = graph_from_edgelist(el, directed = TRUE)

# Authors with at least one tie, and follow edges among them.
vcount(g)
ecount(g)

# Authors with no tie to another AI author are not in g.
length(ai_users) - vcount(g)

# Density: proportion of all possible edges that exist.
round(edge_density(g), 4)

# Weakly connected components (direction ignored).
comp = components(g, mode = "weak")
comp$no
max(comp$csize)

# Degree distribution (Module 7).
plot(degree_distribution(g), type = "h",
     xlab = "Degree", ylab = "Proportion",
     main = "Degree Distribution of the AI Author Network")


# 4. COLOUR AUTHORS BY TOPIC -------------------------

# Topic of each author = cluster of their first collected post.
author.cluster = rep(0, vcount(g))
for (a in 1:vcount(g)) {
  author.cluster[a] = posts$cluster[posts$author_handle == V(g)$name[a]][1]
}
table(author.cluster)

# One fixed colour per cluster, so node colours match the legend.
cluster.colours = c("#9AA0A6", "#7B5EA7", "#B8BCC2", "#E0A030",
                    "#1B9E77", "#2C6FB7", "#5F6368")
cluster.names = c("Job-advert bots", "Research & books", "AP-NORC poll",
                  "Gemini & agents", "Personal opinions", "News & links",
                  "Market-report spam")


# 5. CENTRALITY --------------------------------------

# In-degree: number of AI authors who follow the account.
in_degree = degree(g, mode = "in")
head(sort(in_degree, decreasing = TRUE), 5)

# PageRank (Module 8): follow a link 80% of the time, as in the lab.
pr = page_rank(g, damping = 0.8)$vector
round(head(sort(pr, decreasing = TRUE), 5), 4)

# Betweenness: how often an account lies on shortest paths.
btw = betweenness(g, directed = TRUE)
round(head(sort(btw, decreasing = TRUE), 5), 1)


# 6. PLOT THE FULL NETWORK ---------------------------

# Labels only for the 5 accounts with the highest in-degree.
top5 = names(sort(in_degree, decreasing = TRUE))[1:5]
labels = ifelse(V(g)$name %in% top5, V(g)$name, NA)

par(mar = c(0, 0, 2, 0))
set.seed(3020)
plot(g, layout = layout_with_fr(g),
     vertex.size = 1.5 + sqrt(in_degree) * 1.5,
     vertex.color = cluster.colours[author.cluster], vertex.frame.color = "white",
     vertex.label = labels, vertex.label.cex = 0.7, vertex.label.color = "black",
     vertex.label.dist = 1.5, vertex.label.degree = -pi / 2,
     edge.arrow.size = 0.1, edge.color = "grey70",
     main = "Follow Network of AI Post Authors")
legend("bottomleft", legend = cluster.names, col = cluster.colours,
       pch = 19, cex = 0.7, bty = "n")


# 7. PLOT A SUBGRAPH ---------------------------------

# As in Module 7, keep authors connected to more than one other
# author. Centrality above is still calculated on the full network g.
keep = degree(g) > 1
g2 = induced_subgraph(g, V(g)[keep])
vcount(g2)
ecount(g2)

labels2 = ifelse(V(g2)$name %in% top5, V(g2)$name, NA)

par(mar = c(0, 0, 2, 0))
set.seed(3020)
plot(g2, layout = layout_with_kk(g2),
     vertex.size = 2 + sqrt(degree(g2, mode = "in")) * 1.5,
     vertex.color = cluster.colours[author.cluster[keep]], vertex.frame.color = "white",
     vertex.label = labels2, vertex.label.cex = 0.7, vertex.label.color = "black",
     vertex.label.dist = 1.5, vertex.label.degree = -pi / 2,
     edge.arrow.size = 0.15, edge.color = "grey70",
     main = "Authors with Degree > 1")
legend("bottomleft", legend = cluster.names, col = cluster.colours,
       pch = 19, cex = 0.7, bty = "n")


# 8. LIMITATIONS -------------------------------------

# - Up to about 300 follows were collected per author, so
#   some real edges are missing.
# - Following is not the same as discussing AI together.
# - Centrality describes this sample only, not all of Bluesky.
