# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 01_data_collection.R
#
# Collects AI-related Bluesky posts (Modules 4, 5, 7)
# and the follow relationships of their authors
# (Module 7 Part 6).
#
# WARNING: running this script collects a NEW sample.
# To reproduce the reported results, start at Script 02.
# =====================================================


# 1. LOAD PACKAGE ------------------------------------

# Load functions for retrieving Bluesky posts.
library(atrrr)


# 2. AUTHENTICATE WITH BLUESKY -----------------------

# Use a Bluesky app password (stored outside the code).
auth(user = Sys.getenv("BLUESKY_HANDLE"),
     password = Sys.getenv("BLUESKY_APP_PASSWORD"))


# 3. SEARCH FOR AI-RELATED POSTS ---------------------

# Retrieve up to 250 recent posts for each search term.
posts_ai = search_post("artificial intelligence", sort = "latest", limit = 250)
posts_ml = search_post("machine learning", sort = "latest", limit = 250)
posts_chatgpt = search_post("ChatGPT", sort = "latest", limit = 250)
posts_genai = search_post("generative AI", sort = "latest", limit = 250)

# Inspect the returned data before using it.
dim(posts_ai)
dim(posts_ml)
dim(posts_chatgpt)
dim(posts_genai)
names(posts_ai)


# 4. KEEP THE COLUMNS NEEDED -------------------------

# Keep the post id, author, text, time and engagement counts.
# (Other columns are nested lists that cannot be saved to CSV.)
columns = c("uri", "author_handle", "text", "created_at",
            "like_count", "repost_count", "reply_count", "quote_count")

posts_ai = posts_ai[, columns]
posts_ml = posts_ml[, columns]
posts_chatgpt = posts_chatgpt[, columns]
posts_genai = posts_genai[, columns]

# Record which search term found each post.
posts_ai$search_keyword = "artificial intelligence"
posts_ml$search_keyword = "machine learning"
posts_chatgpt$search_keyword = "ChatGPT"
posts_genai$search_keyword = "generative AI"


# 5. COMBINE AND REMOVE DUPLICATES -------------------

# Combine the four searches into one table.
posts = rbind(posts_ai, posts_ml, posts_chatgpt, posts_genai)
nrow(posts)

# A post can match more than one search term; keep one copy.
posts = posts[!duplicated(posts$uri), ]
nrow(posts)

# Number of distinct authors.
length(unique(posts$author_handle))

# Time period covered by the sample (some posts have no date).
sum(is.na(posts$created_at))
range(posts$created_at, na.rm = TRUE)

# Save the posts.
write.csv(posts, "data/raw/bluesky_posts.csv", row.names = FALSE)


# 6. COLLECT FOLLOW RELATIONSHIPS (Module 7 Part 6) --

# Edge definition: A -> B means author A follows account B.
authors = unique(posts$author_handle)
length(authors)

# Apply get_follows() to every author (up to 200 follows each).
more_friends = lapply(authors, get_follows, limit = 200)

# Extract only the account handles from each result.
more_friends_handles = lapply(more_friends, `[[`, "actor_handle")

# Count the retrieved follows for each author.
sapply(more_friends_handles, length)

# Make the edges, one author at a time.
el_friends = lapply(seq_along(authors), function(i) {
  # Repeat this author's handle for each account they follow.
  cbind(from = rep(authors[i], length(more_friends_handles[[i]])),
        # Use the followed accounts as the edge destinations.
        to = more_friends_handles[[i]])
})

# Join all edges into one edge list.
el = unique(do.call(rbind, el_friends))

# Remove self-follows, where an account would point to itself.
el = el[el[, "from"] != el[, "to"], , drop = FALSE]

# Number of follow relationships and a preview.
dim(el)
head(el)

# Save the follow relationships.
write.csv(el, "data/raw/bluesky_follow_edges.csv", row.names = FALSE)
