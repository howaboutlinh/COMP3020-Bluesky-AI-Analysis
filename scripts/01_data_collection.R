
# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 01_data_collection.R
# =====================================================

# 1. LOAD PACKAGE ------------------------------------

library(bskyr)


# 2. BLUESKY AUTHENTICATION --------------------------

auth = bs_auth(
  user = Sys.getenv("BLUESKY_HANDLE"),
  pass = Sys.getenv("BLUESKY_APP_PASSWORD")
)


# 3. COLLECT BLUESKY POSTS ---------------------------

# Search for AI-related discussions.

posts_ai = bs_search_posts(
  query = "artificial intelligence",
  limit = 250,
  auth = auth
)

posts_ml = bs_search_posts(
  query = "machine learning",
  limit = 250,
  auth = auth
)

posts_chatgpt = bs_search_posts(
  query = "ChatGPT",
  limit = 250,
  auth = auth
)

posts_genai = bs_search_posts(
  query = "generative AI",
  limit = 250,
  auth = auth
)


# 4. INSPECT COLLECTED DATA --------------------------

# Check data structure and volume.
dim(posts_ai)
dim(posts_ml)
dim(posts_chatgpt)
dim(posts_genai)

# Inspect collected variables.
names(posts_ai)
head(posts_ai)


# 5. SAVE ORIGINAL DATA ------------------------------

# Preserve the original API results.
save(
  posts_ai,
  posts_ml,
  posts_chatgpt,
  posts_genai,
  file = "data/raw/bluesky_posts.RData"
)


# 6. COMBINE POSTS -----------------------------------

# Combine results from four keywords.
posts = dplyr::bind_rows(
  posts_ai,
  posts_ml,
  posts_chatgpt,
  posts_genai,
  .id = "search_keyword"
)

# Assign the correct keyword labels.
posts$search_keyword = c(
  "artificial intelligence",
  "machine learning",
  "ChatGPT",
  "generative AI"
)[as.integer(posts$search_keyword)]

dim(posts)


# 7. REMOVE DUPLICATE POSTS --------------------------

# Count duplicate post URIs.
sum(duplicated(posts$uri))

# Keep unique posts.
posts = posts[!duplicated(posts$uri), ]

# Check the final number of posts.
nrow(posts)


# 8. INSPECT VARIABLES -------------------------------

# Inspect engagement variables.
summary(posts$like_count)
summary(posts$repost_count)
summary(posts$reply_count)
summary(posts$quote_count)

# Check unique authors using post URIs.
length(unique(
  sub("^at://([^/]+)/.*$", "\\1", posts$uri)
))


# 9. SAVE COLLECTED DATA -----------------------------

# Preserve nested post, author and reply information.
saveRDS(
  posts,
  "data/raw/bluesky_posts_raw.rds"
)

# Save the flat post table required by Script 02.
# Do not re-run this script for report generation: it makes live API requests.
write.csv(posts, "data/processed/bluesky_text_posts.csv", row.names = FALSE)
