# COMP3020 - AI Discussions on Bluesky
# 01_data_collection.R

# 1. LIBRARIES

library(dplyr)


# 2. COLLECTION SETTINGS

keywords <- c(
  "artificial intelligence",
  "machine learning",
  "ChatGPT",
  "generative AI"
)

posts_per_keyword <- 250


# 3. DATA PATHS

raw_path <- "data/raw/bluesky_posts_raw.rds"


# 4. LOAD OR COLLECT POSTS

if (file.exists(raw_path)) {
  message("Loading existing raw data: ", raw_path)
  raw_api_posts <- readRDS(raw_path)
} else {
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

  library(bskyr)

  bluesky_handle <- Sys.getenv("BLUESKY_HANDLE")
  bluesky_app_password <- Sys.getenv("BLUESKY_APP_PASSWORD")

  if (!nzchar(bluesky_handle) || !nzchar(bluesky_app_password)) {
    stop(
      "Set BLUESKY_HANDLE and BLUESKY_APP_PASSWORD before collecting data."
    )
  }

  auth <- bs_auth(
    user = bluesky_handle,
    pass = bluesky_app_password
  )

  all_posts <- list()

  for (i in seq_along(keywords)) {
    keyword <- keywords[[i]]
    message("Collecting: ", keyword)

    result <- tryCatch(
      bs_search_posts(
        query = keyword,
        limit = posts_per_keyword,
        auth = auth
      ),
      error = function(e) {
        warning(keyword, ": ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(result) && nrow(result) > 0) {
      all_posts[[keyword]] <- result
    }

    if (i < length(keywords)) {
      Sys.sleep(2)
    }
  }

  failed_keywords <- setdiff(keywords, names(all_posts))

  if (length(failed_keywords) > 0) {
    stop(
      "Collection failed for: ",
      paste(failed_keywords, collapse = ", "),
      ". Raw data was not saved."
    )
  }

  raw_api_posts <- bind_rows(
    all_posts,
    .id = "search_keyword"
  )

  saveRDS(raw_api_posts, raw_path)
}

# Deduplicate only the downstream working data. The saved raw API data is
# retained without removing posts returned by more than one search query.

raw_posts <- raw_api_posts |>
  distinct(uri, .keep_all = TRUE)


# 5. EXTRACT NESTED FIELDS

get_field <- function(x, field) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }

  value <- x[[field]]

  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }

  as.character(value[[1]])
}

get_reply_uri <- function(record, type = "parent") {
  if (is.null(record) || length(record) == 0) {
    return(NA_character_)
  }

  reply <- record[["reply"]]

  if (is.null(reply) || length(reply) == 0) {
    return(NA_character_)
  }

  if (
    is.null(reply[["parent"]]) &&
    is.list(reply[[1]])
  ) {
    reply <- reply[[1]]
  }

  target <- reply[[type]]

  if (is.null(target) || is.null(target[["uri"]])) {
    return(NA_character_)
  }

  as.character(target[["uri"]][[1]])
}

get_did <- function(uri) {
  if (
    is.na(uri) ||
    !grepl("^at://did:[^/]+/", uri)
  ) {
    return(NA_character_)
  }

  sub("^at://([^/]+)/.*$", "\\1", uri)
}


# 6. CLEAN POST DATA

clean_posts <- raw_posts |>
  mutate(
    author_did = vapply(
      author, get_field, character(1), field = "did"
    ),

    author_handle = vapply(
      author, get_field, character(1), field = "handle"
    ),

    text = vapply(
      record, get_field, character(1), field = "text"
    ),

    created_at = vapply(
      record, get_field, character(1),
      field = "created_at"
    ),

    parent_uri = vapply(
      record, get_reply_uri, character(1),
      type = "parent"
    ),

    root_uri = vapply(
      record, get_reply_uri, character(1),
      type = "root"
    )
  ) |>
  select(
    uri,
    cid,
    search_keyword,
    author_did,
    author_handle,
    text,
    created_at,
    indexed_at,
    like_count,
    repost_count,
    reply_count,
    quote_count,
    parent_uri,
    root_uri
  )


# 7. PREPARE ANALYSIS DATA

# Exclude posts with missing or empty text.

text_posts <- clean_posts |>
  filter(
    !is.na(text),
    nzchar(trimws(text))
  )

# A directed edge connects a reply author
# to the author of the parent post.

reply_edges <- clean_posts |>
  filter(
    !is.na(parent_uri),
    !is.na(author_did)
  ) |>
  mutate(
    from = author_did,
    to = vapply(
      parent_uri,
      get_did,
      character(1)
    )
  ) |>
  filter(
    !is.na(to),
    from != to
  ) |>
  transmute(
    from,
    to,
    reply_uri = uri,
    parent_uri,
    created_at
  )

# Count repeated replies between the same users.

network_edges <- reply_edges |>
  count(from, to, name = "weight")


# 8. EXPORT DATA

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write.csv(
  clean_posts,
  "data/processed/bluesky_posts.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  text_posts,
  "data/processed/bluesky_text_posts.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  reply_edges,
  "data/processed/bluesky_reply_edges.csv",
  row.names = FALSE,
  na = ""
)

write.csv(
  network_edges,
  "data/processed/bluesky_network_edges.csv",
  row.names = FALSE,
  na = ""
)


# 9. DATA QUALITY SUMMARY

cat("\nDATA COLLECTION COMPLETED\n")

cat("Clean posts:", nrow(clean_posts), "\n")

cat("Text posts:", nrow(text_posts), "\n")

cat(
  "Unique authors:",
  n_distinct(clean_posts$author_did, na.rm = TRUE),
  "\n"
)

cat(
  "Missing text:",
  nrow(clean_posts) - nrow(text_posts),
  "\n"
)

cat(
  "Duplicate posts:",
  sum(duplicated(clean_posts$uri)),
  "\n"
)

cat(
  "Reply interactions:",
  nrow(reply_edges),
  "\n"
)

cat(
  "Unique network edges:",
  nrow(network_edges),
  "\n"
)
