# COMP3020 - AI Discussions on Bluesky
# 02_text_analysis.R
# Prepare the canonical text dataset for RQ1 topic clustering.


# 1. PATHS AND SETTINGS

input_path <- "data/processed/bluesky_text_posts.csv"
analysis_path <- "data/processed/bluesky_text_analysis.csv"
representation_path <-
  "data/processed/bluesky_text_representation.rds"
duplicate_log_path <-
  "data/processed/bluesky_text_duplicate_log.csv"
term_frequency_path <-
  "data/processed/bluesky_term_frequencies.csv"
term_figure_path <- "figures/rq1_top_terms.png"

# Requiring occurrence in at least three documents reduces one-off and
# two-document noise while retaining substantially more information than more
# restrictive thresholds. The effect is reported when the script runs.
minimum_document_frequency <- 3
maximum_document_proportion <- 0.80
top_terms_to_plot <- 20


# 2. LOAD AND VALIDATE THE CANONICAL TEXT DATA

if (!file.exists(input_path)) {
  stop("Missing canonical text dataset: ", input_path)
}

posts <- read.csv(
  input_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)

required_columns <- c(
  "uri",
  "cid",
  "search_keyword",
  "author_did",
  "author_handle",
  "text",
  "created_at",
  "like_count",
  "repost_count",
  "reply_count",
  "quote_count"
)

missing_columns <- setdiff(required_columns, names(posts))

if (length(missing_columns) > 0) {
  stop(
    "Input is missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

input_count <- nrow(posts)
missing_text <- is.na(posts$text) | !nzchar(trimws(posts$text))
missing_text_count <- sum(missing_text)

if (missing_text_count > 0) {
  stop(
    "The canonical text dataset contains ", missing_text_count,
    " missing or empty text rows. Re-run scripts/01_data_collection.R."
  )
}

if (anyDuplicated(posts$uri) > 0) {
  stop("The canonical text dataset contains duplicate post URIs.")
}


# 3. CREATE A DEDUPLICATED TEXT-ANALYSIS DATASET

# Exact duplicate texts can over-weight repeated or syndicated content during
# clustering. The canonical input remains unchanged; only the separate text
# analysis dataset removes later occurrences of exactly identical text.

duplicate_text <- duplicated(posts$text)
duplicate_text_count <- sum(duplicate_text)
first_text_row <- match(posts$text, posts$text)

# Public post text can still contain credential-shaped strings. Remove that
# shape only from generated analysis outputs; canonical source files remain
# untouched. The replacement is empty so it adds no artificial topic term.
credential_shape <- "(?i)[a-z0-9]{4}(?:-[a-z0-9]{4}){3}"
safe_text <- gsub(credential_shape, "", posts$text, perl = TRUE)
safe_text <- gsub("[[:blank:]]+(?=\\r?\\n|$)", "", safe_text, perl = TRUE)

duplicate_log <- data.frame(
  duplicate_uri = posts$uri[duplicate_text],
  retained_uri = posts$uri[first_text_row[duplicate_text]],
  text = safe_text[duplicate_text],
  stringsAsFactors = FALSE
)

analysis_posts <- posts[!duplicate_text, , drop = FALSE]
analysis_posts$text <- safe_text[!duplicate_text]


# 4. CLEAN AND TOKENISE TEXT

# AI-specific terms are deliberately retained. Stemming and lemmatisation are
# not used because they are unnecessary for this introductory workflow and can
# make the resulting topic terms harder to interpret.

stop_words <- c(
  "a", "about", "after", "again", "against", "all", "also", "am", "an",
  "and", "any", "are", "as", "at", "be", "because", "been", "before",
  "being", "between", "both", "but", "by", "can", "could", "did", "do",
  "does", "doing", "down", "during", "each", "few", "for", "from",
  "further", "had", "has", "have", "having", "he", "her", "here",
  "hers", "herself", "him", "himself", "his", "how", "i", "if", "in",
  "into", "is", "it", "its", "itself", "just", "me", "more", "most",
  "my", "myself", "no", "nor", "not", "now", "of", "off", "on",
  "once", "only", "or", "other", "our", "ours", "ourselves", "out",
  "over", "own", "same", "she", "should", "so", "some", "such",
  "than", "that", "the", "their", "theirs", "them", "themselves",
  "then", "there", "these", "they", "this", "those", "through", "to",
  "too", "under", "until", "up", "very", "was", "we", "were", "what",
  "when", "where", "which", "while", "who", "whom", "why", "will",
  "with", "would", "you", "your", "yours", "yourself", "yourselves",
  "http", "https", "www", "com", "bsky", "social"
)

clean_text <- tolower(enc2utf8(analysis_posts$text))
clean_text <- gsub("[’‘]", "'", clean_text)
clean_text <- gsub("\\bcan't\\b", "can not", clean_text, perl = TRUE)
clean_text <- gsub("\\bwon't\\b", "will not", clean_text, perl = TRUE)
clean_text <- gsub("n't\\b", " not", clean_text, perl = TRUE)
clean_text <- gsub("'re\\b", " are", clean_text, perl = TRUE)
clean_text <- gsub("'ve\\b", " have", clean_text, perl = TRUE)
clean_text <- gsub("'ll\\b", " will", clean_text, perl = TRUE)
clean_text <- gsub("'d\\b", " would", clean_text, perl = TRUE)
clean_text <- gsub("'m\\b", " am", clean_text, perl = TRUE)
clean_text <- gsub("'s\\b", "", clean_text, perl = TRUE)
clean_text <- gsub("https?://[^[:space:]]+", " ", clean_text)
clean_text <- gsub("www\\.[^[:space:]]+", " ", clean_text)
clean_text <- gsub("@[[:alnum:]_.-]+", " ", clean_text)
clean_text <- gsub("[^[:alpha:][:space:]]", " ", clean_text)
clean_text <- gsub("[[:space:]]+", " ", clean_text)
clean_text <- trimws(clean_text)

tokens_before_stop_words <- strsplit(clean_text, "[[:space:]]+")
tokens_before_stop_words <- lapply(
  tokens_before_stop_words,
  function(tokens) tokens[nchar(tokens) >= 3 | tokens == "ai"]
)

token_count_before_stop_words <- sum(lengths(tokens_before_stop_words))

document_tokens <- lapply(
  tokens_before_stop_words,
  function(tokens) tokens[!tokens %in% stop_words]
)

token_count_after_stop_words <- sum(lengths(document_tokens))

analysis_posts$cleaned_text <- vapply(
  document_tokens,
  paste,
  collapse = " ",
  FUN.VALUE = character(1)
)


# 5. INSPECT AND FILTER THE VOCABULARY

all_terms <- sort(unique(unlist(document_tokens, use.names = FALSE)))

if (length(all_terms) == 0) {
  stop("No terms remain after text preprocessing.")
}

document_frequency <- vapply(
  all_terms,
  function(term) {
    sum(vapply(document_tokens, function(tokens) term %in% tokens, logical(1)))
  },
  integer(1)
)

maximum_document_frequency <- floor(
  maximum_document_proportion * nrow(analysis_posts)
)

vocabulary <- all_terms[
  document_frequency >= minimum_document_frequency &
    document_frequency <= maximum_document_frequency
]

if (length(vocabulary) < 2) {
  stop("Fewer than two terms remain after document-frequency filtering.")
}


# 6. BUILD DOCUMENT-TERM AND TF-IDF MATRICES

document_term_matrix <- matrix(
  0,
  nrow = nrow(analysis_posts),
  ncol = length(vocabulary),
  dimnames = list(analysis_posts$uri, vocabulary)
)

for (i in seq_along(document_tokens)) {
  retained_tokens <- document_tokens[[i]][
    document_tokens[[i]] %in% vocabulary
  ]

  if (length(retained_tokens) > 0) {
    counts <- table(retained_tokens)
    document_term_matrix[i, names(counts)] <- as.numeric(counts)
  }
}

retained_terms_per_document <- rowSums(document_term_matrix)
documents_without_retained_terms <- sum(retained_terms_per_document == 0)

term_frequency_matrix <- document_term_matrix
nonzero_documents <- retained_terms_per_document > 0
term_frequency_matrix[nonzero_documents, ] <-
  document_term_matrix[nonzero_documents, , drop = FALSE] /
  retained_terms_per_document[nonzero_documents]

retained_document_frequency <- colSums(document_term_matrix > 0)
inverse_document_frequency <- log(
  nrow(document_term_matrix) / retained_document_frequency
)

tf_idf_matrix <- sweep(
  term_frequency_matrix,
  MARGIN = 2,
  STATS = inverse_document_frequency,
  FUN = "*"
)

# L2 normalisation prevents longer posts from dominating Euclidean-distance
# clustering solely because they contain more words.
row_norm <- sqrt(rowSums(tf_idf_matrix^2))
tf_idf_matrix[row_norm > 0, ] <-
  tf_idf_matrix[row_norm > 0, , drop = FALSE] / row_norm[row_norm > 0]

analysis_posts$retained_term_count <- retained_terms_per_document


# 7. TERM-FREQUENCY OUTPUT

term_frequencies <- data.frame(
  term = vocabulary,
  total_count = colSums(document_term_matrix),
  document_frequency = retained_document_frequency,
  stringsAsFactors = FALSE
)

term_frequencies <- term_frequencies[
  order(
    term_frequencies$total_count,
    term_frequencies$document_frequency,
    decreasing = TRUE
  ),
  ,
  drop = FALSE
]


# 8. SAVE REUSABLE OUTPUTS

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)

write.csv(analysis_posts, analysis_path, row.names = FALSE, na = "")
write.csv(duplicate_log, duplicate_log_path, row.names = FALSE, na = "")
write.csv(term_frequencies, term_frequency_path, row.names = FALSE)

text_representation <- list(
  uri = analysis_posts$uri,
  document_term_matrix = document_term_matrix,
  tf_idf_matrix = tf_idf_matrix,
  vocabulary = vocabulary,
  document_frequency = retained_document_frequency,
  settings = list(
    minimum_document_frequency = minimum_document_frequency,
    maximum_document_proportion = maximum_document_proportion,
    stop_words = stop_words,
    l2_normalised = TRUE
  )
)

saveRDS(text_representation, representation_path)

plot_terms <- head(term_frequencies, top_terms_to_plot)

png(term_figure_path, width = 1200, height = 900, res = 140)
par(mar = c(5, 10, 4, 2) + 0.1)
barplot(
  rev(plot_terms$total_count),
  names.arg = rev(plot_terms$term),
  horiz = TRUE,
  las = 1,
  col = "#4472C4",
  xlab = "Token count",
  main = "Most frequent terms in Bluesky AI posts"
)
dev.off()


# 9. CONSOLE SUMMARY

cat("\nRQ1 TEXT PREPARATION COMPLETED\n")
cat("Input posts:", input_count, "\n")
cat("Missing/empty text rows:", missing_text_count, "\n")
cat("Exact duplicate text rows identified:", duplicate_text_count, "\n")
cat("Posts retained for text analysis:", nrow(analysis_posts), "\n")
cat("Tokens before stop-word removal:", token_count_before_stop_words, "\n")
cat("Tokens after stop-word removal:", token_count_after_stop_words, "\n")
cat("Vocabulary before frequency filtering:", length(all_terms), "\n")
cat("Final vocabulary size:", length(vocabulary), "\n")
cat(
  "Document representation dimensions:",
  nrow(tf_idf_matrix), "x", ncol(tf_idf_matrix), "\n"
)
cat(
  "Documents with no term after frequency filtering:",
  documents_without_retained_terms, "\n"
)
cat(
  "Every representation row maps to a unique URI:",
  identical(rownames(tf_idf_matrix), analysis_posts$uri), "\n"
)

cat("\nFILES CREATED\n")
cat(analysis_path, "\n")
cat(representation_path, "\n")
cat(duplicate_log_path, "\n")
cat(term_frequency_path, "\n")
cat(term_figure_path, "\n")
