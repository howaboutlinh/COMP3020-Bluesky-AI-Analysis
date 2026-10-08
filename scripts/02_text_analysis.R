
# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 02_text_analysis.R
# =====================================================

library(tm)
library(SnowballC)


# 1. LOAD TEXT DATA ----------------------------------

# Load posts prepared in Script 01.
posts = read.csv("data/processed/bluesky_text_posts.csv")

# Keep one copy of each exact post text.
posts = posts[!duplicated(posts$text), ]

# Check the data.
nrow(posts)
head(posts$text)


# 2. CREATE TEXT CORPUS ------------------------------

# Each post is treated as one document.
corpus = Corpus(VectorSource(posts$text))


# 3. CLEAN THE TEXT ----------------------------------

# Convert text to lowercase.
corpus = tm_map(corpus, content_transformer(tolower))

# Remove numbers and punctuation.
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)

# Remove common English words.
corpus = tm_map(corpus, removeWords, stopwords("english"))

# Remove extra spaces.
corpus = tm_map(corpus, stripWhitespace)

# Reduce words to their stems.
corpus = tm_map(corpus, stemDocument)


# 4. DOCUMENT-TERM MATRIX ----------------------------

# Rows represent posts; columns represent words.
dtm = DocumentTermMatrix(corpus)

# Convert to a standard matrix.
M = as.matrix(dtm)

# Check the matrix.
dim(M)

# Find the most frequent words.
frequency = colSums(M)
head(sort(frequency, decreasing = TRUE), 20)


# 5. CALCULATE TF-IDF --------------------------------

# N = total number of documents.
N = nrow(M)

# IDF gives less weight to common words.
IDF = log(N / colSums(M > 0))

# TF measures term frequency in each document.
TF = log(M + 1)

# Calculate TF-IDF weights.
tfidf = TF %*% diag(IDF)

# Keep the word names.
colnames(tfidf) = colnames(M)

# Check the result.
dim(tfidf)


# 6. REMOVE EMPTY DOCUMENTS -------------------------

# Find posts containing no remaining terms.
empty = which(rowSums(tfidf) == 0)

# Keep posts containing at least one term.
tfidf = tfidf[rowSums(tfidf) > 0, , drop = FALSE]
posts = posts[rowSums(M) > 0, ]

# Check the number of remaining posts.
nrow(posts)
nrow(tfidf)


# 7. SAVE RESULTS ------------------------------------

# Save TF-IDF data for K-means clustering.
saveRDS(
  tfidf,
  "data/processed/bluesky_tfidf.rds"
)

# Save matching post information.
write.csv(
  posts,
  "data/processed/bluesky_text_analysis_posts.csv",
  row.names = FALSE
)
