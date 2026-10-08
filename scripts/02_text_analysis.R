# =====================================================
# COMP3020 - AI Discussions on Bluesky
# 02_text_analysis.R
#
# Purpose: clean the post text, describe it with two
# visualisations and build the TF-IDF matrix used for
# clustering. Steps follow Modules 4, 5 and 6.
# =====================================================

library(tm)
library(SnowballC)
library(wordcloud)


# 1. LOAD DATA ---------------------------------------

posts = read.csv("data/raw/bluesky_posts.csv")
nrow(posts)


# 2. REMOVE UNSUITABLE RECORDS -----------------------

# Remove posts with missing or empty text.
posts = posts[!is.na(posts$text), ]
posts = posts[trimws(posts$text) != "", ]
nrow(posts)

# Pattern for web addresses: "http(s)://...", "www...." and
# shortened links such as "nytimes.com/2026/..." (any word
# containing a dot followed by a slash).
url.pattern = "https?://[^[:space:]]+|www\\.[^[:space:]]+|[^[:space:]]+\\.[^[:space:]]+/[^[:space:]]*"

# Remove repeated text (e.g. bots posting the same message).
# URLs are removed and case is ignored before comparing,
# so copies that only differ by a link are also detected.
compare.text = gsub(url.pattern, "", posts$text)
compare.text = tolower(trimws(compare.text))
posts = posts[!duplicated(compare.text), ]
nrow(posts)


# 3. CREATE AND CLEAN THE CORPUS ---------------------

# A corpus stores each post as one document so tm_map()
# can apply the same cleaning step to every post.
corpus = Corpus(VectorSource(posts$text))

# Remove URLs so web addresses do not become terms.
remove_urls = content_transformer(function(x) gsub(url.pattern, " ", x))
corpus = tm_map(corpus, remove_urls)

# Convert to ASCII to remove emoji and unusual characters.
corpus = tm_map(corpus, function(x) iconv(x, to = "ASCII", sub = " "))

corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, tolower)

# The search terms appear in almost every post and would
# dominate the results (Module 6 removes the search names
# for the same reason), so they are removed with stop words.
search_words = c("artificial", "intelligence", "machine", "learning",
                 "chatgpt", "generative")
corpus = tm_map(corpus, removeWords, c(stopwords("english"), search_words))

# Reduce related word forms to a common stem
# (e.g. "models" and "model" both become "model").
corpus = tm_map(corpus, stemDocument)


# 4. DOCUMENT-TERM MATRIX ----------------------------

# Rows are posts, columns are terms, values are counts.
# Note: terms shorter than 3 letters (e.g. "ai") are
# dropped by DocumentTermMatrix() by default.
Bluesky.dtm = DocumentTermMatrix(corpus)
Bluesky.matrix = as.matrix(Bluesky.dtm)
dim(Bluesky.matrix)

# Remove posts left with no terms after cleaning,
# from both the matrix and the post table.
keep = rowSums(Bluesky.matrix) > 0
Bluesky.matrix = Bluesky.matrix[keep, ]
posts = posts[keep, ]
dim(Bluesky.matrix)


# 5. WORD FREQUENCY ----------------------------------

# Total count of each term across all posts.
w = colSums(Bluesky.matrix)

# The 20 most frequent terms.
o = order(w, decreasing = TRUE)[1:20]
w[o]

# Save term frequencies for the report.
term.freq = data.frame(term = colnames(Bluesky.matrix), total_count = w)
write.csv(term.freq, "data/processed/bluesky_term_frequencies.csv", row.names = FALSE)


# 6. VISUALISATION 1: FREQUENT WORDS -----------------

par(mar = c(4, 7, 3, 1))
barplot(rev(w[o]), names.arg = rev(colnames(Bluesky.matrix)[o]),
        horiz = TRUE, las = 1, col = "steelblue",
        main = "Top 20 Most Frequent Terms", xlab = "Frequency")

# Word cloud of frequent terms (Module 5).
wordcloud(names(w), w, min.freq = 10, max.words = 100,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"))


# 7. VISUALISATION 2: POST LENGTH --------------------

# Number of characters in each post.
posts$post_length = nchar(posts$text)
summary(posts$post_length)

hist(posts$post_length, col = "lightblue",
     main = "Distribution of Post Lengths",
     xlab = "Post length (characters)", ylab = "Number of posts")


# 8. TF-IDF WEIGHTING (Module 4 Part 2) --------------

# TF-IDF gives more weight to terms that distinguish a
# post and less weight to terms that occur everywhere.
N = nrow(Bluesky.matrix)

# IDF: log of (number of posts / posts containing the term).
IDF = log(N / colSums(Bluesky.matrix > 0))

# TF: log-scaled term counts.
TF = log(Bluesky.matrix + 1)

# Multiply each term column by its IDF value.
Bluesky.weighted.matrix = TF %*% diag(IDF)
colnames(Bluesky.weighted.matrix) = colnames(Bluesky.matrix)
dim(Bluesky.weighted.matrix)


# 9. SAVE RESULTS ------------------------------------

saveRDS(Bluesky.weighted.matrix, "data/processed/bluesky_tfidf.rds")
write.csv(posts, "data/processed/bluesky_text_posts.csv", row.names = FALSE)

