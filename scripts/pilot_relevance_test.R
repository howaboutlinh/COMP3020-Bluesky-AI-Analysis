library(bskyr)

# Bluesky authentication
bs_set_user(Sys.getenv("BLUESKY_HANDLE"))
bs_set_pass(Sys.getenv("BLUESKY_APP_PASSWORD"))

# PILOT 1: ARTIFICIAL INTELLIGENCE


test_ai <- bs_search_posts(
  query = "artificial intelligence",
  sort = "latest",
  limit = 50
)

dim(test_ai)
names(test_ai)
View(test_ai)
names(test_ai$record[[1]])

pilot_ai <- data.frame(
  post_id = test_ai$cid,
  text = sapply(test_ai$record, function(x) x$text),
  likes = test_ai$like_count,
  reposts = test_ai$repost_count,
  replies = test_ai$reply_count
)

View(pilot_ai)
dim(pilot_ai)
head(pilot_ai$text, 10)

pilot_ai$relevant <- NA

View(pilot_ai)

write.csv(
  pilot_ai,
  "data/pilot/pilot_artificial_intelligence.csv",
  row.names = FALSE
)

pilot_ai_checked <- read.csv(
  "data/pilot/pilot_artificial_intelligence.csv"
)

table(
  pilot_ai_checked$relevant,
  useNA = "ifany"
)

mean(pilot_ai_checked$relevant) * 100

pilot_summary <- data.frame(
  keyword = "artificial intelligence",
  posts_tested = nrow(pilot_ai_checked),
  relevant_posts = sum(pilot_ai_checked$relevant == TRUE),
  irrelevant_posts = sum(pilot_ai_checked$relevant == FALSE),
  relevance_rate = mean(pilot_ai_checked$relevant) * 100
)

pilot_summary



# PILOT 2: GENERATIVE AI

test_genai <- bs_search_posts(
  query = "generative AI",
  sort = "latest",
  limit = 50
)

dim(test_genai)
names(test_genai)
names(test_genai$record[[1]])

pilot_genai <- data.frame(
  post_id = test_genai$cid,
  text = sapply(test_genai$record, function(x) x$text),
  likes = test_genai$like_count,
  reposts = test_genai$repost_count,
  replies = test_genai$reply_count
)

View(pilot_genai)

pilot_genai$relevant <- NA

dim(pilot_genai)
View(pilot_genai)

write.csv(
  pilot_genai,
  "data/pilot/pilot_generative_ai.csv",
  row.names = FALSE
)
