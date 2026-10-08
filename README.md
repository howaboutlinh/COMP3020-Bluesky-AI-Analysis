# AI Discussions on Bluesky

**COMP3020 Social Web Analytics | Group Project | Western Sydney University | 2026**

## Overview

This project analyses public Bluesky posts about artificial intelligence (AI). It combines text analysis, clustering, hypothesis testing and network analysis in R, following the methods taught in the COMP3020 labs (Modules 4–10).

## Research Questions

1. **RQ1 – Topics:** What are the main topics in AI-related posts on Bluesky? *(text analysis and k-means clustering)*
2. **RQ2 – Engagement:** Does user engagement differ between these topics? *(randomisation test, t-test and ANOVA)*
3. **RQ3 – Network:** How are the authors of AI posts connected through follow relationships, and which accounts are central? *(directed follow network and centrality)*

## Data

Data were collected with the `atrrr` package on 8–9 October 2026 (AEDT).

| Item | Value |
|---|---|
| Search terms | artificial intelligence, machine learning, ChatGPT, generative AI |
| Posts collected | 1,189 (1,168 unique) |
| Distinct authors | 887 |
| Time period of posts | 7 Oct 2026 15:24 UTC – 8 Oct 2026 16:11 UTC |
| Posts used after cleaning | 1,031 |
| Follow relationships collected | 118,988 |

- **Posts:** `search_post()`, up to about 300 latest posts per search term.
- **Follows:** `get_follows()` for every post author, up to about 300 follows each.
- In the network, a **node** is an author of an AI post and an **edge A → B** means author A follows author B.

## Repository Structure

```text
COMP3020-Bluesky-AI-Analysis/
├── data/
│   ├── raw/
│   │   ├── bluesky_posts.csv            # collected posts (Script 01)
│   │   └── bluesky_follow_edges.csv     # follow relationships (Script 01)
│   └── processed/
│       ├── bluesky_text_posts.csv       # cleaned posts (Script 02)
│       ├── bluesky_tfidf.rds            # TF-IDF matrix (Script 02)
│       ├── bluesky_term_frequencies.csv # word frequencies (Script 02)
│       ├── clustering_elbow_values.csv  # elbow method values (Script 03)
│       ├── clustering_top_terms.csv     # top terms per cluster (Script 03)
│       └── bluesky_clustered_posts.csv  # posts with cluster labels (Script 03)
├── scripts/
│   ├── 01_data_collection.R             # collect posts and follows
│   ├── 02_text_analysis.R               # cleaning, word frequency, TF-IDF
│   ├── 03_clustering.R                  # RQ1: cosine distance, MDS, k-means
│   ├── 04_hypothesis_testing.R          # RQ2: randomisation test, t-test, ANOVA
│   └── 05_network_analysis.R            # RQ3: follow network and centrality
├── figures/                             # plots used in the poster
├── report/
│   ├── analytical_report.Rmd            # report source
│   └── analytical_report.pdf            # submitted report
└── README.md
```

## Requirements

R (version 4.x) with the following packages:

```r
install.packages(c("atrrr", "tm", "SnowballC", "wordcloud", "igraph", "rmarkdown"))
```

## How to Reproduce the Analysis

Open R with the repository root as the working directory.

**To reproduce the reported results**, run Scripts 02 → 05 in order. They use the saved data in `data/raw/` and do not contact the Bluesky API:

```text
scripts/02_text_analysis.R
scripts/03_clustering.R
scripts/04_hypothesis_testing.R
scripts/05_network_analysis.R
```

To regenerate the report, open `report/analytical_report.Rmd` and click **Knit**. The report re-runs the analysis from `data/raw/`.

**Script 01 collects a new sample.** It needs a Bluesky account and an app password, set as environment variables before running:

```r
Sys.setenv(BLUESKY_HANDLE = "your-handle.bsky.social",
           BLUESKY_APP_PASSWORD = "xxxx-xxxx-xxxx-xxxx")
```

Bluesky content changes over time, so running Script 01 again produces a different dataset. It also overwrites the files in `data/raw/`. Credentials are not stored in this repository.

## Main Findings

- **RQ1:** Seven topic clusters were found. The two largest are general AI news and link sharing (589 posts) and personal opinions about using AI (261 posts). Smaller clusters include AI research and books, Google Gemini, and automated content (job-advert bots, market-report spam, a repeated poll headline).
- **RQ2:** Opinion posts received more engagement than news posts (mean 4.30 vs 2.43; randomisation test p = 0.032, t-test p = 0.046). Across all seven clusters the difference was not significant (ANOVA p = 0.119). Automated content received almost no engagement.
- **RQ3:** The follow network is sparse. 340 of 803 authors have at least one tie, with 480 edges and one large component of 294 authors. News organisations (AP, Bloomberg, EFF, The Economist) have the highest in-degree, while individual commentators rank highest on PageRank and betweenness.

## Limitations

- Keyword search over about 25 hours, sorted by latest posts. Results describe this sample only.
- Non-English text is mostly lost when it is converted to ASCII.
- The elbow plot has no clear elbow, so k = 7 was chosen for interpretability.
- Engagement is very skewed (median 0).
- At most about 300 follows per author were collected, and a follow does not show actual interaction.

## Team

- Kimmy Le
- Phoebe Le
- Quinn Nguyen