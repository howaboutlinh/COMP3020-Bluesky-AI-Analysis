# AI Discussions on Bluesky

**COMP3020 Social Web Analytics | Group Project | Western Sydney University**

## Overview

This project investigates AI-related discussions on Bluesky using text mining, topic clustering, statistical analysis, and social network analysis.

Public Bluesky posts related to four AI search terms are analysed:

- Artificial Intelligence
- Generative AI
- ChatGPT
- Machine Learning

The project examines the major themes present in AI-related discussions, differences in user engagement across discussion topics, and the structure of user interactions through replies.

## Research Questions

1. **Topic Analysis:** What are the main topics in AI-related discussions on Bluesky?
2. **User Engagement:** Does user engagement differ across AI discussion topics?
3. **Network Analysis:** How are users connected through AI-related discussions, and which users occupy central positions?

## Methodology

### 1. Data Collection

Public Bluesky posts were collected using the `bskyr` package and four AI-related search terms:

- Artificial Intelligence
- Generative AI
- ChatGPT
- Machine Learning

The original API results are preserved in the raw dataset before downstream cleaning and analysis.

A pilot relevance assessment was also conducted to evaluate the suitability of the selected search terms.

### 2. Text Preprocessing

Post text is prepared for analysis through a reproducible preprocessing pipeline.

The pipeline includes:

- removal of URLs and mentions
- contraction handling
- punctuation and number removal
- tokenisation
- stopword removal
- exact-text duplicate handling
- document-frequency filtering
- TF-IDF transformation
- L2 normalisation

Posts without usable terms after preprocessing are retained for traceability but excluded from clustering.

### 3. Topic Clustering

K-means clustering is applied to the normalised TF-IDF representation of the processed Bluesky posts.

Candidate values of `k` are evaluated using within-cluster sum of squares and cluster interpretability. The final clustering solution is used to investigate the major lexical themes within AI-related discussion.

The clustering results are interpreted as descriptive lexical groupings rather than naturally occurring or mutually exclusive topic boundaries.

### 4. User Engagement Analysis

Engagement analysis will compare engagement metrics across the discussion topics identified in the clustering analysis.

The corresponding analysis is implemented in:

`04_hypothesis_testing.R`

### 5. Social Network Analysis

Reply interactions are represented as directed user-to-user edges. Network analysis will examine the structure of these interactions and identify users occupying central positions.

The corresponding analysis is implemented in:

`05_network_analysis.R`

## Dataset

The canonical raw dataset is stored at:

```text
data/raw/bluesky_posts_raw.rds
```

It contains **976 collected Bluesky posts**.

Processed datasets used by the analysis are stored in:

```text
data/processed/
```

The raw dataset is included so that the analytical pipeline can be reproduced without recollecting posts from the Bluesky API.

## Repository Structure

```text
COMP3020-Bluesky-AI-Analysis/
├── data/
│   ├── raw/            # Original collected Bluesky data
│   ├── processed/      # Processed datasets used for analysis
│   └── pilot/          # Pilot keyword relevance datasets
│
├── scripts/
│   ├── 01_data_collection.R
│   ├── 02_text_analysis.R
│   ├── 03_clustering.R
│   ├── 04_hypothesis_testing.R
│   ├── 05_network_analysis.R
│   └── pilot_relevance_test.R
│
├── figures/            # Generated analysis figures
├── report/             # Analytical report
├── poster/             # Final project poster
├── archive/            # Earlier datasets retained for reference
├── .gitignore
└── README.md
```

## Reproducing the Analysis

### Requirements

The project is implemented in **R**.

The required R packages depend on the individual analysis scripts. Current core dependencies include:

```r
dplyr
ggplot2
bskyr
```

Additional package dependencies used by the engagement and network analyses will be documented after those components are completed.

### Running the Project

Clone the repository and open R with the repository root as the working directory.

Run the analysis scripts in numerical order:

```text
scripts/01_data_collection.R
scripts/02_text_analysis.R
scripts/03_clustering.R
scripts/04_hypothesis_testing.R
scripts/05_network_analysis.R
```

The scripts use relative paths from the repository root and do not require machine-specific working-directory paths.

### Reproducing from the Existing Raw Dataset

Because the original Bluesky data is included in:

```text
data/raw/bluesky_posts_raw.rds
```

reproducing the analytical results does **not** require recollecting data from Bluesky.

When the raw dataset already exists, the data collection pipeline loads the stored dataset rather than making new API requests.

The subsequent analysis can therefore be reproduced from the repository data.

### Recollecting Data from Bluesky

Bluesky credentials are required only when collecting a new raw dataset.

The collection script expects the following environment variables:

```text
BLUESKY_HANDLE
BLUESKY_APP_PASSWORD
```

Credentials must be stored locally and are **not included in this repository**.

Because Bluesky content changes over time, recollecting the data may produce a dataset different from the one used for the submitted analysis. The included raw dataset should therefore be used when reproducing the reported results.

## Analysis Outputs

Generated processed datasets are written to:

```text
data/processed/
```

Generated figures are written to:

```text
figures/
```

For the topic-analysis pipeline, the main outputs include the processed text representation, clustering assignments, cluster summaries, representative posts, and clustering visualisations.

The analytical report is maintained in:

```text
report/
```

The final project poster is maintained in:

```text
poster/
```

## Reproducibility Notes

- Raw API data is preserved separately from processed data.
- Analysis scripts use relative repository paths.
- Randomised clustering uses a fixed random seed.
- Credentials and local environment files are excluded from version control.
- Generated analytical results can be recreated from the included raw dataset.
- Earlier datasets retained for historical reference are stored in `archive/` and are not used as the canonical analysis dataset.

## Project Status

| Component | Script | Status |
|---|---|---|
| Data collection and preparation | `01_data_collection.R` | Complete |
| Text preprocessing and TF-IDF | `02_text_analysis.R` | Complete |
| Topic clustering | `03_clustering.R` | Complete |
| User engagement analysis | `04_hypothesis_testing.R` | In progress |
| Social network analysis | `05_network_analysis.R` | In progress |
| Analytical report | `report/` | In progress |
| Poster | `poster/` | In progress |

## Team Members

- Kimmy Le
- Phoebe Le
- Quinn Nguyen

**Western Sydney University**  
COMP3020 Social Web Analytics | 2026
