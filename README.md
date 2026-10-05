# AI Discussions on Bluesky

**COMP3020 Social Web Analytics | Group Project | Western Sydney University**

## Overview

This project investigates AI-related discussions on Bluesky using text mining, topic clustering, statistical analysis, and social network analysis.

Public posts related to four AI keywords are analysed:

- Artificial Intelligence
- Generative AI
- ChatGPT
- Machine Learning

The project examines the major topics discussed, differences in user engagement across topics, and the structure of user interactions through replies.

## Research Questions

1. **Topic Analysis:** What are the main topics in AI-related discussions on Bluesky?
2. **User Engagement:** Does user engagement differ across AI discussion topics?
3. **Network Analysis:** How are users connected through AI-related discussions, and which users occupy central positions?

## Methodology

### 1. Data Collection
AI-related public posts are collected from Bluesky using the `bskyr` package. Raw API data is preserved before downstream cleaning and processing.

### 2. Text Mining and Topic Analysis
Post text is cleaned and transformed for text analysis. Clustering methods are used to identify major topics within AI-related discussions.

### 3. User Engagement Analysis
Engagement metrics are compared across identified discussion topics using statistical analysis and hypothesis testing.

### 4. Social Network Analysis
Reply interactions between users are represented as a directed network. Network measures are used to examine connectivity and identify structurally important users.

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
├── figures/            # Generated charts and network visualisations
├── report/             # Analytical report
├── poster/             # Final project poster
├── archive/            # Earlier datasets retained for reference
├── .gitignore
└── README.md
```

## Team Members

- Kimmy Le
- Phoebe Le
- Quinn Nguyen

**Western Sydney University**  
COMP3020 Social Web Analytics | 2026
