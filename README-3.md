# MLB Prospect Success Prediction

Predicting whether a Minor League prospect will sustain a Major League career, using MiLB performance stats and FanGraphs scouting grades.

![Hitters Model Comparison](mlb_project_demo_images/hitters_model_comparison.png)

![Hitters ROC Curves](mlb_project_demo_images/hitters_roc_curves.png)

![Hitters PCA of Clusters](mlb_project_demo_images/hitters_pca_clusters.png)

## Overview

This project estimates the likelihood that a Minor League prospect sustains a Major League career (defined as 2+ MLB seasons with 100+ plate appearances for hitters, or 20+ innings pitched for pitchers), using only Minor League performance data and FanGraphs "The Board" scouting grades.

- **Feature engineering**: hitter and pitcher features capturing performance ceiling, floor, career averages, and multi-season trajectories (e.g. `ceil_avg`, `floor_k_pct`, `avg_slope`)
- **Modeling**: Logistic Regression, Random Forest, and XGBoost, compared using cross-validation, ROC-AUC, accuracy, sensitivity, and specificity
- **Clustering**: K-means clustering (K=7) on engineered features to group prospects into statistical profiles, visualized via PCA
- **Dashboard**: an interactive R Shiny app for exploring individual prospect predictions, player information, and model output

## Results

- **Hitters**: XGBoost performed best (AUC = 0.867), followed closely by Logistic Regression and Random Forest (AUC = 0.852 each)
- **Pitchers**: XGBoost again led (AUC = 0.768), with Random Forest close behind (AUC = 0.766)
- Cluster analysis identified distinct hitter and pitcher archetypes, with predicted MLB probability varying meaningfully by cluster (e.g. Hitters Cluster 2 averaged 83.1% predicted MLB probability vs. 39.0% for Cluster 6)

## Repository Structure

```
├── R/
│   ├── fangraphs_join_fixed.R       # Data pipeline: pulls & joins FanGraphs grades + MiLB/MLB stats
│   ├── mlb_likelihood_statsonly.R   # Feature engineering, modeling, clustering
│   └── prospect_dashboard.R         # Interactive R Shiny dashboard
├── mlb_project_demo_images/
│   ├── hitters_model_comparison.png
│   ├── hitters_roc_curves.png
│   └── hitters_pca_clusters.png
└── README.md
```

## Data Sources

- Minor and Major League statistics: MLB Stats API
- Scouting grades: FanGraphs "The Board"

## Tools

R, dplyr, caret, randomForest, xgboost, ggplot2, factoextra, Shiny, plotly
