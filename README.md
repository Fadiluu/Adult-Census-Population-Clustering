# Adult Census Population Clustering — Data Mining with K-Means, Hierarchical & DBSCAN

> An end-to-end unsupervised learning pipeline in R that applies K-Means, Hierarchical, and Density-Based (DBSCAN) clustering to the UCI Adult Census dataset — uncovering socioeconomic and demographic population segments through stratified sampling, one-hot encoding, and silhouette-based cluster evaluation.

![R](https://img.shields.io/badge/R-4.0%2B-276DC3?style=flat-square&logo=r)
![License](https://img.shields.io/badge/License-MIT-green?style=flat-square)
![Status](https://img.shields.io/badge/Status-Complete-brightgreen?style=flat-square)
![Domain](https://img.shields.io/badge/Domain-Data%20Mining-blueviolet?style=flat-square)

---

## What is This Project?

This project performs a full cluster analysis on the **UCI Adult Census dataset** — a real-world dataset containing demographic and socioeconomic information about US adults including age, education, income, marital status, race, sex, and work class.

The goal is to discover natural groupings within the population without labels, using three distinct clustering methods and evaluating their quality using silhouette scores and sum-of-squares plots. The project covers the full pipeline from raw data preprocessing through to interpretable cluster profiles.

---

## Demo / Quick Look

**K-Means (4 Clusters) — Cluster Profiles:**

| Cluster | Age | Income | Work Class | Education | Marital Status | Race | Gender |
|---|---|---|---|---|---|---|---|
| 1 | 35–45 | 40K–60K | Private | Bachelor's+ | Married | White | Male |
| 2 | 25–35 | 20K–40K | Self-Employed | Some College / Associate's | Single/Separated | Black / Asian-Pac | Male or Female |
| 3 | 45–55 | 60K–80K | Private | Master's+ | Married | White | Male |
| 4 | 25–35 | 15K–30K | Private | High School / Some College | Single/Separated | Black / Asian-Pac | Male or Female |

**DBSCAN Parameters:** `eps = 0.85`, `minPts = 9` — identified one major core cluster with multiple border points, noise depicted in black.

---

## Project Structure

```
adult-census-clustering/
├── adult-census-population-clustering.R    # Full pipeline — all 6 sections
├── .gitignore                              # Excludes cluster CSVs, R temp files
└── README.md
```

> **Dataset:** The `adult.csv` file is not included in this repo. Download it from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/2/adult). Place it in the same directory as the R script before running.

---

## Tech Stack

| Category | Tools |
|---|---|
| Language | R 4.0+ |
| IDE | RStudio (recommended) |
| Data Wrangling | `tidyverse`, `dplyr` |
| Sampling | `splitstackshape` |
| Encoding | `fastDummies` |
| Clustering | `cluster`, `dbscan`, `mclust` |
| Visualization | `ggplot2`, `factoextra`, `plotly`, `pheatmap` |
| Dataset | UCI Adult Census Income Dataset |

---

## Dataset

**Source:** [UCI Machine Learning Repository — Adult Dataset](https://archive.ics.uci.edu/dataset/2/adult)

**Original features (15 columns):**

| Column | Type | Kept? | Reason |
|---|---|---|---|
| `age` | Numeric | ✅ | Key demographic |
| `workclass` | Categorical | ✅ | Recoded |
| `fnlwgt` | Numeric | ❌ | Purpose unclear |
| `education` | Categorical | ✅ | Recoded |
| `education_num` | Numeric | ❌ | Redundant with education |
| `marital_status` | Categorical | ✅ | Recoded |
| `occupation` | Categorical | ❌ | Represented by workclass |
| `relationship` | Categorical | ❌ | Represented by marital_status |
| `race` | Categorical | ✅ | Kept |
| `sex` | Categorical | ✅ | Kept |
| `capital_gain` | Numeric | ❌ | >90% zeros |
| `capital_loss` | Numeric | ❌ | >90% zeros |
| `hours_per_week` | Numeric | ❌ | Removed to reduce complexity |
| `native_country` | Categorical | ❌ | Represented by race |
| `income` | Categorical | ✅ | Recoded to binary (0/1) |

**Final features used:** `age`, `income`, `workclass`, `education`, `marital_status`, `race`, `sex` — 7 columns, ~32,000 rows before sampling.

---

## Getting Started

### 1. Clone the repository

```bash
git clone https://github.com/YOUR_USERNAME/adult-census-clustering.git
cd adult-census-clustering
```

### 2. Download the dataset

Get `adult.csv` from the [UCI ML Repository](https://archive.ics.uci.edu/dataset/2/adult) and place it in the project root directory alongside the R script.

### 3. Open in RStudio

Open `adult-census-population-clustering.R` in RStudio. The script will install all required packages automatically on first run via the `install.packages()` calls at the top.

### 4. Run the script

Run all sections top to bottom. Each section is clearly labelled with comments:

```
#0. Importing Libraries
#1. Loading and Pre-processing Data
#2. Estimation of number of clusters
#3. K-means Clustering
#4. Hierarchical Clustering
#5. Density Clustering
#6. Cluster Evaluation
```

---

## How It Works

### Section 1 — Preprocessing

**Removing unnecessary columns:**
`fnlwgt`, `education_num`, `occupation`, `relationship`, `capital_gain`, `capital_loss`, `native_country`, `hours_per_week` dropped using `select()`.

**NA handling:**
Empty strings converted to `NA`, then `complete.cases()` used to drop all rows with missing values. Only `workclass` had NA values in the retained columns.

**Stratified Random Sampling:**
The original ~32,000 rows are too large for clustering. `stratified()` from `splitstackshape` samples 1% of the data (`size=0.01`) preserving the distribution of `workclass`, `education`, and `race` — the three most imbalanced columns.

**Recoding:**
Values simplified using `recode()`:
- `workclass`: `Self-emp-not-inc` + `Self-emp-inc` → `Self-emp`; `Federal-gov` + `State-gov` + `Local-gov` → `gov`
- `marital_status`: `Married-civ-spouse` + `Married-spouse-absent` + `Married-AF-spouse` → `Married`; `Never-married` → `Single`
- `education`: School years collapsed to `HS` (High School), `MS` (Middle School), `PS` (Primary School)
- `income`: `<=50K` → `0`, `>50K` → `1`

**One-Hot Encoding:**
`dummy_cols()` from `fastDummies` encodes all 5 categorical columns with `remove_first_dummy = TRUE` to avoid multicollinearity. Produces 22 binary columns.

**Standard Scaling:**
`scale()` applied to all 24 final columns (2 numeric + 22 one-hot). Standard scaling chosen over min-max due to the presence of one-hot encoded binary columns.

---

### Section 2 — Estimating Number of Clusters

Four methods used to estimate the optimal k:

| Method | Suggested k | Notes |
|---|---|---|
| Between SS / Total SS plot | 4–6 | Elbow visible in this range |
| Within Sum of Squares (WSS) | 5–6 | Clear elbow at k=5 |
| Gap Statistic | 1 | Discarded — 1 cluster is not meaningful |
| Silhouette | 8 | Upper bound estimate |

**Decision:** Experiment with k in the range **4–8** for further analysis.

---

### Section 3 — K-Means Clustering

`kmeans()` run with `nstart=25` and `iter.max=100` for stability.

**K=4 clusters:** Clusters 1 and 3 show higher income and education (Bachelors+/Masters+, married, white, private sector). Clusters 2 and 4 show lower income, self-employed or private, single/separated, non-white demographics.

**K=5 clusters:** Adds a fifth profile — self-employed individuals with Bachelor's+ degrees who are single or divorced, predominantly white or other race, female-leaning.

Cluster profiles visualised with:
- `fviz_cluster()` — 2D PCA projection (Dim1=12.2%, Dim2=7.7% variance explained)
- `geom_boxplot()` — age distribution per cluster
- `geom_bar(position="fill")` — income, workclass, marital status, education, race, sex proportions per cluster

---

### Section 4 — Hierarchical Clustering

`hclust()` with Ward's method (`ward.D2`) on full Euclidean distance matrix.
`cutree()` used to extract k=4 and k=7 cluster assignments.

**K=4 highlights:**
- Cluster 1: Low income, private sector, assoc/bachelor's/HS education, mostly married, predominantly white, 60–70% male
- Cluster 2: Balanced income, self-employed majority, bachelor's/master's, married, predominantly white male
- Cluster 3: Low income, 70% self-employed, all widows, female, predominantly white
- Cluster 4: Low income, 80% self-employed, all widows, balanced gender, 60% Black

**K=7 highlights:**
- Cluster 5: All currently in High School, balanced gender, predominantly white
- Cluster 6: 90% earn below 50K, all Black, all without a current partner
- Cluster 7: 50%+ Assoc-voc education, mostly private sector, majority married

Visualised with `fviz_dend()` in both standard and phylogenic tree layouts.

---

### Section 5 — DBSCAN Density Clustering

`kNNdistplot(data, k=6)` used to visually identify the elbow/knee point for `eps`.

**Parameters chosen:**
- `eps = 0.85` — distance threshold identified from knee of KNN distance plot (red dashed line at `h=0.85`)
- `minPts = 9` — set to number of features + 1 (standard heuristic)

**Result:** One major core cluster (>9 points), multiple border points, noise points shown in black. Three total clusters identified.

---

### Section 6 — Cluster Quality Evaluation

**K-Means Silhouette (k=4):** Cluster 3 has the most misclassified points. Clusters 2 and 4 are the cleanest.

**K-Means Silhouette (k=5):** Most clusters except Cluster 5 show notable misclassification.

**DBSCAN Silhouette:** No within-cluster misclassification, but many noise points that could plausibly belong to clusters are misclassified as outliers.

---

## Results Summary

| Method | Config | Strength | Weakness |
|---|---|---|---|
| K-Means | k=4 | Clear demographic segments, clean clusters 2 & 4 | Cluster 3 misclassification |
| K-Means | k=5 | More granular profiling | More misclassification overall |
| Hierarchical | k=4 | Reveals widow/gender-specific clusters | Less scalable |
| Hierarchical | k=7 | Very fine-grained segments (e.g., HS students, all-Black cluster) | Hard to interpret at scale |
| DBSCAN | eps=0.85, minPts=9 | No forced shape assumption, handles noise | Many noise points unassigned |

---

## Limitations

- **Sampling trade-off:** Only 1% of data used for clustering — larger samples may reveal different cluster boundaries.
- **One-hot expansion:** 5 categorical columns expand to 22 binary features, which can dilute the signal of the 2 numeric features.
- **Low silhouette scores overall:** The data does not have strongly separable natural clusters — max average silhouette width is ~0.20, indicating overlapping groups.
- **DBSCAN noise sensitivity:** With `eps=0.85`, a large proportion of points fall outside any cluster — tuning required for production use.
- **Static analysis:** Clusters are a snapshot of census data and may not reflect current population dynamics.

---

## Requirements

All packages are installed at the top of the script. Requires **R 4.0+** and RStudio.

```r
install.packages(c(
  "tidyverse", "fastDummies", "cluster", "factoextra",
  "plotly", "pheatmap", "igraph", "mclust",
  "dbscan", "splitstackshape"
))
```

---

## Roadmap

- [ ] Try fuzzy clustering (e.g., `fanny()`) for soft cluster assignments
- [ ] Experiment with larger sample sizes using cloud compute
- [ ] Add interactive cluster explorer with Shiny
- [ ] Test with PCA dimensionality reduction before clustering
- [ ] Compare with Gaussian Mixture Models (`Mclust`) at scale

---

## License

This project is licensed under the MIT License.

---

*Muhammed Fadil | University Project | University of Wollongong in Dubai Graduate*
