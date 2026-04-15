# Traffic Accident Severity Prediction using Big Data

This project builds a distributed machine learning pipeline using PySpark to predict the severity of traffic accidents. The goal is to analyze environmental and situational factors, handle extreme class imbalances, and apply ensemble techniques to assist in traffic safety risk analysis.

## Project Objectives

* Analyze environmental and situational indicators related to accident severity
* Handle extreme class imbalance within large-scale datasets
* Build and optimize ensemble machine learning models using distributed computing
* Evaluate model stability and performance using cross-validation techniques

## Dataset

The dataset used in this project contains large-scale traffic accident records. It includes multiple indicators related to environmental conditions, such as weather, road surface, and light levels.

## Target Variable

**Accident_Severity**
This variable indicates whether the recorded traffic accident is classified as a "Slight" or "Severe" incident.

## Machine Learning Workflow

1. Spark Session Initialization
2. Data Loading & Distribution
3. Feature Engineering
4. Advanced Resampling
5. Base Model Training (KNN, SVM, Decision Trees)
6. Bagging Implementation
7. Heterogeneous Ensemble Construction
8. Weighted Voting Application
9. 10-Fold Cross-Validation
10. Model Evaluation

## Data Preprocessing

The dataset was processed to handle the severe imbalance between majority and minority classes. Several potential factors were identified as missing or removed due to a lack of availability in the standard dataset, which impacted minority class prediction.

| Feature | Reason for Limitation/Removal |
| :--- | :--- |
| Driver Health | Not available in dataset but critical for severe accidents |
| Vehicle Mechanics | Lacking informative or discriminative power in current data |
| Standard Categorical Text | Removed and replaced with numerical equivalents for modeling |

## Feature Encoding

Raw environmental variables were transformed into numerical features to ensure compatibility with high-dimensional algorithms.

## Addressing Class Imbalance

**Advanced Resampling & Cost-Sensitive Learning**

To fix the model's struggle in predicting the minority class (severe accidents), resampling was applied to the training folds. 
Benefits include:
* Exposing the model to rare, high-severity accident patterns
* Penalizing misclassifications of severe accidents heavily via weighted voting

## Ensemble Architecture

**Bagging & Heterogeneous Ensembles**

Standard algorithms were wrapped into complex architectures to capture different patterns in the traffic data. This helps improve generalization and identify edge cases.

## Machine Learning Model

The primary models used as base estimators in this project are **Support Vector Machines (SVM), K-Nearest Neighbors (KNN), Decision Trees, and Random Forests**.
These were combined into a Heterogeneous Weighted Ensemble that leverages the unique strengths of each individual algorithm.

## Hyperparameter & Architecture Optimization

Model architectures were optimized using:
**Simple SVC Bagging & Weighted Voting**
These methods efficiently wrap base classifiers (like SVC) to reduce variance, prevent overfitting, and ensure stable predictions across diverse data.

## Model Evaluation

The model performance was evaluated using:
**10-Fold Cross-Validation**

| Metric | Description |
| :--- | :--- |
| Mean Accuracy | Average overall prediction correctness across 10 distinct data folds |
| Standard Deviation | Measures model stability and variance to ensure results are not due to overfitting |

## Technologies Used

* Python
* Apache Spark (PySpark)
* Scikit-learn
* PySpark MLlib
* Jupyter Notebook
