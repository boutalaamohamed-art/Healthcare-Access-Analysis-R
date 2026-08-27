# Healthcare Access & Socioeconomic Inequality Analysis

## Overview

This project analyzes healthcare access and socioeconomic disparities using R. The analysis focuses on how healthcare utilization, barriers to care, and chronic disease prevalence vary across demographic and socioeconomic groups.

The dataset contains over **1 million observations and 88 variables**, including information on age, gender, income, education, employment, poverty status, healthcare utilization, and health conditions.

## Objectives

The project explores several questions:

* How does access to healthcare differ across socioeconomic groups?
* Are individuals below the poverty threshold more likely to delay healthcare because of cost?
* How does chronic disease prevalence vary by age and income?
* How do healthcare utilization patterns differ by gender, education, and employment status?
* How does mental health service utilization vary across demographic and socioeconomic groups?

## Data Preparation

The analysis includes several data-cleaning and validation steps:

* Identified and removed duplicate individual records
* Standardized inconsistent categorical variables
* Examined missing values across the dataset
* Checked for logical inconsistencies
* Created age groups and socioeconomic subgroups
* Constructed a binary chronic-disease indicator

## Analysis

The project uses descriptive statistics and grouped analysis to examine:

* Healthcare access and affordability
* Delayed healthcare due to cost
* Mental health service utilization
* Depression and anxiety indicators
* Chronic disease prevalence
* Differences across income, poverty, age, gender, education, and employment groups

A **Pearson chi-squared test** was also used to examine the association between poverty status and chronic disease prevalence.

## Key Findings

* Individuals below the poverty threshold showed a higher probability of delaying healthcare due to cost across much of the age distribution.
* Chronic disease prevalence increased substantially with age and was generally higher among lower-income groups.
* Healthcare utilization varied across education and employment groups, with differences also observed between genders.
* The analysis found a statistically significant association between poverty status and chronic disease prevalence.

## Tools

* **R**
* **dplyr** — data manipulation and aggregation
* **ggplot2** — data visualization
* Base R — data inspection, cleaning, and statistical testing

## Repository Contents

* `healthcare_analysis.R` — R code used for data preparation, analysis, statistical testing, and visualization
* `healthcare_analysis.pdf` — full project report containing methodology, results, visualizations, and interpretation

## About

This project was completed as part of my Master's studies in **Economics and Business Analytics at Johannes Kepler University Linz (JKU)** and demonstrates practical experience in data cleaning, statistical analysis, visualization, and interpretation using R.

