
<img src="https://github.com/user-attachments/assets/d503aec3-c21a-4e48-99e9-4e5285845b45" alt="ShiNyP-icon" width="100"/>

# _ShiNyP_: An Interactive Shiny-based Platform for Genome-Wide SNP Analysis and Visualization

![CI](https://img.shields.io/badge/build-passing-brightgreen)
[![R-CMD-check](https://github.com/irudnyts/openai/workflows/R-CMD-check/badge.svg)](https://github.com/irudnyts/openai/actions)
![Version](https://img.shields.io/badge/version-0.1.0-blue)
<!-- badges: end -->


## 🔸Overview

**_ShiNyP_ is a platform designed for real-time processing, analysis, and visualization of SNP datasets.**

**📄Input data:** Genome-wide biallelic SNP in Variant Call Format (VCF).

**📊Analysis:** Data QC, population genetics analysis, core collection, and more.

**📋Output:** Publication-ready figures, tables, analyzed data objects, and AI-driven reports.

For detailed instructions on each feature, please visit ➡️ [**User Guide**](https://teddyenn.github.io/ShiNyP-guide/) ⬅️

## 🔸Quickstart

### ✅ Prerequisites
- R Version: R ≥ 4.4 (compatible with Bioconductor version 3.19)
- R Tools:
  - Windows: Rtools
  - macOS: Xcode Command Line Tools
  - Linux: Development tools (e.g., r-base-dev)


### 1️⃣ Pre-install Required Package
   ```R
   install.packages("BiocManager")
   BiocManager::install(version = "3.19")
   BiocManager::install(c("qvalue", "SNPRelate", "ggtree"), force = TRUE)
   ```
### 2️⃣ Install the _ShiNyP_ Package from GitHub
   ```R
   install.packages("remotes")
   remotes::install_github("TeddYenn/ShiNyP", force = TRUE)
   ```
### 3️⃣ Start the _ShiNyP_ Platform
   ```R
   library(ShiNyP)
   ShiNyP::run_ShiNyP()
   ```
### 4️⃣ Run _ShiNyP_ Analysis
Input your SNP data in VCF format, or feel free to use the built-in demo data.


## 🔸URLs

▪️ Journal Article: 

▪️ User Manual: [https://teddyenn.github.io/ShiNyP-guide](https://teddyenn.github.io/ShiNyP-guide)

▪️ Demo Datasets: [https://github.com/TeddYenn/ShiNyP/tree/main/tools/Demo_Datasets](https://github.com/TeddYenn/ShiNyP/tree/main/tools/Demo_Datasets)

▪️ ShiNyP Outputs (Samples): [https://zenodo.org/records/14813628](https://zenodo.org/records/14813628)

▪️ Online Platform (Demo): [https://teddyhuang.shinyapps.io/ShiNyP_Demo/](https://teddyhuang.shinyapps.io/ShiNyP_Demo/)

▪️ GitHub Repository: [https://github.com/TeddYenn/ShiNyP](https://github.com/TeddYenn/ShiNyP)


## 🔸Support
If you encounter any issues or have suggestions for new features, please submit a report through our [Feedback Form](https://forms.gle/GPCggSo5czyNLfoB7) or email us at: teddyhuangyh@gmail.com


## 🔸Citation

```
Huang et al. (upcoming 2025) ShiNyP: An Interactive Shiny-based Platform for Genome-Wide SNP Analysis and Visualization
Under Review…
```
