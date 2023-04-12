# TesCa_hormones
Heterogeneity and clinical significance of sex hormone levels in testis carcinoma

## Summary

The analysis pursues thee goals:

* differences between two main histological types of testicle cancer: seminoma and non-seminomatous germ cell tumors (NSGCT)

* characteristic, co-regulation and phenotyping by serum levels of sex hormones prior to the cancer surgery

* prognostic significance of pre-surgery sex hormone levels for relapse-free survival

<p align = "center"> 
<img src = "https://user-images.githubusercontent.com/80723424/231457221-f6ab5a0a-7404-4ed8-8c36-1f9e7d2daab4.png" width = "65%">
</p>

These tasks are addressed in a local cohort of 518 testis cancer patients. You may follow the analysis progress [here](https://github.com/PiotrTymoszuk/TesCa_hormones/tree/main/report).

## Terms of use

To reference and use analysis results, please cite our GitHub repository and our publication if available. In any questions, please contact [Renate Pichler](mailto:renate.pichler@i-med.ac.at) or [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com).

## Usage

To make sure to install required development packages prior to runung the pipeline:

```r

devtools::install_github('PiotrTymoszuk/ExDA')
devtools::install_github('PiotrTymoszuk/kmOptimizer')
devtools::install_github('PiotrTymoszuk/coxExtensions')
devtools::install_github('PiotrTymoszuk/trafo')
devtools::install_github('PiotrTymoszuk/clustTools')
devtools::install_github('PiotrTymoszuk/soucer')
devtools::install_github('PiotrTymoszuk/figur')
devtools::install_github('PiotrTymoszuk/caretExtra')

```
To launch the entire pipeline, source the `exec.R` file:

```r

source('exec.R')

```

## Contact

The maintainer of the repository is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com).
