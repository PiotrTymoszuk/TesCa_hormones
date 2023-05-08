# TesCa_hormones
Heterogeneity and clinical significance of sex hormone levels in testis carcinoma

## Summary

The analysis pursues thee goals:

* differences between two main histological types of testicle cancer: seminoma and non-seminomatous germ cell tumors (NSGCT)

* characteristic, co-regulation and phenotyping by serum levels of sex hormones prior to the cancer surgery

* prognostic significance of pre-surgery sex hormone levels for relapse-free survival

* investigation of expression of sex hormone-related genes in testicle cancer tissue from the TCGA cohort and its implication for tumor biology, immunity, signaling and prognosis

<br>

<p align = "center"> 
<img src = "https://user-images.githubusercontent.com/80723424/233738860-8d88f793-04c6-46a4-bcb8-42efd38b5767.png" width = "65%">
</p>

These tasks are addressed in a multicenter retrospective cohort of 518 testis cancer patients and the testis cancer collactive of the TCGA pan-cancer project (n = 149). 
<br>
<br>
You may follow the analysis progress [here](https://github.com/PiotrTymoszuk/TesCa_hormones/tree/main/report).
<br>
Parts of the Testicular Cancer manuscript with analyses of the retrospective cohort (Figures, Tables, Supplement) are available [here](https://github.com/PiotrTymoszuk/TesCa_hormones/tree/main/tesca%20paper).

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
