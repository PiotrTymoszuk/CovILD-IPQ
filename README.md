# CovILD-IPQ
Analysis pipeline for modeling of illness perception one year after COVID-19

## Summary

We investigate severity and quality of illness perception (IP) in a cross-sectional observational cohort of COVID-19 convalescents ([CovILD study](https://clinicaltrials.gov/ct2/show/NCT04416100?term=CovILD&draw=2&rank=1)) suffering from persistent somatic symptoms or heart/lung abnormalities at one year after diagnosis. By regularized regression and unspervised clustering we could discover a strong association of persistent somatic symptoms (PSS) such as fatigue, physical performance loss or sleep problems with the total IP severity and its emotion/concern/consequences component. For details, please refer to our [paper](https://www.sciencedirect.com/science/article/pii/S0022399923000910?via%3Dihub).

<br>

<p align = "center"> 
<img src = "https://user-images.githubusercontent.com/80723424/226296837-d0d1f5b2-1540-436c-aaa6-e396206608a9.png" width = "80%">
</p>

<sub style = "font-size:12px; text-align:right; font-style:italic;">
Factors influencing overall illness perception (IP, A) and its emotion/concern/consequences component (B) in COVID-19 convalescents at one year after SARS-CoV-2 infection. The key influencing factors were explanatory variables selected concomitantly by three regularized regression algorithms: LASSO, Elastic Net and Bayesian LASSO.
</sub>

<br>

## Terms of use

Please cite the repository and the [peer-reviewed publication of the analysis results](https://www.sciencedirect.com/science/article/pii/S0022399923000910?via%3Dihub) (DOI: 10.1016/j.jpsychores.2023.111234). The raw data files will be made upon request to the senior study author, [Prof. Judith Löffler-Ragg](mailto:judith.loeffler@i-med.ac.at).

## Basic usage

The following development packages are required to run the pipeline:

```r

devtools::install_github('PiotrTymoszuk/soucer') ## script sourcing
devtools::install_github('PiotrTymoszuk/ExDA') ## exploratory data analysis and staristical hypothesis testing
devtools::install_github('PiotrTymoszuk/clustTools') ## factor analysis and unsupervised clustering
devtools::install_github('PiotrTymoszuk/caretExtra') ## fit statistics and quality control for the Caret models
devtools::install_github('PiotrTymoszuk/lmqc') ## fit statistics and quality control for linear models
devtools::install_github('PiotrTymoszuk/figur') ## management of figures and tables in Rmd documents
devtools::install_github('PiotrTymoszuk/trafo') ## handling of tabular data

```

Source 'exec.R' to launch the entire pipeline:

```r

source('exec.R')

```

## Contact

The repository maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com). Data requests should be addressed to [Prof. Judith Löffler-Ragg](mailto:judith.loeffler@i-med.ac.at).
