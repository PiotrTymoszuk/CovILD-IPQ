# CovILD-IPQ
Analysis pipeline for modeling of illness perception one year after COVID-19

## Summary

We investigate severity and quality of illness perception (IP) in a cross-sectional observational cohort of COVID-19 convalescents ([CovILD study](https://clinicaltrials.gov/ct2/show/NCT04416100?term=CovILD&draw=2&rank=1)) suffering from persistent somatic symptoms or heart/lung abnormalities at one year after diagnosis. By regularized regression and unspervised clustering we could discover a strong association of persistent somatic symptoms such as fatigue, physical performance loss or sleep problems with the total IP severity and its emotion/concern/consequences component. For details, please refer to our [preprint](https://www.medrxiv.org/content/10.1101/2022.09.05.22279602v1).

## Terms of use

Please cite the repository, [preprint](https://www.medrxiv.org/content/10.1101/2022.09.05.22279602v1) and the peer-reviewed publication of the analysis results when available. The raw data files will be made upon request to the senior study author, [Prof. Judith Löffler-Ragg](mailto:judith.loeffler@i-med.ac.at).

## Basic usage

The following development packages are required to run the pipeline:

```r

devtools::install_github('PiotrTymoszuk/soucer') ## script sourcing
devtools::install_github('PiotrTymoszuk/ExDA') ## exploratory data analysis and staristical hypothesis testing
devtools::install_github('PiotrTymoszuk/clustTools') ## factor analysis and unsupervised clustering
devtools::install_github('PiotrTymoszuk/caretExtra') ## fit statistics and quality control for the Caret models
devtools::install_github('PiotrTymoszuk/lmqc') ## fit statistics and quality control for linear models
devtools::install_github('PiotrTymoszuk/figur') ## management of figures and tables in Rmd documents

```

Source 'exec.R' to launch the entire pipeline:

```r

source('exec.R')

```

## Contact

The repository maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com). Data requests should be addressed to [Prof. Judith Löffler-Ragg](mailto:judith.loeffler@i-med.ac.at).
