licor
=====

The package provides currently minimal functionality to convert licor data 
to a more common data format of a matrix of 0 and 1. 

The function identifies all unique base pair weights and sorts them ascending into columns.
Genotypes are marked with 0 and 1 for absence or presence of the fragment.

Very much work in progress.

Installation
============

```{r}
install.packages("devtools")

devtools::install_github("licor", user="c5sire")


```

Usage
======

The function will ask using a file dialog for a data archive to process. The file format has no header. The columns are
1. Name of genotype (or box)
2. Name of marker
3. and beyond: data columns

```{r}
library(licor)

data = licor2matrix()
```

