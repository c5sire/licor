licor
=====

The package provides currently minimal functionality to convert licor data 
to a more common data format of a matrix of 0 and 1. 

The function identifies all unique base pair weights and sorts them ascending into columns.
Genotypes are marked with 0 and 1 for absence or presence of the fragment.

Very much work in progress.

Installation
============

The following commands must be copied into the R console.

```{r}
# This step is only once necessary of if you want to update
install.packages("devtools")

# Excecute this to make sure to have the latest version
devtools::install_github("licor", user="c5sire")


```

R console usage
======

The function will ask using a file dialog for a data archive to process. The file format has no header. The columns are
1. Name of genotype (or box)
2. Name of marker
3. and beyond: data columns

```{r}
library(licor)

converLicorData() #interactive modus

```

Html user interface
=======
```{r}
library(licor)

#This will create a local server and a html interface
runLicorMatrix()


```



