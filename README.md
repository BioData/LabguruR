# LabguruR

The goal of LabguruR is to allow researchers, data scientists, bioniformaticians to directly communicate with Labguru. 
Currently the package allows you to pull and push datasets and visualizations. 
The currrent package is in development and you can add tickets and issues; 


## Installation

You can install LabguruR from github with:


``` r
# Install devtools if needed
if (!require(devtools)) install.packages("devtools")

devtools::install_github("BioData/LabguruR")
```

You require httr and jsonlite to use LabguruR. If these dependencies weren't installed automatically (or you're not sure), run:

``` r
if (!require(httr)) install.packages("httr")
if (!require(jsonlite)) install.packages("jsonlite")
```

Load LabguruR

``` r
library(LabguruR)
```

## Example

For examples see the vignettes. Start with the how_to vignette (LabguruR/inst/doc/how_to.html).
