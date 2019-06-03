# LabguruR

The goal of LabguruR is to allow researchers, data scientists, bioniformaticians to directly communicate with Labguru. 
Currently the package allows you to pull and push datasets and visualizations. 
The currrent package is under development and you can add tickets and issues; 


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

# Authenticate

First get an authentication token using labguru_authenticate.

``` r
labguru_authenticate(email    = "my@@email.com",
                     password = "mypassword",
                     server   = "https://my.labguru.com",
                     set_sys  = TRUE)
```

With set_sys = TRUE your system variables LABGURU_SERVER and LABGURU_TOKEN are automatically set and can be picked up by future function calls conencting to the Labguru API. Check these variables using labguru_sys_variables()

``` r
labguru_sys_variables()
```

# Upload data to Labguru

You can now upload a dataset to Labguru using labguru_upload_dataset. There is sample data you can load from the package.

``` r
data("compensation")
compensation
```

``` r

up_0 <- labguru_upload_dataset(dataset = compensation,
                               name    = paste0("compensation_", gsub("[^[:alnum:]]","", date())))
up_0
```

# List available datasets from labguru

``` r
ld <- labguru_list_datasets(page = 1)
head(ld)
```

# Download single dataset

``` r
df <- labguru_download_dataset(dataset_id = 1)
head(df)
```



Upload a R script

``` r
up_1 <- labguru_upload_file(file  = "../data-raw/sample_upload_script.R", 
                            title = "My R script")
up_1
```


Upload an image

``` r
up_2 <- labguru_upload_file(file  = "../data-raw/grazing.png", 
                            title = paste0("grazing_", gsub("[^[:alnum:]]","", date())))
up_2
```

Attach uploaded file to a database

``` r
up_3 <- labguru_link_visualization(dataset_id = 5511,
                                   attachment_id = up_2$id,
                                   name = "my_link")
up_3
```

Upload image with immediate link to database

``` r
up_4 <- labguru_upload_visualization(file  = "../data-raw/grazing.png", 
                                     title = paste0("grazing_attached_", gsub("[^[:alnum:]]","", date())),
                                     dataset_id = 61,
                                     name = "my_second_link")
up_4
```


# ic50 Analysis

## Only downloading a plate's measure, dilution and control files

Stores measure.txt, control.txt and dilution.txt in a new plate folder. If plate/ directory already exists it only runs if this directory is empty.

``` r
labguru_download_plate(plate = 271,
                       dir = "./plate")
```

## ic50 analysis storing plate information and results locally

This function downloads the plate using plate_id and stores those results in outdir_plate. It then looks for the data files in the indir directory and performes the ic50 analysis using plates, inhib, normalize and graphics. It stores results in outdir_results directory. If img_png is true it also stores the images from the dose_response_curves pdf as single png images.

``` r
library(ic50) # ic50 library has to be loaded otherwise you get errors with '.lastxx_measure' files

ic50 <- labguru_ic50_analysis(plate_id       = 271, 
                              indir          = ".",
                              outdir_plate   = "./plate",
                              outdir_results = "./results",
                              plates         = 2,
                              inhib          = rep(0.5,7),
                              normalize      = "mean",
                              graphics       = "single",
                              img_png        = TRUE)
ic50
```

## Upload ic50 results to Labguru

``` r
up_5 <- labguru_ic50_upload(table       = ic50$result,
                            name        = paste0("data_", gsub("[^[:alnum:]]","", date())),
                            results_dir = ic50$dir,
                            img_pdf     = "img")
up_5
```

# Experiment

## Project

List all projects

``` r
projects <- labguru_list_projects(get_cols = "all")
dplyr::glimpse(projects)
```

Download project information

``` r
project_1 <- labguru_get_project(project_id = 1)
project_1
```

Start new project

``` r
project_new <- labguru_add_project(title       = "My new project",
                                   description = "This project is an analysis of ...",
                                   return      = "all")
project_new
```


## Folder

List all folders

``` r
folders <- labguru_list_folders(project_id = 91, # project_id = NULL for all folders in all projects (default)
                                get_cols   = "all")
dplyr::glimpse(folders)
```

Download folder information

``` r
folder_1 <- labguru_get_folder(folder_id = 31)
folder_1
```

Start new folder

``` r
folder_new <- labguru_add_folder(title       = "My new folder",
                                 project_id  = 91,
                                 description = "This folder is a test from LabguruR",
                                 return      = "all")
folder_new
```


## Experiment

List all experiments



``` r
experiments <- labguru_list_experiments(folder_id  = 41,
                                        get_cols   = "limited")
experiments
```

Download experiment information

``` r
experiment_1 <- labguru_get_experiment(experiment_id = 141)
experiment_1[1:4]
```

Start new experiment

``` r
experiment_new <- labguru_add_experiment(title       = "My new experiment 26-7-2018",
                                         project_id  = 91,
                                         folder_id   = 41,
                                         description = "This experiment is a test from LabguruR  26-7-2018",
                                         return      = "all")
experiment_new[1:4]
```

## Experiment procedures

``` r
ep_list <- labguru_list_experiment_procedures(experiment_id = 141, get_cols = "all")
dplyr::glimpse(ep_list)
```

``` r
ep_new <- labguru_add_experiment_procedure(name          = "New procedure 26-7-2018",
                                           experiment_id = 141)
ep_new
```


``` r
ep_271 <- labguru_get_experiment_procedure(experiment_procedure_id = 721)
ep_271
```

## Elements

List elements is EMPTY!

``` r
el_list <- labguru_list_elements(experiment_id = 141)
```

``` r
el_new <- labguru_add_element(data = "<p>Hello World</p>",
                              experiment_procedure_id = 721)
el_new
```


``` r
el_1341 <- labguru_get_element(element_id = 1341)
el_1341
```

### Upload R script as element

``` r
el_rsc  <- labguru_add_element(rscript = "../data-raw/sample_upload_script.R",
                               experiment_procedure_id = 721)
el_rsc
```

### Upload image as element

Use labguru image id (attachments_id)

``` r
el_img  <- labguru_add_element(img_id = 1971,
                               experiment_procedure_id = 721)
el_img
```
