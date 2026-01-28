# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

LabguruR is an R wrapper for the Labguru API (laboratory information management system). It enables programmatic interaction with Labguru from R for managing projects, experiments, datasets, visualizations, and performing IC50 analysis.

## Build & Development Commands

```r
# Load package for development
devtools::load_all()

# Generate documentation from roxygen comments
devtools::document()

# Build source tarball
devtools::build()

# Run R CMD CHECK
devtools::check()
```

```bash
# Command line equivalents
R CMD build LabguruR
R CMD check LabguruR
```

No formal test suite exists; vignettes in `vignettes/` serve as integration examples.

## Architecture

### Layered Design

**Layer 1: Core API** (`R/core_api_functions.R`)
- `labguru_get_by_id()`, `labguru_list_items()`, `labguru_post_item()` - HTTP wrappers using httr/jsonlite

**Layer 2: Input Validation** (`R/check_arguments.R`)
- `check_arg_*()` functions for type validation
- All exported functions validate inputs before API calls

**Layer 3: Domain Modules** (one file per entity type)
- `authenticate.R` - Token management, environment variable setup
- `dataset_functions.R` - Upload/download/list datasets
- `project.R`, `folder.R`, `experiment.R` - CRUD operations for Labguru hierarchy
- `experiment_procedures.R`, `elements.R` - Procedure and element management
- `upload_file.R` - File and visualization uploads
- `ic50.R` - IC50 dose-response analysis integration

### Entity Hierarchy
```
Project
  └─ Folder (Milestone)
      └─ Experiment
          └─ Experiment Procedure
              └─ Element (HTML, Image, R Script)
```

### Function Naming Convention
All public functions: `labguru_<operation>_<entity>()`
- Examples: `labguru_add_project()`, `labguru_list_datasets()`, `labguru_download_dataset()`

### Authentication Pattern
Functions default to environment variables:
```r
function(..., server = Sys.getenv("LABGURU_SERVER"), token = Sys.getenv("LABGURU_TOKEN"))
```

Set via `labguru_authenticate()` with `set_sys = TRUE` or `labguru_set_token()`.

## Key Dependencies

- **httr** - HTTP requests
- **jsonlite** - JSON parsing
- **magick** - Image processing (PDF to PNG conversion)
- **ic50** (optional, Suggests) - IC50 analysis; checked at runtime with `requireNamespace()`

## Code Patterns

**Standard API function structure:**
1. Validate inputs with `check_arg_*()` functions
2. Build URL with `httr::modify_url()`, encode with `URLencode()`
3. Make request with `httr::GET()` or `httr::POST()`
4. Validate response content type is `application/json`
5. Parse with `jsonlite::fromJSON(..., simplifyVector = FALSE)`
6. Return based on `return` parameter ("id" or "all")

**Return value pattern:**
```r
if (return == "id") {
  return(list(id = parsed$id))
} else {
  return(parsed)
}
```

## Sample Data

- `data/compensation.rda` - Example dataset for documentation
- `data-raw/ic50/` - Test data for IC50 analysis examples
