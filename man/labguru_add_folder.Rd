% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/folder.R
\name{labguru_add_folder}
\alias{labguru_add_folder}
\title{Labguru add folder}
\usage{
labguru_add_folder(
  title,
  project_id,
  description = NULL,
  return = "id",
  server = Sys.getenv("LABGURU_SERVER"),
  token = Sys.getenv("LABGURU_TOKEN")
)
}
\arguments{
\item{title}{character(1) The title of the folder}

\item{project_id}{numeric(1) The project id for which to add a new folder}

\item{description}{character(1) The description of the folder}

\item{return}{character(1) whether the function returns either 'id' (default) or 'all' folder information}

\item{server}{character(1) indicating the server URL}

\item{token}{character(1) access token for API authentication}
}
\value{
list with either folder id only or all folder information
}
\description{
Add a new folder to labguru
}
\examples{
\dontrun{
labguru_add_folder(project_id  = 1,
                   title       = "My new folder",
                   description = "This folder contains ...")
}
}
