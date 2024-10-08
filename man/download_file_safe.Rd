% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{download_file_safe}
\alias{download_file_safe}
\title{Download a File Safely}
\usage{
download_file_safe(url, destfile)
}
\arguments{
\item{url}{Character. The URL from which to download the file.}

\item{destfile}{Character. The destination file path where the downloaded file will be saved.}
}
\value{
A character string representing the path to the saved file.
}
\description{
This function downloads a file from a URL to a specified destination. If a file with the
same name already exists, it appends a counter to the file name to avoid overwriting.
}
