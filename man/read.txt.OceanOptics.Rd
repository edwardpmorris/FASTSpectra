% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_txt_OceanOptics.R
\name{read.txt.OceanOptics}
\alias{read.txt.OceanOptics}
\title{Parse Ocean Optics SpectraSuite "text with header"}
\usage{
read.txt.OceanOptics(files = "*.txt", label = list(spc = "DN (counts)"))
}
\arguments{
\item{files}{A specific file path or list of file paths. Default is select all text files in working directory \code{'*.txt'}}

\item{label}{A list of form \code{list(parameter=expression(parameter))}. Default is to label the parameter as "DN (counts)"}
}
\value{
A \pkg{hyperSpec} object including a matrix of spectra, metadata extracted from the spectra headers and file information
}
\description{
Given the path to a folder of Ocean Optics SpectraSuite "text with header" files, ending in \code{'*.txt'}, this function will parse all files returning a \pkg{hyperSpec} object that includes the spectra and metadata
}
\details{
At present only "text with header" format is supported, however further formats maybe added.
}
\examples{
# set path to data files
file.path <- system.file("extdata", package = "FASTSpectra")

# parse spectra into hyperSpec object
dn <- read.txt.OceanOptics(files=paste0(file.path,"/*.txt"))
summary(dn)
}

