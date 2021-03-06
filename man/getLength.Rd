% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hlpr.R
\name{getLength}
\alias{getLength}
\title{Check that all params have length = 1 or same length > 1}
\usage{
getLength(..., atoms = c("atom"))
}
\arguments{
\item{...}{Any number of atoms, vectors, lists}

\item{atoms}{Vector of class names that lengths are assumed to be 1}
}
\value{
Maximum length of params
}
\description{
Check that all params have length = 1 or same length > 1
}
