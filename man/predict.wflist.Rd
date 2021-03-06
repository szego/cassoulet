% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict.wflist}
\alias{predict.wflist}
\title{Predict using multiple models}
\usage{
\method{predict}{wflist}(objects, new_data, mode)
}
\arguments{
\item{objects}{A named list of fitted \code{workflow} objects created by \code{wf_list()}.}

\item{new_data}{A \code{data.frame} containing data with which to predict.}

\item{mode}{Either "regression" or "classification".}
}
\value{
A \code{data.frame} with one column for each fit's predictions.
}
\description{
Makes predictions from multiple \code{workflow} objects.
}
