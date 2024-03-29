% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapetools.R
\name{crop}
\alias{crop}
\title{Crop data frame at specific keyword}
\usage{
crop(df, direction = NULL, pos = NULL, regex = NULL, use_after = FALSE)
}
\arguments{
\item{df}{Data frame to be processed}

\item{direction}{Directoin of the cluster revolution}

\item{pos}{Positon of row/column to scan using \code{regex}}

\item{regex}{Regular expression to match keywords}

\item{use_after}{If TRUE, use part after match}
}
\description{
Crop data frame at specific keyword
}
