% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combining.R
\name{join_data_list}
\alias{join_data_list}
\title{Merge together a list of dataframes by SID into a single dataframe.}
\usage{
join_data_list(data_list)
}
\arguments{
\item{data_list}{List of data.frames}
}
\description{
Simpler than the \code{\link{combine_datasets}} function. Only allows for
merging by the variable SID (subject identification number).
}
