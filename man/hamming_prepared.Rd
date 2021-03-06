% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hamming_prepared.R
\name{hamming_prepared}
\alias{hamming_prepared}
\title{Prepare a prevalence weighted set from raw data.}
\usage{
hamming_prepared(raw_data, cols, weighting = NULL)
}
\arguments{
\item{raw_data}{matrix or data.frame containing binary coded data.}

\item{cols}{Names or integer of column positions; if not specified then all columns are used.}

\item{weighting}{Weighting measure to use passed as a string, options include \code{prevalence},
\code{inv_pr}, \code{inv_pr_wt}, \code{floored_pr}, or NULL TO BE UPDATED.}
}
\value{
A list with two items:
\describe{
  \item{data}{A matrix of unique combinations in the provided data
  weighted according to the specified weighting.}
  \item{key}{A vector of keys for each row of the returned \code{data},
  which is made up of the concatenated binary string across columns}.
  }
}
\description{
Apply a weighting to a binary data.frame or matrix.
}
\examples{
combos <- matrix(c(0,0,1,1,
                   1,0,1,1,
                   0,1,1,1), ncol=4, byrow=TRUE)
hp <- hamming_prepared(combos, 1:3, weighting = 'prevalence')
}
