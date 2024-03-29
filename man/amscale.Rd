% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amscale.R
\name{amscale}
\alias{amscale}
\title{Aldrich-McKlevey Scaling}
\usage{
amscale(x, resp = NULL, polarity = NULL)
}
\arguments{
\item{x}{A dataframe or matrix containing numeric values of respondent placements of stimuli, with stimuli on columns and respondents on rows.}

\item{resp}{An optional numeric vector containing respondent self-placements.}

\item{polarity}{An optional integer giving the column index of a stimulus that you wish to have a negative value. All stimuli and respondent self-placements will also be coded accordingly.}
}
\value{
An object of class "\code{amscale}". The function \code{summary} prints a summary of the model and
its results. An object of class "\code{amscale}" contain the following elements:\tabular{ll}{
\code{stimuli} \tab Double vector containing scaled stimuli estimates. \cr
\tab \cr
\code{respondent} \tab Dataframe of the same length as the input containing respondent intercepts, weights; and if resp was not NULL respondent ideal points. \cr
\tab \cr
\code{fit} \tab The adjusted fit statistic proposed in Aldrich and McKelvey's paper (1977) for the model. \cr
\tab \cr
\code{ninput} \tab The number of respondents in the initial input. \cr
\tab \cr
\code{nresp} \tab The number of respondents used for scaling the stimuli in the model after filtering for missing data. \cr
\tab \cr
\code{nstim} \tab The number of stimuli scaled in the model.
}
}
\description{
Performs Aldrich-McKlevey's (1977) scaling method for perceptual
data using the QR decomposition method (Swatton 2021) for computation.
The function will automatically filter missing data. Inspired by the
\code{aldmck} function from the \code{basicspace} R package (Poole et
al 2016).
}
\references{
\insertRef{aldrich1977}{psmisc}

\insertRef{poole2016}{psmisc}

\insertRef{swatton2021}{psmisc}
}
