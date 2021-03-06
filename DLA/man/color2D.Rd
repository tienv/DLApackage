% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DisplayLabelArray.R
\name{color2D}
\alias{color2D}
\title{Plot 2d label matrix}
\usage{
color2D(x, cellcolors, border, xlab = "", ylab = "", ...)
}
\arguments{
\item{x}{label matrix to be displayed.}

\item{cellcolors}{color matrix corresponding to x.}

\item{border}{color for cell borders. If NA, not showing cell borders.}

\item{xlab}{Label of x axis.}

\item{ylab}{label of y axis.}

\item{...}{arguments passed to plot().}
}
\value{
Plot of x. No return value.
}
\description{
Display 2d label matrix, different labels will have different colors.
}

