% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DII_NHANES_PLUS_RESULT.R
\name{DII_NHANES_PLUS_RESULT}
\alias{DII_NHANES_PLUS_RESULT}
\title{DII_NHANES_PLUS_RESULT: Extract Precomputed DII Results for NHANES Cycles}
\usage{
DII_NHANES_PLUS_RESULT(
  NHANESCycle = c("0102", "0304", "0506", "0708", "0910", "1112", "1314", "1516", "1718",
    "1720"),
  DAY = c("first", "second", "both")
)
}
\arguments{
\item{NHANESCycle}{A character string indicating the NHANES cycle of interest (e.g., "0102", "0708").}

\item{DAY}{A character string indicating which day's result to retrieve ('first', 'second', 'both').}
}
\value{
A dataframe containing the DII and its component values for the specified NHANES cycle. DII represents the sum of all component values, while DII_NOETOH is the DII value excluding alcohol.
}
\description{
This function allows researchers to retrieve precomputed Dietary Inflammatory Index (DII) results for specified NHANES cycles from stored data. It provides a convenient way to access DII values without recalculating them, saving time and computational resources.
}
\examples{
# Retrieve DII values for NHANES 2009-2010 on Day 1
DII_DAY1_0910 <- DII_NHANES_PLUS_RESULT(NHANESCycle = "2009-2010", DAY = c('first'))
# Retrieve DII values for a NHANES 2015-2016 on Day 2
DII_DAY2_1112 <- DII_NHANES_PLUS_RESULT(NHANESCycle = "2011-2012", DAY = c('second'))
# Retrieve DII values for a NHANES 2007-2008 on both Day 1 and Day 2
DII_Both_0708 <- DII_NHANES_PLUS_RESULT(NHANESCycle = "2007-2008", DAY = c('both'))
}
