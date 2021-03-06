% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Time_long.R
\name{Time_long}
\alias{Time_long}
\title{Timne Metrics for Whole Dataset}
\usage{
Time_long(count.data, weartime, thresh, smallerthan = TRUE,
  bout.length = 1)
}
\arguments{
\item{count.data}{\code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
The first two columns have to be ID and Day.}

\item{weartime}{\code{data.frame} with dimension of \code{count.data}.
The first two columns have to be ID and Day.}

\item{thresh}{threshold to binarize the data.}

\item{smallerthan}{Find a state that is smaller than a threshold, or greater than or equal to.}

\item{bout.length}{minimum duration of defining an active bout; defaults to 1.}
}
\value{
A dataframe with some of the following columns
\item{ID}{identifier of the person}
\item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
\item{time}{time of certain state}
}
\description{
This function is a whole dataset wrapper for \code{Time}
}
\examples{
data(example_activity_data)
count = example_activity_data$count
wear = example_activity_data$wear
sed_all = Time_long(count.data = count,weartime = wear,
thresh = 100,bout.length = 1,smallerthan = TRUE)


}
