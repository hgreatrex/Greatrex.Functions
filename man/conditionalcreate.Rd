% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conditionalcreate.r
\name{conditionalcreate}
\alias{conditionalcreate}
\title{conditionalcreate}
\usage{
conditionalcreate(dir, silent = TRUE)
}
\arguments{
\item{dir}{The location and name of the directory you want to create  (e.g. "~/Desktop/test")}

\item{silent}{Do you want to print progress messages? By default this function is silent}
}
\value{
Nothing, it just makes the directory
}
\description{
Create a directory if it doesn't already exist
}
\details{
This is just a shortcut function to silently make a directory if it doesn't exist and to have better error messages.
}
\examples{
# Or change to a folder on your computer
conditionalcreate("~/Desktop/TestFolder")
conditionalcreate("~/Desktop/TestFolder", silent=FALSE)
}
