\name{licor2matrix}
\alias{licor2matrix}
\title{Convert licor data into matrix of zero and one}
\usage{
  licor2matrix(data = NULL)
}
\arguments{
  \item{data}{the output from the read.licor function}
}
\value{
  list filename and data if succesful; NA if otherwise
}
\description{
  Assumes a bp weight value or -1 and -9; the latter will
  be replaced by 0; the weight by 1. The column will be
  renamed with the bp weight. The function also creates a
  file of the transformed data in .csv format within the
  the same path. The file name is appended with '_out' and
  the extension set to '.csv'.
}
\author{
  Reinhard Simon
}

