\name{licor2matrix}
\alias{licor2matrix}
\title{Convert licor data into matrix of zero and one}
\usage{
  licor2matrix(filename = NULL, write = T)
}
\arguments{
  \item{filename}{an optional filename given the full path
  or relative path. If not given the user is asked.}

  \item{write}{logical shall an output file automatically
  be written? Default is TRUE.}
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

