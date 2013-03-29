\name{load.scripts.and.data}
\alias{load.scripts.and.data}
\title{Load all script and data files from specified directories}
\usage{
  load.scripts.and.data(path,
    pattern = list(scripts = "*.R$", data = "*.rda$|*.Rdata$"),
    ignore.case = TRUE)
}
\arguments{
  \item{path}{A character vector of file paths.}

  \item{pattern}{A named list of patterns to match for
  loading scripts and data files. See "Notes".}

  \item{ignore.case}{Logical. Should letter case be
  considered when searching for data files and script
  files? Defaults to \code{FALSE}.}
}
\description{
  A convenience function to read all the data files and
  scripts from specified directories. In general, should
  only need to specify the directories. Specify directories
  without trailing slashes.
}
\note{
  The pre-defined pattern is \code{list(scripts = "*.R$",
  data = "*.rda$|*.Rdata$")}. This should match most
  conventionally used file extensions for R's native script
  and data files. Alternative patterns should be specified
  in the same form.
}
\examples{
\dontrun{
load.scripts.and.data(c("~/Dropbox/Public",
                 "~/Dropbox/Public/R Functions"))
}
}
\author{
  Ananda Mahto
}

