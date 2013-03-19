#'Load all script and data files from specified directories
#'
#'A convenience function to read all the data files and scripts from specified
#'directories. In general, should only need to specify the directories. Specify
#'directories without trailing slashes.
#'
#'
#'@param path A character vector of file paths.
#'@param pattern A named list of patterns to match for loading scripts and data
#'files. See "Notes".
#'@param ignore.case Logical. Should letter case be considered when searching
#'for data files and script files? Defaults to \code{FALSE}.
#'@note The pre-defined pattern is \code{list(scripts = "*.R$", data =
#'"*.rda$|*.Rdata$")}. This should match most conventionally used file
#'extensions for R's native script and data files. Alternative patterns should
#'be specified in the same form.
#'@author Ananda Mahto
#'@examples
#'
#'\dontrun{
#'load.scripts.and.data(c("~/Dropbox/Public",
#'                    "~/Dropbox/Public/R Functions"))
#'}
#'
load.scripts.and.data <- function(path, pattern = list(scripts = "*.R$",
                          data = "*.rda$|*.Rdata$"), ignore.case=TRUE) {

  file.sources = list.files(path, pattern=pattern$scripts,
                            full.names=TRUE, ignore.case=ignore.case)
  data.sources = list.files(path, pattern=pattern$data,
                            full.names=TRUE, ignore.case=ignore.case)
  sapply(data.sources, load, .GlobalEnv)
  sapply(file.sources, source, .GlobalEnv)
}
