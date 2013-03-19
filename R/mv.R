#'Rename an object in the workspace
#'
#'Renames an object in the workspace, "removing" the orinal object. This does
#'so without creating a copy of the original object. If an object in the
#'workspace currently exists with the new name specified, the function prompts
#'the user to verify that they want to overwrite that object before proceeding.
#'
#'
#'@param currentName The current name of the object
#'@param newName The new name for the object
#'@author Rolf Turner
#'@references A good amount of discussion on when R makes a copy in memory in
#'this discussion thread:
#'\url{https://stat.ethz.ch/pipermail/r-help/2008-March/156028.html}.
#'@examples
#'
#'x <- runif(1e7)
#'ls()
#'x.add <- tracemem(x)
#'mv(x, y)
#'identical(x.add, tracemem(y))
#'ls()
#'\dontshow{rm(x.add, y)}
#'
mv <- function (currentName, newName) {
  cnm <- deparse(substitute(currentName))
  nnm <- deparse(substitute(newName))
  if (!exists(cnm, where = 1, inherits = FALSE))
    stop(paste(cnm, "does not exist.\n"))
  if (exists(nnm, where = 1,inherits = FALSE)) {
    ans = readline(paste("Overwrite ", nnm, "? (y/n) ", sep =  ""))
    if (ans != "y")
      return(invisible())
  }
  assign(nnm, currentName, pos = 1)
  rm(list = cnm, pos = 1)
  invisible()
}
