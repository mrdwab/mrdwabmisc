#'Linearize (un-nest) nested lists
#'
#'Implements a recursive algorithm to linearize nested \code{list}s upto any
#'arbitrary level of nesting (limited by R's allowance for recursion-depth). By
#'linearization, it is meant to bring all list branches emanating from any
#'nth-nested trunk upto the top-level trunk such that the return value is a
#'simple non-nested list having all branches emanating from this top-level
#'branch.
#'
#'Since \code{data.frame}s are essentially \code{list}s a boolean option is
#'provided to switch on/off the linearization of \code{data.frame}s. This has
#'been found desirable in the author's experience.
#'
#'Also, one would typically want to preserve names in the lists in a way as to
#'clearly denote the association of any list element to its nth-level history.
#'As such we provide a clean and simple method of preserving names information
#'of list elements. The names at any level of nesting are appended to the names
#'of all preceding trunks using the \code{NameSep} option string as the
#'seperator.  The default "/" has been chosen to mimic the unix tradition of
#'filesystem hierarchies. The default behavior works with existing names at any
#'n-th level trunk, if found; otherwise, coerces simple numeric names
#'corresponding to the position of a list element on the nth-trunk. Note,
#'however, that this naming pattern does not ensure unique names for all
#'elements in the resulting list. If the nested lists had non-unique names in a
#'trunk the same would be reflected in the final list. Also, note that the
#'function does not at all handle cases where \emph{some} names are missing and
#'some are not.
#'
#'Clearly, preserving the n-level hierarchy of branches in the element names
#'may lead to names that are too long.  Often, only the depth of a list element
#'may only be important. To deal with this possibility a boolean option called
#'\code{ForceNames} has been provided.  \code{ForceNames} shall drop all
#'original names in the lists and coerce simple numeric names which simply
#'indicate the position of an element at the nth-level trunk as well as all
#'preceding trunk numbers.
#'
#'@param NList The input \code{list}
#'@param LinearizeDataFrames Logical. Should columns in \code{data.frame}s in
#'the list be "linearized" as vectors? Defaults to \code{FALSE}.
#'@param NameSep Character to be used when creating names.  Defaults to "/" to
#'mimic directory listings.
#'@param ForceNames Logical. Should the present names be discarded and new
#'simplified names be created? Defaults to \code{FALSE}
#'@author Akhil S Bhel
#'@seealso \code{\link{unlist}}
#'@references
#'\url{https://sites.google.com/site/akhilsbehl/geekspace/articles/r/linearize_nested_lists_in_r}
#'@examples
#'
#'# Create some sample data
#'NList <- list(a = "a", # Atom
#'       b = 1:5, # Vector
#'       c = data.frame(x = runif(5), y = runif(5)), # Add a data.frame
#'       d = matrix(runif(4), nrow = 2), # Throw in a matrix for good measure
#'       e = list(l = list("a", "b"), # Introduce nesting
#'                m = list(1:5, 5:10),
#'                n = list(list(1), list(2)))) # More nesting
#'
#'LinearizeNestedList(NList)
#'LinearizeNestedList(NList, LinearizeDataFrames = TRUE)
#'LinearizeNestedList(NList, ForceNames = TRUE)
#'LinearizeNestedList(NList, ForceNames = TRUE, NameSep = "_")
#'\dontshow{rm(NList)}
#'
LinearizeNestedList <- function(NList, LinearizeDataFrames=FALSE,
                                NameSep="/", ForceNames=FALSE) {
    # Sanity checks:
    stopifnot(is.character(NameSep), length(NameSep) == 1)
    stopifnot(is.logical(LinearizeDataFrames), length(LinearizeDataFrames) == 1)
    stopifnot(is.logical(ForceNames), length(ForceNames) == 1)
    if (! is.list(NList)) return(NList)
    #
    # If no names on the top-level list coerce names. Recursion shall handle
    # naming at all levels.
    if (is.null(names(NList)) | ForceNames == TRUE)
        names(NList) <- as.character(1:length(NList))
    #
    # If simply a dataframe deal promptly.
    if (is.data.frame(NList) & LinearizeDataFrames == FALSE)
        return(NList)
    if (is.data.frame(NList) & LinearizeDataFrames == TRUE)
        return(as.list(NList))
    #
    # Book-keeping code to employ a while loop.
    A <- 1
    B <- length(NList)
    #
    # We use a while loop to deal with the fact that the length of the nested
    # list grows dynamically in the process of linearization.
    while (A <= B) {
        Element <- NList[[A]]
        EName <- names(NList)[A]
        if (is.list(Element)) {
            #
            # Before and After to keep track of the status of the top-level trunk
            # below and above the current element.
            if (A == 1) {
                Before <- NULL
            } else {
                Before <- NList[1:(A - 1)]
            }
            if (A == B) {
                After <- NULL
            } else {
                After <- NList[(A + 1):B]
            }
            #
            # Treat dataframes specially.
            if (is.data.frame(Element)) {
                if (LinearizeDataFrames == TRUE) {
                    #
                    # `Jump` takes care of how much the list shall grow in this step.
                    Jump <- length(Element)
                    NList[[A]] <- NULL
                    #
                    # Generate or coerce names as need be.
                    if (is.null(names(Element)) | ForceNames == TRUE)
                        names(Element) <- as.character(1:length(Element))
                    #
                    # Just throw back as list since dataframes have no nesting.
                    Element <- as.list(Element)
                    #
                    # Update names
                    names(Element) <- paste(EName, names(Element), sep=NameSep)
                    #
                    # Plug the branch back into the top-level trunk.
                    NList <- c(Before, Element, After)
                }
                Jump <- 1
            } else {
                NList[[A]] <- NULL
                #
                # Go recursive! :)
                if (is.null(names(Element)) | ForceNames == TRUE)
                    names(Element) <- as.character(1:length(Element))
                Element <- LinearizeNestedList(Element, LinearizeDataFrames,
                                               NameSep, ForceNames)
                names(Element) <- paste(EName, names(Element), sep=NameSep)
                Jump <- length(Element)
                NList <- c(Before, Element, After)
            }
        } else {
            Jump <- 1
        }
        #
        # Update book-keeping variables.
        A <- A + Jump
        B <- length(NList)
    }
    return(NList)
}
