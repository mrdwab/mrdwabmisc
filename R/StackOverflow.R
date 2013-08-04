#' Print a list of question and answers from a Stack Overflow page
#' 
#' Print a list of question and answers from a Stack Overflow page (entered by
#' question ID number).
#' 
#' Currently hard-coded ot Stack Overflow, but may be generalized for other
#' Stack Exchange sites.
#' 
#' @param qid The numeric question ID.
#' @return A list of the class \code{c("QAList", "list")}
#' @note The formatting is handled by the \code{print.QAList} print method. The
#' HTML is still viewable by removing \code{"QAList"} from the list class, or
#' by accessing the individual list items.
#' @author Ananda Mahto
#' @examples
#' 
#' temp <- QAList(15332195)
#' temp
#' temp[[1]]
#' 
#' \dontshow{rm(temp)}
#' 
#' @export QAList
QAList <- function(qid) {
  T1 <- SOQuestionPage(qid)
  QTitle <- strsplit(T1[grepl("<h1 itemprop=\"name\">", T1)], ">|<")[[1]][5]
  AnswerIDs <- gsub(".*answer-([0-9]+).*", "\\1", 
                    T1[grepl("<div id=\"answer-", T1)])
  QTags <- T1[which(grepl("<div class=\"post-taglist\">", T1)) + 1]
  QTags <- paste(
    strsplit(QTags, "rel=\"tag\">|</a>")[[1]][c(FALSE, TRUE)], collapse = ", ")
  QAEnds <- which(grepl("<table class=\"fw\">", T1))
  Question <- data.frame(
    Start = which(
      grepl("<div class=\"post-text\" itemprop=\"description\">", T1)) + 1,
    End = QAEnds[1] - 6)
  Answers <- data.frame(
    Start = which(grepl("<td class=\"answercell\">", T1)) + 1,
    End = QAEnds[-1] - 2)
  ParsedPage <- apply(rbind(Question, Answers), 1, function(z) T1[z[1]:z[2]])
  ParsedPage <- lapply(ParsedPage, 
                       function(x) gsub("<div class=\"post-text\">", "", x))
  attr(ParsedPage, "QTitle") <- QTitle
  attr(ParsedPage, "QID") <- as.character(qid)
  attr(ParsedPage, "QTags") <- QTags
  attr(ParsedPage, "AnswerIDs") <- as.character(AnswerIDs)
  class(ParsedPage) <- c("QAList", class(ParsedPage))
  ParsedPage
}













#' Return a list of the code blocks at a Stack Overflow question page
#' 
#' Return a list of the code blocks at a Stack Overflow question page
#' (specified by numeric question ID).
#' 
#' Currently hard-coded ot Stack Overflow, but may be generalized for other
#' Stack Exchange sites.
#' 
#' @param qid The numeric question ID
#' @return A list of the class c("SOCodeBlocks", "list")
#' @note The formatting is handled by the \code{print.SOCodeBlocks} print
#' method. The HTML is still viewable by removing "SOCodeBlocks" from the list
#' class, or by accessing the individual list items.
#' @author Ananda Mahto
#' @examples
#' 
#' temp <- SOCodeBlocks(15332195)
#' temp
#' temp[[6]]
#' 
#' \dontshow{rm(temp)}
#' 
#' @export SOCodeBlocks
SOCodeBlocks <- function(qid) {
  T1 <- QAList(qid)
  T2 <- unlist(lapply(T1, CodeList), recursive=FALSE, use.names=FALSE)
  class(T2) <- c("SOCodeBlocks", class(T2))
  T2
} 

