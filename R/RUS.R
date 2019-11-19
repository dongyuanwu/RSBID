##' A balanced dataset would be return by using random under-sampling (RUS) algorithm.
##'
##' The random under-sampling algorithm randomly chooses the majority samples without
##' replacement according to the sample size of minority class, in order to get a
##' more balanced dataset.
##'
##' @title Random Under-Sampling Algorithm
##' @param data A dataset containing the predictors and the outcome. The predictors
##' can be continuous (\code{numeric} or \code{integer}) or catigorical (\code{character}
##' or \code{factor}). The outcome must be binary.
##' @param outcome The column number or the name of the outcome variable in the dataset.
##' @param perc_min The desired percentage of the size of minority samples that the
##' majority samples would be reached in the new dataset. The default is 100.
##' @return A new dataset has been balanced.
##' @export
##' @examples
##' data(abalone)
##' table(abalone$Class)
##'
##' newdata1 <- RUS(abalone, 'Class')
##' table(newdata1$Class)
##'
##' newdata2 <- RUS(abalone, 'Class', perc_min=200)
##' table(newdata2$Class)

RUS <- function(data, outcome, perc_min = 100) {
    y <- data[, outcome]
    if (length(table(y)) != 2) {
        stop("Sorry, the outcome is not binary, I can't solve this problem :(")
    }
    if (class(y) != "factor" & class(y) != "character") {
        warning("The outcome is a binary variable, but not a factor or character.")
        y <- as.factor(y)
    }
    min_cl <- names(table(y))[which.min(table(y))]
    min_ind <- which(y == min_cl)
    maj_ind <- sample(which(y != min_cl), length(min_ind) * perc_min/100, replace = FALSE)
    newdata <- data[c(min_ind, maj_ind), ]
    return(newdata)
}
