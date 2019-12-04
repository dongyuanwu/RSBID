##' A balanced dataset would be return by using random over-sampling (ROS) algorithm.
##'
##' The random over-sampling algorithm generates new samples by randomly sampling
##' the minority samples with replacement according to the sample size of majority
##' class, in order to get a more balanced dataset.
##'
##' @title Random Over-Sampling Algorithm
##' @param data A dataset containing the predictors and the outcome. The predictors
##' can be continuous (\code{numeric} or \code{integer}) or catigorical (\code{character}
##' or \code{factor}). The outcome must be binary.
##' @param outcome The column number or the name of the outcome variable in the dataset.
##' @param perc_maj The desired percentage of the size of majority samples that the
##' minority samples would be reached in the new dataset. The default is 100.
##' @return A new dataset has been balanced.
##' @export
##' @examples
##' data(abalone)
##' table(abalone$Class)
##'
##' newdata1 <- ROS(abalone, 'Class')
##' table(newdata1$Class)
##'
##' newdata2 <- ROS(abalone, 'Class', perc_maj=50)
##' table(newdata2$Class)

ROS <- function(data, outcome, perc_maj = 100) {
    if (is.character(outcome)) {
        if (!(outcome %in% colnames(data))) {
            stop(paste("This dataset doesn't have a variable names", outcome))
        }
    } else {
        if (outcome < 1 | outcome > ncol(data)) {
            stop(paste("This dataset doesn't have a variable whose column number is", outcome))
        }
    }
    y <- data[, outcome]
    if (length(table(y)) != 2) {
        stop("Sorry, the outcome is not binary, I can't solve this problem :(")
    }
    if (class(y) != "factor" & class(y) != "character") {
        warning("The outcome is a binary variable, but not a factor or character.")
        y <- as.factor(y)
    }
    maj_cl <- names(table(y))[which.max(table(y))]
    maj_ind <- which(y == maj_cl)
    min_ind_ori <- which(y != maj_cl)
    min_ind <- c(min_ind_ori, sample(min_ind_ori, length(maj_ind) * perc_maj/100 - length(min_ind_ori),
        replace = TRUE))
    newdata <- data[c(min_ind, maj_ind), ]
    return(newdata)
}
