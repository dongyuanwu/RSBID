##' A balanced dataset would be return by using Synthetic Minority Over-sampling
##' TEchnique (SMOTE) algorithm.
##'
##' The synthetic minority over-sampling technique artificially generates new samples
##' of the minority class using the nearest neighbours of these cases, in order to get a
##' more balanced dataset.
##'
##' @title Synthetic Minority Over-sampling TEchnique
##' @param data A dataset containing the predictors and the outcome. The predictors
##' can only be continuous (\code{numeric} or \code{integer}). The outcome must be binary.
##' @param outcome The column number or the name of the outcome variable in the dataset.
##' @param perc_maj The desired percentage of the size of majority samples that the
##' minority samples would be reached in the new dataset. The default is 100.
##' @param k The number of nearest neighbours that are used to generate the new samples
##' of the minority class. The default is 5.
##' @return A new dataset has been balanced.
##' @references Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002).
##' SMOTE: synthetic minority over-sampling technique. \emph{Journal of artificial
##' intelligence research}, 16, 321-357.
##' @import stats FNN
##' @export
##' @examples
##' data(abalone)
##' table(abalone$Class)
##'
##' newdata1 <- SMOTE(abalone[, -1], 'Class')
##' table(newdata1$Class)
##'
##' newdata2 <- SMOTE(abalone[, -1], 'Class', perc_maj=50)
##' table(newdata2$Class)



SMOTE <- function(data, outcome, perc_maj = 100, k = 5) {
    
    y <- data[, outcome]
    if (class(outcome) == "character") {
        y_coln <- outcome
        y_ind <- which(outcome == colnames(data))
    } else {
        y_coln <- colnames(data)[outcome]
        y_ind <- outcome
    }
    
    if (length(table(y)) != 2) {
        stop("Sorry, the outcome is not binary, I can't solve this problem :(")
    }
    if (class(y) != "factor" & class(y) != "character") {
        warning("The outcome is a binary variable, but not a factor or character.")
        y <- as.factor(y)
    }
    
    x_cl <- sapply(data[, -y_ind], class)
    if (all(x_cl == "numeric" | x_cl == "integer")) {
        message("All variables are continuous, SMOTE could be used.")
        
    } else if (all(x_cl == "character" | x_cl == "factor")) {
        stop("All variables are categorical, I can't solve this problem :(
             Maybe you can try to make one hot coding for each variable.")
        
    } else if (all(x_cl == "numeric" | x_cl == "integer" | x_cl == "character" | x_cl == "factor")) {
        stop("Variables are continous and categorical, please use SMOTE_NC function.")
        
    } else {
        stop("The types of variables need to be numeric or integer.
             Please check your dataset again.")
    }
    
    min_cl <- names(table(y))[which.min(table(y))]
    min_ind <- which(y == min_cl)
    maj_ind <- which(y != min_cl)
    
    x_min <- data[min_ind, -y_ind]
    x_coln <- colnames(x_min)
    knn_result <- get.knn(x_min, k = k)
    knn_ind <- knn_result$nn.index
    knn_dist <- knn_result$nn.dist
    
    syn_size <- get_syn_size(perc_maj, maj_len = length(maj_ind), min_len = length(min_ind))
    
    new_min <- NULL
    for (i in 1:nrow(x_min)) {
        replacement <- ifelse(syn_size[i] >= k, TRUE, FALSE)
        ind <- sample(knn_ind[i, ], syn_size[i], replace = replacement)
        temp <- runif(syn_size[i], 0, 1) * x_min[ind, ]
        temp <- apply(temp, 1, function(x) x + x_min[i, ])
        temp <- matrix(unlist(temp), ncol = ncol(x_min), byrow = TRUE)
        new_min <- rbind(new_min, temp)
    }
    
    new_min <- as.data.frame(new_min)
    colnames(new_min) <- x_coln
    new_min[, y_coln] <- min_cl
    new_min <- new_min[, colnames(data)]
    newdata <- rbind(data, new_min)
    
    return(newdata)
}
