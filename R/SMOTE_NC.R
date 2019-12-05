##' A balanced dataset would be return by using Synthetic Minority Over-sampling
##' TEchnique-Nominal Continuous (SMOTE-NC) algorithm.
##'
##' The synthetic minority over-sampling technique-nominal continuous artificially
##' generates new samples of the minority class using the nearest neighbours of
##' these cases, in order to get a more balanced dataset. This algorithm could
##' handle mixed datasets of continuous and nominal features, but it could not
##' handle datasets with all nominal features or all continuous features.
##'
##' @title Synthetic Minority Over-sampling TEchnique-Nominal Continuous
##' @param data A dataset containing the predictors and the outcome. The predictors
##' can be continuous (\code{numeric} or \code{integer}) or catigorical (\code{character}
##' or \code{factor}). There must be at least one continuous predictor and at least
##' one categorical predictor. The outcome must be binary.
##' @param outcome The column number or the name of the outcome variable in the dataset.
##' @param perc_maj The desired percentage of the size of majority samples that the
##' minority samples would be reached in the new dataset. The default is 100.
##' @param k The number of nearest neighbours that are used to generate the new samples
##' of the minority class. The default is 5.
##' @return A new dataset has been balanced.
##' @references Chawla, N. V., Bowyer, K. W., Hall, L. O., & Kegelmeyer, W. P. (2002).
##' SMOTE: synthetic minority over-sampling technique. \emph{Journal of artificial
##' intelligence research}, 16, 321-357.
##' @import stats
##' @export
##' @useDynLib RSBID
##' @importFrom Rcpp sourceCpp
##' @examples
##' data(abalone)
##' table(abalone$Class)
##'
##' newdata1 <- SMOTE_NC(abalone, 'Class')
##' table(newdata1$Class)
##'
##' newdata2 <- SMOTE_NC(abalone, 'Class', perc_maj=50)
##' table(newdata2$Class)


SMOTE_NC <- function(data, outcome, perc_maj = 100, k = 5) {
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
        warning("The outcome is not a factor or character.")
        y <- as.factor(y)
    }

    x_cl <- sapply(data[, -y_ind], class)
    if (all(x_cl == "numeric" | x_cl == "integer")) {
        stop("All variables are continuous, please use SMOTE function.")

    } else if (all(x_cl == "character" | x_cl == "factor")) {
        stop("All variables are categorical, I can't solve this problem :(
             Maybe you can try to make one hot coding for each variable.")

    } else if (all(x_cl == "numeric" | x_cl == "integer" | x_cl == "character" | x_cl == "factor")) {
        message("Variables are continous and categorical, SMOTE_NC could be used.")

    } else {
        stop("The types of variables need to be numeric, integer, character or factor.
             Please check your dataset again.")
    }


    min_cl <- names(table(y))[which.min(table(y))]
    min_ind <- which(y == min_cl)
    maj_ind <- which(y != min_cl)

    cont_posi <- which(x_cl == "numeric" | x_cl == "integer")
    cat_posi <- which(x_cl == "factor" | x_cl == "character")

    x_min <- data[min_ind, -y_ind]
    x_coln <- colnames(x_min)

    x_min_cont <- as.data.frame(x_min[, cont_posi])
    x_min_cat <- as.data.frame(x_min[, cat_posi])

    if (length(cont_posi) == 1) {
        sd_cont <- apply(x_min_cont, 2, sd)
    } else {
        sd_cont <- sd(x_min_cont[, 1])
    }
    med <- median(sd_cont)

    knn_ind <- NULL
    knn_dist <- NULL

    for (i in 1:nrow(x_min)) {
        ind <- (1:nrow(x_min))[-i]

        if (length(cont_posi) == 1) {
            dist_cont <- apply(as.array(x_min_cont[-i, ]), 1, function(x) get_dist(as.matrix(x, nrow=1), as.matrix(x_min_cont[i, ], nrow=1)))
        } else {
            dist_cont <- apply(x_min_cont[-i, ], 1, function(x) get_dist(as.matrix(x, nrow=1), as.matrix(x_min_cont[i, ], nrow=1)))
        }

        if (length(cat_posi) == 1) {
            diff_cat <- length(cat_posi) - apply(as.array(x_min_cat[-i, ]), 1, function(x) sum(x ==
                x_min_cat[i, ]))
        } else {
            diff_cat <- length(cat_posi) - apply(x_min_cat[-i, ], 1, function(x) sum(x == x_min_cat[i,
                ]))
        }

        dist_cat <- med^2 * diff_cat

        dist <- sqrt(dist_cont + dist_cat)
        dist_ord <- order(dist, decreasing = FALSE)

        knn_ind <- rbind(knn_ind, ind[dist_ord[1:k]])
        knn_dist <- rbind(knn_dist, dist[dist_ord[1:k]])
    }

    syn_size <- get_syn_size(perc_maj, maj_len = length(maj_ind), min_len = length(min_ind))

    new_min <- NULL
    for (i in 1:nrow(x_min)) {
        replacement <- ifelse(syn_size[i] >= k, TRUE, FALSE)
        ind <- sample(knn_ind[i, ], syn_size[i], replace = replacement)
        if (syn_size[i] == 0) next
        new_cont <- runif(syn_size[i], 0, 1) * x_min_cont[ind, ]
        if (length(cont_posi) == 1) {
            new_cont <- apply(as.array(new_cont), 1, function(x) x + x_min_cont[i, ])
        } else {
            new_cont <- apply(new_cont, 1, function(x) x + x_min_cont[i, ])
        }

        new_cont <- as.data.frame(matrix(unlist(new_cont), ncol = ncol(x_min_cont), byrow = TRUE))

        cat_knn <- as.data.frame(x_min_cat[knn_ind[i, ], ])
        new_cat <- NULL
        for (j in 1:syn_size[i]) {
            if (length(cat_posi) == 1) {
                new_cat <- rbind(new_cat, syn_cat(cat_knn[, 1]))
            } else {
                new_cat <- rbind(new_cat, apply(cat_knn, 2, syn_cat))
            }

        }

        new_contcat <- as.data.frame(cbind(new_cont, new_cat))
        colnames(new_contcat) <- x_coln[c(cont_posi, cat_posi)]
        new_contcat <- new_contcat[, x_coln]

        new_min <- rbind(new_min, new_contcat)
    }

    new_min[, y_coln] <- min_cl
    new_min <- new_min[, colnames(data)]
    newdata <- rbind(data, new_min)

    return(newdata)

}
