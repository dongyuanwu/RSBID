##' A balanced dataset would be return by using under-sampling based on clustering
##' (SBC) algorithm.
##'
##' The under-sampling based on clustering algorithm clusters all samples into
##' \code{k} clusters. Then it randomly selects the majority samples by considering
##' the ratio of the number of majority samples to the number of minority samples
##' in the cluster.
##'
##' If we need to sample more majority samples than what is available in the cluster,
##' the sampling with replacement would be used. Otherwise, the sampling without
##' replacement would be used.
##'
##' For the dataset with predictors that are all continuous (\code{numeric} or
##' \code{integer}), \code{k-means} would be used to cluster. For the dataset with
##' predictors that are all categorical (\code{character} or \code{factor}),
##' \code{k-modes} would be used. For the dataset with predictors are continuous or
##' categorical, \code{k-prototypes} would be used.
##'
##' @title Under-Sampling Based on Clustering Algorithm
##' @param data A dataset containing the predictors and the outcome. The predictors
##' can be continuous (\code{numeric} or \code{integer}) or catigorical (\code{character}
##' or \code{factor}). The outcome must be binary.
##' @param outcome The column number or the name of the outcome variable in the dataset.
##' @param perc_min The desired percentage of the size of minority samples that the
##' majority samples would be reached in the new dataset. The default is 100.
##' @param k The number of clusters for the clustering algorithm. The default is 3.
##' @param iter_max The maximum number of iterations of the clustering algorithm.
##' The default is 100.
##' @param nstart The initial number of random sets would be chosen. Only would be
##' used for \code{k-means} and \code{k-prototypes}. The default is 1.
##' @param ... Not used.
##' @return A new dataset has been balanced.
##' @references Yen, S. J., & Lee, Y. S. (2009). Cluster-based under-sampling
##' approaches for imbalanced data distributions. \emph{Expert Systems with Applications},
##' 36(3), 5718-5727.
##' @references Hartigan, J. A., & Wong, M. A. (1979). Algorithm AS 136: A k-means
##' clustering algorithm. \emph{Journal of the Royal Statistical Society. Series C
##' (Applied Statistics)}, 28(1), 100-108.
##' @references Huang, Z. (1997). A fast clustering algorithm to cluster very large
##' categorical data sets in data mining. \emph{DMKD}, 3(8), 34-39.
##' @references Huang, Z. (1998). Extensions to the k-means algorithm for clustering
##' large data sets with categorical values. \emph{Data Mining and Knowledge Discovery},
##' 2(3), 283-304.
##' @import stats clustMixType klaR
##' @export
##' @examples
##' data(abalone)
##' data(bank)
##' table(abalone$Class)
##' table(bank$deposit)
##'
##'# predictors are continuous or categorical
##' newdata1 <- SBC(bank, 'deposit')
##' table(newdata1$deposit)
##'
##' newdata2 <- SBC(bank, 'deposit', perc_min=200)
##' table(newdata2$deposit)
##'
##' # predictors are all continuous
##' newdata3 <- SBC(abalone, 'Class')
##' table(newdata3$Class)
##'
##' # predictors are all categorical
##' bank1 <- bank[, c(2, 3, 5, 11)]
##' newdata4 <- SBC(bank1, 'deposit')
##' table(newdata4$deposit)



SBC <- function(data, outcome, perc_min = 100, k = 3, iter_max = 100, nstart = 1, ...) {
    datnrow <- nrow(data)
    if (nrow(na.omit(data)) < datnrow) {
        stop("Sorry, this dataset has missing value :(")
    }
    if (is.character(outcome)) {
        if (!(outcome %in% colnames(data))) {
            stop(paste("This dataset doesn't have a variable names", outcome))
        } else {
            y_coln <- outcome
            y_ind <- which(outcome == colnames(data))
        }
    } else {
        if (outcome < 1 | outcome > ncol(data)) {
            stop(paste("This dataset doesn't have a variable whose column number is", outcome))
        } else {
            y_coln <- colnames(data)[outcome]
            y_ind <- outcome
        }
    }
    y <- data[, outcome]

    if (length(table(y)) != 2) {
        stop("Sorry, the outcome is not binary, I can't solve this problem :(")
    }
    if (table(y)[1] == table(y)[2]) {
        stop("Sorry, this dataset has been balanced and there is nothing I can do.")
    }
    if (!inherits(y, "character") & !inherits(y, "factor")) {
        warning("The outcome is a binary variable, but not a factor or character.")
        y <- as.factor(y)
    }

    x_cl <- sapply(data[, -y_ind], class)
    if (all(x_cl == "numeric" | x_cl == "integer")) {
        message("All variables are continuous, k-means would be used to cluster.")
        cl_result <- kmeans(data[, -y_ind], centers = k, iter.max = iter_max, nstart = nstart)$cluster
    } else if (all(x_cl == "character" | x_cl == "factor")) {
        message("All variables are categorical, k-modes would be used to cluster.")
        cl_result <- kmodes(data[, -y_ind], modes = k, iter.max = iter_max)$cluster
    } else if (all(x_cl == "numeric" | x_cl == "integer" | x_cl == "character" | x_cl == "factor")) {
        message("Variables are continous and categorical, k-prototypes would be used to cluster.")
        cl_result <- kproto(data[, -y_ind], k = k, iter.max = iter_max, nstart = nstart, verbose = FALSE)$cluster
    } else {
        stop("The types of variables need to be numeric, integer, character or factor.
             Please check your dataset again.")
    }

    min_cl <- names(table(y))[which.min(table(y))]
    min_ind <- which(y == min_cl)
    maj_ind_orig <- which(y != min_cl)
    cl_result <- as.factor(cl_result)
    maj_cluster <- cl_result[maj_ind_orig]
    min_cluster <- cl_result[min_ind]

    ratios <- table(maj_cluster)/table(min_cluster)
    ratios[ratios == Inf] <- table(maj_cluster)[ratios == Inf]

    m <- perc_min/100
    ssize_percl <- round((m * length(min_ind)) * (ratios/sum(ratios)))


    maj_ind_new <- vector(mode = "list", length = k)
    for (i in 1:k) {
        if (ssize_percl[i] > sum(maj_cluster == i)) {
            replacement <- TRUE
        } else replacement <- FALSE
        maj_ind_new[[i]] <- sample(maj_ind_orig[maj_cluster == i], ssize_percl[i], replace = replacement)
    }

    if (sum(ssize_percl) < (m * length(min_ind))) {
        more_maj_num <- m * length(min_ind) - sum(ssize_percl)
        more_maj_ind <- sample(maj_ind_orig, more_maj_num, replace = FALSE)
        maj_ind <- c(unlist(maj_ind_new), more_maj_ind)
    } else {
        maj_ind <- sample(unlist(maj_ind_new), m * length(min_ind), replace = FALSE)
    }


    newdata <- data[c(min_ind, maj_ind), ]
    return(newdata)
}
