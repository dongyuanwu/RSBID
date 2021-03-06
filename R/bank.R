#' Imbalanced binary bank dataset
#'
#' The data is related with direct marketing campaigns of a Portuguese banking institution. The marketing campaigns were based on phone calls. Often, more than one contact to the same client was required, in order to access if the product (bank term deposit) would be ('yes') or not ('no') subscribed.
#'
#' @format A data frame with 877 rows and 11 variables:
#' \describe{
#'   \item{age}{age of clients}
#'   \item{marital}{marital status (categorical: 'divorced','married','single'; note: 'divorced' means divorced or widowed)}
#'   \item{education}{education (categorical: 'primary,'secondary','tertiary','unknown')}
#'   \item{balance}{balance of clients' bank accounts (numeric)}
#'   \item{housing}{has housing loan? (categorical: 'no','yes')}
#'   \item{day}{last contact of the month (numeric)}
#'   \item{duration}{last contact duration, in seconds (numeric)}
#'   \item{campaign}{number of contacts performed during this campaign and for this client (numeric, includes last contact)}
#'   \item{pdays}{number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)}
#'   \item{previous}{number of contacts performed before this campaign and for this client (numeric)}
#'   \item{deposit}{has the client subscribed a term deposit? (binary: 'yes','no')}
#'   ...
#' }
#' @source \url{UCI}{https://archive.ics.uci.edu/ml/datasets/Bank+Marketing}

"bank"
