

## Used in SMOTE-NC function
syn_cat <- function(cat_var) {

    freq <- table(cat_var)
    maxfreq_posi <- which(freq == max(freq))
    maj_cat <- names(freq)[maxfreq_posi]

    if (length(maj_cat) == 1) {
        return(maj_cat)
    } else {
        return(sample(maj_cat, 1))
    }

}

## Used in SMOTE and SMOTE-NC functions
get_syn_size <- function(perc_maj, maj_len, min_len) {
    syn_total_size <- round(perc_maj * maj_len/100 - min_len)
    syn_each_size <- round(syn_total_size/min_len)
    syn_size <- rep(syn_each_size, min_len)

    if ((syn_each_size * min_len) < syn_total_size) {

        syn_more_size <- syn_total_size - syn_each_size * min_len
        syn_more_ind <- sample(1:min_len, syn_more_size)
        syn_size[syn_more_ind] <- syn_size[syn_more_ind] + 1

    } else if ((syn_each_size * min_len) > syn_total_size) {

        syn_less_size <- syn_each_size * min_len - syn_total_size
        syn_less_ind <- sample(1:min_len, syn_less_size)
        syn_size[syn_less_ind] <- syn_size[syn_less_ind] - 1

    }

    return(syn_size)
}
