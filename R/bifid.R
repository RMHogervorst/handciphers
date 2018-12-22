#' create a matrix for bifid handcipher
#'
#' @param key a sentance or word that you use as a key
#' @export
#' @examples
#' bifid_matrix("averysecretkey")
bifid_matrix <- function(key){
    key_ <- deduplicate_letters_from_word(key)
    rest <- add_rest_of_alphabet_in_order(key_)
    fill_matrix <- union(key_,rest)
    fill_matrix <- fill_matrix[fill_matrix!="j"] # remove j
    matrix(fill_matrix, nrow = 5, ncol=5, byrow = TRUE)
}

# deplicate letters from a word
deduplicate_letters_from_word <- function(word){
    unique((strsplit(word,split = "")[[1]]))
}

add_rest_of_alphabet_in_order <- function(charvec){
    letters[!letters %in% charvec]
}

#' with a matrix return the encrypted word.
#'
#' @export
#' @examples
#' encrypt_with_bifid(bifid_matrix,"puppersaresmalldoggos")
#'
encrypt_with_bifid <- function(bifidmatrix, phrase){
    phrase_ <- strsplit(phrase,split = "")[[1]]
    phrase_[phrase_ == "j"] <- "i"
    columns <- purrr::map_int(phrase_,~which(bifidmatrix==.x,arr.ind = TRUE)[2])
    rows <- purrr::map_int(phrase_,~which(bifidmatrix==.x,arr.ind = TRUE)[1])
    # index
    ids <- c(rows,columns)
    start_ <- seq.int(1,length(ids)-1, 2)
    end_ <- seq.int(2, length(ids),2)
    # So now we have the row and columns
    temp <- purrr::map2(start_, end_, ~ids[c(.x,.y)])
    rows_new <- purrr::map_int(temp, 1)
    columns_new <- purrr::map_int(temp, 2)
    # letters from the matrix
    paste0(purrr::map2_chr(rows_new, columns_new, ~bifidmatrix[.x,.y]),collapse = "")
}
