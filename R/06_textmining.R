#' tidy books
#'
#' @param books get from gutenberg_download
#'
#' @return tibble result
#' @import dplyr
#' @import tidytext
#' @export
#'
#' @examples
#' book_tidy()
book_tidy  <- function(books) {
  library(tidytext)
  data("stop_words")
  res <- books %>% unnest_tokens(word,text) %>% anti_join(stop_words)
  return(res)
  
}
#' count the word with order
#'
#' @param tidy_books books tidied 
#' @param sort  defalut TRUE
#'
#' @return tibble
#' @import dplyr
#' @export
#'
#' @examples
#' word_count_dity()
word_count_tidy <- function(tidy_books,sort=TRUE){
  res <- tidy_books %>% count(word,sort = sort)
  return(res)
}

#' plot the word count
#'
#' @param word_count_tidy word tidyed
#' @param miss_count  defalut 100
#'
#' @return return valued with tibble
#' @import ggplot2
#' @import dplyr
#' @export
#'
#' @examples
#' word_count_plot
word_count_plot <- function(word_count_tidy,miss_count=100,xlab=NULL) {
  res <- word_count_tidy %>% filter(n>miss_count) %>%
         mutate(word=reorder(word,n)) 
  res %>% ggplot(aes(word,n)) + geom_col() + xlab(xlab) + coord_flip()
  
}

#' plot the wordcount with wordcloud plot
#'
#' @param word_count_tidy wordcount
#' @param max_words  show the maximun words
#'
#' @return plot
#' @import ggplot2
#' @import wordcloud
#' @import dplyr
#' @export
#'
#' @examples
#' word_count_plot_cloud()
word_count_plot_cloud <- function(word_count_tidy,max_words=100){
  word_count_tidy  %>% with(wordcloud(word,n,max.words = max_words))
}


#' split the word into ngram
#'
#' @param books books contain text col
#' @param ncount count of the word
#'
#' @return tribble
#' @import dplyr
#' @import tidytext
#' @export
#'
#' @examples
#' word_ngram()
word_ngram <- function(books,ncount=3){
  res <- books %>% unnest_tokens(ngrams,text,token = "ngrams",n=ncount)
  return(res)
}

#' calcal the ngrame count
#'
#' @param word_ngram word dealed with ngrame
#' @param sort default true
#'
#' @return tibble
#' @import dplyr
#' @export
#'
#' @examples
#' ngram_count()
ngram_count <- function(word_ngram,sort=TRUE){
  res <- word_ngram %>% count(ngrams,sort=sort)
  return(res)
  
}



