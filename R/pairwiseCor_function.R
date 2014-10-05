#' Pairwise Correlation
#'
#' This function accepts a dataframe as input and returns
#' a dataframe with the pairwise absolute correlation and
#' regular correlation. Make sure to give it numeric input.
#' @param dataFrame The dataframe you want correlations from.
#' @keywords correlation pairwise tidy
#' @export
#' @examples
#' pairwiseCor()

pairwiseCor <- function(dataFrame) {
  pairs <- combn(names(dataFrame), 2, simplify = FALSE)
  df <- data.frame(var1 = rep(0, length(pairs)), var2 = rep(0, length(pairs)),
                   abscor = rep(0, length(pairs)), cor = rep(0, length(pairs)))
  for(i in 1:length(pairs)) {
    df[i,1] <- pairs[[i]][1]
    df[i,2] <- pairs[[i]][2]
    df[i,3] <- abs(cor(dataFrame[,pairs[[i]][1]], dataFrame[,pairs[[i]][2]]))
    df[i,4] <- cor(dataFrame[,pairs[[i]][1]], dataFrame[,pairs[[i]][2]])
  }
  df
}