#' Latex Linear Model
#'
#' This function accepts a linear model as an input
#' and cats a Latex string. You can then paste this into
#' a Latex or RMarkdown file.
#' @param model The model to print as Latex.
#' @keywords model latex
#' @export
#' @examples
#' latexLm()

latexLm <- function(model) {
  # Extract the names of terms and their estimates
  int <- model$coefficients[1]
  response <- colnames(model$model[1])
  terms <- colnames(model$model[-1])
  values <- model$coefficients[-1]
  
  # Create vector to hold strings
  strs <- c(1:length(terms))
  
  # Insert values, term names and plus signs
  for (i in 1:length(strs)) {
    ifelse(i == length(strs),
           strs[i] <- paste(format(values[i], digits = 3), terms[i]),
           strs[i] <- paste(format(values[i], digits = 3), terms[i], "+"))
  }
  
  # Start to build the final string
  finalString <- paste('\\widehat{',response,'}=',int,'+')
  
  # Add remaining strings
  for (j in 1:length(strs)) {
    finalString <- paste(finalString, strs[j])
  }
  
  # Cat for now, not sure best way to output
  cat(finalString)
}