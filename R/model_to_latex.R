#' Make Bold
#'
#' Makes some text bold in Latex.
#' @param x text to be made bold
#' @keywords latex
#' @export
#' @examples
#' makeBold()

makeBold <- function(x) {
  return(paste('\\textbf{', x, '}', sep=''))
}

#' Get Stars
#'
#' Returns the correct number of stars for the p-value.
#' @param pValue the p-value
#' @keywords latex
#' @export
#' @examples
#' getStars()

getStars <- function(pValue) {
  stars <- '~~~'
  if (pValue < 0.05) {stars <- '*~~'}
  if (pValue < 0.01) {stars <- '**~'}
  if (pValue < 0.001) {stars <- '***'}
  return(stars)
}

#' Model to Latex
#'
#' Takes a model and prints a pretty Latex table of the model. (Doesn't return anything.)
#' @param model model
#' @param roundN number of digits to round to. Defaults to 3
#' @param boldSig make significant factors bold? Defaults to TRUE.
#' @param usePrettyNames Boolean. Instead of your variable names, use pretty names for each of the factors instead? Defaults to FALSE
#' @param prettyNames if usePrettyNames==TRUE, vector of names to use for variables
#' @param modelType type of model. Defaults to lmer, choices are 'lmer', 'glm', or 'gam'
#' @keywords latex
#' @export
#' @examples
#' modelToLatex()

modelToLatex <- function(model, roundN=3, boldSig=TRUE, usePrettyNames=FALSE, prettyNames='', modelType='lmer') {
  header <- c('\\begin{table}[hbt]','\\begin{center}','\\begin{tabular}{l c c c c}',
              '\\hline', 'Term & Estimate & Std Error & t value & p value \\\\', '\\hline')

  ## Get a data frame of the model
  if (modelType=='lmer') {
    values <- as.data.frame(summary(model)$coefficients)
    modelTerms <- c()
    print(values)
    for (i in 1:nrow(values)) {
      term <- gsub('_', '-', rownames(values)[i][1])
      if (usePrettyNames == TRUE) {term <- gsub('_', '-', prettyNames[i])}
      estimate <- round(values[i,1][1], roundN)
      stdError <- round(values[i,2][1], roundN)
      tValue <- round(values[i,4][1], roundN)
      pValue <- round(values[i,5][1], roundN)
      stars <- '~~~'
      if (pValue < 0.05) {stars <- '*~~'}
      if (pValue < 0.01) {stars <- '**~'}
      if (pValue < 0.001) {stars <- '***'}
      tableRow <- c(term, estimate, stdError, tValue, paste(pValue, stars))
      if ((boldSig == TRUE) & (pValue < 0.05)) {tableRow <- sapply(tableRow, makeBold)}
      if (pValue == 0) {tableRow[5] <- makeBold('$<$ 0.001 ***')}
      modelTerms <- c(modelTerms, paste(tableRow[1], tableRow[2], tableRow[3], tableRow[4], tableRow[5], sep=" & "), "\\\\")
    }
  }

  if (modelType=='glm') {
    values <- as.data.frame(summary(model)$coefficients)
    modelTerms <- c()
    print(values)
    for (i in 1:nrow(values)) {
      term <- gsub('_', '-', rownames(values)[i][1])
      if (usePrettyNames == TRUE) {term <- gsub('_', '-', prettyNames[i])}
      estimate <- round(values[i,1][1], roundN)
      stdError <- round(values[i,2][1], roundN)
      tValue <- round(values[i,3][1], roundN)
      pValue <- round(values[i,4][1], roundN)
      stars <- '~~~'
      if (pValue < 0.05) {stars <- '*~~'}
      if (pValue < 0.01) {stars <- '**~'}
      if (pValue < 0.001) {stars <- '***'}
      tableRow <- c(term, estimate, stdError, tValue, paste(pValue, stars))
      if ((boldSig == TRUE) & (pValue < 0.05)) {tableRow <- sapply(tableRow, makeBold)}
      if (pValue == 0) {tableRow[5] <- makeBold('$<$ 0.001 ***')}
      modelTerms <- c(modelTerms, paste(tableRow[1], tableRow[2], tableRow[3], tableRow[4], tableRow[5], sep=" & "), "\\\\")
    }
  }

  else if (modelType == 'gam') {
    coeffs <- as.data.frame(summary(model)$p.coeff)
    stdErrors <- as.data.frame(summary(model)$se)
    tValues <- as.data.frame(summary(model)$p.t)
    pValues <- as.data.frame(summary(model)$p.pv)
    for (i in 1:nrow(coeffs)) {
      term <- gsub('_', '-', rownames(coeffs)[i][1])
      if (usePrettyNames == TRUE) {term <- gsub('_', '-', prettyNames[i])}
      estimate <- round(coeffs[i,1][1], roundN)
      stdError <- round(stdErrors[i,1][1], roundN)
      tValue <- round(tValues[i,1][1], roundN)
      pValue <- round(pValues[i,1][1], roundN)
      stars <- '~~~'
      if (pValue < 0.05) {stars <- '*~~'}
      if (pValue < 0.01) {stars <- '**~'}
      if (pValue < 0.001) {stars <- '***'}
      tableRow <- c(term, estimate, stdError, tValue, paste(pValue, stars))
      if ((boldSig == TRUE) & (pValue < 0.05)) {tableRow <- sapply(tableRow, makeBold)}
      if (pValue == 0) {tableRow[5] <- makeBold('$<$ 0.001 ***')}
      modelTerms <- c(modelTerms, paste(tableRow[1], tableRow[2], tableRow[3], tableRow[4], tableRow[5], sep=" & "), "\\\\")
    }
    # Smooth terms
    smooth <- as.data.frame(summary(model)$s.table)
    term <- rownames(smooth)[1]
    edf <- round(smooth[1,1][1],roundN)
    refdf <- round(smooth[1,2][1],roundN)
    fValue <- round(smooth[1,3][1],roundN)
    pValue <- round(smooth[1,4][1],roundN)
    stars <- getStars(pValue)
    tableRow <- c(term, edf, refdf, fValue, paste(pValue, stars))
    if ((boldSig == TRUE) & (pValue < 0.05)) {tableRow <- sapply(tableRow, makeBold)}
    if (pValue == 0) {tableRow[5] <- makeBold('$<$ 0.001 ***')}
    headerRow <- 'Smooth Terms & edf & Ref df & F & p value \\\\'
    modelTerms <- c(modelTerms, '\\\\', '\\hline', headerRow, paste(tableRow[1], tableRow[2], tableRow[3], tableRow[4], tableRow[5], sep=" & "), "\\\\")
  }

  footer <- c('\\hline', '\\end{tabular}', '\\end{center}', '\\caption{CAPTION}',
              '\\label{LABEL}', '\\end{table}')

  toPrint <- c(header, modelTerms, footer)
  cat(paste(toPrint, delim='\n'))
}
