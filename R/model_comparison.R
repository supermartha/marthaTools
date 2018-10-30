#' Convert to Model
#'
#' Takes a model and a factor to add and returns a new model with the additional factor to add. Used by the step_up function.
#' @param model original model
#' @param to_add vector of strings of names of the factors to test
#' @param dat name of the dataframe
#' @param type type of model. Options are 'glmBinomial', 'glm', 'lmer', 'glmer' (binomial), 'gam', or 'gamBinomial'
#' @keywords model comparison
#' @export
#' @examples step_up(myModel, c("gender", "region", "age"), dat="myData", type="lmer")
#' convert_to_model()

convert_to_model <- function(model, to_add, dat, type) {
  x <- attr(terms(model), "term.labels")
  dependent <- as.character(attr(terms(model), "variables")[[2]])
  if (type=='glmBinomial') {
    call <- paste("glm(", dependent, " ~ ", paste(x, collapse=" + "), "+", to_add, ", data=", dat, ", family='binomial')")
  }
  else if (type=='glm') {
    call <- paste("glm(", dependent, " ~ ", paste(x, collapse=" + "), "+", to_add, ", data=", dat, ")")
  }
  else if (type=='lmer') {
    modelTerms <- strsplit(paste(deparse(formula(model)), collapse = ''), '\\+')[[1]]
    randomEffects <- ''
    # Figure out if random effect
    for (item in modelTerms) {
      item <- trimws(item)
      lastChar <- substr(item, nchar(item), nchar(item))
      if (substr(item, 1, 1)=='('){
        if (lastChar==')') {randomEffects <- paste(randomEffects, '+', item)}
        else {firstHalf <- item}
      }
      else if (lastChar==')') {
        newEffect <- paste(firstHalf, '+', item)
        randomEffects <- paste(randomEffects, '+', newEffect)
      }
    }
    if (length(x) == 0) {
      call <- paste("lmer(", dependent, " ~ ", to_add, randomEffects, ", data=", dat, ")")
    }
    else {call <- paste("lmer(", dependent, " ~ ", paste(x, collapse=" + "), "+", to_add, randomEffects, ", data=", dat, ")")}
    # print(call)
  }

  else if (type=='glmer') {
    modelTerms <- strsplit(paste(deparse(formula(model)), collapse = ''), '\\+')[[1]]
    randomEffects <- ''
    # Figure out if random effect
    for (item in modelTerms) {
      item <- trimws(item)
      lastChar <- substr(item, nchar(item), nchar(item))
      if (substr(item, 1, 1)=='('){
        if (lastChar==')') {randomEffects <- paste(randomEffects, '+', item)}
        else {firstHalf <- item}
      }
      else if (lastChar==')') {
        newEffect <- paste(firstHalf, '+', item)
        randomEffects <- paste(randomEffects, '+', newEffect)
      }
    }
    if (length(x) == 0) {
      call <- paste("glmer(", dependent, " ~ ", to_add, randomEffects, ", data=", dat, ", family='binomial', glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))")
    }
    else {call <- paste("glmer(", dependent, " ~ ", paste(x, collapse=" + "), "+", to_add, randomEffects, ", data=", dat, ", family='binomial', glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))")}
  }

  else if (type=='gam') {
    smooth <- paste("te(", x[length(x)-1], ",", x[length(x)], ")")
    if (length(x) > 2) {x <- x[1:(length(x)-2)]
    to_paste <- paste(paste(x, collapse=" + "), "+")
    }
    else {to_paste <- ''}
    call <- paste("gam(", dependent, " ~ ", to_paste, to_add, "+", smooth, ", data=", dat, ")")
  }
  else if (type=='gamBinomial') {
    smooth <- paste("te(", x[length(x)-1], ",", x[length(x)], ")")
    if (length(x) > 2) {x <- x[1:(length(x)-2)]
    to_paste <- paste(paste(x, collapse=" + "), "+")
    }
    else {to_paste <- ''}
    call <- paste("gam(", dependent, " ~ ", to_paste, to_add, "+", smooth, ", data=", dat, ", family='binomial')")
  }
  #print(paste('call:', call))
  new <- eval(parse(text = call))
  return(new)
}

#' Step Up
#'
#' Takes a model and a vector of factors to step up with. Builds a model with each one added and does model comparison with the simpler model for each, returning p-values and AIC.
#' @param model model to step up from
#' @param to_test vector of factor names to add (as strings, not objects)
#' @param aic_only Boolean, defaults to TRUE. Don't remember what this does.
#' @param dat name of the dataframe
#' @param type type of model. Options are 'glmBinomial', 'glm', 'lmer', 'gam', 'gamBinomial'. Defaults to 'glm'.
#' @keywords model comparison
#' @export
#' @examples
#' step_up()

step_up <- function(model, to_test, aic_only=TRUE, dat='df_put', type="glm") {
  options(warn=1)
  ### Takes a model to step up from, and a list of factors to test
  p <- c()
  aic <- c()

  for (factor in to_test) {
    print(paste('        ...testing', factor))
    new <- convert_to_model(model, factor, dat, type)
    res <- anova(model, new, test="Chisq")
    p <- c(p, res[["Pr(>Chi)"]][2])
    if (type %in% c("lmer", "glmer")) {p <- c(p, res[["Pr(>Chisq)"]][2])}
    aic <- c(aic, extractAIC(new)[[2]])
    # print(p)
  }

  for (count in seq(length(to_test))) {
    num_stars = ''
    if (p[count] < 0.001) {num_stars = '***'}
    else if (p[count] < 0.01) {num_stars = '**'}
    else if (p[count] < 0.05) {num_stars = '*'}
    print(paste(round(p[count], 4), num_stars, round(aic[count], 2), to_test[count]))
  }
  print(p)
  print(aic)
  # print(paste('p', p))
  # print(paste('aic', aic))

  # Then find lowest p-value
  lowest <- min(p)
  candidates <- which(p==lowest)

  # If not significant, return old model
  if (lowest > 0.05) {
    print('Warning: no model is significantly different')
    return(model)
  }

  #If only one, go ahead and return it
  if (aic_only == FALSE) {
    if (length(candidates) == 1) {
      print(paste('Best new model (p-value):', to_test[candidates[1]]))
    }
  }

  #Else pick the one with lowest AIC
  aic_candidates <- which(aic==min(aic))
  if (length(aic_candidates) == 1) {
    print(paste('Best new model (AIC):', to_test[aic_candidates[1]]))
  }
  else {print('error: 2 models have same p-value and AIC')}

}

#' Step Down
#'
#' Takes a model to step down from. Build models with each of the predictors missing and compares to original model. I think only works for lmer??
#' @param model model to step down from
#' @param dependent name of dependent variable, as a string
#' @param aic_only Boolean, defaults to FALSE. Don't remember what this does.
#' @param dat name of the dataframe. Default to "temp"
#' @keywords model comparison
#' @export
#' @examples
#' step_down()

step_down <- function(model, dependent, aic_only=FALSE, dat="temp") {
  ### Takes a model to step down from, and tests the thing with the highest p-value when comparing models
  options(warn=1)
  p <- c()
  aic <- c()
  chisq <- c()

  predictors <- attr(terms(model), "term.labels")

  modelTerms <- strsplit(paste(deparse(formula(model)), collapse = ''), '\\+')[[1]]
  randomEffects <- ''
  # Figure out if random effect
  for (item in modelTerms) {
    item <- trimws(item)
    lastChar <- substr(item, nchar(item), nchar(item))
    if (substr(item, 1, 1)=='('){
      if (lastChar==')') {randomEffects <- paste(randomEffects, '+', item)}
      else {firstHalf <- item}
    }
    else if (lastChar==')') {
      newEffect <- paste(firstHalf, '+', item)
      randomEffects <- paste(randomEffects, '+', newEffect)
    }
  }


  for (i in seq(length(predictors))) {
    print(paste('        ...testing', predictors[i]))
    before <- predictors[seq(1,i-1)]
    if (i == 1) {before <- c()}
    after <- predictors[seq(i+1,length(predictors))]
    if (i == length(predictors)) {after <- c()}
    new_predictors <- c(before, after)
    call <- paste("lmer(",dependent," ~ ", paste(new_predictors, collapse=" + "), "+", randomEffects, ", data=,", dat,")")
    new <- eval(parse(text=call))
    res <- anova(model, new)
    #print(res)
    p <- c(p, res[["Pr(>Chisq)"]][2])
    aic <- c(aic, res$AIC[2])
    chisq <- c(chisq, res$Chisq[2])
  }

  ### Find the best
  # First, fix weird p-values
  for (i in seq(1,length(chisq))) {
    if (chisq[i] < 1) {p[i] = 1}
  }

  for (count in seq(length(predictors))) {
    num_stars = ''
    if (p[count] < 0.001) {num_stars = '***'}
    else if (p[count] < 0.01) {num_stars = '**'}
    else if (p[count] < 0.05) {num_stars = '*'}
    print(paste(round(p[count], 4), num_stars, round(aic[count], 2), predictors[count]))
  }
  #print(predictors)
  #print(p)
  #print(aic)
  #print(chisq)

  # Then find highest p-value
  highest <- max(p)
  candidates <- which(p==highest)

  to_test <- predictors

  # If highest is below 0.05, you're done, yay!
  if (highest < 0.05) {
    print('Keep old model!')
    return(model)
  }

  #If only one, go ahead and return it
  if (aic_only == FALSE) {
    if (length(candidates) == 1) {
      print(paste('Best new model (p-value): no', to_test[candidates[1]]))
      #return(convert_to_model(model, to_test[candidates[1]], to_test))
    }
  }

  #Else pick the one with highest AIC
  aic_candidates <- which(aic==max(aic))
  if (length(aic_candidates) == 1) {
    print(paste('Best new model (AIC): no', to_test[aic_candidates[1]]))
    print(match(to_test,aic_candidates[1]))
    #return(convert_to_model(model, match(to_test,aic_candidates[1]), to_test))
  }
  else {print('error: 2 models have same p-value and AIC')}

}
