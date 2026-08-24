#' AICtab.R 
#'
#' function to plot AIC values of a list of model, AIC wheights, formulas, and some other useful compartive metrics
#' should be given a list of pre calculated models
#' 
#' in case AICtab does not support different nature of models, use AICmultitab for GAM x GLM x LM x GAMM
#'
#' dependencies :
#' library(dplyr)
#'
#' @param taxon list of models
#'
#' @returns a comparative for model selection
#' @export
#'
#' @examples
#' 
#' model1 <- gam( Y ~ s(X1) + X2 + s(X3, bs = "re"), data=data, method=REML)
#' model2 <- gam( Y ~ s(X1) + X2, data=data, method=REML)
#' model3 <- gam( Y ~ s(X1, by = X2) + s(X3, bs = "re"), data=data, method=REML)
#' 
#' model_list <- (model1 = model1, model2 = model2, model3 = model3)
#' 
#' AICtab(model_list)
#' 
#' 
#' >  model   AIC        df     deviance   r_sq      deltaAIC    weight                     formula
#' > model1 26026.22 141.60786 0.2141931 0.16695134   0.000000  9.491662e-01   Y ~ s(X1) + X2 + s(X3, bs = "re"), data=data, method=REML
#' > model2 26034.69 145.07304 0.2136934 0.16679301   8.474098  1.371561e-02   Y ~ s(X1) + X2, data=data, method=REML
#' > model3 26034.71 145.30084 0.2138358 0.16676168   8.489922  1.360752e-02   Y ~ s(X1, by = X2) + s(X3, bs = "re"), data=data, method=REML
#'
#'
#'

AICtab <- function(cloud) {
  
  aic_tab <- data.frame(
    model = names(cloud),
    AIC   = sapply(cloud, AIC),
    df    = sapply(cloud, \(x) attr(logLik(x), "df")),
    deviance = sapply(cloud, \(x) summary(x)$dev.expl),
    r_sq = sapply(cloud, \(x) summary(x)$r.sq)
    
  )
  
  aic_tab <- aic_tab %>%
    mutate(
      deltaAIC = AIC - min(AIC),
      weight = exp(-0.5 * deltaAIC) /
        sum(exp(-0.5 * deltaAIC))
    )
  
  aic_tab <- aic_tab %>%
    mutate( formula = sapply(cloud, \(x) paste(deparse(formula(x)), collapse = "")),)  %>%
    arrange(AIC)
  
  return(aic_tab)
  
}


AICmultitab <- function(cloud) {
  
  is_gamm <- function(x) {
    is.list(x) && all(c("gam", "lme") %in% names(x))
  } # pour gérer GAMM qui ne renvoie pas toujours une 'classe' GAMM
  
 #nature modèle
  get_model_type <- function(x) {
    if (is_gamm(x)) {
      return("GAMM")
    }
    if (inherits(x, "gam")) {
      return("GAM")
    }
    if (inherits(x, "glm")) {
      return("GLM")
    }
    if (inherits(x, "lm")) {
      return("LM")
    }
    return(class(x)[1])
  }
  
  get_AIC <- function(x) {
    if (is_gamm(x)) {
      return(AIC(x$lme))
    }
    return(AIC(x))
  }
  
  get_df <- function(x) {
    if (is_gamm(x)) {
      return(attr(logLik(x$lme), "df"))
    }
    return(attr(logLik(x), "df"))
  }
  
  get_rsq <- function(x) {
    
    tryCatch({
      if (inherits(x, "gam")) {
        return(summary(x)$r.sq)
      }
      if (is_gamm(x)) {
        return(summary(x$gam)$r.sq)
      }
      if (inherits(x, "lm") && !inherits(x, "glm")) {
        return(summary(x)$adj.r.squared)
      }
      if (inherits(x, "glm")) {
        
        dev <- 1 - (x$deviance / x$null.deviance)
        return(dev)
      }
      return(as.numeric(NA))
    }, error = function(e) as.numeric(NA))
  }
  
  get_devexpl <- function(x) {
    
    tryCatch({
      if (inherits(x, "gam")) {
        return(summary(x)$dev.expl)
      }
      if (is_gamm(x)) {
        return(NA) # pb pour retourner la deviance dans les modeles GAMM, peu utile, on force avec des NA
      }
      if (inherits(x, "glm")) {
        
        dev <- 1 - (x$deviance / x$null.deviance)
        return(dev)
      }
      if (inherits(x, "lm")) {
        return(summary(x)$r.squared)
      }
      return(as.numeric(NA))
    }, error = function(e) as.numeric(NA))
  }
  
  get_method <- function(x) {
    if (is_gamm(x)) {
      meth <- x$gam$method
      if (is.null(meth)) return("GAMM")
      return(paste0(meth))
    }
    if (inherits(x, "gam")) {
      meth <- x$method
      if (is.null(meth)) return("GAM")
      return(paste0(meth))
    }
    if (inherits(x, "glm")) {
      fam <- family(x)$family
      return(paste0(fam))
    }
    if (inherits(x, "lm")) {
      return("LM")
    }
    return(NA_character_)
  }
  
  get_formula <- function(x) {
    if (is_gamm(x)) {
      return(paste(deparse(formula(x$gam)), collapse = " "))
    }
    return(paste(deparse(formula(x)), collapse = " "))
  }
  
  # main table
  aic_tab <- data.frame(
    model = names(cloud),
    model_type = sapply(cloud, get_model_type),
    method = sapply(cloud, get_method),
    AIC = sapply(cloud, get_AIC),
    df = sapply(cloud, get_df),
    deviance = sapply(cloud, get_devexpl),
    r_sq = sapply(cloud, get_rsq),
    stringsAsFactors = FALSE
  )
  
  aic_tab <- aic_tab %>%
    mutate(
      deltaAIC = AIC - min(AIC),
      weight =exp(-0.5 * deltaAIC) / sum(exp(-0.5 * deltaAIC)),
      formula = sapply(cloud, get_formula),
    ) %>%
    arrange(AIC)
  
  return(aic_tab)
}


