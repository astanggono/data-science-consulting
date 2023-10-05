library(data.table)
engagement.model <- function(dt, outcome.name, input.names, model.type){
  res <- fit.model(dt = dt, outcome.name = outcome.name, input.names = input.names, model.type = model.type)
  return(res)
}

fit.model <- function(dt, outcome.name, input.names, model.type, digits = 3){
  library(formulaic)
  the.formula <- create.formula(outcome.name = outcome.name, input.names = input.names, dat = dt, reduce = T)
  
  if(model.type == "logistic"){
    mod <- glm(formula = the.formula, family = "binomial", data = dt)
    mod.summary <- logistic.regression.summary(glm.mod = mod, digits = digits)
  }
  if(model.type == "linear"){
    mod <- lm(formula = the.formula, data = dt)
    mod.summary <- linear.regression.summary(lm.mod = mod, digits = digits)
  }
  mod.summary.rounded <- mod.summary[, lapply(X = .SD, FUN = "round.numerics", digits = digits)]
  return(mod.summary.rounded)
}

logistic.regression.summary <- function(glm.mod, digits = 3, alpha = 0.05){
  library(data.table)
  glm.coefs <- as.data.table(summary(glm.mod)$coefficients, keep.rownames = TRUE)
  setnames(x = glm.coefs, old = "rn", new = "Variable")
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  glm.coefs[, Odds.Ratio := exp(Estimate)]
  glm.coefs[, OR.Lower.95 := exp(Estimate - z * `Std. Error`)]
  glm.coefs[, OR.Upper.95 := exp(Estimate + z * `Std. Error`)]
  
  return(glm.coefs[])
}



linear.regression.summary <- function(lm.mod, digits = 3, alpha = 0.05){
  library(data.table)
  lm.coefs <- as.data.table(summary(lm.mod)$coefficients, keep.rownames = TRUE)
  setnames(x = lm.coefs, old = "rn", new = "Variable")
  
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  lm.coefs[, Coef.Lower.95 := Estimate - z * `Std. Error`]
  lm.coefs[, Coef.Upper.95 := Estimate + z * `Std. Error`]
  return(lm.coefs)
}

percentage.table <- function(x, digits = 1){
  tab <- table(x)
  percentage.tab <- 100*tab/(sum(tab))
  rounded.tab <- round(x = percentage.tab, digits = digits)
  return(rounded.tab)
}

round.numerics <- function(x, digits){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}

get.resp.group <-function(dat,field,dat.grp){
  val = dat[,get(field)]
  return(dat.grp[val>=lower & val<upper, label])
}

get.top.rates.by.product <- function(dat, engagement.col.name, top.num){
  dat[,.(Mean = 100*mean(get(engagement.col.name), na.rm = T)), by = PRODUCT.COL.NAME][order(-Mean)][1:top.num]
}

inverted.mean <- function(x) {
  return(10-mean(x, na.rm = T))
}

mean.na.rm <- function(x){
  return(mean(x,na.rm = T))
}

as.percent <- function(x){
  return(round.numerics(x*100, digits = 3))
}

percent.diff <- function (rate1, rate2){
  rate.diff= mean.na.rm(rate1)-mean.na.rm(rate2)
  return(as.percent(rate.diff))
}

top.n <- function (dat, col.name, n){
  return(dat[order(-get(col.name))][1:n])
}

barplot.output <- function(y.vals, x.vals, title, show.pcts = T, digits =2){
  
  barplot(height = y.vals,
          names.arg = x.vals,
          space=0.01,
          las = 1,
          main = title,
          ylab = "Percentage", 
          #xlab = input$respondent_variable, 
          ylim = c(0, 1.2*max(y.vals, na.rm = TRUE)), 
          col = "dodgerblue")
  digits.str=paste0("%.", digits, "f%%")
  if(show.pcts==T){
    space_val = 0
    text(x = -0.5 + 1:length(y.vals) * (1+space_val), y = y.vals, labels = sprintf(digits.str, y.vals), pos = 3)
  }
  
}