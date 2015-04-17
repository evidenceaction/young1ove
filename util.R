library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(foreach)
library(plm)
library(lmtest)
library(car)
library(yaml)
library(stringr)

predict.rob <- function(x, vcov.=vcov(x), signif.level=0.1, newdata) {
  if (missing(newdata) || is.null(newdata)) { 
    newdata <- x$model 
  }
  
  newdata.col.re <- newdata %>% names %>% paste(collapse="|") %>% sprintf("(^|:)(%s)($|\\[)", .)
  
  m.mat <- x %>% 
    terms %>% 
    (function(trms) {
      newdata.col.re %>% grepl(attr(trms, "term.labels")) %>% not %>% 
        (function(mask) { if (any(mask)) drop.terms(trms, which(mask), keep.response=FALSE) else delete.response(trms) }) }) %>%
    model.matrix(data=newdata)
  
  used.terms <- function(names) {
    ut <- names %>% sub("\\[.+", "", .) %>% grepl(newdata.col.re, .) 
    ut[1] <- TRUE
    
    return(ut)
  }
  
  vcov. %<>% (function(vc) { vc %>% colnames %>% used.terms %>% vc[., .] })
  
  fit <- m.mat %*% x$coef[names(x$coef) %>% used.terms]
  se.fit <- (m.mat %*% vcov. %*% t(m.mat)) %>% diag %>% sqrt

  merr <- qnorm(signif.level/2, lower.tail=FALSE) * se.fit
  
  return(list(fit=fit,
              se.fit=se.fit,
              fit.max=fit + merr,
              fit.min=fit - merr))
}

reg.dhs <- function(.formula, .data, new.data=NULL, ignore.error=FALSE, predict.fit=TRUE) tryCatch({
  reg.res <- plm(.formula, data=.data, model="pooling", index=c("cluster.id", "caseid")) 
  
  robust.vcov <- plm::vcovHC(reg.res, cluster="group")
  
  if (predict.fit) {
    ret <- reg.res %>%
      predict.rob(vcov=robust.vcov, newdata=new.data) %>%
      magrittr::extract(c("fit", "fit.min", "fit.max", "se.fit")) %>% 
      as.data.frame %>%
      (function(results.data) {
        if (!is.null(new.data)) {
          cbind(new.data, results.data)  
        } else if (length(all.vars(.formula)) == 1) {
          results.data[1, ]
        }
      }) 
  } else {
    ret <- coeftest(reg.res, vcov=robust.vcov) %>%
      magrittr::extract(,) %>%
      as.data.frame %>%
      set_names(c("est", "se", "t.value", "p.value")) %>%
      mutate(coef=rownames(.))
  }
  
  ret %>% 
    mutate(country.code=first(.data$country.code),
           year=max(.data$year.interview)) 
}, error=function(err) if (!ignore.error) browser() else data.frame())

estimate.teen.preg <- function(.data) {
  plm(any.preg ~ 1, data=.data, model="pooling", index=c("cluster.id", "caseid")) %>%
    coeftest(vcov=plm::vcovHC(., cluster="group")) %>%
    extract(, c(1:2, 4), drop=FALSE) %>%
    as.data.frame %>%
    set_names(c("est", "se", "p.value")) %>%  
    mutate(country.code=first(.data$country.code),
         year=max(.data$year.interview))
}

add.if.missing.var <- function(.data, var) {
  .data[, setdiff(var, names(.data))] <- NA
  return(.data)
}

prop.plot <- function(.data, ylab, title=NULL, .group="sex.activity", group.name="Sexual Activity", .nrow=2) {
  .data %>% 
    mutate(age=factor(age)) %>% 
    ggplot(aes_string(x="age", y="fit", group=.group)) +
    geom_line(aes_string(color=.group)) +
    geom_ribbon(aes_string(ymin="fit.min", ymax="fit.max", fill=.group), alpha=0.2) +
    labs(x="Age", y=ylab, title=title) +
    facet_wrap(~ country.name, nrow=.nrow) +
    theme(legend.position="top")
}