library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(foreach)
library(plm)
library(lmtest)
library(car)
library(ggplot2)
library(yaml)

options(contrasts=c("contr.Treatment", getOption("contrasts")[2]))

config <- yaml.load_file("local_config.yaml")

predict.rob <- function(x, vcov.=vcov(x), signif.level=0.05, newdata) {
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

reg.dhs <- function(.formula, .data, new.data=NULL) tryCatch({
  plm(.formula, data=.data, model="pooling", index=c("cluster.id", "caseid")) %>%
    predict.rob(vcov=plm::vcovHC(., cluster="group"), newdata=new.data) %>%
    magrittr::extract(c("fit", "fit.min", "fit.max")) %>% 
    as.data.frame %>%
    (function(results.data) {
      if (!is.null(new.data)) {
        cbind(new.data, results.data)  
      } else if (length(all.vars(.formula)) == 1) {
        results.data[1, ]
      }
    }) %>%
#     coeftest(vcov=plm::vcovHC(., cluster="group")) %>%
#     extract(, c(1:2, 4), drop=FALSE) %>%
#     as.data.frame %>%
#     set_names(c("est", "se", "p.value")) %>%  
#     mutate(regressor=rownames(.)) %>%
    mutate(country.code=first(.data$country.code),
           year=max(.data$year.interview))
}, error=function(err) browser())

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

prop.plot <- function(.data) { 
  ggplot(.data, aes(factor(age), fit, group=sex.activity)) +
  geom_line(aes(color=sex.activity)) +
  geom_ribbon(aes(ymin=fit.min, ymax=fit.max, fill=sex.activity), alpha=0.1) +
  facet_wrap(~ country.code)
}

dhs.data <- foreach(dta.file=list.files(paste0(config$data_path, "/DHS_IR"), "??IR.+.DTA", full.names=TRUE, ignore.case=TRUE), .combine=rbind.fill) %do% tryCatch({
  read.dta(dta.file, convert.factors=FALSE) %>% 
    add.if.missing.var(paste0("v", c("750", "763a", "766b", "820", paste0("821", c("a", "b", "c")), "830", paste0("834", c("a", "b", "c"))))) %>%
    transmute(caseid,
              country.code=v000,
              cluster.id=v001,
              hh.id=v002,
              per.id=v003,
              year.interview=v007,
              age=v012,
              year.educ=v107,
              num.births=v201,
              curr.preg=v213,
              child.preg=v219,
              curr.preg.wanted=v225,
              terminated.preg=v228,
              know.contraceptive=v301,
              use.contraceptive=v302,
              curr.contraceptive=v312,
              curr.contraceptive.cat=v313,
              curr.marital.status=v501,
              age.first.intercourse=v525,
              heard.of.sti=v750,
              heard.of.aids=v751,
              last.intercourse.condom=v761,
              caught.sti.12mon=v763a,
              num.men.sex.partner=v766b,
              condom.first.intercourse=v820,
              age.sex.partner.1=v821a,
              age.sex.partner.2=v821b,
              age.sex.partner.3=v821c,
              age.first.sex.partner=v830,
              age.last.sex.partner.1=v834a,
              age.last.sex.partner.2=v834b,
              age.last.sex.partner.3=v834c) %>%
    filter(age <= 19) %>%
    mutate_each(funs(ifelse(. <= 80, ., NA)), matches("age\\.last\\.sex\\.partner\\.\\d")) %>%
    mutate_each(funs(ifelse(. == 9, NA, .)), curr.preg, terminated.preg) %>%
    mutate(any.preg=ifelse(num.births + curr.preg + terminated.preg > 0, 1, 0),
           sex.activity=ifelse(any.preg > 0, "active.preg", ifelse(age.first.intercourse == 0, "inactive", "active.nopreg")) %>% factor,
           curr.contraceptive.modern=ifelse(curr.contraceptive.cat == 3, 1, 0),
           max.age.last.sex.partner=pmax(age.last.sex.partner.1, age.last.sex.partner.2, age.last.sex.partner.3, na.rm=TRUE),
           caught.sti.12mon.know=ifelse(caught.sti.12mon > 1, NA, caught.sti.12mon),
           curr.married=ifelse(curr.marital.status == 9, NA, ifelse(curr.marital.status == 1, 1, 0)))
}, error=function(err) browser()) %>%
  left_join(foreach(dta.file=list.files(paste0(config$data_path, "/DHS_IR"), "??PR.+.DTA", full.names=TRUE, ignore.case=TRUE), .combine=rbind.fill) %do% tryCatch({
    read.dta(dta.file, convert.factors=FALSE) %>% 
      add.if.missing.var(paste0("hv", c("129"))) %>%
    transmute(per.id=hvidx,
              country.code=hv000,
              cluster.id=hv001,
              hh.id=hv002,
              school.attend.status=hv129) %>% 
      mutate(school.attend.status=ifelse(school.attend.status == 0, 
                                         "never", 
                                         ifelse(school.attend.status == 4, 
                                                "dropout",
                                                ifelse(school.attend.status == 5, 
                                                       "left",
                                                       ifelse(school.attend.status >= 8, NA, "attend")))) %>% factor)
  }, error=function(err) browser()), by=c("country.code", "cluster.id", "hh.id", "per.id"))

# preg.est.data <- dhs.data %>%
#   filter(age <= 16) %>%
#   group_by(country.code) %>%
#   do(estimate.teen.preg(.)) 

# preg.fit.data <- dhs.data %>%
#   group_by(country.code) %>% 
#   do(reg.dhs(any.preg ~ 1, .data=.)) 

dhs.data %>% 
  ggplot() +
  geom_bar(aes(factor(age), fill=sex.activity), position="fill") +
  facet_wrap(~ country.code)

year.educ.fit.data <- dhs.data %>%
  mutate(age=factor(age)) %>%
  group_by(country.code) %>%
  do(reg.dhs(year.educ ~ age*sex.activity, .data=., new.data=expand.grid(sex.activity=factor(levels(.$sex.activity)), age=factor(15:19)))) 

year.educ.fit.data %>% prop.plot

dhs.data %>% 
  filter(!is.na(school.attend.status)) %>%
  ggplot() +
  geom_bar(aes(factor(age), fill=school.attend.status), position="fill") +
  facet_wrap(~ country.code)

dropout.fit.data <- dhs.data %>%
  filter(!is.na(school.attend.status)) %>%
  mutate(age=factor(age),
         not.attending.school=ifelse(school.attend.status %in% c("never", "dropout", "left"), 1, 0)) %>%
  group_by(country.code) %>%
  do(reg.dhs(not.attending.school ~ age*sex.activity, .data=., new.data=expand.grid(sex.activity=factor(levels(.$sex.activity)), age=factor(15:19)))) 

dropout.fit.data %>% prop.plot

dhs.data %>%
  filter(!is.na(sex.activity)) %>% 
  ggplot(aes(factor(age), year.educ)) +
  geom_violin(aes(color=sex.activity)) +
  facet_wrap(~ country.code)

sti.fit.data <- dhs.data %>%
  filter(sex.activity != "inactive", !is.na(caught.sti.12mon.know)) %>% 
  mutate(age=factor(age)) %>%
  group_by(country.code) %>%
  do(reg.dhs(caught.sti.12mon.know ~ age*sex.activity, .data=., new.data=expand.grid(sex.activity=factor(levels(droplevels(.$sex.activity))), age=factor(15:19)))) 
  ggplot(aes(factor(age), fit, group=sex.activity)) +
  geom_line(aes(color=sex.activity)) +
  geom_ribbon(aes(ymin=fit.min, ymax=fit.max, fill=sex.activity), alpha=0.1) +
  facet_wrap(~ country.code)

sti.fit.data %>% prop.plot

contraceptive.fit.data <- dhs.data %>%
  filter(sex.activity != "inactive") %>%
  mutate(age=factor(age)) %>%
  group_by(country.code) %>%
  do(reg.dhs(curr.contraceptive.modern ~ age*sex.activity, .data=., new.data=expand.grid(age=factor(15:19), sex.activity=factor(levels(droplevels(.$sex.activity)))))) 

contraceptive.fit.data %>% prop.plot

dhs.data %>%
  filter(sex.activity != "inactive") %>%
  select(country.code, age, sex.activity, starts_with("age.last.sex.partner")) %>%
  gather(partner, partner.age, age.last.sex.partner.1:age.last.sex.partner.3) %>%
  ggplot(aes(factor(age), partner.age)) +
  # geom_violin(aes(color=sex.activity)) +
  geom_boxplot(aes(fill=sex.activity), notch=TRUE) +
  facet_wrap(~ country.code)