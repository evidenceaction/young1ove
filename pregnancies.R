library(foreign)
library(plyr)
library(dplyr)
library(magrittr)
library(foreach)
library(plm)
library(lmtest)
library(ggplot2)

reg.dhs <- function(.formula, .data) {
  plm(.formula, data=.data, model="pooling", index=c("cluster.id", "caseid")) %>%
    coeftest(vcov=plm::vcovHC(., cluster="group")) %>%
    extract(, c(1:2, 4), drop=FALSE) %>%
    as.data.frame %>%
    set_names(c("est", "se", "p.value")) %>%  
    mutate(regressor=rownames(.)) %>%
    mutate(country.code=first(.data$country.code),
           year=max(.data$year.interview))
}

estimate.teen.preg <- function(.data) {
  plm(any.preg ~ 1, data=.data, model="pooling", index=c("cluster.id", "caseid")) %>%
    coeftest(vcov=plm::vcovHC(., cluster="group")) %>%
    extract(, c(1:2, 4), drop=FALSE) %>%
    as.data.frame %>%
    set_names(c("est", "se", "p.value")) %>%  
    mutate(country.code=first(.data$country.code),
         year=max(.data$year.interview))
}


# test.dhs.data <- foreach(dta.file=list.files("~/Data/DHS_IR", "??IR.+.DTA", full.names=TRUE, ignore.case=TRUE), .combine=rbind.fill) %do% {
#   read.dta(dta.file) %>%
#     mutate(country.code=v000,
#               cluster.id=v001,
#               hh.id=v002,
#               per.id=v003,
#               year.interview=v007,
#               age=v012) %>%
#     filter(age == 15)
# }

dhs.data <- foreach(dta.file=list.files("~/Data/DHS_IR", "??IR.+.DTA", full.names=TRUE, ignore.case=TRUE), .combine=rbind.fill) %do% tryCatch({
  read.dta(dta.file) %>% 
    (function(.data) { # Add NA columns for missing variables
      .data[, setdiff(paste0("v", c("750", "763a", "766b", "820", paste0("821", c("a", "b", "c")), "830", paste0("834", c("a", "b", "c")))), names(.data))] <- NA
      return(.data)
    }) %>%
    transmute(caseid,
              country.code=v000,
              cluster.id=v001,
              hh.id=v002,
              per.id=v003,
              year.interview=v007,
              age=v012,
              ever.married=v020,
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
              age.first.intercourse=v525,
              heard.of.sti=v750,
              heard.of.aids=v751,
              last.intercourse.condom=v761,
              caugh.sti.12mon=v763a,
              num.men.sex.partner=v766b,
              condom.first.intercourse=v820,
              age.sex.partner.1=v821a,
              age.sex.partner.2=v821b,
              age.sex.partner.3=v821c,
              age.first.sex.partner=v830,
              age.last.sex.partner.1=v834a,
              age.last.sex.partner.2=v834b,
              age.last.sex.partner.3=v834c) %>%
    mutate_each(funs(ifelse(. <= 80, ., NA)), matches("age\\.last\\.sex\\.partner\\.\\d")) %>%
    mutate(curr.preg=ifelse(curr.preg == "yes", 1, 0),
           terminated.preg=ifelse(terminated.preg == "yes", 1, 0),
           any.preg=ifelse(num.births + curr.preg + terminated.preg > 0, 1, 0),
           curr.contraceptive.modern=ifelse(grepl("modern", curr.contraceptive.cat, ignore.case=TRUE), 1, 0),
           max.age.last.sex.partner=max(age.last.sex.partner.1, age.last.sex.partner.2, age.last.sex.partner.3, na.rm=TRUE)) %>%
    filter(age <= 19)
})

# dhs.data %>% 
#   count(country.code, age, any.preg) %>% 
#   group_by(country.code, age) %>%
#   mutate(any.preg.percent=n/sum(n) * 100) %>%
#   ungroup %>% 
#   filter(any.preg == 1) %>%
#   ggplot() +
#   geom_bar(aes(x=age, y=any.preg.percent), stat="identity") +
#   facet_wrap(~ country.code)
# 
# dhs.data %>% 
#   filter(age <= 16) %>%
#   count(country.code, any.preg) %>% 
#   group_by(country.code) %>%
#   mutate(any.preg.percent=n/sum(n) * 100) %>%
#   ungroup  
#   
# dhs.data %>% 
#   group_by(country.code, age, any.preg) %>%
#   summarize(mean.year.educ=mean(year.educ, na.rm=TRUE))

preg.est.data <- dhs.data %>%
  filter(age <= 16) %>%
  group_by(country.code) %>%
  do(estimate.teen.preg(.)) 

dhs.data %>%
  group_by(country.code) %>%
  do(reg.dhs(year.educ ~ factor(age)*any.preg, .)) 

dhs.data %>%
  filter(age <= 16, any.preg > 0) %>%
  group_by(country.code) %>%
  do(reg.dhs(curr.contraceptive.modern ~ 1, .)) 

dhs.data %>%
  filter(age <= 16, any.preg > 0, !is.na(max.age.last.sex.partner)) %>%
  group_by(country.code) %>%
  do(reg.dhs(max.age.last.sex.partner ~ 1, .)) 