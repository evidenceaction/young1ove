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
library(stringr)

options(contrasts=c("contr.Treatment", getOption("contrasts")[2]))

config <- yaml.load_file("local_config.yaml")

source("util.R")

country.name.data <- data.frame(country.code=c("KE", "LS", "MW", "MZ", "NM", "SZ", "ZA", "ZM", "ZW"),
                                country.name=c("Kenya", "Lesotho", "Malawi", "Mozambique", "Namibia", "Swaziland", "South Africa", "Zambia", "Zimbabwe"))


women.dhs.data <- foreach(dta.file=list.files(paste0(config$data_path, "/DHS_IR"), "??IR.+.DTA", full.names=TRUE, ignore.case=TRUE), .combine=rbind.fill) %do% tryCatch({
  read.dta(dta.file, convert.factors=FALSE) %>% 
    add.if.missing.var(paste0("v", c("750", "763a", "766b", "820", paste0("821", c("a", "b", "c")), "830", paste0("834", c("a", "b", "c"))))) %>%
    transmute(caseid,
              country.code=v000,
              cluster.id=v001,
              hh.id=v002,
              per.id=v003,
              year.interview=v007,
              age=v012,
              age.group=v013,
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
    # filter(age <= 19) %>%
    mutate_each(funs(ifelse(. <= 80, ., NA)), matches("age\\.last\\.sex\\.partner\\.\\d")) %>%
    mutate_each(funs(ifelse(. == 9, NA, .)), curr.preg, terminated.preg) %>%
    mutate(any.preg=ifelse(num.births + curr.preg + terminated.preg > 0, 1, 0),
           sex.activity=ifelse(any.preg > 0, "active.preg", ifelse(age.first.intercourse == 0, "inactive", "active.nopreg")) %>% factor,
           curr.contraceptive.modern=ifelse(curr.contraceptive.cat == 3, 1, 0),
           max.age.last.sex.partner=pmax(age.last.sex.partner.1, age.last.sex.partner.2, age.last.sex.partner.3, na.rm=TRUE),
           mean.age.last.sex.partner=rowMeans(select(., starts_with("last.sex.partner")), na.rm=TRUE),
           spread.age.last.sex.partner=pmax(age.last.sex.partner.1, age.last.sex.partner.2, age.last.sex.partner.3, na.rm=TRUE) - 
             pmin(age.last.sex.partner.1, age.last.sex.partner.2, age.last.sex.partner.3, na.rm=TRUE),
           caught.sti.12mon.know=ifelse(caught.sti.12mon > 1, NA, caught.sti.12mon),
           curr.married=ifelse(curr.marital.status == 9, NA, ifelse(curr.marital.status == 1, 1, 0)),
           num.men.sex.partner=ifelse(num.men.sex.partner >= 98, NA, num.men.sex.partner) + curr.married)
}, error=function(err) browser()) %>%
  left_join(foreach(dta.file=list.files(paste0(config$data_path, "/DHS_IR"), "??PR.+.DTA", full.names=TRUE, ignore.case=TRUE), .combine=rbind.fill) %do% tryCatch({
    read.dta(dta.file, convert.factors=FALSE) %>% 
      add.if.missing.var(paste0("hv", c("129", "121"))) %>%
      transmute(per.id=hvidx,
                country.code=hv000,
                cluster.id=hv001,
                still.in.school=hv110,
                hh.id=hv002,
                curr.school.attend=hv121,
                school.attend.status=hv129) %>% 
      mutate(school.attend.status=ifelse(school.attend.status == 0, 
                                         "never", 
                                         ifelse(school.attend.status == 4, 
                                                "dropout",
                                                ifelse(school.attend.status == 5, 
                                                       "left",
                                                       ifelse(school.attend.status >= 8, NA, "attend")))) %>% factor,
             still.in.school=ifelse(still.in.school == 9, NA, still.in.school),
             curr.school.attend=ifelse(curr.school.attend == 9, NA, 1*(curr.school.attend != 0)))
  }, error=function(err) browser()), by=c("country.code", "cluster.id", "hh.id", "per.id")) %>% 
  mutate(gender="female")

men.dhs.data <- foreach(dta.file=list.files(paste0(config$data_path, "/DHS_IR"), "??MR.+.DTA", full.names=TRUE, ignore.case=TRUE), .combine=rbind.fill) %do% tryCatch({
  read.dta(dta.file, convert.factors=FALSE) %>% 
    transmute(caseid=mcaseid,
              country.code=mv000,
              cluster.id=mv001,
              hh.id=mv002,
              per.id=mv003,
              year.interview=mv007,
              age=mv012,
              age.group=mv013)
}, error=function(err) browser()) %>% 
  mutate(gender="male")

all.dhs.data <- rbind.fill(women.dhs.data, men.dhs.data) %>% 
  mutate(recode.ver=sub("^.+", "", country.code),
           country.code=sub("\\d$", "", country.code)) %>%
  left_join(foreach(dta.file=list.files(paste0(config$data_path, "/DHS_IR"), "??AR.+.DTA", full.names=TRUE, ignore.case=TRUE), .combine=rbind.fill) %do% tryCatch({
    read.dta(dta.file, convert.factors=FALSE) %>% 
      transmute(cluster.id=hivclust,
                hh.id=hivnumb,
                per.id=hivline,
                blood.test=hiv03) %>% 
      mutate(hiv.positive=ifelse(blood.test %in% 1:3, 1, ifelse(blood.test > 3, NA, 0)),
             country.code=str_match(dta.file, ignore.case("/(.{2})AR"))[2],
             recode.ver=str_match(dta.file, ignore.case("AR(\\d)"))[2])
  }, error=function(err) browser()), by=c("country.code", "cluster.id", "hh.id", "per.id")) %>%
  left_join(foreach(dta.file=list.files(paste0(config$data_path, "/DHS_IR"), "??OB.+.DTA", full.names=TRUE, ignore.case=TRUE), .combine=rbind.fill) %do% tryCatch({
    read.dta(dta.file, convert.factors=FALSE) %>% 
      transmute(cluster.id=syphclust,
                hh.id=syphnumb,
                per.id=syphline,
                blood.test=syph03) %>% 
      mutate(syph.positive=ifelse(blood.test > 1, NA, blood.test),
             country.code=str_match(dta.file, ignore.case("/(.{2})OB"))[2],
             recode.ver=str_match(dta.file, ignore.case("OB(\\d)"))[2])
  }, error=function(err) browser()), by=c("country.code", "cluster.id", "hh.id", "per.id")) %>%
  left_join(country.name.data, by="country.code") %>% 
  group_by(country.code) %>%
  mutate(country.name=sprintf("%s (%d)", country.name, max(year.interview))) %>% 
  ungroup %>%
  mutate_each(funs(factor), gender) %>%
  mutate(age.group=factor(age.group, levels=1:10, labels=c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64")))

dhs.data <- all.dhs.data %>% filter(age <= 19, gender == "female")

save(all.dhs.data, dhs.data, file="dhs.RData")

# preg.est.data <- dhs.data %>%
#   filter(age <= 16) %>%
#   group_by(country.code) %>%
#   do(estimate.teen.preg(.)) 

# preg.fit.data <- dhs.data %>%
#   group_by(country.code) %>% 
#   do(reg.dhs(any.preg ~ 1, .data=.)) 

# Sexual Activity Proportions
dhs.data %>% 
  ggplot() +
  geom_bar(aes(factor(age), fill=sex.activity), position="fill") +
  labs(x="Age", y="Proportion") +
  scale_fill_discrete("Sexual Activity", labels=c("Active, No Pregnancies", "Active, Had Pregnancy", "Inactive")) +
  facet_wrap(~ country.name) +
  theme(legend.position="top")

year.educ.fit.data <- dhs.data %>%
  mutate(age=factor(age)) %>%
  group_by(country.name) %>%
  do(reg.dhs(year.educ ~ age*sex.activity, .data=., new.data=expand.grid(sex.activity=factor(levels(.$sex.activity)), age=factor(15:19)))) 

year.educ.fit.data %>% 
  prop.plot("Years of Education") +
  scale_fill_discrete("Sexual Activity", labels=c("Active, No Pregnancies", "Active, Had Pregnancy", "Inactive")) +
  scale_color_discrete("Sexual Activity", labels=c("Active, No Pregnancies", "Active, Had Pregnancy", "Inactive")) 

dhs.data %>% 
  filter(!is.na(school.attend.status)) %>%
  ggplot() +
  geom_bar(aes(factor(age), fill=school.attend.status), position="fill") +
  facet_wrap(~ country.code)

dropout.fit.data <- dhs.data %>%
  filter(!is.na(school.attend.status) | !is.na(still.in.school) | !is.na(curr.school.attend)) %>%
  mutate(age=factor(age),
         not.attending.school=ifelse(!is.na(still.in.school), 
                                     still.in.school, 
                                     ifelse(is.na(school.attend.status), 
                                            curr.school.attend,
                                            ifelse(school.attend.status %in% c("never", "dropout", "left"), 0, 1)))) %>%
  group_by(country.name) %>%
  do(reg.dhs(not.attending.school ~ age*sex.activity, .data=., new.data=expand.grid(sex.activity=factor(levels(.$sex.activity)), age=factor(15:19)))) 

dropout.fit.data %>% 
  prop.plot("Attending School") +
  scale_fill_discrete("Sexual Activity", labels=c("Active, No Pregnancies", "Active, Had Pregnancy", "Inactive")) +
  scale_color_discrete("Sexual Activity", labels=c("Active, No Pregnancies", "Active, Had Pregnancy", "Inactive")) 

# dhs.data %>%
#   filter(!is.na(sex.activity)) %>% 
#   ggplot(aes(factor(age), year.educ)) +
#   geom_violin(aes(color=sex.activity)) +
#   facet_wrap(~ country.code)

sti.fit.data <- dhs.data %>%
  filter(sex.activity != "inactive", !is.na(caught.sti.12mon.know)) %>% 
  mutate(age=factor(age)) %>%
  group_by(country.code) %>%
  do(reg.dhs(caught.sti.12mon.know ~ age*sex.activity, .data=., new.data=expand.grid(sex.activity=factor(levels(droplevels(.$sex.activity))), age=factor(15:19)))) 

sti.fit.data %>% prop.plot("Caught Any STI Within Last 12 Months", "Self reported and conditional on knowing")

contraceptive.fit.data <- dhs.data %>%
  filter(sex.activity != "inactive") %>%
  mutate(age=factor(age)) %>%
  group_by(country.name) %>%
  # do(reg.dhs(curr.contraceptive.modern ~ age*sex.activity, .data=., new.data=expand.grid(age=factor(15:19), sex.activity=factor(levels(droplevels(.$sex.activity)))))) 
  do(reg.dhs(curr.contraceptive.modern ~ age, .data=., new.data=expand.grid(age=factor(15:19)))) 

contraceptive.fit.data %>% #prop.plot("Currently Using Modern Contraceptive")
    mutate(age=factor(age)) %>% 
    ggplot(aes(x=age, y=fit, group=1)) +
    geom_ribbon(aes(ymin=fit.min, ymax=fit.max), alpha=0.2) +
    geom_line() +
    facet_wrap(~ country.name) +
    theme(legend.position="top")

num.sex.partner.fit.data <- dhs.data %>%
  filter(sex.activity != "inactive", !is.na(num.men.sex.partner)) %>%
  mutate(age=factor(age)) %>%
group_by(country.name) %>%
  do(reg.dhs(num.men.sex.partner ~ age*sex.activity, .data=., new.data=expand.grid(age=factor(15:19), sex.activity=factor(levels(droplevels(.$sex.activity)))))) 

# xx <- dhs.data %>%
#   filter(sex.activity != "inactive", !is.na(max.age.last.sex.partner), age <= 16) %>%
#   mutate(age=factor(age)) %>%
#   group_by(country.name) %>%
#   do(reg.res=plm(num.men.sex.partner ~ max.age.last.sex.partner, data=., model="pooling", index=c("cluster.id", "caseid")))

num.sex.partner.fit.data %>% 
  prop.plot("Number of Sexual Partners with Last 12 Months") + #, "Including husband if married") +
  scale_fill_discrete("Sexual Activity", labels=c("Active, No Pregnancies", "Active, Had Pregnancy", "Inactive")) +
  scale_color_discrete("Sexual Activity", labels=c("Active, No Pregnancies", "Active, Had Pregnancy", "Inactive")) 

terminated.fit.data <- dhs.data %>%
  filter(sex.activity == "active.preg", !is.na(num.men.sex.partner)) %>%
  mutate(age=factor(age)) %>%
  group_by(country.code) %>%
  do(reg.dhs(num.men.sex.partner ~ age*terminated.preg, .data=., new.data=expand.grid(age=factor(15:19), terminated.preg=0:1), ignore.error=TRUE)) 

terminated.fit.data %>% 
  mutate(terminated.preg=factor(terminated.preg)) %>%
  prop.plot(ylab="Number of Sexual Partners with Last 12 Months", title="Conditional on Prior Pregnancy", .group="terminated.preg", group.name="Terminated Pregnancy")

# xx <- dhs.data %>% 
#   select(country.code, cluster.id, caseid, age, sex.activity, terminated.preg, starts_with("age.last.sex.partner")) %>%
#   gather(partner, partner.age, age.last.sex.partner.1:age.last.sex.partner.3) %>%
#   filter(sex.activity == "active.preg", !is.na(partner.age), !is.na(terminated.preg)) %>%
#   # filter(sex.activity == "active.preg", !is.na(max.age.last.sex.partner)) %>%
#   mutate_each(funs(factor), age) %>%
#   mutate(caseid=paste(caseid, partner, sep="-")) %>%
  # group_by(country.code) %>%
#   do(reg.res=plm(terminated.preg ~ partner.age, data=., model="pooling", index=c("cluster.id", "caseid")))

dhs.data %>%
  select(country.code, age, sex.activity, terminated.preg, starts_with("age.last.sex.partner")) %>%
  gather(partner, partner.age, age.last.sex.partner.1:age.last.sex.partner.3) %>%
  filter(sex.activity == "active.preg", !is.na(partner.age)) %>%
  mutate_each(funs(factor), age, terminated.preg) %>%
  group_by(country.code) %>%
  ggplot(aes(factor(age), partner.age)) +
  # geom_violin(aes(color=sex.activity)) +
  geom_boxplot(aes(fill=terminated.preg)) +
  labs(x="Age", y="Partner's Age", title="Ages of last three sexual partners") +
  scale_fill_discrete("Terminated Pregnancy") +
  facet_wrap(~ country.code, nrow=2)

# dhs.data %>%
#   filter(sex.activity != "inactive", !is.na(num.men.sex.partner)) %>%
#   ggplot(aes(factor(age), num.men.sex.partner)) +
#   geom_violin(aes(color=sex.activity)) +
#   # geom_boxplot(aes(fill=sex.activity), notch=TRUE) +
#   labs(x="Age", y="Number of Sexual Partners with Last 12 Months") +
  # scale_fill_discrete("Sexual Activity") +
#   facet_wrap(~ country.code)

# Age of last three sexual partners
dhs.data %>%
  select(country.name, age, sex.activity, starts_with("age.last.sex.partner")) %>%
  gather(partner, partner.age, age.last.sex.partner.1:age.last.sex.partner.3) %>%
  filter(sex.activity != "inactive", !is.na(partner.age)) %>%
  ggplot(aes(factor(age), partner.age)) +
  # geom_violin(aes(color=sex.activity)) +
  geom_boxplot(aes(fill=sex.activity)) +
  labs(x="Age", y="Partner's Age", title="Ages of last three sexual partners") +
  scale_fill_discrete("Sexual Activity", labels=c("Active, No Pregnancies", "Active, Had Pregnancy")) +
  facet_wrap(~ country.name, nrow=2) +
  theme(legend.position="top")

dhs.data %>%
  filter(num.men.sex.partner > 1) %>%
  ggplot(aes(sex.activity, spread.age.last.sex.partner)) +
  # geom_violin(aes(color=sex.activity)) +
  geom_violin() +
  labs(x="Age", y="Partner's Age", title="Ages of last three sexual partners") +
  # scale_fill_discrete("Sexual Activity", labels=c("Active, No Pregnancies", "Active, Had Pregnancy")) +
  facet_wrap(~ country.name, nrow=2) +
  theme(legend.position="top")

hiv.prop.fit.data <- all.dhs.data %>%
  filter(age < 50) %>%
  filter(!is.na(hiv.positive)) %>% 
  mutate_each(funs(droplevels), age.group) %>%
  # mutate(age=factor(age)) %>%
  group_by(country.name) %>%
  # do(reg.dhs(hiv.positive ~ sex.activity*age, .data=., new.data=expand.grid(sex.activity=factor(levels(.$sex.activity)), age=factor(15:19)))) 
  # do(reg.dhs(hiv.positive ~ age, .data=., new.data=expand.grid(age=factor(.$age %>% unique)))) 
  do(reg.dhs(hiv.positive ~ age.group*gender, .data=., new.data=expand.grid(gender=unique(.$gender), 
                                                                            age.group=unique(.$age.group))))
                                                                      # age=factor(.$age %>% unique)))) 

hiv.prop.fit.data %>%
  ggplot(aes(age.group, group=gender, fill=gender)) +
  geom_bar(aes(y=fit), stat="identity", alpha=0.5, position="dodge") +
  geom_text(aes(y=fit, label=sprintf("%.1f", fit * 100)), position=position_dodge(width=1)) +
  # geom_errorbar(aes(ymin=fit.min, ymax=fit.max), position="dodge", width=0.5) +
  labs(y="Proportion of HIV Positive Women") +
  scale_x_discrete("Age") +
  facet_wrap(~ country.name)

partner.age.hiv.fit.data <- dhs.data %>%
  filter(!is.na(hiv.positive)) %>% 
  select(year.interview, country.code, country.name, cluster.id, caseid, age, sex.activity, hiv.positive, starts_with("age.last.sex.partner")) %>%
  gather(partner, partner.age, age.last.sex.partner.1:age.last.sex.partner.3) %>%
  filter(!is.na(hiv.positive)) %>% 
  mutate_each(funs(factor), age, hiv.positive) %>%
  mutate(caseid=paste(caseid, partner, sep="-")) %>%
  group_by(country.name) %>%
  do(reg.dhs(partner.age ~ age*hiv.positive, .data=., ignore.error=TRUE, new.data=expand.grid(age=factor(15:19), hiv.positive=factor(0:1)))) 

partner.age.hiv.fit.data %>% 
  prop.plot("Partner's Age", .group="hiv.positive") +
  scale_fill_discrete("HIV Status", labels=c("Negative", "Positive")) +
  scale_color_discrete("HIV Status", labels=c("Negative", "Positive")) 
  
dhs.data %>%
  filter(!is.na(hiv.positive)) %>% 
  select(country.name, age, sex.activity, hiv.positive, starts_with("age.last.sex.partner")) %>%
  gather(partner, partner.age, age.last.sex.partner.1:age.last.sex.partner.3) %>%
  filter(sex.activity != "inactive", !is.na(partner.age)) %>%
  ggplot(aes(factor(age), partner.age)) +
  # geom_violin(aes(color=sex.activity)) +
  geom_boxplot(aes(fill=factor(hiv.positive))) +
  labs(x="Age", y="Partner's Age", title="Ages of last three sexual partners") +
  scale_fill_discrete("HIV Status") +
  coord_cartesian(ylim=c(10, 40)) +
  facet_wrap(~ country.name, nrow=1) +
  theme(legend.position="top")

dhs.data %>%
  filter(!is.na(hiv.positive), age <= 19) %>% 
  select(country.name, age, sex.activity, hiv.positive, starts_with("age.last.sex.partner")) %>%
  gather(partner, partner.age, age.last.sex.partner.1:age.last.sex.partner.3) %>%
  filter(sex.activity != "inactive", !is.na(partner.age)) %>%
  ggplot(aes(factor(hiv.positive), partner.age)) +
  geom_boxplot(notch=FALSE, outlier.color="hotpink") +
  labs(x="HIV Status", y="Partner's Age", title="Ages of last three sexual partners (for girls 15-19 years old)") +
  scale_x_discrete(labels=c("Negative", "Positive")) +
  coord_cartesian(ylim=c(10, 40)) +
  facet_wrap(~ country.name, nrow=1) +
theme(legend.position="top")

# syph.prop.fit.data <- dhs.data %>%
#   filter(!is.na(syph.positive)) %>% 
#   mutate(age=factor(age)) %>%
#   group_by(country.name) %>%
#   do(reg.dhs(syph.positive ~ age, .data=., new.data=expand.grid(age=factor(15:19)))) 
# 
# syph.prop.fit.data %>%
#   ggplot(aes(age, fit)) +
#   geom_bar(aes(group=1), stat="identity") +
#   labs(x="Age", y="Proportion of Syphilis Positive Women") +
#   facet_wrap(~ country.name)
# 
# dhs.data %>%
#   filter(!is.na(syph.positive), age <= 19) %>% 
#   select(country.name, age, sex.activity, syph.positive, starts_with("age.last.sex.partner")) %>%
#   gather(partner, partner.age, age.last.sex.partner.1:age.last.sex.partner.3) %>%
#   filter(sex.activity != "inactive", !is.na(partner.age)) %>%
#   ggplot(aes(factor(syph.positive), partner.age)) +
#   geom_boxplot(notch=FALSE, outlier.color="hotpink") +
#   scale_x_discrete(labels=c("Negative", "Positive")) +
#   facet_wrap(~ country.name, nrow=1) +
#   theme(legend.position="top")