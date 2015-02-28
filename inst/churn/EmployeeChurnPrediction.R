#Forked from:
#http://practicalrvideos.blogspot.com/2013/08/survival-analysis-in-r-weibull-and-cox.html

#the three best references that I've found!
#http://cran.r-project.org/web/views/Survival.html
#http://www.ats.ucla.edu/stat/r/examples/alda/ch14.htm
#http://cran.r-project.org/doc/contrib/Fox-Companion/appendix-cox-regression.pdf


#use survival analysis when you want to know how long it takes for an "event"
#to occur:
#failure of a machine, removal of a tree, time to find employment


setwd("/Users/juaneshutte/Documents/ZOOM/Survival Analysis/")
data = read.csv("EmployeeChurndata.csv", sep=";")

data[is.na(data)] <- 0
attach(data)
head(data)

require(survival)

tree_surv = Surv(Tenure, Resigned)
head(tree_surv)



########################### testing assumptions #######################################
#Big assumption: does the removal time follow a Weibull distribution?
uncensored = subset(data, Resigned == 1)
head(uncensored)
plot(density(as.numeric(uncensored$Tenure)))
library(MASS)
fitdistr(uncensored$Tenure, "weibull")
#shape = 1.49148917, scale = 144.55829331
x = 0:400
w = dweibull(x, shape = 1.93, scale = 26.04)
lines(w, lty = 2)

#QQ plot
x = rweibull(n=nrow(uncensored),shape=1.93, scale=26.04) 
qqplot(x, uncensored$Tenure, xlab = "Weibull theoretical quantiles", 
       ylab = "Data quantiles", main = expression(italic(T[i] == (L[i]+R[i])/2)))
abline(0,1)
#######################################################################################








#################### Weibull accelerated failure time model ###########################
#parametric survival models are estimated with survreg() function
weibull = survreg(tree_surv ~ as.factor(Levinson.career.stage) + as.factor(Gender)       + as.factor(Race)       
                + as.factor(Department)         + as.factor(Marital.status)        + as.factor(Education)            
                + as.factor(Location)        + as.factor(Willingness.to.relocate)            + as.numeric(Months.since.lateral.Promotion)
                + as.numeric(Months.since.Vertical.Promotion)          + as.numeric(Self.perf)     + as.numeric(Supervisor.report.perf)             
                + as.factor(Salary.band)               + as.numeric(Absences.in.1.year)          + as.factor(Job.Level)              
                + as.factor(Supervisor)           + as.numeric(Number.of.courses.completed.within.effective.date)               + as.numeric(Number.of.elective.courses.relevant.or.irrelevant)                     
                 + as.factor(Education.reimbursement)                 + as.numeric(Job.Portal.clicks.last.3.months)                             
                 + as.factor(Critical.Role)   + as.factor(Stressful.Role) 
                 + as.factor(Job.satisfaction)             + as.factor(Growth.satisfaction)    + as.factor(Satisfaction.with.pay)              
                 + as.factor(Perception.of.promotion.opp)               + as.factor(Percieved.fairness)          + as.factor(Perception.of.work.life.balance)                
                 + as.factor(Organisational.commitment), 
                  dist = "weibull")
summary(weibull)


#predicted survival time for the first tree with condfidence intervals
predict = predict(weibull, newdata = data, type='response', se=TRUE)

head(predict$fit)
head(predict$se)

save(weibull, file="/Users/juaneshutte/Tech/Git/EmployeeChurnPred/data/weibull.rda")


head(churn(input="EmployeeChurndata_test.csv"))









newdata = read.csv("EmployeeChurndata_test.csv", sep=";", header=T)
stopifnot("Employee.number" %in% names(newdata))
stopifnot("Levinson.career.stage" %in% names(newdata))
stopifnot("Gender" %in% names(newdata))
stopifnot("Race" %in% names(newdata))
stopifnot("Department" %in% names(newdata))
stopifnot("Marital.status" %in% names(newdata))
stopifnot("Education" %in% names(newdata))
stopifnot("Location" %in% names(newdata))
stopifnot("Willingness.to.relocate" %in% names(newdata))
stopifnot("Months.since.lateral.Promotion" %in% names(newdata))
stopifnot("Months.since.Vertical.Promotion" %in% names(newdata))
stopifnot("Self.perf" %in% names(newdata))
stopifnot("Supervisor.report.perf" %in% names(newdata))
stopifnot("Salary.band" %in% names(newdata))
stopifnot("Absences.in.1.year" %in% names(newdata))
stopifnot("Job.Level" %in% names(newdata))
stopifnot("Supervisor" %in% names(newdata))
stopifnot("Number.of.courses.completed.within.effective.date" %in% names(newdata))
stopifnot("Number.of.elective.courses.relevant.or.irrelevant" %in% names(newdata))
stopifnot("Education.reimbursement" %in% names(newdata))
stopifnot("Job.Portal.clicks.last.3.months" %in% names(newdata))
stopifnot("Critical.Role" %in% names(newdata))
stopifnot("Stressful.Role" %in% names(newdata))
stopifnot("Job.satisfaction" %in% names(newdata))
stopifnot("Growth.satisfaction" %in% names(newdata))
stopifnot("Satisfaction.with.pay" %in% names(newdata))
stopifnot("Perception.of.promotion.opp" %in% names(newdata))
stopifnot("Percieved.fairness" %in% names(newdata))
stopifnot("Perception.of.work.life.balance" %in% names(newdata))
stopifnot("Organisational.commitment" %in% names(newdata))
stopifnot("Current.tenure" %in% names(newdata))

to.drop <- c("Employee.number","Current tenure")
newdata[is.na(newdata)] <- 0
feat.space <- newdata[,!(names(newdata) %in% to.drop)]

#safety_model is included with the package
require(survival, quietly=TRUE)
require(dplyr, quietly=TRUE)
newdata$Tenure <- predict(weibull, newdata = feat.space, type='response')
newdata$Months.till.resignation <- newdata$Tenure - newdata$Current.tenure
newdata <- arrange(newdata, Months.till.resignation)
save(newdata, file="/Users/juaneshutte/Tech/Git/EmployeeChurnPred/data/newdata.rda")
