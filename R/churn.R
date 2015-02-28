churn <- function(input){
  #input can either be csv file or data    
  newdata <- if(is.character(input) && file.exists(input)){
    read.csv(input, sep=";",header=T)
  } else {
    as.data.frame(input)
  }
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
  return(newdata)
  
}