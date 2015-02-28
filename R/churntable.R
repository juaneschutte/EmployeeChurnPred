churntable <- function(input){
  require(rCharts, quietly=TRUE)
  newdata_update <- select(newdata, Employee.number, Department, Critical.Role, Months.till.resignation)
  newdata_update <- arrange(newdata_update, Months.till.resignation)
  dt <- dTable(newdata_update, sPaginationType= "full_numbers")
  dt 
}