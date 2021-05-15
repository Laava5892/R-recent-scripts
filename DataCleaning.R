getwd()
dir <- "/Users/laavanyaganesh/Desktop/LQD/"
setwd(dir)
`Bank.Training.(1)` <- read.csv("~/Desktop/LQD/Bank-Training (1).csv")
View(`Bank.Training.(1)`)
X = `Bank.Training.(1)`
X$y = as.character(X$y)
X$y[X$y == "no"] <- "0"
X$y[X$y == "yes"] <- "1"
X$job = as.character(X$job)
X$job[X$job == "admin."] <- "1"
X$job[X$job == "blue-collar"] <- "2"
X$job[X$job == "entrepreneur"] <- "3"
X$job[X$job == "housemaid"] <- "4"
X$job[X$job == "management"] <- "5"
X$job[X$job == "retired"] <- "6"
X$job[X$job == "self-employed"] <- "7"
X$job[X$job == "services"] <- "8"
X$job[X$job == "student"] <- "9"
X$job[X$job == "technician"] <- "10"
X$job[X$job == "unemployed"] <- "11"
X$job[X$job == "unknown"] <- "12"
X$marital = as.character(X$marital)
X$marital[X$marital == "divorced"] <- "1"
X$marital[X$marital == "married"] <- "2"
X$marital[X$marital == "single"] <- "3"
X$marital[X$marital == "unknown"] <- "4"
X$education = as.character(X$education)
X$education[X$education == "basic.4y"] <- "1"
X$education[X$education == "basic.6y"] <- "2"
X$education[X$education == "basic.9y"] <- "3"
X$education[X$education == "high.school"] <- "4"
X$education[X$education == "illiterate"] <- "5"
X$education[X$education == "professional.course"] <- "6"
X$education[X$education == "university.degree"] <- "7"
X$education[X$education == "unknown"] <- "8"
X$default = as.character(X$default)
X$default[X$default == "yes"] <- "1"
X$default[X$default == "no"] <- "0"
X$default[X$default == "unknown"] <- " "
X$housing = as.character(X$housing)
X$housing[X$housing == "yes"] <- "1"
X$housing[X$housing == "no"] <- "0"
X$housing[X$housing == "unknown"] <- " "
X$loan = as.character(X$loan)
X$loan[X$loan == "yes"] <- "1"
X$loan[X$loan == "no"] <- "0"
X$loan[X$loan == "unknown"] <- " "
X$contact = as.character(X$contact)
X$contact[X$contact == "cellular"] <- "1"
X$contact[X$contact == "telephone"] <- "2"
X$month = as.character(X$month)
X$month[X$month == "jan"] <- "1"
X$month[X$month == "feb"] <- "2"
X$month[X$month == "mar"] <- "3"
X$month[X$month == "apr"] <- "4"
X$month[X$month == "may"] <- "5"
X$month[X$month == "jun"] <- "6"
X$month[X$month == "jul"] <- "7"
X$month[X$month == "aug"] <- "8"
X$month[X$month == "sep"] <- "9"
X$month[X$month == "oct"] <- "10"
X$month[X$month == "nov"] <- "11"
X$month[X$month == "dec"] <- "12"
X$day_of_week = as.character(X$day_of_week)
X$day_of_week[X$day_of_week == "mon"] <- "1"
X$day_of_week[X$day_of_week == "tue"] <- "2"
X$day_of_week[X$day_of_week == "wed"] <- "3"
X$day_of_week[X$day_of_week == "thu"] <- "4"
X$day_of_week[X$day_of_week == "fri"] <- "5"
X$poutcome = as.character(X$poutcome)
X$poutcome[X$poutcome == "failure"] <- "1"
X$poutcome[X$poutcome == "nonexistent"] <- "2"
X$poutcome[X$poutcome == "success"] <- "3"
View(X)
X$job_admin. <- ifelse(X$job==1, "1", "0")
X$job_blue_collar <- ifelse(X$job==2, "1", "0")
X$job_entrepreneur <- ifelse(X$job==3, "1", "0")
X$job_housemaid <- ifelse(X$job==4, "1", "0")
X$job_management <- ifelse(X$job==5, "1", "0")
X$job_retired <- ifelse(X$job==6, "1", "0")
X$job_self_employed <- ifelse(X$job==7, "1", "0")
X$job_services <- ifelse(X$job==8, "1", "0")
X$job_student <- ifelse(X$job==9, "1", "0")
X$job_technician <- ifelse(X$job==10, "1", "0")
X$job_unemployed <- ifelse(X$job==11, "1", "0")
X$job_unknown <- ifelse(X$job==12, "1", "0")
X$marital_divorced <- ifelse(X$marital==1, "1", "0")
X$marital_married <- ifelse(X$marital==2, "1", "0")
X$marital_single <- ifelse(X$marital==3, "1", "0")
X$marital_unknown <- ifelse(X$marital==4, "1", "0")
X$education_basic.4y <- ifelse(X$education==1, "1", "0")
X$education_basic.6y <- ifelse(X$education==2, "1", "0")
X$education_basic.9y <- ifelse(X$education==3, "1", "0")
X$education_high.school <- ifelse(X$education==4, "1", "0")
X$education_illiterate <- ifelse(X$education==5, "1", "0")
X$education_professional.course <- ifelse(X$education==6, "1", "0")
X$education_university.degree <- ifelse(X$education==7, "1", "0")
X$education_unknown <- ifelse(X$education==8, "1", "0")
X$contact_cellular <- ifelse(X$contact==1, "1", "0")
X$contact_telephone <- ifelse(X$contact==2, "1", "0")
X$month_jan <- ifelse(X$month==1, "1", "0")
X$month_feb <- ifelse(X$month==2, "1", "0")
X$month_mar <- ifelse(X$month==3, "1", "0")
X$month_apr <- ifelse(X$month==4, "1", "0")
X$month_may <- ifelse(X$month==5, "1", "0")
X$month_jun <- ifelse(X$month==6, "1", "0")
X$month_jul <- ifelse(X$month==7, "1", "0")
X$month_aug <- ifelse(X$month==8, "1", "0")
X$month_sep <- ifelse(X$month==9, "1", "0")
X$month_oct <- ifelse(X$month==10, "1", "0")
X$month_nov <- ifelse(X$month==11, "1", "0")
X$month_dec <- ifelse(X$month==12, "1", "0")
X$day_of_week_mon <- ifelse(X$day_of_week==1, "1", "0")
X$day_of_week_tue <- ifelse(X$day_of_week==2, "1", "0")
X$day_of_week_wed <- ifelse(X$day_of_week==3, "1", "0")
X$day_of_week_thu <- ifelse(X$day_of_week==4, "1", "0")
X$day_of_week_fri <- ifelse(X$day_of_week==5, "1", "0")
X$poutcome_failure <- ifelse(X$poutcome==1, "1", "0")
X$poutcome_nonexistent <- ifelse(X$poutcome==2, "1", "0")
X$poutcome_success <- ifelse(X$poutcome==3, "1", "0")
View(X)
bank <- data.frame(X$age, X$job_admin., X$job_retired, X$job_student, X$job_unknown, X$job_services, X$job_housemaid, X$job_management, X$job_technician, X$job_unemployed, X$job_entrepreneur, X$marital_single, X$marital_married, X$marital_unknown, X$marital_divorced, X$education_unknown, X$education_basic.4y, X$education_basic.6y, X$education_basic.9y, X$education_illiterate, X$education_high.school, X$education_university.degree, X$education_professional.course, X$default, X$housing, X$loan, X$contact_cellular, X$contact_telephone, X$month_jan, X$month_feb, X$month_mar, X$month_apr, X$month_may, X$month_jun, X$month_jul, X$month_aug, X$month_sep, X$month_oct, X$month_nov, X$month_dec, X$day_of_week_mon, X$day_of_week_tue, X$day_of_week_wed, X$day_of_week_thu, X$day_of_week_fri, X$duration, X$campaign, X$pdays, X$previous, X$poutcome_failure, X$poutcome_success, X$poutcome_nonexistent, X$emp.var.rate, X$cons.price.idx, X$cons.conf.idx, X$euribor3m, X$nr.employed,X$y)
names(bank) <- c("age", "job_admin", "job_retired", "job_student", "job_unknown", "job_services", "job_housemaid", "job_management", "job_technician", "job_unemployed", "job_entrepreneur", "marital_single", "marital_married", "marital_unknown", "marital_divorced", "education_unknown", "education_basic.4y", "education_basic.6y", "education_basic.9y", "education_illiterate", "education_high.school", "education_university.degree","education_professional.course", "default", "housing", "loan", "contact_cellular", "contact_telephone", "month_jan", "month_feb", "month_mar", "month_apr", "month_may", "month_jun", "month_jul", "month_aug", "month_sep", "month_oct", "month_nov", "month_dec", "day_of_week_mon", "day_of_week_tue", "day_of_week_wed", "day_of_week_thu", "day_of_week_fri", "duration", "campaign", "pdays", "previous", "poutcome_failure", "poutcome_success", "poutcome_nonexistent", "emp.var.rate", "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed", "y" )
View(bank)
write.csv(bank, file = "bank.csv")
