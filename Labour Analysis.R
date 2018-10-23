#Download the 'Combined_Data.csv' file and place in the working directory

#Set the working directory

#Calling packages after installing 
library(dplyr)

#Creating variables for analysis

yes_vector = filter(Combined_Data, Settled == "Yes")
no_vector = filter(Combined_Data, Settled == "No")
missing_complaintdate = is.na(Combined_Data$`Date of Complaint`)
missing_act = is.na(Combined_Data$Act)
missing_hearing = is.na(Combined_Data$`Date of First Hearing`)
missing_closure = is.na(Combined_Data$`Date of Closure`) & (Combined_Data$Settled == 'Yes')


#Determining fraction of missing complaint dates, fraction of missing hearing dates, fraction of missing closure dates and fraction of missing acts.

sum(missing_complaintdate) / count(Combined_Data)
sum(missing_hearing)/count(Combined_Data)
sum(missing_closure)/ count(yes_vector)
sum(missing_act)/count(Combined_Data)

#Determining fraction of complaints that took more than 15 days, number of cases with available data and average time to resolve cases.

complaint_andhearing = filter(Combined_Data, !is.na(Combined_Data$'Date of Complaint') & !is.na(Combined_Data$'Date of First Hearing'))
filtered_complaint_andhearing  = filter(complaint_andhearing, difftime(complaint_andhearing$'Date of First Hearing', complaint_andhearing$'Date of Complaint', units = c('days')) >= 0 )
complaint_hearingdiff = difftime(filtered_complaint_andhearing$'Date of First Hearing', filtered_complaint_andhearing$'Date of Complaint', units = c('days'))
more_fifteen = complaint_hearingdiff > 15
sum(more_fifteen)/length(complaint_hearingdiff)
count(complaint_andhearing)
mean(complaint_hearingdiff)


#Determining number of unsetled cases, number of cases with available data on reolution and complaint date and average time to resolve complaints.

count(no_vector)/ count(Combined_Data)
complaint_andresolution = filter(Combined_Data, !is.na(Combined_Data$'Date of Complaint') & !is.na(Combined_Data$`Date of Closure`))
filtered_complaintandres = filter(complaint_andresolution,difftime(complaint_andresolution$'Date of Closure', complaint_andresolution$'Date of Complaint', units = c('days')) >= 0 )
complaint_closurediff = difftime(filtered_complaintandres$'Date of Closure', filtered_complaintandres$'Date of Complaint', units = c('days'))
count(filtered_complaintandres)
mean(complaint_closurediff)

#Determining number of non-resolved cases and average time for which cases have remained unresolved.

complaint_nonresolve = filter(no_vector, !is.na(no_vector$`Date of Complaint`))
count(complaint_nonresolve)
x = structure(rep(as.Date("2018-07-11"), count(complaint_nonresolve) , class="Date"))
filtered_complaint_nonresolve = filter(complaint_nonresolve, difftime(x, complaint_nonresolve$`Date of Complaint`) > 0)
y = structure(rep(as.Date("2018-07-11"), count(filtered_complaint_nonresolve) , class="Date"))
complaint_nonresolvediff = difftime(y, filtered_complaint_nonresolve$`Date of Complaint`)
mean(complaint_nonresolvediff)    

#Determining fraction of missing data

available_data = filter(Combined_Data, !is.na(Combined_Data$Act) & !is.na(Combined_Data$Settled) & !is.na(Combined_Data$`Date of Complaint`) & !is.na(Combined_Data$`Date of First Hearing`) & !is.na(Combined_Data$`Date of Closure`))
1 - (count(available_data) / count(Combined_Data))