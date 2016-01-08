#Bring in Q/All.Projects_by_species/aa_Spapefiles_Maps/aa_QGIS Projects/AlpinePhenologyproject/
R_input_data/QR_r_input.csv
bloom<-read.csv(file.choose("Q:\\Research\\All_Projects_by_Species\\"),        
                header = TRUE, as.is=TRUE)        
head(bloom)
names(bloom)
str(bloom)  #tells me column name, data type and examples
mtext("Temperature (C)", side=3, line=2, col=2)#adds secondary x-axis label

#Climate data figures
#This section runs linear regression on Avg High temp by Year
hist(bloom$Avg_Hi)
plot(bloom$Year,bloom$Avg_Hi, main="Average High Temperature by Year",xlab="Year", ylab="Degrees Celsius")
abline(lm(bloom$Avg_Hi~bloom$Year))
summary(lm(bloom$Avg_Hi~bloom$Year))
plot(lm(bloom$Avg_Hi~bloom$Year)) #check residuals for heteroscedasticity and nonlinearity

#This section runs linear regression on Avg Low temp by Year
hist(bloom$Avg_Lo)
plot(bloom$Year,bloom$Avg_Lo, main="Average Low Temperature by Year",xlab="Year", ylab="Degrees Celsius")
abline(lm(bloom$Avg_Lo~bloom$Year))
summary(lm(bloom$Avg_Lo~bloom$Year))
plot(lm(bloom$Avg_Lo~bloom$Year)) #check residuals for heteroscedasticity and nonlinearity


#This section runs linear regression on Precipitation by Year
hist(bloom$Raw_Precip)
plot(bloom$Year,bloom$Raw_Precip, main="Precipitation by Year",xlab="Year", ylab="Precipitation (mm)")
abline(lm(bloom$Raw_Precip~bloom$Year))
summary(lm(bloom$Raw_Precip~bloom$Year))
plot(lm(bloom$Raw_Precip ~bloom$Year)) #check residuals for heteroscedasticity and nonlinearity

#Graph of all spp data by year
hist(bloom$startDayOfYear)
plot(bloom$Year,bloom$startDayOfYear, main="First Collection Date for All Species",xlab="Year", ylab="Julian Date")
abline(lm(bloom$startDayOfYear~bloom$Year))
summary(lm(bloom$startDayOfYear~bloom$Year))
plot(lm(bloom$Raw_Precip ~bloom$Year)) #check residuals for heteroscedasticity and nonlinearity


