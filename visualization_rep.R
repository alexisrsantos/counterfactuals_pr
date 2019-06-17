
#Library to read historical data
library(readxl)

#First we read the historical files for Deaths and Population of Puerto Rico
data <- read_excel("FILE")

#Exclude 2017
data2<-subset(data,year<2017)

# Limit the analysis to 2010-2016
data2<-subset(data2,year>2009)

#Observed deaths for 2017
observed_deaths<-11459

# We calculate average deaths for the 2010-2016
mean<-mean(data2$sep_dec_deaths)

# We calculate the standard deviations
sd<-sqrt(var(data2$sep_dec_deaths))

#Now I calculate the upper/lower limit of the Confidence Intervals
lb<-mean-1.96*sd
ub<-mean+1.96*sd

#Normal distribution generation
x<-seq(-4,4,length=100)*sd+mean
hx<-dnorm(x,mean,sd)

#Now we calculate the counterfactual for three peer-reviewed publications on the topic
#First, we do the paper by Kishore et. al. (2018) - first one to come out
# They had two scenarios, for a total of six counterfactuals

# Unadjusted estimate
kishore1<-793
kishore2<-4645
kishore3<-8498

#Adjusted Model
kishore4<-1506
kishore5<-5740
kishore6<-9889

# To get at Kishore et al's counterfactual we substact their excess estimate
# from the total deaths for the period of interest
c_kishore1<-observed_deaths-kishore1
c_kishore2<-observed_deaths-kishore2
c_kishore3<-observed_deaths-kishore3
c_kishore4<-observed_deaths-kishore4
c_kishore5<-observed_deaths-kishore5
c_kishore6<-observed_deaths-kishore6

# We now go to the paper by Santos and Howard (2018)
# They had one scenario and two estimates from a Letter to the Editor
# Thus, they have 5 counterfactuals
santos_howard_1<-1006
santos_howard_2<-1139
santos_howard_3<-1272

# Now we calculate the counterfactual
c_santos_howard_1<-observed_deaths-santos_howard_1
c_santos_howard_2<-observed_deaths-santos_howard_2
c_santos_howard_3<-observed_deaths-santos_howard_3

# We obtain data from Santos-Burgoa and colleagues (2018), two scenarios
# for a total of 6 counterfactuals 
# Census Model
Santos_Burgoa1<-993
Santos_Burgoa2<-1237
Santos_Burgoa3<-1471

# Displacement model
Santos_Burgoa4<-1872
Santos_Burgoa5<-2098
Santos_Burgoa6<-2315

#Now we calculate the counterfactual
c_Santos_Burgoa1<-observed_deaths-Santos_Burgoa1
c_Santos_Burgoa2<-observed_deaths-Santos_Burgoa2
c_Santos_Burgoa3<-observed_deaths-Santos_Burgoa3

c_Santos_Burgoa4<-observed_deaths-Santos_Burgoa4
c_Santos_Burgoa5<-observed_deaths-Santos_Burgoa5
c_Santos_Burgoa6<-observed_deaths-Santos_Burgoa6

###We obtain the data from Cruz-Cano and Mead 
Cruz_Cano1<-707
Cruz_Cano2<-1205
Cruz_Cano3<-1702

#Now we calculate the counterfactual
c_Cruz_Cano1<-observed_deaths-Cruz_Cano1
c_Cruz_Cano2<-observed_deaths-Cruz_Cano2
c_Cruz_Cano3<-observed_deaths-Cruz_Cano3

#Rivera Rolke 2009
Rivera_Rolke1<-1069
Rivera_Rolke2<-1318
Rivera_Rolke3<-1568

#Now we calculate the counterfactual
c_Rivera_Rolke1<-observed_deaths-1069
c_Rivera_Rolke2<-observed_deaths-1318
c_Rivera_Rolke3<-observed_deaths-1568

###################
### Final Figure###
###################

#Five graphs in one image
par(mfrow=c(3,2))

#Put graphs together
#Here, we input the plot parameters, title, and axist comment
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="A. Distribution of Deaths in Puerto Rico based on 2010-2016 patterns")

#Line for the distribution
lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

#Put death counts 2010-2016 within the distribution
abline(v=10037,col="red3",lty=1,lwd=2.5) #2010 
abline(v=9847,col="blue",lty=1,lwd=2.5) #2011 
abline(v=9903,col="black",lty=1,lwd=2.5)  #2012 
abline(v=9702,col="orange",lty=1,lwd=2.5) #2013
abline(v=10931,col="darkgreen",lty=1,lwd=2.5) #2014
abline(v=9438,col="pink",lty=1,lwd=2.5) #2015
abline(v=10063,col="purple",lty=1,lwd=2.5) #2016

legend(8000,hx[50],legend=c("2010", "2011", "2012", "2013","2014 (Chikungunya)","2015","2016"),
       col=c("red3", "blue","black","orange","darkgreen","pink","purple"),bg="transparent", box.lty=0,lty=c(1,1,1,1,1,1,1), cex=1)

# Plot for Kishore et al
plot(x,hx,xlim=c(lb-8000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="B. Counterfactuals by Kishore et al. (2018)")

#Line for the distribution
lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

#Include the counterfactuals in the plot
abline(v=c_kishore1,col="red",lty=2,lwd=2)
abline(v=c_kishore2,col="red",lwd=2)  
abline(v=c_kishore3,col="red",lty=2,lwd=2) 
abline(v=c_kishore4,col="blue",lty=2,lwd=2) 
abline(v=c_kishore5,col="blue",lwd=2) 
abline(v=c_kishore6,col="blue",lty=2,lwd=2) 

legend(1000,hx[50],legend=c("Unasjusted", "Adjusted", "95% CI: Unasjusted", "95% CI: Adjusted"),
       col=c("red", "blue","red","blue"),bg="white", box.lty=0,lty=c(1,1,2,2), cex=1)

# Santos and Howard
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="C. Counterfactuals by Santos-Lozada and Howard (2018, 2019)")

#Line for the distribution
lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

#Include the counterfatuals in the plot
abline(v=c_santos_howard_1,col="red",lty=2,lwd=2)
abline(v=c_santos_howard_2,col="red",lty=1,lwd=2)  
abline(v=c_santos_howard_3,col="red",lty=2,lwd=2) 

legend(8000,hx[50],legend=c("Epidemiologic Threshold Method", "95% CI: Coverage Error"),
       col=c("red", "red"),bg="transparent", box.lty=0,lty=c(1,2,1,1), cex=1)

# Santos-Burgoa et al. 
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="D. Counterfactuals by Santos-Burgoa et. al. (2018)")

lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

# Include the counterfactual in the plot
abline(v=c_Santos_Burgoa1,col="red",lty=2,lwd=2)
abline(v=c_Santos_Burgoa2,col="red",lwd=2)  
abline(v=c_Santos_Burgoa3,col="red",lty=2,lwd=2) 
abline(v=c_Santos_Burgoa4,col="blue",lty=2,lwd=2)
abline(v=c_Santos_Burgoa5,col="blue",lwd=2)  
abline(v=c_Santos_Burgoa6,col="blue",lty=2,lwd=2)

legend(8000,hx[50],legend=c("Census model", "Displacement model", "95% CI: Census", "95% CI: Displacement"),
       col=c("red", "blue","red","blue"),bg="transparent", box.lty=0,lty=c(1,1,2,2), cex=1)

# Counterfactuals by Cruz-Cano and Mead
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="E. Counterfactual by Cruz-Cano and Mead (2019)")

lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

# Include the counterfactual in the plot
abline(v=c_Cruz_Cano1,col="red",lty=2,lwd=2)
abline(v=c_Cruz_Cano2,col="red",lwd=2)  
abline(v=c_Cruz_Cano3,col="red",lty=2,lwd=2) 

legend(8000,hx[50],legend=c("Time-Series Model", "95% CI"),
       col=c("red", "red"),bg="transparent", box.lty=0,lty=c(1,2), cex=1)

# Counterfactuals by Rivera and Rolke
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="F. Counterfactual by Rivera and Rolke (2019)")

lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

# Include the counterfactual in the plot
abline(v=c_Rivera_Rolke1,col="red",lty=2,lwd=2)
abline(v=c_Rivera_Rolke2,col="red",lwd=2)  
abline(v=c_Rivera_Rolke3,col="red",lty=2,lwd=2) 

legend(8000,hx[50],legend=c("Posterior Simulation", "95% CI"),
       col=c("red", "red"),bg="transparent", box.lty=0,lty=c(1,2), cex=1)
