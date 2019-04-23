#First we read the historical files for Deaths and Population of Puerto Rico
data <- read_excel("FILE")

library(readxl)

# We calculate average deaths for the 2000-2016
# Then, I calculate the standard deviations
data2<-subset(data,year<2017)

# Use the following line to limit the analysis to 2010-2016
# Just remove the number sign
#data2<-subset(data2,year>2009)

#rm(data)

mean<-mean(data2$sep_dec_deaths)

sd<-sqrt(var(data2$sep_dec_deaths))

#Now I calculate the upper/lower limit of the Confidence Intervals
lb<-mean-1.96*sd
ub<-mean+1.96*sd

x<-seq(-4,4,length=100)*sd+mean
hx<-dnorm(x,mean,sd)

#Here, we input the plot parameters, title, and axist comment
plot(x,hx,xlim=c(lb-1500,ub+1500),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="Distribution of Deaths in Puerto Rico based on 2000-2016 patterns")

#Line for the distribution
lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

#Here we confirm that 95% of the area is covered by calculating shaded area
area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
area

#Now we calculate the counterfactual for three peer-reviewed publications on the topic
#First, we do the paper by Kishore et. al. (2018) - first one to come out
# They had two scenarios, for a total of six counterfactuals
kishore1<-793
kishore2<-4645
kishore3<-8498
kishore4<-1506
kishore5<-5740
kishore6<-9889

# To get at Kishore et al's counterfactual we substact their excess estimate
# from the total deaths for the period of interest
c_kishore1<-11459-kishore1
c_kishore2<-11459-kishore2
c_kishore3<-11459-kishore3
c_kishore4<-11459-kishore4
c_kishore5<-11459-kishore5
c_kishore6<-11459-kishore6

#Now we plot the counterfactual over the distribution
abline(v=c_kishore1,col="red",lty=2,lwd=2)
abline(v=c_kishore2,col="red",lwd=2)  
abline(v=c_kishore3,col="red",lty=4,lwd=2) 
abline(v=c_kishore4,col="blue",lty=2,lwd=2) 
abline(v=c_kishore5,col="blue",lwd=2) 
abline(v=c_kishore6,col="blue",lty=4,lwd=2) 

#Some fall outside the distribution, thus, we adjust the marging of the plot
#Here, we adjust the plot parameters 
plot(x,hx,xlim=c(lb-8000,ub+1500),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="Counterfactuals by Kishore et al.")

#Line for the distribution
lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

#Here we confirm that 95% of the area is covered by calculating shaded area
area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
area

#We put the lines again
#Now we plot the counterfactual over the distribution
abline(v=c_kishore1,col="red",lty=2,lwd=2)
abline(v=c_kishore2,col="red",lwd=2)  
abline(v=c_kishore3,col="red",lty=4,lwd=2) 
abline(v=c_kishore4,col="blue",lty=2,lwd=2) 
abline(v=c_kishore5,col="blue",lwd=2) 
abline(v=c_kishore6,col="blue",lty=4,lwd=2) 

# We now go to the paper by Santos and Howard (2018)
# They had one scenario
santos_howard_1<-1006
santos_howard_2<-1139
santos_howard_3<-1272

#These two are from Santos-Lozada and Howard (2019)
santos_howard_4<-1011
santos_howard_5<-1157

# Now we calculate the counterfactual
c_santos_howard_1<-11459-santos_howard_1
c_santos_howard_2<-11459-santos_howard_2
c_santos_howard_3<-11459-santos_howard_3

#These two are from Santos-Lozada and Howard (2019)
c_santos_howard_4<-11459-santos_howard_4
c_santos_howard_5<-11459-santos_howard_5

#We put a fresh plot
#Here, we adjust the plot parameters 
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="Counterfactual by Santos & Howard (2018)")

#Line for the distribution
lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

#Here we confirm that 95% of the area is covered by calculating shaded area
area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
area

#We put the lines again
#Now we plot the counterfactual over the distribution
abline(v=c_santos_howard_1,col="red",lty=2,lwd=2)
abline(v=c_santos_howard_2,col="red",lwd=2)  
abline(v=c_santos_howard_3,col="red",lty=2,lwd=2) 
abline(v=c_santos_howard_3,col="red",lty=2,lwd=2)
abline(v=c_santos_howard_4,col="blue",lwd=2) #Reply letter
abline(v=c_santos_howard_5,col="darkgreen",lwd=2) #Reply letter

# GWU Counterfactuals
# GWU Census Model
Santos_Burgoa1<-993
Santos_Burgoa2<-1237
Santos_Burgoa3<-1471

# Displacement model
Santos_Burgoa4<-1872
Santos_Burgoa5<-2098
Santos_Burgoa6<-2315

#Now we calculate the counterfactual
c_Santos_Burgoa1<-11459-Santos_Burgoa1
c_Santos_Burgoa2<-11459-Santos_Burgoa2
c_Santos_Burgoa3<-11459-Santos_Burgoa3

c_Santos_Burgoa4<-11459-Santos_Burgoa4
c_Santos_Burgoa5<-11459-Santos_Burgoa5
c_Santos_Burgoa6<-11459-Santos_Burgoa6

# We get a fresh plot for this counterfactual
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="Counterfactual by Santos-Burgoa et. al. (2018)")

lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

#Area under the curve
area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
area

# Putting the counterfactual
abline(v=c_Santos_Burgoa1,col="red",lty=2,lwd=2)
abline(v=c_Santos_Burgoa2,col="red",lwd=2)  
abline(v=c_Santos_Burgoa3,col="red",lty=4,lwd=2) 
abline(v=c_Santos_Burgoa4,col="blue",lty=2,lwd=2)
abline(v=c_Santos_Burgoa5,col="blue",lwd=2)  
abline(v=c_Santos_Burgoa6,col="blue",lty=4,lwd=2)

###Cruz Cano
#Now we calculate the counterfactual
Cruz_Cano1<-707
Cruz_Cano2<-1205
Cruz_Cano3<-1702

c_Cruz_Cano1<-11459-Cruz_Cano1
c_Cruz_Cano2<-11459-Cruz_Cano2
c_Cruz_Cano3<-11459-Cruz_Cano3

# We get a fresh plot for this counterfactual
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="Counterfactual by Cruz-Cano and Mead (2019)")

lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

#Area under the curve
area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
area

# Putting the counterfactual
abline(v=c_Cruz_Cano1,col="red",lty=2,lwd=2)
abline(v=c_Cruz_Cano2,col="red",lwd=2)  
abline(v=c_Cruz_Cano3,col="red",lty=4,lwd=2) 

###################
### Final Figure###
###################

par(mfrow=c(3,2))

#Put graphs together
#Here, we input the plot parameters, title, and axist comment
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="A. Distribution of Deaths in Puerto Rico based on 2000-2016 patterns")

#Line for the distribution
lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")
abline(v=10037,col="red3",lty=2,lwd=2) #2010 period
abline(v=9847,col="blue",lty=2,lwd=2) #2011 period
abline(v=9903,col="black",lty=2,lwd=2)  #2012 period
abline(v=9702,col="orange",lty=2,lwd=2) #2013
abline(v=10931,col="darkgreen",lty=2,lwd=2) #2014
abline(v=9438,col="paleturquoise4",lty=2,lwd=2) #2015
abline(v=10063,col="purple",lty=2,lwd=2) #2016


legend(8000,1.015158e-03,legend=c("2010", "2011", "2012", "2013","2014 (Chikungunya)","2015","2016"),
       col=c("red3", "blue","black","orange","darkgreen","paleturquoise4","purple"),bg="transparent", box.lty=0,lty=c(2,2,2,2,2,2,2), cex=0.8)


# Kishore et al
plot(x,hx,xlim=c(lb-8000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="B. Counterfactuals by Kishore et al. (2018)")

#Line for the distribution
lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")
abline(v=c_kishore1,col="red",lty=2,lwd=2)
abline(v=c_kishore2,col="red",lwd=2)  
abline(v=c_kishore3,col="red",lty=2,lwd=2) 
abline(v=c_kishore4,col="blue",lty=2,lwd=2) 
abline(v=c_kishore5,col="blue",lwd=2) 
abline(v=c_kishore6,col="blue",lty=2,lwd=2) 

legend(2900,1.015158e-03,legend=c("Unasjusted", "Adjusted", "95% CI: Unasjusted", "95% CI: Adjusted"),
       col=c("red", "blue","red","blue"),bg="transparent", box.lty=0,lty=c(1,1,2,2), cex=0.8)

# Santos and Howard
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="C. Counterfactuals by Santos-Lozada and Howard (2018, 2019)")

#Line for the distribution
lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

#We put the lines again
#Now we plot the counterfactual over the distribution
abline(v=c_santos_howard_1,col="red",lty=2,lwd=2)
abline(v=c_santos_howard_2,col="red",lwd=2)  
abline(v=c_santos_howard_3,col="red",lty=2,lwd=2) 
abline(v=c_santos_howard_3,col="red",lty=2,lwd=2)
abline(v=c_santos_howard_4,col="blue",lwd=2)
abline(v=c_santos_howard_5,col="darkgreen",lwd=2)

legend(8000,1.015158e-03,legend=c("Epidemiologic Threshold Method", "95% CI: Coverage Error","Regression 1","Regression 2"),
       col=c("red", "red","blue","darkgreen"),bg="transparent", box.lty=0,lty=c(1,2,1,1), cex=0.8)

# Santos-Burgoa et al. 
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="D. Counterfactuals by Santos-Burgoa et. al. (2018)")

lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

# Putting the counterfactual
abline(v=c_Santos_Burgoa1,col="red",lty=2,lwd=2)
abline(v=c_Santos_Burgoa2,col="red",lwd=2)  
abline(v=c_Santos_Burgoa3,col="red",lty=2,lwd=2) 
abline(v=c_Santos_Burgoa4,col="blue",lty=2,lwd=2)
abline(v=c_Santos_Burgoa5,col="blue",lwd=2)  
abline(v=c_Santos_Burgoa6,col="blue",lty=2,lwd=2)

legend(8000,1.015158e-03,legend=c("Census model", "Displacement model", "95% CI: Census", "95% CI: Displacement"),
       col=c("red", "blue","red","blue"),bg="transparent", box.lty=0,lty=c(1,1,2,2), cex=0.8)


# We get a fresh plot for this counterfactual
plot(x,hx,xlim=c(lb-1000,ub+1000),type="n",xlab="95% Confidence Intervals of the Mean in Gray",ylab="",yaxt='n',
     main="E. Counterfactual by Cruz-Cano and Mead (2019)")

lines(x,hx)

i<-x>=lb & x <=ub
polygon(c(lb,x[i],ub),c(0,hx[i],0),col="grey89")

#Area under the curve
area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
area

# Putting the counterfactual
abline(v=c_Cruz_Cano1,col="red",lty=2,lwd=2)
abline(v=c_Cruz_Cano2,col="red",lwd=2)  
abline(v=c_Cruz_Cano3,col="red",lty=2,lwd=2) 

legend(8000,1.015158e-03,legend=c("Time-Series Model", "95% CI"),
       col=c("red", "red"),bg="transparent", box.lty=0,lty=c(1,2), cex=0.8)