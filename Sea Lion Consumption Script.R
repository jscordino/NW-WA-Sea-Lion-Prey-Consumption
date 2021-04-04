##### Prey consumption models for Steller and Califonria sea lions as presented 
##### in Scordino et al. "Dietary niche overlap and prey consumption estimates 
##### for Steller sea lion and California sea lion in northwest Washington, 
##### 2010-2013". In submission. Fishery Bulletin.

### Packages
library(ggplot2)
#install.packages("truncnorm")
library(truncnorm)
#install.packages("ANOVAreplication")
library(ANOVAreplication)

# Throughout this code EJ stands for Eumetopias jubatus (Steller sea lions) and
# ZC stands for Zalophus californianus (California sea lions)

###### COnsumption estimate for Steller sea lions ########

# Percent of body weight consumed based on body weight estimates  
# and sex are from Winship et al. 2006.
EJFLarge <- rnorm(1,0.05,0.01)
EJFMed <- runif(1,0.06,0.07)
EJFMedSm <- runif(1,0.07,0.08)
EJFSmall <- runif(1,0.08,0.09)

EJMLarge <- runif(1,0.03,0.04)
EJMMed <- rnorm(1,0.04,0.01)
EJMMedSm <- runif(1,0.05,0.06)
EJMSmall <- runif(1,0.06,0.07)
EJMVSmall <- runif(1,0.08,0.09)

# Formulas for estimating the daily consumption of a randomly drawn juvenile female
# juvenile male, adult female, and adult male using the consumption of body weight 
# parameters defined above and the uniform distribution of reported weight at age 
# for each age and sex class as reported in Winship et al. 2001. Juvenile weights 
# are for sea lions ages 1 to 5. Juvenile females use non-pregnant 5 year old weight
# as the maximum weight for the category. Adults use age classes of 5 up to maximum 
# growth from Winship et al. 2001.

IndDailyConsumptionJuvF<- function() 
{
  JuvF <- runif(1,89,195)
  if(JuvF > 194){JuvF*EJFMed}
  if(JuvF > 152){JuvF*EJFMedSm}
  if(JuvF > 89){JuvF*EJFSmall}
}


IndDailyConsumptionAdF<- function() 
{
  AdFemale <- runif(1,195,305)
  if(AdFemale > 223){AdFemale*EJFLarge}
  if(AdFemale > 195){AdFemale*EJFMed}
}

IndDailyConsumptionJuvM<- function() 
{
  JuvM <- runif(1,125,286)
  if(JuvM > 182){JuvM*EJMSmall}
  if(JuvM > 125){JuvM*EJMVSmall}
}

IndDailyConsumptionAdM<- function() 
{
  AdMale <- runif(1,286,681)
  if(AdMale > 623){AdMale*EJMLarge}
  if(AdMale > 455){AdMale*EJMMed}
  if(AdMale > 310){AdMale*EJMMedSm}
  if(AdMale > 286) {AdMale*EJMSmall}
}



# Counts are from surveys conducted from 2010 through 2013. I only included surveys that
# had all haulouts surveyed in that day. For a few surveys I pulled EBOD counts into a 
# day that otherwise had an incomplete count. Raw data for the counts are available from
# Scordino and Akmajian 2021

# Total Non-pups by season, sample size (n), and negative binomial distribution size		
# Season	  Mean	  Size    n
# Fall	    825.11	6.38    9
# Spring	  644.69	18.87   13
# Summer	  744.47	68.93   15
# Winter	  539.00	24.27   4

# I used data from supplemental table 2 from Wright et al. 2017 to calculate
# expected ratio of females to males for juvenile Steller sea lions rather than
# assuming a 50:50 sex ratio. I used data for cohorts from 2001 through 2007 since they all
# had survival rates through age 5. I then averaged the survivorship at age for
# age 1 through age 5 for each cohort by sex and used the ratio of the two to inform what percent
# of juveniles were female and male.

# Correction factors from Olesiuk 2018 were used assuming that the correction factor for
# fall and spring have the same CF as caculated for winter. This assumption is supported
# by the findings of Whitlock et al. 2020 of very low attendance in May and by the end of August that
# were similar to Olesiuk's estimate for winter CF. Overall the correction factors from
# Olesiuk 2018 were similar to the estimates of Whitlock et al. 2020. These correction 
# factors were for Steller sea lions.

# The correction factor for winter, spring, and fall was informed by the forumula 
# rnorm(1,2.778,0.0583)
# A separate correction factor was calculated for summer with the formula 
# rnorm(1,1.48,0.0831)

# Count data from Scordino and Akmajian (2021) were used to determine the proportion of 
# the hauled out Steller sea lions that were adult male, adult female, and juvenile.
# I have used the excel file 'SSFO pRey Table and Abundance Parameters for Modeling'
# sheet 'EJ Seasonal 2010-2013' to calculate mean and standard deviation of total
# non-pup counts. This calculation used more surveys than did the calculations, usign 
# the same excel file and sheet, for %male, %female, and %juvenile as I was able to
# use counts that did not have demographics for all haulouts surveyed within a day.

#Season	  Avg F Sd F  Avg J Sd J  Avg M Sd M  n
#Fall  	  0.383	0.110	0.410	0.039	0.207	0.116 7
#Spring 	0.379	0.099	0.426	0.098	0.195	0.071 11
#Summer 	0.261	0.055	0.408	0.055	0.330	0.071 9
#Winter 	0.390	0.104	0.406	0.046	0.205	0.074 4

# The resultant formulas for males for each season were as follows:
# EjMFall <- rtruncnorm(n=1,a=0,mean=0.207,sd=0.116)
# EjMSpring <- rtruncnorm(n=1,a=0,mean=0.195,sd=0.071)
# EjMSummer <- rtruncnorm(n=1,a=0,mean=0.330,sd=0.071)
# EjMWinter <- rtruncnorm(n=1,a=0,mean=0.205,sd=0.074)

# The resultant formulas for juveniles for each season were as follows:
#EjJFall <- rtruncnorm(n=1,a=0,mean=0.410,sd=0.039)
#EjJSpring <- rtruncnorm(n=1,a=0,mean=0.426,sd=0.098)
#EjJSummer <- rtruncnorm(n=1,a=0,mean=0.408,sd=0.055)
#EjJWinter <- rtruncnorm(n=1,a=0,mean=0.406,sd=0.046)

# The resultant formulas for females for each season were as follows:
# EjFFall <- rtruncnorm (n=1,a=0, mean=0.383,sd=0.110)
# EjFSpring <- rtruncnorm(n=1,a=0,mean=0.379,sd=0.099)
# EjFSummer <- rtruncnorm(n=1,a=0,mean=0.261,sd=0.055)
# EjFWinter <- rtruncnorm(n=1,a=0,mean=0.390,sd=0.104)


# The formula for abundance of non-pups in fall was rtruncnorm(n=1, a=0, mean=825.11, sd=363.96)
# The formula for abundance of non-pups in spring was rtruncnorm(n=1, a=0, mean=644.69, sd=157.96)
# The formula for abundance of non-pups in summer was rtruncnorm(n=1, a=0, mean=744.47, sd=96.62)
# The formula for abundance of non-pups in winter was rtruncnorm(n=1, a=0, mean=539.00, sd=124.81)

# The final formula to calculate consumption for each season was split into four parts: 
# juvenile female, juvenile male, adult female, and adult male.
# Within each demographic group the formula was (abudance of non-pups*proportion of demographic
# group*number of days in the season*1/1000(to covert from kg to mt)*count correction factor
# *the estimate of daily consumption of the demographic group). For juveniles an added factor
# multiplied by the estimated proportion of juveniles for each sex, respectively.

EjFall <-(replicate(n=10000, (rnbinom(n=1,mu=825.11,size=6.38)*rtruncnorm(n=1,a=0,mean=0.410,sd=0.039)*0.544*(1/1000)*(30+31+30)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvF())))+
  (replicate(n=10000, (rnbinom(n=1,mu=825.11,size=6.38)*rtruncnorm(n=1,a=0,mean=0.410,sd=0.039)*0.456*(1/1000)*(30+31+30)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvM())))+
  (replicate(n=10000, (rnbinom(n=1,mu=825.11,size=6.38)*rtruncnorm (n=1,a=0, mean=0.383,sd=0.110)*(1/1000)*(30+31+30)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdF())))+
  (replicate(n=10000, (rnbinom(n=1,mu=825.11,size=6.38)*rtruncnorm(n=1,a=0,mean=0.207,sd=0.116)*(1/1000)*(30+31+30)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdM())))

EjWinter <-(replicate(n=10000, (rnbinom(n=1, mu=539.00, size=24.27)*rtruncnorm(n=1,a=0,mean=0.406,sd=0.046)*0.544*(1/1000)*(31+31+28)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvF()))+
  (replicate(n=10000, rnbinom(n=1, mu=539.00, size=24.27)*rtruncnorm(n=1,a=0,mean=0.406,sd=0.046)*0.456*(1/1000)*(31+31+28)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvM()))+
  (replicate(n=10000, rnbinom(n=1, mu=539.00, size=24.27)*rtruncnorm(n=1,a=0,mean=0.390,sd=0.104)*(1/1000)*(31+31+28)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdF()))+
  (replicate(n=10000, rnbinom(n=1, mu=539.00, size=24.27)*rtruncnorm(n=1,a=0,mean=0.205,sd=0.074)*(1/1000)*(31+31+28)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdM())))

EjSpring <-(replicate(n=10000, (rnbinom(n=1, mu=644.69, size=18.87)*rtruncnorm(n=1,a=0,mean=0.426,sd=0.098)*0.544*(1/1000)*(31+30+31)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvF()))+
  (replicate(n=10000, rnbinom(n=1, mu=644.69, size=18.87)*rtruncnorm(n=1,a=0,mean=0.426,sd=0.098)*0.456*(1/1000)*(31+30+31)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvM()))+
  (replicate(n=10000, rnbinom(n=1, mu=644.69, size=18.87)*rtruncnorm(n=1,a=0,mean=0.379,sd=0.099)*(1/1000)*(31+30+31)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdF()))+
  (replicate(n=10000, rnbinom(n=1, mu=644.69, size=18.87)*rtruncnorm(n=1,a=0,mean=0.195,sd=0.071)*(1/1000)*(31+30+31)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdM())))

EjSummer <-(replicate(n=10000, (rnbinom(n=1, mu=744.47, size=68.93)*rtruncnorm(n=1,a=0,mean=0.408,sd=0.055)*0.544*(1/1000)*(31+31+30)*rnorm(1,1.48,0.0831)*IndDailyConsumptionJuvF()))+
  (replicate(n=10000, rnbinom(n=1, mu=744.47, size=68.93)*rtruncnorm(n=1,a=0,mean=0.408,sd=0.055)*0.456*(1/1000)*(31+31+30)*rnorm(1,1.48,0.0831)*IndDailyConsumptionJuvM()))+
  (replicate(n=10000, rnbinom(n=1, mu=744.47, size=68.93)*rtruncnorm(n=1,a=0,mean=0.261,sd=0.055)*(1/1000)*(31+31+30)*rnorm(1,1.48,0.0831)*IndDailyConsumptionAdF()))+
  (replicate(n=10000, rnbinom(n=1, mu=744.47, size=68.93)*rtruncnorm(n=1,a=0,mean=0.330,sd=0.071)*(1/1000)*(31+31+30)*rnorm(1,1.48,0.0831)*IndDailyConsumptionAdM())))

EjFallMean <-mean(EjFall)
EjFallSd <- sd(EjFall)

EjWinterMean <- mean(EjWinter)
EjWinterSd <- sd (EjWinter)

EjSpringMean <- mean(EjSpring)
EjSpringSd <- sd(EjSpring)

EjSummerMean <- mean(EjSummer)
EjSummerSd <- sd(EjSummer)

EjFullYear  <-((replicate(n=10000,  (rnbinom(n=1, mu=825.11, size=6.38)*rtruncnorm(n=1,a=0,mean=0.410,sd=0.039)*0.544*(1/1000)*(30+31+30)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvF())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=825.11, size=6.38)*rtruncnorm(n=1,a=0,mean=0.410,sd=0.039)*0.456*(1/1000)*(30+31+30)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvM())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=825.11, size=6.38)*rtruncnorm (n=1,a=0, mean=0.383,sd=0.110)*(1/1000)*(30+31+30)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdF())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=825.11, size=6.38)*rtruncnorm(n=1,a=0,mean=0.207,sd=0.116)*(1/1000)*(30+31+30)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdM())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=539.00, size=24.27)*rtruncnorm(n=1,a=0,mean=0.406,sd=0.046)*0.544*(1/1000)*(31+31+28)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvF())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=539.00, size=24.27)*rtruncnorm(n=1,a=0,mean=0.406,sd=0.046)*0.456*(1/1000)*(31+31+28)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvM())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=539.00, size=24.27)*rtruncnorm(n=1,a=0,mean=0.390,sd=0.104)*(1/1000)*(31+31+28)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdF())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=539.00, size=24.27)*rtruncnorm(n=1,a=0,mean=0.205,sd=0.074)*(1/1000)*(31+31+28)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdM())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=644.69, size=18.87)*rtruncnorm(n=1,a=0,mean=0.426,sd=0.098)*0.544*(1/1000)*(31+30+31)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvF())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=644.69, size=18.87)*rtruncnorm(n=1,a=0,mean=0.426,sd=0.098)*0.456*(1/1000)*(31+30+31)*rnorm(1,2.778,0.0583)*IndDailyConsumptionJuvM())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=644.69, size=18.87)*rtruncnorm(n=1,a=0,mean=0.379,sd=0.099)*(1/1000)*(31+30+31)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdF())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=644.69, size=18.87)*rtruncnorm(n=1,a=0,mean=0.195,sd=0.071)*(1/1000)*(31+30+31)*rnorm(1,2.778,0.0583)*IndDailyConsumptionAdM())))+   
                 (replicate(n=10000, (rnbinom(n=1, mu=744.47, size=68.93)*rtruncnorm(n=1,a=0,mean=0.408,sd=0.055)*0.544*(1/1000)*(31+31+30)*rnorm(1,1.48,0.0831)*IndDailyConsumptionJuvF())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=744.47, size=68.93)*rtruncnorm(n=1,a=0,mean=0.408,sd=0.055)*0.456*(1/1000)*(31+31+30)*rnorm(1,1.48,0.0831)*IndDailyConsumptionJuvM())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=744.47, size=68.93)*rtruncnorm(n=1,a=0,mean=0.261,sd=0.055)*(1/1000)*(31+31+30)*rnorm(1,1.48,0.0831)*IndDailyConsumptionAdF())))+
                 (replicate(n=10000, (rnbinom(n=1, mu=744.47, size=68.93)*rtruncnorm(n=1,a=0,mean=0.330,sd=0.071)*(1/1000)*(31+31+30)*rnorm(1,1.48,0.0831)*IndDailyConsumptionAdM()))))
 
EjFullYearMean <- mean(EjFullYear)
EjFullYearSd <- sd(EjFullYear)


########################### California sea lion Estimator #########
# correction factor from Lowry and Forney 2005 for the portion of the California sea lion
# population that are at sea and not available for counts at haulout: runif(1,1.77,2.13)
# The correction factor was calculated for northern California.

# Size-specific consumption multipliers are from Winship et al. 2006. All sea lions are assumed to be male.
ZcLarge <- rnorm(1,0.04,0.01)
ZcMed <- runif(1,0.04,0.05)
ZcMedSm <- runif(1,0.05,0.06)
ZcSmall <- runif(1,0.06,0.07)
ZcVSmall <- runif(1,0.08,0.09)

# The average weight of California sea lions present was informed by California 
# sea lions captured by  Gearin et al. 2017 for Ballard, Washington and from
# instrumented (mean 294.2, sd 69.1, n 28) and non-instrumented animals 
# (mean 247.1, sd 59.5, n 319) from Wright et al. 2010 at Astoria, Oregon.
# Pooled variance formula from https://www.easycalculation.com/formulas/pooled-standard-deviation.html
BallardWt <- c(492,272,392,363,299,301,301,289,270)
MeanZcWt <-((9*mean(BallardWt)+(28*294.2)+(319*247.1))/(28+319+9))
varZcWt <-((9-1)*(sd(BallardWt)^2)+((28-1)*69.1^2)+((319-1)*59.5^2))/(28+319+9-3)
sdZcWt <- sqrt(varZcWt)

# IndDailyConsumption is a function that calculates consumption given a 
# randomly generated sea lion weight and the appropriate multiplier from
# Winship et al. 2006.
IndDailyConsumption<- function() 
{
  ZcWeight <- rnorm(1,MeanZcWt,sdZcWt)
  if(ZcWeight > 318){ZcWeight*ZcLarge}
  if(ZcWeight > 220){ZcWeight*ZcMed}
  if(ZcWeight > 139){ZcWeight*ZcMedSm}
  if(ZcWeight > 73) {ZcWeight*ZcSmall}
  if(ZcWeight > 0)  {ZcWeight*ZcVSmall}
}

# Count data is provided in Scordino and Akamajian 2021. Summarized count results are below
# with their negative binomial distribution size.
#        Mean	      size            
#Fall	  1573.615385	3.13   
#Spring	419.3333333	4.09   
#Winter	153.25	    0.61
#Summer	349.3043478	4.86


FallStudy <-replicate(n=10000, rnbinom(1,size=3.13,mu=1573.62)*(1/1000)*(30+31+30)*runif(1,1.77,2.13)*IndDailyConsumption())
SpringStudy <-replicate(n=10000, rnbinom(1,size=4.09,mu=419.33)*(1/1000)*(31+30+31)*runif(1,1.77,2.13)*IndDailyConsumption()) 
SummerStudy <-replicate(n=10000, rnbinom(1,size = 0.61, mu=349.30)*(1/1000)*(31+31+30)*runif(1,1.77,2.13)*IndDailyConsumption())
WinterStudy <-replicate(n=10000, rnbinom(1,size=4.86, mu=153.25)*(1/1000)*(28+31+31)*runif(1,1.77,2.13)*IndDailyConsumption())

ZcFullYear <-(replicate(n=10000, rnbinom(1,size=3.13,mu=1573.62)*(1/1000)*(30+31+30)*runif(1,1.77,2.13)*IndDailyConsumption()))+
  (replicate(n=10000, rnbinom(1,size=4.09,mu=419.33)*(1/1000)*(31+30+31)*runif(1,1.77,2.13)*IndDailyConsumption()))+ 
  (replicate(n=10000, rnbinom(1,size = 0.61, mu=349.30)*(1/1000)*(31+31+30)*runif(1,1.77,2.13)*IndDailyConsumption()))+
  (replicate(n=10000, rnbinom(1,size=4.86, mu=153.25)*(1/1000)*(28+31+31)*runif(1,1.77,2.13)*IndDailyConsumption()))

ZcFullYearMean <- mean(ZcFullYear)
ZcFullYearSd <- sd(ZcFullYear)


##Figure 3 from Scordino et al. (in submission)#######################

#Background data for plot
EjSeasonMean<-c(mean(EjSpring), mean(EjSummer), mean(EjFall), mean(EjWinter))
ZcSeasonMean<-c(mean(SpringStudy), mean(SummerStudy),mean(FallStudy),mean(WinterStudy))

EjSeasonSD<-c(sd(EjSpring), sd(EjSummer), sd(EjFall), sd(EjWinter))
ZcSeasonSD<-c(sd(SpringStudy), sd(SummerStudy), sd(FallStudy), sd(WinterStudy))

#Set up your plot size and font style
par(mar=c(3,5.5,1,0), oma=c(0,1.5,0.5,0),family="sans", font.main=1)
#Plot Steller data - manually add x-axis and y-axis names
SSL<-barplot(EjSeasonMean, col="gray", las=1, cex.axis=1.5, xlab=NA, xlim=c(2,13.5),ylim=c(0,10000), axes=TRUE, space=2, main=NA, cex.main=1.1)
axis(1,at=c(-0.5,3,6,9,12,13.5), labels=c("","Spring","Summer","Fall","Winter",""), cex.axis=1.5, tcl=0)
mtext("Average metric tons consumed per season", side=2, line=5, cex=1.5)
#Add error bars for Steller data
segments(SSL, EjSeasonMean+EjSeasonSD, SSL, EjSeasonMean-EjSeasonSD)
arrows(SSL, EjSeasonMean+EjSeasonSD, SSL, EjSeasonMean-EjSeasonSD, lwd = 1.5, angle = 90, code = 3, length = 0.05)
#Plot California data with add=TRUE and do not replot axes or axes titles
CSL<-barplot(ZcSeasonMean, beside=TRUE, add=TRUE, col="white", las=1, cex.axis=1.5, xlab=NA, xlim=c(0,5),ylim=c(0,10000), axes=FALSE, space=c(3,2,2,2))
#Add error bars for California data
segments(CSL, ZcSeasonMean+ZcSeasonSD, CSL, ZcSeasonMean-ZcSeasonSD)
arrows(CSL, ZcSeasonMean+ZcSeasonSD, CSL, ZcSeasonMean-ZcSeasonSD, lwd = 1.5, angle = 90, code = 3, length = 0.05)
legend(9.3,11000,c("Steller sea lions","California sea lions"),pch=c(22,22), pt.bg=c("gray","white"),pt.cex=2, cex=1.6, bty="n")

############################################################


# Literature cited

# Gearin, P. J., S. R. Melin, R. L. Delong, M. E. Gosho, and S. J. Jeffries. 2017. 
# Migration patterns of adult male California sea lions (Zalophus californianus). 
# NOAA Tech. Memo. NMFS-AFSC-346 29 p. https://doi.org/10.7289/V5/TM-AFSC-346.

# Lowry, M. S., and K. A. Forney. 2005. Abundance and distribution of California sea
# lions (Zalophus californianus) in central and northern California during 1998 and 
# summer 1999. Fish. Bull. 103:331-343.

# Olesiuk, P. F. 2018. Recent trends in Abundance of Steller Sea Lions (Eumetopias 
# jubatus) in British Columbia. Dep. Fish Ocean. Can. Sci. Advis. Secr. Res. Doc., 
# Vol. 006, 67 p.

# Scordino, J. J., and A. M. Akmajian. 2021. Steller and California sea lion count and 
# diet data for haulout sites in northwest Washington, 2010-2013. Mendeley Data

# Scordino, J. J., A. M. AKmajian, S.D. Riemer, and S. L. Edmondson. In Submission.
# Dietary niche overlap and prey consumption estimates for Steller sea lion and 
# California sea lion in northwest Washington, 2010-2013. Fish. Bull. submitted April 2021.

# Whitlock, S. L., J. N. Womble, and J. T. Peterson. 2020. Modelling pinniped abundance
# and distribution by combining counts at terrestrial sites and in-water sightings. 
# Ecol. Modell. 420:108965. https://doi.org/10.1016/j.ecolmodel.2020.108965.

# Winship, A. J., A. W. Trites, and D. G. Calkins. 2001. Growth in body size of the 
# Steller sea lion (Eumetopias jubatus). J. Mammal. 82:500-519. 
# https://doi.org/10.1644/1545-1542(2001)082<0500:gibsot>2.0.co;2.

# Winship, A. J., A. M. J. Hunter, D. A. S. Rosen, and A. W. Trites. 2006. Food 
# consumption by sea lions: Existing data and techniques. In Sea lions of the world 
# (A.W. Trites, S. K. Atkinson, D. P DeMaster, L. W. Fritz, T. S. Gelatt, L. D. Rea, 
# and K. M. Wynne, eds.), p. 177-191. Alaska Sea Grant College Program, University of 
# Alaskka Fairbanks. https://doi.org/10.4027/slw.2006.13.

# Wright, B. E., M. J. Tennis, and R. F. Brown. 2010. Movements of male California sea 
# lions captured in the Columbia River. Northwest Sci. 84:60-72. 
# https://doi.org/10.3955/046.084.0107.

# Wright, B. E., R. F. Brown, R. L. DeLong, P. J. Gearin, S. D. Riemer, J. L. Laake, 
# and J. J. Scordino. 2017. Survival rates of Steller sea lions from Oregon and 
# California. J. Mammal. 98:885-894. https://doi.org/10.1093/jmammal/gyx033.


