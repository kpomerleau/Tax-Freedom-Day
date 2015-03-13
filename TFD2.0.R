# Sets the working directory. This sets it to the "rtfd" folder on my desktop

setwd("C:/Users/kep/Documents/GitHub/Tax-Freedom-Day")

#Clears all datasets and variables from memory

rm(list=ls())

########################Cbo Fiscal Year to Calendar Year Conversions############################## 
  
  cbo<-read.csv("cboquarters.csv", header = TRUE, fill = TRUE, sep = ",")
  
    #This requires the installation of the foreach loop package.This makes life way easier.
    
    library(foreach)
    
   #starts at two, 1 is year indicator, which needs to be skipped
      for (n in 2:length(cbo)){
        
        cbo[,n]<-as.numeric(foreach(i=cbo$year) %do% (cbo[,n][cbo$year==(i)]*0.75+cbo[,n][cbo$year==(i+1)]*0.25))
        
      }
    
    #excise tax total:
  
      cbo$excisetaxes<-as.numeric(cbo$highway+cbo$tobacco+cbo$alcohol+cbo$airport+cbo$healthinsurers+cbo$otherexcise)
  
    #manual adjustments can be made here:
        
      #None needed for 2015. Extenders are currently expired
        

############################BEA Quarterly Data######################################
  #Previous year's calendar data is incomplete. This takes the previous year's quarterly data,
  #extrapolates the missing 4th quarter data (usually by GDP growth) and averages them together
  #to get the calendar data.

  #Loads the Data

    qbea<-read.csv("beaquarters.csv", header = TRUE, fill = FALSE, sep = ",")
  
  #Goes through all the quarterly data, grows fourth quarter of missing data by 4th quarter GDP growth

    for (i in 1:length(qbea)) {
      
      if (is.na(qbea[4,i])==TRUE) {
      
        qbea[4,i]<-(qbea[3,i]/qbea[3,2])*qbea[4,2]
      
      }
    
    }

#Creates calendar year estimations of data
bealast<-NULL
for (i in 2:length(qbea)) {
  bealast[i]<-colMeans(qbea[i])
}
#############################Create Calendar Year Dataset############################

  #Imports all necessary CY data from previous years.

    bea<-read.csv("bea.csv", header = TRUE, fill = FALSE, sep = ",")

  #merges the bea calendar year dataset with the calculated last-year data
  
    final<-rbind(bea, bealast)

  #lables last year in the dataset

    final$year[(length(final$year))]<-final$year[(length(final$year)-1)]+1

  #impute CGSI foreign for all years (This is needed to calculate Net CGSI taxpayments at state and federal levels)

    final$cgsiforeign<-final$cgsi+final$statecgsi-final$cgsidomestic

  #Imputes missing CY data (Mainly the Insurance payments)
  #This imputes the missing values by growing the previous year by GDP growth
    
    for (i in 1:length(final)) {
      
      if (is.na(final[length(final$year),i])==TRUE) {
      
        final[length(final$year),i]<-(final[length(final$year),2]/final[length(final$year)-1,2])*final[length(final$year)-1,i]
      
      }
    
    }

#Computes current year's values

#adds empty row to final dataset for current year
temprow<-as.numeric(matrix(nrow=1, ncol=length(final)))
temprow[1]<-final[length(final$year),1]+1
final<-rbind(final,temprow)

#Calculates current year values
#This seems burdensome, but some of these have specific calculations that need to be defined

#Needed Growth Rates
gdpgrowth<-(cbo$gdp[2]/cbo$gdp[1])
fedtaxgrowth<-(cbo$individualincometax[2]/cbo$individualincometax[1])
excisetaxgrowth<-(cbo$excisetaxes[2]/cbo$excisetaxes[1])
customsgrowth<-(cbo$customs[2]/cbo$custom[1])
fedreservesgrowth<-(cbo$fedreserve[2]/cbo$fedreserve[1])
citgrowth<-(cbo$corporateincome[2]/cbo$corporateincome[1])
domesticprofitsgrowth<-(cbo$domesticprofits[2]/cbo$domesticprofits[1])
fedssigrowth<-(cbo$sitax[2]/cbo$sitax[1])
estatetaxgrowth<-(cbo$estateandgift[2]/cbo$estateandgift[1])
wagesgrowth<-(cbo$wages[2]/cbo$wage[1])
foreignssigrowth<-(cbo$sitax[2]/cbo$sitax[1])
deficitgrowth<-(cbo$deficit[2]/cbo$deficit[1])

#Current Year Projections, done individually now.
final$gdp[length(final$year)]<-gdpgrowth*final$gdp[length(final$year)-1]
final$incomereceiptsrestofworld[length(final$year)]<-as.numeric(NA)
final$incomepaymentsrestofworld[length(final$year)]<-as.numeric(NA)
final$nnp[length(final$year)]<-gdpgrowth*final$nnp[length(final$year)-1]
final$consumptionfixedcapital[length(final$year)]<-as.numeric(NA)
final$personalcurrenttaxes[length(final$year)]<-fedtaxgrowth*final$personalcurrenttaxes[length(final$year)-1]
final$excisetaxes[length(final$year)]<-excisetaxgrowth*final$excisetaxes[length(final$year)-1]
final$customs[length(final$year)]<-customsgrowth*final$customs[length(final$year)-1]
final$fedreserve[length(final$year)]<-fedreservesgrowth*final$fedreserve[length(final$year)-1]
final$othercit[length(final$year)]<-citgrowth*final$othercit[length(final$year)-1]
final$cgsi[length(final$year)]<-fedssigrowth*final$cgsi[length(final$year)-1]
final$estatetax[length(final$year)]<-estatetaxgrowth*final$estatetax[length(final$year)-1]+4
final$statepctincome[length(final$year)]<-wagesgrowth*final$statepctincome[length(final$year)-1]
final$statepctother[length(final$year)]<-wagesgrowth*final$statepctother[length(final$year)-1]
final$statesalestaxes[length(final$year)]<-gdpgrowth*final$statesalestaxes[length(final$year)-1]
final$statepropertytaxes[length(final$year)]<-gdpgrowth*final$statepropertytaxes[length(final$year)-1]
final$stateothertopi[length(final$year)]<-gdpgrowth*final$stateothertopi[length(final$year)-1]
final$statecit[length(final$year)]<-domesticprofitsgrowth*final$statecit[length(final$year)-1]
final$statecgsi[length(final$year)]<-wagesgrowth*final$statecgsi[length(final$year)-1]
final$stateestatetax[length(final$year)]<-gdpgrowth*final$stateestatetax[length(final$year)-1]
final$pbgc[length(final$year)]<-gdpgrowth*final$pbgc[length(final$year)-1]
final$veteransemployer[length(final$year)]<-gdpgrowth*final$veteransemployer[length(final$year)-1]
final$workerscomp[length(final$year)]<-gdpgrowth*final$workerscomp[length(final$year)-1]
final$militarymed[length(final$year)]<-gdpgrowth*final$militarymed[length(final$year)-1]
final$stateworkerscomp[length(final$year)]<-gdpgrowth*final$stateworkerscomp[length(final$year)-1]
final$suppmedical[length(final$year)]<-gdpgrowth*final$suppmedical[length(final$year)-1]
final$veteransemployee[length(final$year)]<-gdpgrowth*final$veteransemployee[length(final$year)-1]
final$cgsidomestic[length(final$year)]<-gdpgrowth*final$cgsidomestic[length(final$year)-1]
final$cgsiforeign[length(final$year)]<-foreignssigrowth*final$cgsiforeign[length(final$year)-1]
final$capitalstock[length(final$year)]<-gdpgrowth*final$capitalstock[length(final$year)-1]
final$cgsirestofworld[length(final$year)]<-as.numeric(NA)
final$netsaving[length(final$year)]<-deficitgrowth*final$netsaving[length(final$year)-1]
final$netlendingborrowing[length(final$year)]<-as.numeric(NA)
final$netfedcgsi<-final$cgsi-final$pbgc-final$veteransemployer-final$militarymed-final$workerscomp-final$suppmedical-final$veteransemployee-(final$cgsiforeign*(final$cgsi/(final$cgsi+final$statecgsi)))
final$netstatecgsi<-final$statecgsi-final$stateworkerscomp-(final$cgsiforeign*(final$statecgsi/(final$statecgsi+final$cgsi)))
final$fedtaxes<-final$personalcurrenttaxes+final$excisetaxes+final$customs+final$fedreserve+final$othercit+final$estatetax+final$netfedcgsi+final$capitalstock
final$statetaxes<-final$statepctincome+final$statepctother+final$statesalestaxes+final$statepropertytaxes+final$stateothertopi+final$statecit+final$stateestatetax+final$netstatecgsi

#Tax Freedom Day Variables Calculated:

  final$tfd<-((final$fedtaxes+final$statetaxes)/final$nnp)*365

  final$tfddate<-format(as.Date(final$tfd, origin="2015-01-01"), format="%B %d")
  
  final$tfddeficit<-((final$fedtaxes+final$statetaxes-final$netsaving)/final$nnp)*365
  
  final$tfddeficit<-format(as.Date(final$tfddeficit, origin="2015-01-01"), format="%B %d")

############################################################






#############################################################


#Tax Freedom Day is 

format(final$tfddate[length(final$year)], format="%B %d")

#deficit inclusive tax freedom day is

format(final$tfddeficit[length(final$year)], format="%B %d")

plot(final$year,final$tfd , type="l", main= "Tax Freedom Day, 1929-2015", xlab = "Year", ylab = "Day Into the Year")
