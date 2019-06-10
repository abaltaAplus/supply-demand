library(jsonlite)
library("xlsx")
library(quantmod)
library("httr")
# chooseHour=22
chooseLag=1

targetDay=as.Date('2019-06-10')

##  Supply-Demand Curve Example
# importing the curve
# url<-"http://gateway.santraltakip.com/seffaflik/transparency/market/supply-demand-curve"
# status="success"
# count=1
# while((count==1 | status=="fail") & count<=10) {
#   res <- try(assign("response",GET(url = paste0(url,"?period=",targetDay), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")),timeout(15))),silent = TRUE)
#   if(class(res) == "try-error") {
#     status="fail"
#   } else {
#     status="success"
#   }
#   print(paste0("trial ",count,", status: ",status))
#   count=count+1
# }
# 
# if(response$status_code==200)
# {
#   ff=fromJSON(content(response,as="text", encoding = "UTF-8"))
#   originalCurves=data.frame(date=as.Date(ff$body$supplyDemandCurves[,1],format = "%Y-%m-%dT%H:%M:00.000+0300"),hour=as.integer(substr(ff$body$supplyDemandCurves[,1],12,13)),price=ff$body$supplyDemandCurves[,2],supply=abs(ff$body$supplyDemandCurves[,3]),demand=ff$body$supplyDemandCurves[,4])
# }
# supplyDemandCurves=originalCurves

url<-"https://volt-santraltakip-api-documentdb-api.azurewebsites.net/api/Epias/Market/ArzTalepEgrisi"

# url <- "https://api.epias.com.tr/epias/exchange/transparency/market/supply-demand-curve"
file=GET(url = paste0(url,"?startDate=",targetDay,"&endDate=",targetDay), add_headers(.headers=c("Content-Type"="application/json")))
ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
gmtHours=as.POSIXct(ff$supplyDemandCurves[[1]]$date)+60*60*3
df=data.frame(date=format(gmtHours,"%Y-%m-%d"),hour=as.integer(format(gmtHours,"%H")),price=as.numeric(ff$supplyDemandCurves[[1]]$price),supply=abs(as.numeric(ff$supplyDemandCurves[[1]]$supply)),demand=as.numeric(ff$supplyDemandCurves[[1]]$demand))
supplyDemandCurves=df

supplyDemandCurves$bids=NA

for (i in min(supplyDemandCurves$hour):max(supplyDemandCurves$hour)) {
  supplyDemandCurves[supplyDemandCurves$hour==i,6]=supplyDemandCurves[supplyDemandCurves$hour==i,4]-Lag(supplyDemandCurves[supplyDemandCurves$hour==i,4],k=1)
}

bidSummary=supplyDemandCurves[is.na(supplyDemandCurves$bids)==F&supplyDemandCurves$bids>1,]
priceLevels<-seq(0,2000,5)
cumulativeBids<-data.frame(priceLevels,NA)

# i=0
for (i in 0:23) {
  for (j in 2:length(priceLevels)) {
    df=bidSummary[bidSummary$hour==i,c(3,6)]
    cumulativeBids[j,i+2]=sum(df[df$price<=priceLevels[j],2])
  }
  
  cumulativeBids=data.frame(cumulativeBids,NA)
  
}

cumulativeBids[1,]=0

cumulativeBids[,ncol(cumulativeBids)]<-NULL

colnames(cumulativeBids)[2:ncol(cumulativeBids)]=format(seq(as.POSIXct('1991-02-04 00:00'), by='hours', length.out = 24), '%H:%M')

cumulativeBids$AVG=apply(cumulativeBids[,-1],1,mean)

refDate=targetDay

targetDay=targetDay-chooseLag

##  Supply-Demand Curve Example
# importing the curve
# url<-"http://gateway.santraltakip.com/seffaflik/transparency/market/supply-demand-curve"
# status="success"
# count=1
# while((count==1 | status=="fail") & count<=10) {
#   res <- try(assign("response",GET(url = paste0(url,"?period=",targetDay), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")),timeout(15))),silent = TRUE)
#   if(class(res) == "try-error") {
#     status="fail"
#   } else {
#     status="success"
#   }
#   print(paste0("trial ",count,", status: ",status))
#   count=count+1
# }
# 
# if(response$status_code==200)
# {
#   ff=fromJSON(content(response,as="text", encoding = "UTF-8"))
#   originalCurves=data.frame(date=as.Date(ff$body$supplyDemandCurves[,1],format = "%Y-%m-%dT%H:%M:00.000+0300"),hour=as.integer(substr(ff$body$supplyDemandCurves[,1],12,13)),price=ff$body$supplyDemandCurves[,2],supply=abs(ff$body$supplyDemandCurves[,3]),demand=ff$body$supplyDemandCurves[,4])
# }
# supplyDemandCurves=originalCurves

url<-"https://volt-santraltakip-api-documentdb-api.azurewebsites.net/api/Epias/Market/ArzTalepEgrisi"

# url <- "https://api.epias.com.tr/epias/exchange/transparency/market/supply-demand-curve"
file=GET(url = paste0(url,"?startDate=",targetDay,"&endDate=",targetDay), add_headers(.headers=c("Content-Type"="application/json")))
ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
gmtHours=as.POSIXct(ff$supplyDemandCurves[[1]]$date)+60*60*3
df=data.frame(date=format(gmtHours,"%Y-%m-%d"),hour=as.integer(format(gmtHours,"%H")),price=as.numeric(ff$supplyDemandCurves[[1]]$price),supply=abs(as.numeric(ff$supplyDemandCurves[[1]]$supply)),demand=as.numeric(ff$supplyDemandCurves[[1]]$demand))
supplyDemandCurves=df

supplyDemandCurves$bids=NA

for (i in min(supplyDemandCurves$hour):max(supplyDemandCurves$hour)) {
  supplyDemandCurves[supplyDemandCurves$hour==i,6]=supplyDemandCurves[supplyDemandCurves$hour==i,4]-Lag(supplyDemandCurves[supplyDemandCurves$hour==i,4],k=1)
}

bidSummary=supplyDemandCurves[is.na(supplyDemandCurves$bids)==F&supplyDemandCurves$bids>1,]
priceLevels<-seq(0,2000,5)
comparison<-data.frame(priceLevels,NA)

i=0
for (i in 0:23) {
  for (j in 2:length(priceLevels)) {
    df=bidSummary[bidSummary$hour==i,c(3,6)]
    comparison[j,i+2]=sum(df[df$price<=priceLevels[j],2])
  }
  
  comparison=data.frame(comparison,NA)
  
}

comparison[1,]=0

comparison[,ncol(comparison)]<-NULL

colnames(comparison)[2:ncol(comparison)]=format(seq(as.POSIXct('1991-02-04 00:00'), by='hours', length.out = 24), '%H:%M')

comparison$AVG=apply(comparison[,-1],1,mean)

# saat giriniz(ortalama icin 25 giriniz)
targetHour=16

plot(cumulativeBids$priceLevels,cumulativeBids[,(targetHour+1)], type='l', main = paste0('Supply Curve of ',targetHour,':00')
     # )
     ,xlim = c(100,200), ylim = c(0,6000), lwd=2.5,xlab = 'PTF(TL/MWh)', ylab = 'Supply(MWh)')

lines(comparison$priceLevels,comparison[,(targetHour+1)], col='red', lwd=2.5)



url <-"http://gateway.santraltakip.com/seffaflik/transparency/market/day-ahead-mcp"
status="success"
count=1
while((count==1 | status=="fail") & count<=10) {
  res <- try(assign("response",GET(url = paste0(url,"?startDate=",refDate,'&endDate=', refDate), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")),timeout(7))),silent = TRUE)
  if(class(res) == "try-error") {
    status="fail"
  } else {
    status="success"
  }
  print(paste0("trial ",count,", status: ",status))
  count=count+1
}

if(response$status_code==200)
{
  ff=fromJSON(content(response,as="text", encoding = "UTF-8"))
  benchmarkPTF=data.frame(hour=as.character(as.POSIXlt(ff$body$dayAheadMCPList[,1],format = "%Y-%m-%dT%H:%M:00.000+0300")),marketTradePrice=ff$body$dayAheadMCPList[,2])
}

if(targetHour==25) {
  v=mean(benchmarkPTF$marketTradePrice)
} else {
  v=benchmarkPTF$marketTradePrice[targetHour+1]
}

abline(v=v,col='blue', lwd=2)

legend("topleft", c(format(refDate, '%d.%m.%Y'), format(targetDay, '%d.%m.%Y') ,'Eq. Price'),
       
       lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       
       lwd=c(2.5,2.5,2.5),col=c("black","red",'blue'))

write.xlsx(cumulativeBids, 'hourlySupplyCurve.xlsx', sheetName=format(refDate, '%d.%m.%Y'), row.names = F)

write.xlsx(comparison, 'hourlySupplyCurve.xlsx', sheetName=format(targetDay, '%d.%m.%Y'), row.names = F, append=T)

url <- "https://api.epias.com.tr/epias/exchange/transparency/production/dpp?"
file=GET(url = paste0(url,"startDate=",targetDay,'&endDate=',refDate), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))

if(file$status_code==200)
{
  ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
  KGUP=data.frame(date=as.Date(ff$body$dppList$tarih),hour=ff$body$dppList$saat,wind=ff$body$dppList$ruzgar)
}

url <-"https://api.epias.com.tr/epias/exchange/transparency/market/day-ahead-mcp"
file=GET(url = paste0(url,"?startDate=",targetDay,'&endDate=', targetDay), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))

if(file$status_code==200)
{
  ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
  benchmarkPTF=data.frame(date=substr(ff$body$dayAheadMCPList[,1],1,10),hour=as.integer(substr(ff$body$dayAheadMCPList[,1],12,13)),marketTradePrice=ff$body$dayAheadMCPList[,2])
}

## block bids
url <-"https://api.epias.com.tr/epias/exchange/transparency/market/amount-of-block"
file=GET(url = paste0(url,"?startDate=",targetDay,'&endDate=', targetDay), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))

if(file$status_code==200)
{
  ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
  blockBids=data.frame(date=substr(ff$body$amountOfBlockList[,1],1,10),hour=as.integer(substr(ff$body$amountOfBlockList[,1],12,13)),matchedBlock=ff$body$amountOfBlockList[,5])
}

refBlock=blockBids[blockBids$hour==chooseHour,3]

refPrice=benchmarkPTF[benchmarkPTF$hour==chooseHour,3]

targetBidVolume=sum(bidSummary[bidSummary$hour==chooseHour & bidSummary$price<=refPrice,6])

comparisonDate=targetDay-chooseLag

targetDay=comparisonDate

# importing the curve 2
url <- "https://api.epias.com.tr/epias/exchange/transparency/market/supply-demand-curve"
file=GET(url = paste0(url,"?period=",targetDay), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))
if(file$status_code==200)
{
  ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
  supplyDemandCurves=data.frame(date=as.Date(ff$body$supplyDemandCurves[,1],format = "%Y-%m-%dT%H:%M:00.000+0300"),hour=as.integer(substr(ff$body$supplyDemandCurves[,1],12,13)),price=ff$body$supplyDemandCurves[,2],supply=ff$body$supplyDemandCurves[,3],demand=ff$body$supplyDemandCurves[,4])
}

supplyDemandCurves$bids=NA

for (i in min(supplyDemandCurves$hour):max(supplyDemandCurves$hour)) {
  supplyDemandCurves[supplyDemandCurves$hour==i,6]=-(supplyDemandCurves[supplyDemandCurves$hour==i,4]-Lag(supplyDemandCurves[supplyDemandCurves$hour==i,4],k=1))
}

bidSummary=supplyDemandCurves[is.na(supplyDemandCurves$bids)==F&supplyDemandCurves$bids>1,]

url <-"https://api.epias.com.tr/epias/exchange/transparency/market/day-ahead-mcp"
file=GET(url = paste0(url,"?startDate=",targetDay,'&endDate=', targetDay), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))

if(file$status_code==200)
{
  ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
  benchmarkPTF=data.frame(date=substr(ff$body$dayAheadMCPList[,1],1,10),hour=as.integer(substr(ff$body$dayAheadMCPList[,1],12,13)),marketTradePrice=ff$body$dayAheadMCPList[,2])
}

comparisonBidVolume=sum(bidSummary[bidSummary$hour==chooseHour & bidSummary$price<=refPrice,6])
comparisonPrice=benchmarkPTF[benchmarkPTF$hour==chooseHour,3]

## block bids
url <-"https://api.epias.com.tr/epias/exchange/transparency/market/amount-of-block"
file=GET(url = paste0(url,"?startDate=",targetDay,'&endDate=', targetDay), add_headers(.headers=c("x-ibm-client-id"= "1f9e3bd7-de7c-4eb0-baa1-74257e22df93",Accept="application/json")))

if(file$status_code==200)
{
  ff=fromJSON(content(file,as="text", encoding = "UTF-8"))
  blockBids=data.frame(date=substr(ff$body$amountOfBlockList[,1],1,10),hour=as.integer(substr(ff$body$amountOfBlockList[,1],12,13)),matchedBlock=ff$body$amountOfBlockList[,5])
}
A
comparisonBlock=blockBids[blockBids$hour==chooseHour,3]

message=paste0('When equilibrium is reached at ', round(refPrice,2), '\n TL/MWh, ', round(targetBidVolume,2), ' MW of hourly bids are accepted \n whereas ', round(chooseLag,2), ' days ago there exists \ntotal of ', round(comparisonBidVolume,2), 'MWs of bids \n until same price level, \n there equilibrium is at ', round(comparisonPrice,2),' TL/MWh,\n Blocks of selected hour is at total ', round(refBlock,2), ' MWs, \n whereas ', chooseLag, ' days ago this total is equal to ', round(comparisonBlock,2), ' MWs')

tableC=data.frame(matrix(c(targetBidVolume,comparisonBidVolume,refBlock,comparisonBlock), nrow=2,ncol = 2), row.names = c('hourly bid level', 'block bid level'))
colnames(tableC)=c(targetDay+chooseLag, targetDay)


barplot(t(matrix(c(targetBidVolume,comparisonBidVolume,refBlock,comparisonBlock), nrow=2,ncol = 2,)),main=NULL,
        names.arg = c(targetDay+chooseLag,targetDay), col=c("darkblue","red"), cex.names =1, legend.text = c('block bid volume','hourly bid volume'))

message
