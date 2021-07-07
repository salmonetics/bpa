dams <- c("BON","DWR","IHR","JHAW","LGS","LWG","LMN","MCPW","PRD","RIS","RRH","TDA","WAN","WEL")

getAnnualDamCounts <- function(proj){
  url1 <- 'http://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=csv&proj='
  url2 <- '&startdate=1%2F1&enddate=12%2F31&run='
  url <- paste(url1,proj,sep="")
  url <- paste(url,url2,sep="")
  temp <- read.csv(url)
  temp <- filter(temp,!is.na(Year))
  return(temp)
}

getAnnualCounts <- function(){
  require(dplyr)
  require(reshape2)
  dams <- c("BON","IHR","JDA","LGS","LWG","LMN","MCN","PRD","PRO","ROZ","RIS","RRH","TDA","TUM","WAN","WEL","WFF","ZOS")
  projects <- c("Willamette Falls","Bonneville","The Dalles","John Day","McNary","Ice Harbor","Lower Monumental","Little Goose","Lower Granite","Prosser","Roza","Priest Rapids","Wanapum","Rock Island","Tumwater","Rocky Reach","Wells","Zosel")
  data <- data.frame()
  for(i in 1:length(dams)){
    data <- rbind(data,getAnnualDamCounts(dams[i]))
    print(dams[i])
  }
  data <- select(data,1,2,3,4,5,6,7,8,9,10,11)
  data1 <- melt(data, id.vars = c("Project", "Year"), variable.name = "Species", value.name = "Counts")
  data1 <-filter(data1,Counts>0)
  
  data1$Project <- factor(data1$Project, levels=rev(projects))
  return(data1)
  
}
#retrieve from DART
damcounts <- getAnnualCounts()

geomean<-function(x){
  iii<-(x>0)
  y<-exp(mean(log(x[iii]),na.rm=TRUE))
  return(round(y))
}

#modify functions to generalize for number of geomean years
getMeanData <- function(df,project,year,species,span){
  return(filter(df,Project==project,Year > year-span,Year <= year,Species==species))
}

getGeoMeans <- function(df,span,min.years){
  geos <- data.frame()
  for(i in 1:nrow(df)){
    mdat <- getMeanData(df,df$Project[i],df$Year[i],df$Species[i],span)
    mdat <- mdat[!is.na(mdat$Counts),]
    if(nrow(mdat) >= min.years){
      g <- geomean(mdat$Counts)
      geos <- rbind(geos,data.frame(Project=df$Project[i],Year=df$Year[i],Species=df$Species[i],Geomean=g))
    }
  }
  geos$Geomean <- ifelse(is.nan(geos$Geomean),NA,geos$Geomean)
  newname <- paste(span,"yrGeomean",sep="")
  names(geos) <- c("Project","Year","Species",newname)
  return(geos)
}

addGeoMeans <- function(df){
  gmeans5 <- getGeoMeans(df,5,4)
  gmeans10 <- getGeoMeans(df,10,8)
  df1 <- merge(df,gmeans5,all.x=T)
  df2 <- merge(df1,gmeans10,all.x=T)
  return(df2)
}

#add geomeans and write file
damcounts.means <- addGeoMeans(damcounts)
write.csv(damcounts.means,"DamCounts_05-04-2021.csv",row.names=F)

getProjectMeans <- function(proj){
  url1 <- "http://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=adult&outputFormat=csvSingle&year%5B%5D=2016&loc%5B%5D="
  url2 <- "&ftype%5B%5D=fc&data%5B%5D=Adult+Chin+10YrAvg&data%5B%5D=Adult+Chin+Jck+10YrAvg&data%5B%5D=Adult+Stlhd+10YrAvg&data%5B%5D=Adult+Stlhd-W+10YrAvg&data%5B%5D=Adult+Coho+10YrAvg&data%5B%5D=Adult+Sock+10YrAvg&data%5B%5D=Adult+Lamprey+10YrAvg&data%5B%5D=Adult+Shad+10YrAvg&data%5B%5D=&data%5B%5D=Dissolved+Gas+Pct+10YrAvg&data%5B%5D=Inflow+10YrAvg&data%5B%5D=Outflow+10YrAvg&data%5B%5D=Spill+10YrAvg&data%5B%5D=Temp+%28SC%29+10YrAvg&data%5B%5D=Temp+%28WQM%29+10YrAvg&startdate=1%2F1&enddate=12%2F31&avgyear=2016&sumAttribute=none&consolidate=1&zeros=1&grid=1&y1min=0&y1max=&y2min=&y2max=&size=medium"
  url <- paste(url1,proj,sep="")
  url <- paste(url,url2,sep="")
  temp <- read.csv(url)
  temp <- filter(temp,year=="Avg07-16")
  return(temp)
}

getMeans <- function(){
  require(dplyr)
  require(reshape2)
  dams <- c("BON","IHR","JDA","LGS","LWG","LMN","MCN","PRD","PRO","ROZ","RIS","RRH","TDA","TUM","WAN","WEL","WFF","ZOS")
  projects <- c("Willamette Falls","Bonneville","The Dalles","John Day","McNary","Ice Harbor","Lower Monumental","Little Goose","Lower Granite","Prosser","Roza","Priest Rapids","Wanapum","Rock Island","Tumwater","Rocky Reach","Wells","Zosel")
  data <- data.frame()
  for(i in 1:length(dams)){
    data <- rbind(data,getProjectMeans(dams[i]))
    print(dams[i])
  }
  data <- select(data,2,3,6,7)
  names(data) <- c("Day","Dam","Variable","Value")
  #data1 <- melt(data, id.vars = c("Project", "Day"), variable.name = "Species", value.name = "Counts")
  #data1 <-filter(data1,Counts>0)
  #data$Project <- factor(data$Project, levels=rev(projects))
  return(data)
}


getTime <- function(url){
time1 <- as.numeric(Sys.time())
temp <- read.csv(url)
print(as.numeric(Sys.time())-time1)
return(temp)
}

getProjectSummaries <- function(){
  dams <- c("BON","IHR","JDA","LGS","LWG","LMN","MCN","PRD","PRO","ROZ","RIS","RRH","TDA","TUM","WAN","WEL","WFF")
  data <- data.frame()
  for(i in 1:length(dams)){
    data <- rbind(data,getProjectSummary(dams[i]))
    print(dams[i])
  } 
  return(data)
}

getProjectSummary <- function(dam){
  url1 <- "http://www.cbr.washington.edu/dart/cs/php/rpt/adult_project_sum.php?sc=1&outputFormat=csv&year=2017&proj="
  url <- paste(url1,dam,sep="")
  data <- read.csv(url)
  data$project <- dam
  return(data %>% filter(Date.Range != ""))
}

getCounts <- function(year,dam){
  url <- paste("http://www.cbr.washington.edu/dart/cs/php/rpt/adult_daily.php?sc=1&outputFormat=csv&year=",year,"&proj=",dam,"&span=no&startdate=1%2F1&enddate=12%2F31&run=&syear=",year,"&eyear=",year,"&avg=1",sep="")
  counts <- read.csv(url) %>% filter(Date != "")
  suppressWarnings(counts$Date <- ymd(counts$Date))
  counts <- counts %>% filter(!is.na(Date))
  counts$day<- yday(counts$Date)
  n <- names(counts)
  counts <- counts %>% select(1,5,7,9,11,13,15,17,19,21,23,25,27)
  counts[is.na(counts)] <- 0
  for(i in 2:11){
    counts[,i] <- cumsum(counts[,i])
  }
  print(dam)
  return(counts)
}

getAllMeans <- function(){
  dams <- c("BON","IHR","JDA","LGS","LWG","LMN","MCN","PRD","PRO","ROZ","RIS","RRH","TDA","TUM","WAN","WEL","WFF")
  means <- data.frame()
  for(i in 1:length(dams)){
  #for(i in 1:2){
    mean <- getCounts(2019,dams[i])
    means <- rbind(means,mean)
  }
  names(means) <- c("Project","Chinook","Jack.Chinook","Steelhead","Wild.Steelhead","Sockeye","Coho","Jack.Coho","Shad","Lamprey","Bull.Trout","Pink","day")
  for(i in 1:366){
    temp <- filter(means,day==i)
    temp$day <- NULL
    write.csv(temp,paste("10YrMeans/10YrMeanDay",i,".csv",sep=""),row.names=FALSE)
  }
  return(means)
}
