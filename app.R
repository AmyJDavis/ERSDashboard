########################################################################
########################################################################
###
### Shiny app for Rabies quarterly reports as a dashboard
###   - This version uses the ERS area instead of county boundaries only to show point needs
###   - Using the actual update ERS area!!! Sent from Jordona 11/20/2024
###   - This version include many tab options for beta testing
###
### Amy J Davis
### June 6, 2024, updated February 25, 2025
###
########################################################################
########################################################################

###  Details of this app
###  This app will be a visualization of ERS samples by state, location, and category
###     We might want to include some metrics of how well the states are doing
###     sample-wise, point-wise, and coverage-wise

### Libraries
library(shiny) 
library(shinythemes)
library(DT)
library(leaflet)
library(leaflegend)
library(readxl)
library(tidyverse)
library(shinycssloaders)
library(htmltools)
library(reshape2)
library(viridis)
library(data.table)
library(shinyjs)
library(shinycssloaders)
library(shinydashboard)
library(pwr)
#library(shinyWidgets)
library(hablar)
library(sf)
library(mapview)
library(webshot2)
library(KernSmooth)
library(patchwork)
require(tigris)
library(bslib)
library(ggrepel)
library(plotly)
library(lwgeom)


get_popup_content <- function(dfsp) {
  paste0(
    "<b>ID: ", dfsp$IDNUMBER, "</b>",
    "<br>",
    "<br>Species: ", dfsp$SPECIES,
    "<br>County in record: ", dfsp$COUNTY,
    "<br>ERS Category: ",dfsp$Category,
    "<br>Rabies: ",dfsp$RABIESBRAINRESULTS)
}

get_genetic_popup_content <- function(gensf) {
  paste0(
    "<b>ID: ", gensf$IDNUMBER, "</b>",
    "<br>",
    "<br>Species: ", gensf$SPECIES,
    "<br>County in record: ", gensf$COUNTY,
    "<br>Sample source: ",gensf$Sample,
    "<br>Fate: ",gensf$FATE,
    "<br>Method: ",gensf$METHOD)
}
addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  # I added my modification here, see below
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity))
}

col_types=c('date','text','text','text','text','text','text','text','text','text','text','text','date','text','numeric','text','text','text',
            'numeric','numeric','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text',
            'text','text','text','text','text','text','text','text','text','text','text','text','numeric','text','text','text','text','text',
            'text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','date','text',
            'text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text',
            'text','text')
ersnames=data.frame(Num=1:6,Name=c("Strange Acting","Found Dead","Roadkill",
                                   "Surveillance Trapped","NWCO/Other","Unknown"),
                    Points=c(15,15,5,2,1,1))

### Set the rabies states
rabiestates=data.frame(StateName=state.name,StateAbb=state.abb)[c(1,7,8,10,17,19:21,22,24,29,30,32,33,35,38:40,42,45,46,48),]
rabiestates$Fips=tidycensus::fips_codes[match(rabiestates$StateAbb,tidycensus::fips_codes$state),"state_code"]
ers_states=c("Alabama","Georgia","Maine","Massachusetts","Michigan","Mississippi",
             "New Hampshire","New York","North Carolina","Ohio","Pennsylvania",
             "Tennessee","Vermont","West Virginia")

### Color palette options
classcolors=c("#034521","#F2EFE9","#353264","#5456AA","#7084E5","white","#FFF7E1","#FEED89","#FECF49")
classcolors2=c("#034521","#F2EFE9","#353264","#7476B8","white","#FBF1AB","#FECF49")
classcolorstodate=c("#034521","#F2EFE9","#440154FF","#31688EFF","#35B779FF","#FDE725FF")
classcolorstodate=c("#034521","#F2EFE9","#440154","#31688E","#35B779","#FDE725")

gencolors=c("#000004","#721F81","#F1605D","#FCFDBF")

# Options for Spinner
options(spinner.color="#0C1936", spinner.color.background="#ffffff", spinner.size=2)
options(shiny.maxRequestSize = 30*1024^2)


###
## Read in the ORV and Archived county ERS shapefiles
orv=read_sf("www/CY2023ORV_merged.shp")
orv=st_transform(orv,crs=4326)

orvhatch <- HatchedPolygons::hatched.SpatialPolygons(orv, density = c(20), angle = c(45, 135))
st_crs(orvhatch)=4326

# ctyorv=read_sf("C:/Users/apamydavis/OneDrive - USDA/Documents/Rabies/ORV shapefiles/ctyorv.shp")
ctyorv=read_sf("www/ctyallERSv3cc.shp",crs=4326)
#ctyorv=st_transform(ctyorv,crs=4326)
ctyorv$FIPS=paste0(ctyorv$STATEFP,ctyorv$COUNTYFP)
ctyorv$HighPriority=rbinom(dim(ctyorv)[1],1,0.02)

## Getting general state goal stuff
ctyorv1b=ctyorv%>%st_drop_geometry()%>%
  dplyr::select(STATE_NAME,PTGoal,StrgA_N,FndDd_N,Rdkll_N,SrvTr_N,NWCO_Nd)
names(ctyorv1b)=c("STATE_NAME","PTGoal", "StrageAct_Needed","FoundDead_Needed",
                  "Roadkill_Needed","SurvTrap_Needed","NWCO_Needed")


## Read in the current dataset 
#ersdata <- read_excel("www/2024 ERS_19Feb2025.xlsx",col_types = col_types)
## Read in the current dataset 
#ersdata <- read_excel("www/2024 ERS_19Feb2025.xlsx",col_types = col_types)
#ersdata <- read_excel("www/DummyERS_Feb2025.xlsx",col_types = col_types)
ersdata <- read_excel("www/2025 USDA ERS Jan_Feb for Dashboard.xlsx",col_types = col_types)

## If the ERSCATEGORY has been left blank, this will take the information from the FREETEXT
ersdata$ERSCATEGORY=ifelse(is.na(ersdata$ERSCATEGORY)|ersdata$ERSCATEGORY=="null",gsub("\\;.*","",ersdata$FREETEXT),ersdata$ERSCATEGORY)
ersdata$ERSCATEGORY=ifelse(nchar(ersdata$ERSCATEGORY)==1,ersdata$ERSCATEGORY,
                           ifelse(grepl("ERS=",toupper(ersdata$ERSCATEGORY)),gsub("ERS=","",toupper(ersdata$ERSCATEGORY)),NA))


## Lets see about combining with old data before any processing
### Old data for comparisons
old=read.csv("www/ERS_2006-2024_ProcessedAll.csv")
old=old[,1:94]
old$DATE=as.POSIXct(old$DATE,"%Y-%m-%d",tz = "UTC")

# change some dumb names
names(old)[which(names(old)%in%setdiff(names(old),names(ersdata)))]=setdiff(names(ersdata),names(old))
old=old[,match(names(ersdata),names(old))]
old=rbind(old,ersdata)
old$STATE_NAME=rabiestates[match(old$STATE,rabiestates$StateAbb),"StateName"]

# Process older data
old=old%>%filter(SPECIES=="RACCOONS") %>% 
  mutate(Point=ifelse(ERSCATEGORY==1,14,
                      ifelse(ERSCATEGORY==2,20,
                             ifelse(ERSCATEGORY==3,4,1))),
         Month=format(DATE,"%m"),
         Year=format(DATE,"%Y"))

old$TilNow=ifelse(as.numeric(format(old$DATE,"%m"))<
                    max(as.numeric(format(ersdata$DATE,"%m")))+1,"Year To Date","Total")


## Now process like normal
ersdata$DATE2=as.POSIXct(ersdata$DATE,format="%Y-%m-%d")
ersdata=ersdata[!is.na(ersdata$ERSCATEGORY),]
ersdata$Category=ersnames[match(ersdata$ERSCATEGORY,ersnames$Num),"Name"]

## Remove non rabies states
ersdata=ersdata[which(ersdata$STATE%in%rabiestates$StateAbb),]

## Specific fix for DeKalb county Alabama and Dona Ana county New Mexico
ersdata$COUNTY[ersdata$COUNTY=="DE KALB"]="DEKALB"
ersdata$COUNTY[ersdata$COUNTY=="DONA ANA"]="DOÑA ANA"
ersdata$COUNTY[ersdata$COUNTY=="LA SALLE"&ersdata$STATE=="LA"]="LASALLE"
ersdata$STATE_NAME=rabiestates[match(ersdata$STATE,rabiestates$StateAbb),"StateName"]
ersdata$STCO=tolower(paste(ersdata$STATE_NAME,ersdata$COUNTY,sep=","))
ersdata$Points=ersnames[match(ersdata$ERSCATEGORY,ersnames$Num),"Points"]

## Get points
### Calculating points from our data
## Need to calculate the points overlapping with polygons as counties are split
##  between high and low priority areas
erssf=st_as_sf(ersdata[which(!is.na(ersdata$LONGITUDE)),],coords = c("LONGITUDE","LATITUDE"),crs=4326)

## Trying a join 
sf_use_s2(FALSE)
pwp=st_join(erssf,ctyorv)
## 
pwp$TierName=ifelse(is.na(pwp$Tier),"None",
                    ifelse(pwp$Tier==1,"High","Low"))
pwp$CtyTier=paste(pwp$NAME,pwp$TierName,sep="-")
pwp=pwp%>%
  mutate(Points=replace_na(Points,0),
         STATE_NAME=STATE_NAME.x) %>% 
  dplyr::select(-STATE_NAME.x,-STATE_NAME.y)

## Summarizing data by county-tier designation
NRMPcounty=pwp%>%st_drop_geometry%>%
  group_by(StCtyTier,STATE_NAME)%>%
  summarise(Pts=sum(Points),
            PtGoal=mean(PTGoal),
            Status=ifelse(Pts>=PtGoal,"Goal met","Not met"))

###
### Getting state level goals
###
ctyorv1b=ctyorv%>%st_drop_geometry()%>%
  dplyr::select(STATE_NAME,PTGoal,StrgA_N,FndDd_N,Rdkll_N,SrvTr_N,NWCO_Nd)
names(ctyorv1b)=c("STATE_NAME","PTGoal", "StrageAct_Needed","FoundDead_Needed",
                  "Roadkill_Needed","SurvTrap_Needed","NWCO_Needed")
statsamp=ctyorv1b%>%group_by(STATE_NAME)%>%
  summarise(PointGoal=sum(PTGoal,na.rm = TRUE),
            StrageAct_Needed=sum(StrageAct_Needed,na.rm = TRUE),
            FoundDead_Needed=sum(FoundDead_Needed,na.rm = TRUE),
            Roadkill_Needed=sum(Roadkill_Needed,na.rm = TRUE),
            SurvTrap_Needed=sum(SurvTrap_Needed,na.rm = TRUE),
            NWCO_Needed=sum(NWCO_Needed,na.rm = TRUE))
########
###
###   Getting county-tier evaluations based on points from this year
###
########
## ERS area by county tier information
ctypts=left_join(ctyorv,NRMPcounty,by=c("StCtyTier"))
ctypts=ctypts%>%
  mutate(STATE_NAME=STATE_NAME.x,
         Pts=replace_na(Pts,0),
         TierName=replace_na(TierName,"None"),
         PtDiff=Pts-PTGoal,
         PtLevs=ifelse(Tier==0,ifelse(Pts>0,-5,-4),
                       ifelse(PtDiff<(-50),-3,
                              ifelse(PtDiff<(-10),-2,
                                     ifelse(PtDiff<0,-1,
                                            ifelse(PtDiff==0,0,
                                                   ifelse(PtDiff<10,1,
                                                          ifelse(PtDiff<50,2,3))))))),
         Point_Levels=factor(ifelse(Tier==0,ifelse(Pts>0,"Bonus","No ERS"),
                                    ifelse(PtDiff<(-50),"Need 50+ points",
                                           ifelse(PtDiff<(-10),"Need 10+ points",
                                                  ifelse(PtDiff<0,"Need 1+ points",
                                                         ifelse(PtDiff==0,"Goal met",
                                                                ifelse(PtDiff<10,"Good job",
                                                                       ifelse(PtDiff<50,"Great job","Awesome job!"))))))),
                             ordered=TRUE,levels= c("Awesome job!","Great job","Good job",
                                                    "Goal met","Need 1+ points","Need 10+ points","Need 50+ points","No ERS","Bonus")),
         PtGoalProgress=round(PTGoal*(max(as.numeric(format(ersdata$DATE,"%m")))/12),0),
         PtDiffProgress=Pts-PtGoalProgress,
         PtLevProgress=ifelse(Tier==0,ifelse(Pts>0,-3,-2),
                              ifelse(PtDiff==0,1,
                                     ifelse(PtDiff>0,2,
                                            ifelse(PtDiffProgress<0,-1,0)))),
         PointLevelProgress=factor(case_when(PtLevProgress==(-3)~"Bonus",
                                             PtLevProgress==(-2)~"No ERS",
                                             PtLevProgress==(-1)~"Points needed",
                                             PtLevProgress==0~"On track",
                                             PtLevProgress==1~"Goal met",
                                             PtLevProgress==2~"Goal exceeded"),
                                   ordered=TRUE,levels=c("Bonus","No ERS","Points needed","On track","Goal met","Goal exceeded")),
         PtLevs2=ifelse(Tier==0,ifelse(Pts>0,-4,-3),
                        ifelse(PtDiff<(-9),-2,
                               ifelse(PtDiff<(0),-1,
                                      ifelse(PtDiff==0,0,
                                             ifelse(PtDiff>9,2,1))))),
         Point_Levels2=factor(ifelse(Tier==0,ifelse(Pts>0,"Bonus","No ERS"),
                                     ifelse(PtDiff<(-10),"Need 10+ points",
                                            ifelse(PtDiff<(-1),"Need 1+ points",
                                                   ifelse(PtDiff==0,"Goal met",
                                                          ifelse(PtDiff>10,"Awesome job!","Great job"))))),
                              ordered=TRUE,levels = c("Awesome job!","Great job","Goal met","Need 1+ points","Need 10+ points","No ERS","Bonus"))) %>% 
  dplyr::select(-STATE_NAME.x,-STATE_NAME.y)



######
###
### Evaluating the spatial coverage of the previous years compared to this year to date
###
oldsf=st_as_sf(old,coords = c("LONGITUDE","LATITUDE"),crs=4326)
#oldsf=st_transform(oldsf,crs=3395)

oldcty=st_join(oldsf[,-which(names(oldsf)=="STATE_NAME")],ctyorv)

## Summarize stuff by year and cty area
oldsum=oldcty %>% st_drop_geometry %>% 
  group_by(Year,StCtyTier,STATE_NAME) %>% 
  summarise(Pts=sum(Point))

# Get all the county info for evaluation from ctyorv
ctycomp=ctyorv %>% st_drop_geometry() %>% 
  dplyr::select(StCtyTier,TierName,STATE_NAME,PTGoal)

countyareas=unique(ctycomp$StCtyTier)
yrs=unique(oldsum$Year)
allcombs=expand.grid(countyareas,yrs)
names(allcombs)=c("StCtyTier","Year")

ctyold=allcombs %>% left_join(ctycomp)
ctyold=ctyold %>% left_join(oldsum)
ctyold=ctyold %>%
  mutate(Pts=replace_na(Pts,0),
         Status=ifelse(PTGoal==0,ifelse(Pts>0,"Bonus","No goal"),
                       ifelse(Pts>=PTGoal,"Goal met","Not met")))

## Now getting state level summary data % counties with points met per year
oldstsum=ctyold %>% 
  group_by(STATE_NAME,Year) %>% 
  summarise(TotCtyAreas=length(which(PTGoal!=0)),
            Nummet=length(which(Status%in%c("Bonus","Goal met"))),
            PercentMet=pmin(1,Nummet/TotCtyAreas)*100) 

## ERS regions
ersarea=read_sf("www/2024 ERS Areas.shp")




#########################
### 
### Get new genetic samples from the ERS data
###
#########################

### Matt's assessment
mattgoal=read.csv("www/Raccoon genetic archive sampling counties 2025.csv")

### Needing to do a lot of fixing to make it have the same names as I need :(
mattgoal$ST=state.abb[match(mattgoal$State,state.name)]
mattgoal$StCtyTier=paste(mattgoal$ST,mattgoal$County,mattgoal$ERS_Tiers,sep="-")

setdiff(mattgoal$StCtyTier,ctyorv$StCtyTier)
mattgoal=mattgoal|>dplyr::select(StCtyTier,Samples_needed,County_area,Priority,Notes)

### Merge with ctyorv
ctyorv=left_join(ctyorv,mattgoal,by="StCtyTier")


### Get new genetic samples from the ERS data
# Starting simple only DNASAMPLE==YES
ersgen=ersdata|>filter(DNASAMPLE=="YES")|>
  dplyr::select(DATE,STATE,COUNTY,LATITUDE,LONGITUDE,SPECIES,IDNUMBER,SEX,RELATIVEAGE,FATE,METHOD)|>
  mutate(Sample="New")


### Read in the data from Matt that has the genetic data up to 2024
gen=read.csv("www/GeneticsDataFromMatt.csv")
gen=gen %>% filter(!is.na(LONGITUDE),LONGITUDE<0,SPECIES=="RACCOONS",STATE%in%rabiestates$StateAbb) %>% 
  mutate(
    DATE=as.POSIXct(DATE,format="%m/%d/%Y"),
    FATE=" ",
    METHOD=" ",
    Sample="Archived"
  )
genc=rbind(gen,ersgen)


###  Making up some goals and priority areas, but these will be provided by Matt
gensf=st_as_sf(gen,coords=c("LONGITUDE","LATITUDE"),crs = (4326))
ctygen=st_intersects(ctyorv,gensf)
ctyorv$GenSampsArchived=sapply(ctygen,length)

ersgensf=st_as_sf(ersgen,coords = c("LONGITUDE","LATITUDE"),crs=4326)
ctyersgen=st_intersects(ctyorv,ersgensf)
ctyorv$GenSampsNew=sapply(ctyersgen,length)


###
statmaps=sf::read_sf("www/states.shp")
statmaps=st_transform(statmaps,crs=4326 )
statmap1=statmaps[which(statmaps$STATE_FIPS%in%rabiestates$Fips),]

##
## Trying to get an estimate of spatial coverage within a county area
##
gencsf=st_as_sf(genc,coords=c("LONGITUDE","LATITUDE"),crs=4326)
genpts=st_join(gencsf,ctyorv)
genpts=st_transform(genpts,crs=3395)
capoly=genpts|>
  summarize() |>
  st_buffer(dist=10000)

## Cropping the buffered areas by county tier areas
ctyorvx=st_transform(ctyorv,crs=3395)
ctyorvx <- st_make_valid(ctyorvx)

capc=st_intersection(ctyorvx,capoly)
capc=capc|>
  mutate(GenArea=as.numeric(st_area(geometry)/1000^2))


## Add genetic coverage data to ctyorv
ctyorv$AreaKm=as.numeric(st_area(ctyorvx$geometry)/1000^2)
ctyorv$GenCoverkm=capc$GenArea[match(ctyorv$StCtyTier,capc$StCtyTier)]
ctyorv$GenAreaPerCover=ctyorv$GenCoverkm/ctyorv$AreaKm*100

### Now creating a genetic evaluation 
ctyorv=ctyorv|>
  mutate(GenPriority=factor(ifelse(is.na(Priority),"No",
                                   ifelse(Priority==1,"High","Low")),
                            ordered=TRUE,levels=c("No","Low","High")),
         Samples_needed=replace_na(Samples_needed,0),
         Notes=replace_na(Notes," "),
         GenAreaPerCover=replace_na(GenAreaPerCover,0),
         GenSampDiff=pmax(0,Samples_needed-GenSampsNew),
         GenLevel=factor(ifelse(Samples_needed==0&GenSampsArchived==0&GenSampsNew==0,"No new goal",
                                ifelse(GenSampDiff==0,"Genetic goal met",
                                       ifelse(GenSampDiff<10,"1+ samples needed","10+ samples needed"))),
                         ordered=TRUE,levels=c("10+ samples needed","1+ samples needed",
                                               "Genetic goal met","No new goal")),
         GenPtLev=as.numeric(GenLevel))




########################
###
### State maps
###
########################
statmapsall=sf::read_sf("www/states.shp")
statmaps=statmapsall[which(statmapsall$STATE_FIPS%in%rabiestates$Fips),]
statmaps=st_transform(statmaps,crs=4326)

## Allow blank state selection for app
rabiestates=rbind(c(" "," "," "),rabiestates)


## Creating decreasing legend
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft", 
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins	
      
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
      
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
      
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, 
                       na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}



# Define UI f
ui <- dashboardPage(
  
  skin='yellow',
  title="ERS Data Collection Dashboard",
  header = dashboardHeader(titleWidth='100%',
                           # Set height of dashboardHeader
                           tags$li(class = "dropdown",
                                   tags$style(".main-header {max-height: 100px}"),
                                   tags$style(".main-header .logo {height: 100px;}"),
                                   tags$style(".sidebar-toggle {height: 100px; padding-top: 1px !important;}"),
                                   tags$style(".navbar {min-height:100px !important}")
                           ),
                           title=span(tags$img(src='USDA_bw_transparent.png', style='margin-top:8px;',height=90,width=120,align="left"),
                                      column(10, class="title-box", 
                                             tags$h1(class="primary-title", style='margin-top:25px;font-size=50px',
                                                     "ERS Data Collection Dashboard"),
                                             tags$h3(class="secondary-title", style='margin-top:8px;font-size=30px',
                                                     "Data up to: February 27, 2025")))),
  
  # Sidebar  
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
    width=350,
    title=tags$h2(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"State selection",align='left'),
    useShinyjs(),
    # This makes web page load the JS file in the HTML head.
    # The call to singleton ensures it's only included once
    # in a page. It's not strictly necessary in this case, but
    # it's good practice.
    singleton(tags$head(tags$script(src = "message-handler.js"))),
    tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"  Select to see a specific state's information.",align='left'),
    selectInput("staten","Select a State",choices = rabiestates$StateName,selected = " "),
    tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"To download a report of ERS surveillance data, first select a state. A download button will appear below, click the button to generate report.",align='left'),
    shinyjs::hidden(downloadButton(outputId = "reportX",label =  "Download State Report",style="color:black;font-size:18px")),
    tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"_________________________",align='left'),
    conditionalPanel(condition = 'input.tabs=="maptabv1"',
                     tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Mapping options",align='left'),
                     radioButtons(inputId = "mapheat",label = "Show density of surveillance samples or county points assessment?",
                                  choices = c("County assessment","County assessment to date","Heat map","Neither"),selected = "County assessment"),
                     radioButtons(inputId = "ShowPts",label = "Display surveillance sample locations?",
                                  choices = c("Yes","No"),selected="No")),
    conditionalPanel(condition = 'input.tabs=="maptabv2"',
                     tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Mapping options",align='left'),
                     radioButtons(inputId = "mapheat",label = "Show density of surveillance samples or county points assessment?",
                                  choices = c("County assessment","Heat map","Neither"),selected = "County assessment"),
                     radioButtons(inputId = "ShowPts",label = "Display surveillance sample locations?",
                                  choices = c("Yes","No"),selected="No")),
    conditionalPanel(condition = 'input.tabs=="gentab"',
                     tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Genetic mapping options",align='left'),
                     radioButtons(inputId = "mapheatgen",label = "Show density of genetic samples or county assessment?",
                                  choices = c("County assessment","Heat map","Spatial gaps","Neither"),selected = "County assessment"),
                     radioButtons(inputId = "ShowPtsgen",label = "Display genetic sample locations?",
                                  choices = c("Yes","No"),selected="No"))
  ),
  
  # Show output
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
                      .main-sidebar{
                      position:fixed;
                      width:350px;
                      height:100%;
                      overflow-y:auto; /*Enables scrolling in sidebar*/
                      background-color:#222d32;/*match dashboard color*/
                      }
                      .content-wrappeer,.right-side{
                      margin-left:350px; /*Match sidebar width*/
                      }
                      .nav-tabs > li > a {
                        border: 1px solid #222D32 !important; /* Outline around individual tabs */
                        border-radius: 5px; /* Optional: rounded corners */
                        margin: 2px; /* Space between tabs */
                      }
                      .nav-tabs > li.active > a {
                        background-color: #222D32 !important; /* Highlight active tab */
                        color: white !important;
                      }
                      .shiny-notification{
                      position:fixed;
                      bottom:10px;
                      left:10px;
                      right:auto;
                      background-color:#F7BF63;
                      color:black;
                      font-weight:bold;
                      padding:10px;
                      border-radius:5px;
                      }
                        "
        ))
    ),
    tabsetPanel(id="tabs",selected="guidetab",
                tabPanel(title = "User Guide",value="guidetab",icon=icon("info"),
                         box(width=12,title=span("How to use this dashboard",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             column(7,HTML(
                               "<p>Welcome to the ERS Data Collection Dashboard.  The intent of this dashboard is to empower rabies biologists and state directors by providing a clear, user-friendly way to visualize and interact with the data you collect. This should serve as a tool is to help gain insights into program performance, easily track metrics of success, and efficiently generate reports that can be shared with stakeholders to demonstrate impact and progress.</p>
                                <h4><strong>This dashboard will show: </strong></h4>
                                       <ul>
                                           <li> ERS data from the current calendar year </li>
                                           <li>	Data will be updated monthly – see updated date at top</li>
                                           <li>	The NRMP has identified high and low tiers within the ERS priority area (see figure to the right)</li>
                                           <li>	Counties that overlap these regions may be split into high, low, and none ERS priority regions for sampling </li>
                                           <li> The NRMP set ERS point goals by state. </li>
                                           <li>	To get point goals by county, the ERS tier and the area (km²) of the county are used (larger counties will have a higher point goal and areas in the high ERS tier have higher point goals than low ERS Tiers)</li>

                                       </ul>

                               "
                             ),style="font-size:130%;"),
                             column(5,img(src="CtyorvTierMap.png",width="100%")),
                             column(11,HTML(
                               "<h4><strong>ERS point descriptions: </strong></h4>
                                       <ol>
                                           <li> <strong>Strange acting</strong> (collected via Matrix collaborators or by WS) = 15 points/sample. </li>
                                           <li>	<strong>Found dead </strong>(not road kills—e.g., found dead on barn floor, in lawn, etc.) by WS or via Matrix collaborators = 15 points/sample.</li>
                                           <li>	<strong>Road kill</strong> collected from formal surveys or opportunistically by WS = 5 points/sample
                                                <ul>
                                                    <li> Plus 1 point/50 miles driven during a formalized road kill survey (note these additional points will not be visualized in this dashboard.</li>
                                                </ul> </li>
                                           <li>	<strong>WS trapped</strong> or shot for ERS in specified raccoon rabies risk areas where other forms of surveillance are deemed largely impractical = 2 points/sample. </li>
                                           <li> <strong>NWCO/Other</strong> nuisance-trapped or homeowner-derived samples, etc. exhibiting no signs, wounds or lesions indicative of rabies (i.e., apparently healthy animals) = 1 point/sample. This category includes WS “Incidental Take” samples.</li>
                                           <li> <strong>Unknown</strong> Samples for which fate information is unknown that are collected by a cooperator or placed in matrix freezers with incomplete data = 1 point/sample. This category should be used only in rare instances.</li>
                                       </ol>
                               "
                             ),style="font-size:130%;")),
                         box(width=12,title=span("Tab Descriptions",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             column(11, HTML(
                               "<p>You can toggle through the different tabs to see different aspects of the data. To use many features of this dashboard, you need to select a state of interest (using the panel on the left). Once a state is selected you can see different tables and figures that this dashboard produces, and you can download a report of that state's data. Below are descriptions of the tabs and how to use them.</p>

                               <ul>
                                    <li><strong>Points Overview</strong> – This tab summarizes the ERS points collected to the updated date. There is a pie chart that shows the number of samples by ERS category and the number of points by category. There are bar charts showing counties with point goals exceeded and counties where additional points are needed. </li>
                                    <li><strong>Trend Over Time</strong>  –  This tab compares the ERS data collected to date with previous years of data. There are two main components of evaluation of these data: total number of points collected and spatial coverage of ERS points.</li>
                                    <li><strong>Distribution Map v1</strong> – This tab has an interactive map that lets you see the ERS sample locations. The default plot shows the county evaluations (has the county goal be met based on the number of samples collected to date). There is an option to gauge if the counties are on track within the calendar year to meet the end of year goals. Another option shows a heat map to visualize the density of samples and where they were collected. You can also choose to include the actual location data, to see where all samples have been collected. As you scroll over the county evaluation map it will tell you which county your cursor is in, what the county goal is, and how many points have been collected so far. If you click on a point, an info box will pop up that tells you the IDNUMBER, SPECIES, COUNTY, and ERSCATEGORY on that MIS record.  At the bottom of the tab there is a table showing county level evaluations but is limited to counties that need additional points.</li>
                                    <li><strong>Distribution Map v2</strong>  – This is similar to the v1 map and is only for the beta testing phase. There are a few differences to gauge preferences from users at this stage.  The county evaluation map has fewer colors (removing the 50+ points needed option). The information boxes at the top show slightly different information. The descriptions are largely the same, but only change in how they reference the colors used in the maps. The table at the bottom shows all counties and not just those needing additional points.  I’m hoping by showing some options users can get a sense of which options they prefer. </li>
                                    <li><strong>General State Needs</strong>  – This tab shows the number of points needed by state. As different surveillance categories are worth more points than others, it also shows how many samples (if only samples of that type were collected) that would need to be collected to meet the state’s point goal. This tab does not change as different states are selected. </li>
                                    <li><strong>Genetic Samples</strong>  – In addition to ERS surveillance, rabies biologists also collect genetic samples to support the raccoon genetic archive. This tab focuses solely on the genetic sampling efforts but includes both samples that have previously been collected (archived) and samples that have been collected this calendar year (new). There is an interactive map that has a few options. The default option is the county assessment map that shows areas where additional genetic samples are needed and where genetic samples goals have been met. There is a heat map option to show the areas where more genetic samples have been collected. Getting samples that are spatially spread out is important for the genetic archive. There is a spatial gaps map option that shows all sample locations with buffers around them. Any area not covered by the collective buffers of all of the samples would be good areas to target. You can also display the locations of samples on the map. Finally, there is a table of the county goals and points collected. </li>
                                </ul>
                               "
                             ),style="font-size:130%;")
                         ),
                         box(width=12,title=span("Troubleshooting",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             # 
                             column(11,p("",style="font-size:130%;"),
                                    p("We have tried to make this app as user-friendly as possible. However, we know that issues may arise. ",style="font-size:130%;"), 
                                    p("     •	",strong("Email Amy Davis (Amy.J.Davis@usda.gov) if you have issues. "),style="font-size:130%;")
                             )
                         )
                ),
                tabPanel(title = "Points Overview",value="overviewtab",icon = icon("bar-chart"),
                         column(width=4,box(width=3.95,title=span("Samples",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            # varImp Plot
                                            plotlyOutput('PieSampPlots')
                         )),
                         column(width=4,box(width=3.95,title=span("Points",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            # varImp Plot
                                            plotlyOutput('PiePtsPlots')
                         )),
                         column(width=4,box(width=3.8,height=NULL, title =span("Points by category", style="color:white;font-size:28px"),
                                            collapsible = TRUE, 
                                            collapsed = FALSE,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            HTML("
                                              <p>These pie charts highlight how different categories of surveillance relate to the overall proportion of ERS points. The greater impact of strange acting and found dead samples, for example, are shown by comparing their relative sizes in the two pie charts.  </p>
                                          
                                              <h4><strong>Samples Pie Chart</strong></h4>
                                              <ul>
                                                <li> This chart shows the number of samples by surveillance category. </li>
                                              </ul>
                                              <h4><strong>Points Pie Chart</strong></h4>
                                              <ul>
                                                <li> This chart shows the percent of ERS points by surveillance category. The values are percents.  </li>
                                              </ul>
                                              <p>If no points have been collected in the state in this calendar year, no pie charts will be shown.</p>
                                            "),style="font-size:130%;")),
                         column(width=8,height=NULL,box(width=7.9,height=NULL,title=span("Points collected and points needed",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                                        # varImp Plot
                                                        plotlyOutput('CountyBars',height=800)
                         )),
                         column(width=4,box(width=3.9,height=NULL, title = span("County area evaluations", style="color:white;font-size:28px"),
                                            collapsible = TRUE, 
                                            collapsed = FALSE,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            HTML("
                                              <p>County evaluation bar charts. If there are two columns, the left-hand side shows the counties where the county area point goal was met or exceeded and the right-hand side shows the counties where points are still needed.</p>
                                              <p>If no county goals have been met, only the county area needs plot is shown. If all county goals are met only the county areas met plot is shown. </p>
                                              <h4><strong>County-areas where point goals are met</strong></h4>
                                              <ul>
                                                <li> The bars on this chart show the point goals (all of which have been met) in black and any points above the point goal are shown in gold. The plot is sorted with counties with the most points collected on top.</li>
                                              </ul>
                                              <h4><strong>County-area where points are still needed </strong></h4>
                                              <ul>
                                                <li> The bars on this chart show the points collected towards the point goal in black and the points still needed to achieve the point goal are shown in red. The plot is sorted with counties with the highest point goals on top. </li>
                                              </ul>
                                            "),style="font-size:130%;"))
                         
                ),
                tabPanel(title = "Trends Over Time",value="timetab",icon = icon("chart-line"),
                         column(width=7,box(width=6.8,title=span("Progress tracking ",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            withSpinner(plotlyOutput('ProgressPlot'),color = "#0C1936"))),
                         column(width=5,box(width=4.8,height=NULL,title=span("Description of progress plot",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            HTML("
                                          <p> The point of this plot is to get an idea of how point collection in this year is compared to a similar point in past years. </p>
                                          <ul>
                                            <li> The <span style='display: inline-block; width:15px;height:15px;background-color:#BEBEBE;margin-right: 5px;'></span> grey bars represent the total ERS points collected across the entire calendar year. </li>
                                            <li> The <span style='display: inline-block; width:15px;height:15px;background-color:#27408B;margin-right: 5px;'></span> blue portions of the bars show the progress in ERS point collection to the same month in this calendar year. </li>
                                            <li> The current year data only has data up to this month (no grey bar).  </li>
                                            <li> The <span style='display: inline-block; width:30px;height:3px;background-color:red;vertical-align:middle;margin-right: 5px;'></span> red horizontal line is the ERS point goal for the state. This is an overall objective but does not account for the spatial coverage.  </li>
                                          </ul>
                                         "),style="font-size:130%;")),
                         column(width=7,box(width=6.8,title=span("Spatial coverage tracking ",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            withSpinner(plotlyOutput('SpatialProgressPlot'),color = "#0C1936"))),
                         column(width=5,box(width=4.8,height=NULL,title=span("Description of spatial progress plot",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            HTML("
                                          <p> Here we are showing the percent of county-areas within a state where the ERS point goal has been met.  This figure highlights how good the spatial coverage of points collected is, this is another way of evaluating ERS data collection. </p>
                                          <ul>
                                            <li> Only county-areas in the ERS low or high tier area are evaluated here or if bonus points outside of the area were collected. </li>
                                            <li> If all county-areas have their point goal met, the bar will be at 100%.  </li>
                                            <li> The spatial coverage from previous years is shown as <span style='display: inline-block; width:15px;height:15px;background-color:#404040;margin-right: 5px;'></span> dark grey bars. </li>
                                            <li> The spatial coverage in the current year is shown in <span style='display: inline-block; width:15px;height:15px;background-color:#7FFFD4;margin-right: 5px;'></span> turquoise.  </li>
                                            <li> The goal would be to have 100% coverage, and the  <span style='display: inline-block; width:30px;height:3px;background-color:red;vertical-align:middle;margin-right: 5px;'></span> red line denotes that goal. </li>
                                          </ul>
                                         "),style="font-size:130%;"))
                ),
                tabPanel(title = "Distribution Map v1",value="maptabv1",icon = icon("map"),
                         box(width=12,height=NULL,title=span("Summary of location info",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             # Dynamic valueBoxes
                             valueBoxOutput("totsamps"),
                             valueBoxOutput("countyerror"),
                             valueBoxOutput("countymet")
                         ),
                         column(width=7,box(width=6.8,title=span("Map of MIS samples",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            withSpinner(leafletOutput(outputId = "mapx",height = 600),color = "#0C1936"),
                                            downloadButton( outputId = "dl",label = "Save the map?")
                         )),
                         column(width=5,box(width = 4.8,height=NULL, title = span("ERS Map Overview", style="color:white;font-size:28px"),
                                            collapsible = TRUE, 
                                            collapsed = FALSE,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            HTML("
    <p>This interactive map provides multiple ways to visualize Enhanced Rabies Surveillance (ERS) data. Users can toggle between different display options to assess sample coverage and distribution.You can save the map as an image by clicking the button on the bottom of the map.</p>

    <h4><strong>County-Level Assessment (Default View)</strong></h4>
    <ul>
      <li>The default map evaluates whether ERS point goals have been met at a county (or sub-county) level.</li>
      <li>Most areas are assessed by county, but some counties are divided into high-priority and low-priority ERS areas, meaning they have separate evaluations.</li>
      <li>Color Coding: <span style='display: inline-block; width:15px;height:15px;background-color:#5456AA;margin-right: 5px;'></span> purple-blue colors mean more samples are needed, <span style='display: inline-block; width:15px;height:15px;background-color:white;margin-right: 5px;'></span>white means point goal was met, <span style='display: inline-block; width:15px;height:15px;background-color:#FECF49;margin-right: 5px;'></span> gold colors means point goal was exceeded, and <span style='display: inline-block; width:15px;height:15px;background-color:#034521;margin-right: 5px;'></span> dark green means bonus samples were collected in areas outside the ERS region.</li>
    </ul>
        <h4><strong>County-Level Assessment To Date</strong></h4>
    <ul>
      <li>County point goals are for the entire calendar year. Use this option to show where you are on track to meet point goals, where points are still needed to be on track, and where the year goal is already met or exceeded. </li>
      <li>Similar to the general county assessment, areas are assessed by county and some are divided into high-priority and low-priority ERS areas.</li>
      <li>Color Coding: <span style='display: inline-block; width:15px;height:15px;background-color:#440154FF;margin-right: 5px;'></span> purple means more points are needed, <span style='display: inline-block; width:15px;height:15px;background-color:#31688EFF;margin-right: 5px;'></span> blue means sample collection is on track to meet point goals, <span style='display: inline-block; width:15px;height:15px;background-color:#35B779FF;margin-right: 5px;'></span> green means yearly point goals are already met, and <span style='display: inline-block; width:15px;height:15px;background-color:#FDE725FF;margin-right: 5px;'></span> yellow means yearly point goals have been exceeded.  <span style='display: inline-block; width:15px;height:15px;background-color:#034521;margin-right: 5px;'></span> Dark green are bonus areas were points were collected outside ERS regions.</li>
    </ul>
    <h4><strong>Heat Map</strong></h4>
    <ul>
      <li>Instead of the county-level assessment, users can switch to a heat map showing sample density (number of samples per km²).</li>
      <li>This helps visualize the density of samples across space without reference to jurisdictional borders. </li>
      <li>If there are no points in the state in this calendar year, no heat map will appear.</li>
      <li>Heatmaps do not render when saving the map. To save the image take a screen shot.</li>
    </ul>
    <h4><strong>Display Surveillance Sample Locations</strong></h4>
    <ul>
      <li>Users can choose to display individual sample locations on the map.</li>
      <li>Clicking a sample dot provides additional details including: Species of the sampled animal, county where it was collected, surveillance category (i.e., Strange Acting, Found Dead, Road Kill, Surveillance Trapped, NWCO/Other, Unknown), and rabies test result if available (Positive or Negative).</li>
      <li>If there are no points in the state in this calendar year, no points will appear.</li>
    </ul>
  "),style="font-size:110%;"
                         )),
                         column(width=7,box(width=6.8,height=NULL,title=span("Table of point goals, point collected, and points needed by county. Only counties where the goals were not met are shown.",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            dataTableOutput('tablex')
                         )),
                         column(width=5,box(width = 4.8,height=NULL, title = span("ERS Table Description", style="color:white;font-size:28px"),
                                            collapsible = TRUE, 
                                            collapsed = FALSE,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            HTML("
    <p>This table shows the goal for points, number of points collected, and number of points needed for each county, ERS tier level, and subset of county as necessary. Only countie tier areas where the goal has not been met are shown here.</p>

    <h4><strong>Table components</strong></h4>
    <ul>
      <li><strong>State:</strong> State of interest.</li>
      <li><strong>County Tier:</strong> County of interest. Some counties are completely in the high or low ERS tier and some have regions in both tiers, in that case the county evaluations are split into high and low tier areas. A few counties in Maine and New York are very large and have been additionally subset, if there is a number next to the county tier that denotes the subset region.</li>
      <li><strong>Goal:</strong> Target number of ERS points for the county.</li>
      <li><strong>Points:</strong> The number of ERS points so far collected for the county.</li>
      <li><strong>Needed:</strong> Number of ERS points needed for the county. If zero points are needed, the goal has been met and those data are omitted from this table for simplicity.</li>
    </ul>
  "),style="font-size:120%;"))
                ),
                tabPanel(title = "Distribution Map v2",value="maptabv2",icon = icon("map"),
                         box(width=12,height=NULL,title=span("Summary of location info",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             # Dynamic valueBoxes
                             infoBoxOutput("totsampsinf"),
                             infoBoxOutput("countyerrorinf"),
                             infoBoxOutput("countymetinf")
                         ),
                         column(width=7,box(width=6.8,title=span("Map of MIS samples",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            withSpinner(leafletOutput(outputId = "mapx2",height = 600),color = "#0C1936"),
                                            downloadButton( outputId = "dl2",label = "Save the map?")
                         )),
                         column(width=5,box(width = 4.8,height=NULL, title = span("ERS Map Overview", style="color:white;font-size:28px"),
                                            collapsible = TRUE, 
                                            collapsed = FALSE,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            HTML("
    <p>This interactive map provides multiple ways to visualize Enhanced Rabies Surveillance (ERS) data. Users can toggle between different display options to assess sample coverage and distribution.You can save the map as an image by clicking the button on the bottom of the map.</p>

    <h4><strong>County-Level Assessment (Default View)</strong></h4>
    <ul>
      <li>The default map evaluates whether ERS point goals have been met at a county (or sub-county) level.</li>
      <li>Most areas are assessed by county, but some counties are divided into high-priority and low-priority ERS areas, meaning they have separate evaluations.</li>
      <li>Color Coding: generally, <span style='color: #5456AA; font-weight: bold;'>purple-blue</span> colors mean more samples are needed, white means point goal was met, <span style='color: #FECF49; font-weight: bold;'>gold</span> colors means point goal was exceeded, and <span style='color: #034521; font-weight: bold;'>dark green</span> means bonus samples were collected in areas outside the ERS region.</li>
    </ul>
    <h4><strong>Heat Map</strong></h4>
    <ul>
      <li>Instead of the county-level assessment, users can switch to a heat map showing sample density (number of samples per km²).</li>
      <li>This helps visualize the density of samples across space without reference to jurisdictional borders. </li>
      <li>If there are no points in the state in this calendar year, no heat map will appear.</li>
      <li>Heatmaps do not render when saving the map. To save the image take a screen shot.</li>
    </ul>
    <h4><strong>Display Surveillance Sample Locations</strong></h4>
    <ul>
      <li>Users can choose to display individual sample locations on the map.</li>
      <li>Clicking a sample dot provides additional details including: Species of the sampled animal, county where it was collected, surveillance category (i.e., Strange Acting, Found Dead, Road Kill, Surveillance Trapped, NWCO/Other, Unknown), and rabies test result if available (Positive or Negative).</li>
      <li>If there are no points in the state in this calendar year, no points will appear.</li>
    </ul>
  "),style="font-size:120%;"
                         )),
                         column(width=7,box(width=6.8,height=NULL,title=span("Table of point goals, point collected, and points needed by county. Only counties where the goals were not met are shown.",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            dataTableOutput('tablex2'))),
                         column(width=5,box(width = 4.8,height=NULL, title = span("ERS Map Overview", style="color:white;font-size:28px"),
                                            collapsible = TRUE, 
                                            collapsed = FALSE,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            HTML("
    <p>This table shows the goal for points, number of points collected, and number of points needed for each county, ERS tier level, and subset of county as necessary. </p>

    <h4><strong>Table components</strong></h4>
    <ul>
      <li><strong>State:</strong> State of interest.</li>
      <li><strong>County Tier:</strong> County of interest. Some counties are completely in the high or low ERS tier and some have regions in both tiers, in that case the county evaluations are split into high and low tier areas. A few counties in Maine and New York are very large and have been additionally subset, if there is a number next to the county tier that denotes the subset region.</li>
      <li><strong>Goal:</strong> Target number of ERS points for the county.</li>
      <li><strong>Points:</strong> The number of ERS points so far collected for the county.</li>
      <li><strong>Needed:</strong> Number of ERS points needed for the county. If zero points are needed, the goal has been met and that is what is shown in the table.</li>
    </ul>
  "),style="font-size:120%;")
                         )), 
                tabPanel(title = "General State Needs",value="statetab",icon = icon("readme"),
                         box(width=12,title=span("Info",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             # 
                             column(11,p("The table below shows the point goals per state. Since different types of surveillance categories are worth more points than others, the number of samples needed would change if you collected all of your sample from a particular category. If all of your samples are from NWCO you would need considerably more samples to achieve the target point value than if you collected higher value samples (e.g., strange acting, found dead, or roadkill). ",style="font-size:130%;")),
                             column(11,p("These target numbers are to give a general idea on the surveillance needs.  However, the locations of samples are also important. In the Distribution Map tab of this dashboard, you can get a since of how well your state is doing on collecting samples throughout the ERS high priority area within your state. ",style="font-size:130%;"),
                             )
                         ),
                         box(width=11,title=span("Number of samples needed by state shown by category. Fewer samples are needed if they are of higher point vales.",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             dataTableOutput('stateneed')
                         )),
                tabPanel(title = "Genetics Samples",value="gentab",icon = icon("dna"),
                         box(width=12,title=span("Genetic data",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             # 
                             column(11,HTML("Matt Hopken is able to use genetic data to determine population structure of raccoons from genetic samples collected in the field, which helps determine raccoon movement and identify translocation events. </p>
                                    <p>The goal of this section is to present the locations of genetic samples that have been collected prior to this year (archived samples) and samples that have been collected in this calendar year (new samples).  The new samples are those that have DNASAMPLE equal to YES. Not all of these samples are necessarily headed for the genetic archive, some may be collected for other purposes or studies. Information in the comments should help determine if the sample is headed to NWRC. We present the data in a couple different ways to help visualize where additional sampling efforts would be helpful. </p>
                                    <p>If you have any questions about specific data requests, <strong>contact Matt Hopken (Matt.W.Hopken@usda.gov)</strong>. </p> "),style="font-size:120%;"
                             )
                         ),
                         box(width=12,title=span("Summary stats of genetic samples",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             #tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Warning there may be issues if locations are close to county boundaries but the location will be correct.  This is more of a reminder to double check the locations than a confirmation that there are issues. ",align='left'),
                             
                             # 
                             # Dynamic valueBoxes
                             valueBoxOutput("gensampsprev"),
                             valueBoxOutput("gensampsnow"),
                             valueBoxOutput("genneeded")
                         ),
                         column(width=7,box(width=6.8,title=span("Map of genetic samples",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            withSpinner(leafletOutput(outputId = "mapgen",height = 600),color = "#0C1936"),
                                            downloadButton( outputId = "gendl",label = "Save the map?")
                         )),
                         column(width=5,box(width = 4.8,height=NULL, title = span("Genetic Map Overview", style="color:white;font-size:28px"),
                                            collapsible = TRUE, 
                                            collapsed = FALSE,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            HTML("
    <p>This interactive map provides multiple ways to visualize raccoon genetic sample data. Users can toggle between different display options to assess sample coverage and distribution. You can save the map as an image by clicking the button on the bottom of the map.</p>

    <h4><strong>County-Level Assessment (Default View)</strong></h4>
    <ul>
      <li>The option shows where more samples are needed, more spatial coverage is needed, and areas where the goals for genetic sampling have been met at a county (or sub-county) level.</li>
      <li>Most areas are assessed by county, but some counties are divided into high-priority and low-priority ERS areas, meaning they have separate evaluations (same as ERS evaluation).</li>
      <li>Fill Colors: <span style='display: inline-block; width:15px;height:15px;background-color:black;margin-right: 5px;'></span> black areas have no new genetic sampling goals, <span style='display: inline-block; width:15px;height:15px;background-color:#721F81;margin-right: 5px;'></span> purple areas indicate the genetic sampling goal has been met, <span style='display: inline-block; width:15px;height:15px;background-color:#F1605D;margin-right: 5px;'></span> orange areas indicate at least one more new genetic sample is needed, and  <span style='display: inline-block; width:15px;height:15px;background-color:#FCFDBF;margin-right: 5px;'></span> cream areas indicate 10 or more new samples are needed.</li>
      <li>Ouline Colors: <span style='display: inline-block; width:30px;height:3px;background-color:#2BC8F1;vertical-align:middle;margin-right: 5px;'></span> blue borders indicate Matt's 1st priority areas, <span style='display: inline-block; width:30px;height:3px;background-color:#3F3315;vertical-align:middle;margin-right: 5px;'></span> brown borders indicate Matt's 2nd priority areas, and white borders are not prioritized for genetic sampling.</li>
      <li>Pop-up: As you scroll over the county areas, a pop-up with details about the county will come up. If Matt has specific recommendations for where to sample within a county, that information is shown in Notes.</li>
    </ul>
    <h4><strong>Heat Map</strong></h4>
    <ul>
      <li>A heat map showing sample density (number of samples per km²) helps visualize the density of samples across space without reference to jurisdictional borders.</li>
      <li>Heat maps are based on archived and current year samples. </li>
      <li>Heatmaps do not render when saving the map. To save the image take a screen shot.</li>
    </ul>
    <h4><strong>Spatial Gap</strong></h4>
    <ul>
      <li>This view shows a buffer around each genetic sample to make gaps in the spatial cover of genetic samples clearer.</li>
      <li>Any areas not covered by the <span style='display: inline-block; width:15px;height:15px;background-color:steelblue;margin-right: 5px;'></span> blue area are areas to focus on for additional sampling. </li>
    </ul>
    <h4><strong>Display Surveillance Sample Locations</strong></h4>
    <ul>
      <li>Users can choose to display individual sample locations on the map.</li>
      <li>Each archived sample is represented by a <span style='color: black;'>&#9679;</span> black dot, each sample from this calendar year are shown as a <span style='color: red;'>&#9679;</span> red dot.</ul>
      <li>Clicking a sample dot provides additional details including: sample ID, species, county where it was collected, sample source (archived or new), and for archived samples the fate and method of collection are shown.</li>
    </ul>
  "),style="font-size:120%;"
                         )),
                         column(width=7,box(width=6.8,title=span("Table of target numbers and total genetic samples by county.",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            dataTableOutput('tablegen')
                         )),
                         column(width=5,box(width = 4.8,height=NULL, title = span("ERS Table Description", style="color:white;font-size:28px"),
                                            collapsible = TRUE, 
                                            collapsed = FALSE,
                                            solidHeader = TRUE,
                                            status = "primary",
                                            HTML("
    <p>This table shows the goal for the number of genetic samples needed and the number collected by county, ERS tier level, and subset of county as necessary.</p>

    <h4><strong>Table components</strong></h4>
    <ul>
      <li><strong>State:</strong> State of interest.</li>
      <li><strong>County:</strong> County of interest.</li>
      <li><strong>ERS Tier:</strong> ERS Tiers are either high, low, or none. If a number is added, it denotes a large county that has been subset into smaller regions.</li>
      <li><strong>Archived:</strong> The number of archived genetic samples for the county </li>
      <li><strong>Goal:</strong> Target number of genetic samples for the county.</li>
      <li><strong>Samples:</strong> The number of genetic samples collected for the county.</li>
      <li><strong>Status:</strong> An evaluation of the needs for the county.</li>
    </ul>
  "),style="font-size:120%;"))
                         
                )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  # ### Update state input based on what states are available
  # observe({
  #   states=state.name[which(state.abb%in%unique(ersdata$STATE))]
  #   # Can use character(0) to remove all choices
  #   if (is.null(states))
  #     states <- character(0)
  #   states=c(" ",states)
  #   # Can also set the label and select items
  #   updateSelectInput(session, "staten",
  #                     label = "Select a State",
  #                     choices = states,
  #                     selected = head(states, 1)
  #   )
  # })
  
  ###########################
  ###
  ### Information tab
  ###
  ###########################
  
  output$ERSarea<-renderPlot({
    ###
    ### Plot of the ERS tiers
    ###
    par(mar=c(1,1,1,1))
    my_image <- png::readPNG("www/CtyorvTierMap.png")
    
    # Set up a plot area with no plot
    plot(0,0,xaxt="n",yaxt="n",xlab="",ylab="",col="white")
    
    # Get the plot information so the image will fill the plot box, and draw it
    lim <- par()
    rasterImage(my_image, 
                xleft=-1, xright=1, 
                ybottom=-1, ytop=1)
  })
  
  
  ###########################
  ###
  ###  Map tab version 1
  ###
  ###########################
  
  ####
  ### Map issues tab components
  ####
  # Create foundational leaflet map
  # and store it as a reactive expression
  foundational.map1 <- reactive({
    
    data <- ersdata
    statmap1=statmaps[which(statmaps$STATE_FIPS%in%rabiestates$Fips),]
    ctypts1=ctypts
    
    df=data[,c("IDNUMBER","LONGITUDE","LATITUDE","ERSCATEGORY","Category","SPECIES","COUNTY","STATE","STCO","RABIESBRAINRESULTS")]
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      statmap1=statmaps[statmaps$STATE_NAME==input$staten,]
      ctypts1=ctypts[ctypts$STATE_NAME==input$staten,]
    }
    
    df=df[which(!is.na(df$LONGITUDE)),]
    
    if(dim(df)[1]>0){
      loccols=turbo(7)[as.numeric(df$ERSCATEGORY)]
      
      ## Get spatial ERS data
      dfsp=st_as_sf(df,coords = c('LONGITUDE', 'LATITUDE'),crs = 4326)
      
      ##  heat map setup
      ## Create kernel density output
      kde <- bkde2D(as.matrix(df[,c("LONGITUDE","LATITUDE")]),
                    bandwidth=c(.15, .15), gridsize = c(1000,1000))
      # Create Raster from Kernel Density output
      KernelDensityRaster <- raster::raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))
      #set low density cells as NA so we can make them transparent with the colorNumeric function
      KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.05)] <- NA
      #palRaster <- colorBin("RdGy",reverse = TRUE, bins = 10, domain = KernelDensityRaster@data@values, na.color = "transparent")
      #palRaster <- colorBin("YlGnBu",reverse = TRUE, bins = 10, domain = KernelDensityRaster@data@values, na.color = "transparent")
      palRaster <- colorNumeric("YlGnBu",reverse = TRUE, domain = KernelDensityRaster@data@values, na.color = "transparent")
      
    }
    
    
    labelscty <- paste(
      "<b>", ctypts1$NAME,"County","</b>",
      "<br>Tier: ",ctypts1$TierName,
      "<br>Subset: ",ctypts1$Subset,
      "<br>Point Goal: ", ctypts1$PTGoal,
      "<br>Points: ", ctypts1$Pts) %>%
      lapply(htmltools::HTML)
    
    bboxa <- st_bbox(statmap1) %>% 
      as.vector()
    
    l2= leaflet()%>%
      addTiles(group="OpenStreetMap")%>%
      addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery")%>%
      addPolygons(data=ctypts1$geometry,color="black",fillColor = "grey",fillOpacity = 0,opacity = 1,weight = 0.2)%>%
      addPolygons(data=statmap1$geometry,color="black",fillColor = "grey",fillOpacity = 0,opacity = 1,weight = 0.2)%>%
      addPolygons(data = orvhatch,color = "black",fillColor = "grey",
                  fillOpacity = 0.3,opacity = 0.9,weight = 1.5,group="Show ORV")%>%
      addPolygons(data = orv$geometry,color = "black",fillColor = "grey",
                  fillOpacity = 0,opacity = 0.9,weight = 1.5,group="Show ORV")%>%
      addLayersControl(baseGroups = c("OpenStreetMap","Esri.WorldImagery"),
                       overlayGroups=c("Show ORV"), 
                       options = layersControlOptions(collapsed = FALSE))%>%
      fitBounds(bboxa[1], bboxa[2], bboxa[3], bboxa[4])
    
    
    
    if(input$mapheat=="Heat map"){
      ## Redraw the map
      if(dim(df)[1]>0){
        l2=l2%>%
          addRasterImage(KernelDensityRaster, 
                         colors = palRaster, 
                         opacity = .9,
                         project=TRUE) %>%
          addLegend_decreasing(pal = palRaster, 
                               values = KernelDensityRaster@data@values,
                               title = "Density of Points (pts/km2)",decreasing = TRUE)
      }
    }
    if(input$mapheat=="County assessment"){
      l2=l2%>%
        addPolygons(data = ctypts1, 
                    color = "black",
                    opacity = 0.9,
                    fillOpacity = c(.75,0.1,rep(0.75,7))[ctypts1$PtLevs+6],
                    fillColor = classcolors[ctypts1$PtLevs+6],
                    weight  = 0.25,
                    label =   ~labelscty)%>%
        addLegend_decreasing("topright", colors = rev(classcolors),
                             #labels = c("No ERS","Need 50+ points","Need 10+ points","Need 1+ points","Goal met","Good job","Great job","Awesome job!"),
                             labels = c("Awesome job!","Great job","Good job","Goal met","Need 1+ points","Need 10+ points","Need 50+ points","No ERS","Bonus"),
                             title = "ERS assessment",
                             opacity = 1,decreasing = TRUE)
      
    }
    if(input$mapheat=="County assessment to date"){
      l2=l2%>%
        addPolygons(data = ctypts1, 
                    color = "black",
                    opacity = 0.9,
                    fillOpacity = c(.75,0.1,rep(0.75,5))[ctypts1$PtLevProgress+4],
                    fillColor = classcolorstodate[ctypts1$PtLevProgress+4],
                    weight  = 0.25,
                    label =   ~labelscty)%>%
        addLegend_decreasing("topright", colors = rev(classcolorstodate),
                             labels = rev(c("Bonus","No ERS","Points needed","On track","Goal met","Goal exceeded")),
                             title = "ERS assessment to date",
                             opacity = 1,decreasing = TRUE)
      
      
    }
    if(input$ShowPts=="Yes"){
      if(dim(df)[1]>0){
        l2<-l2 %>% 
          addCircleMarkers(
            data = dfsp,color = "black",
            popup = ~get_popup_content(erssf),opacity = 0.9,radius = 0.05)
      }
    }
    
    l2
    
    
  }) # end of foundational.map()
  output$mapx<-renderLeaflet({
    
    # call reactive map
    foundational.map1()
    
  })
  
  ### Option to save the map
  # end of creating user.created.map()
  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a PDF
  
  output$dl <- downloadHandler(
    filename = function(){paste0(input$staten,"_ERS_map.png")},
    
    content = function(file) {
      req(foundational.map1())
      
      progress <- shiny::Progress$new()
      progress$set(message = "Please be patient. This takes a few minutes... ")
      on.exit(progress$close())
      
      Sys.sleep(2)
      
      mapshot(foundational.map1(),file=file,cliprect="viewport",delay=10)
    } # end of content() function
  ) # end of downloadHandler() function
  
  
  ###
  ### Table of county evaluations shown next to map
  ###
  output$tablex <- renderDataTable({
    data<-ersdata
    scd1=ctyorv
    
    df=data[,c("IDNUMBER","LONGITUDE","LATITUDE","ERSCATEGORY","Category","SPECIES","COUNTY","STATE","STCO","RABIESBRAINRESULTS")]
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      scd1=scd1[scd1$STATE_NAME==input$staten,]
    }
    df=df[which(!is.na(df$LONGITUDE)),]
    df$Points=ersnames[match(df$ERSCATEGORY,ersnames$Num),"Points"]
    
    ## Need to calculate the points overlapping with polygons as counties are split
    ##  between high and low priority areas
    dfsf=st_as_sf(df,coords = c("LONGITUDE","LATITUDE"),crs=4326)
    
    ## Trying a join 
    pwp=st_join(dfsf,scd1)
    ## 
    pwp$TierName=ifelse(is.na(pwp$Tier),"None",
                        ifelse(pwp$Tier==1,"High","Low"))
    pwp$CtyTier=paste(pwp$NAME,pwp$TierName,sep="-")
    pwp=pwp%>%
      mutate(Points=replace_na(Points,0))
    
    
    ## Summarizing data by county-tier designation
    NRMPcounty=pwp%>%st_drop_geometry%>%
      group_by(StCtyTier)%>%
      summarise(Pts=sum(Points))
    
    
    ## see what it looks like
    scd1$Point_Goal=scd1$PTGoal
    scd1$Points=as.numeric(unlist((NRMPcounty[match(scd1$StCtyTier,NRMPcounty$StCtyTier),"Pts"])))
    scd1$Points[is.na(scd1$Points)]=0
    scd1$Points_Needed=ifelse(scd1$Point_Goal<=scd1$Points,"Goal met",
                              as.numeric(scd1$Point_Goal-unlist(scd1$Points)))
    
    
    dast=scd1%>%st_drop_geometry()%>%
      filter(Points_Needed!="Goal met")%>%
      mutate(Subset2=ifelse(is.na(Subset)," ",as.character(Subset)),
             County_Tier=paste(NAME,TierName,Subset2,sep=" "))%>%
      arrange(STATE_NAME,County_Tier)%>%
      dplyr::select(STATE_NAME,County_Tier,Point_Goal,Points,Points_Needed)%>%
      rename(State=STATE_NAME,Goal=Point_Goal,Needed=Points_Needed)
    dast
    
  })
  
  output$totsamps <- renderValueBox({
    df<-ersdata
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      scd1=ctyorv[ctyorv$STATE_NAME==input$staten,]
    }
    ctymet=dim(df)[1]
    valueBox(
      ctymet, "Total number of samples collected", icon = icon("hippo"),
      color = "navy"
    )
  })
  
  output$countyerror <- renderValueBox({
    data<-ersdata
    scd1=ctyorv
    
    df=data[,c("IDNUMBER","LONGITUDE","LATITUDE","ERSCATEGORY","Category","SPECIES","COUNTY","STATE","STCO","RABIESBRAINRESULTS")]
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      scd1=ctyorv[ctyorv$STATE_NAME==input$staten,]
    }
    df=df[which(!is.na(df$LONGITUDE)),]
    
    df$Points=ersnames[match(df$ERSCATEGORY,ersnames$Num),"Points"]
    
    ## Need to calculate the points overlapping with polygons as counties are split
    ##  between high and low priority areas
    dfsf=st_as_sf(df,coords = c("LONGITUDE","LATITUDE"),crs=4326)
    
    ## Trying a join 
    pwp=st_join(dfsf,scd1)
    ## 
    pwp$TierName=ifelse(is.na(pwp$Tier),"None",
                        ifelse(pwp$Tier==1,"High","Low"))
    pwp$CtyTier=paste(pwp$NAME,pwp$TierName,sep="-")
    pwp=pwp%>%
      mutate(Points=replace_na(Points,0))
    
    
    ## Summarizing data by county-tier designation
    NRMPcounty=pwp%>%st_drop_geometry%>%
      group_by(StCtyTier)%>%
      summarise(Pts=sum(Points))
    
    
    ## see what it looks like
    scd1$Point_Goal=scd1$PTGoal
    scd1$Points=as.numeric(unlist((NRMPcounty[match(scd1$StCtyTier,NRMPcounty$StCtyTier),"Pts"])))
    scd1$Points[is.na(scd1$Points)]=0
    scd1$Points_Needed=ifelse(scd1$Point_Goal<=scd1$Points,"Goal met",
                              as.numeric(scd1$Point_Goal-unlist(scd1$Points)))
    
    
    dast=scd1%>%st_drop_geometry()%>%dplyr::select(STATE_NAME,NAME,TierName,Point_Goal,Points,Points_Needed)%>%
      filter(Points_Needed!="Goal met")%>%
      arrange(STATE_NAME,NAME,TierName)
    names(dast)=c("State","County","ERS_Tier","Point_Goal","Points","Points_Needed")
    dast
    
    ctymet=dim(dast)[1]
    
    valueBox(
      ctymet, "# of counties needing addition surveillance", icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$countymet <- renderValueBox({
    data<-ersdata
    scd1=ctyorv
    
    df=data[,c("IDNUMBER","LONGITUDE","LATITUDE","ERSCATEGORY","Category","SPECIES","COUNTY","STATE","STCO","RABIESBRAINRESULTS")]
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      scd1=ctyorv[ctyorv$STATE_NAME==input$staten,]
    }
    df=df[which(!is.na(df$LONGITUDE)),]
    df$Points=ersnames[match(df$ERSCATEGORY,ersnames$Num),"Points"]
    
    ## Need to calculate the points overlapping with polygons as counties are split
    ##  between high and low priority areas
    dfsf=st_as_sf(df,coords = c("LONGITUDE","LATITUDE"),crs=4326)
    
    ## Trying a join 
    pwp=st_join(dfsf,scd1)
    ## 
    pwp$TierName=ifelse(is.na(pwp$Tier),"None",
                        ifelse(pwp$Tier==1,"High","Low"))
    pwp$CtyTier=paste(pwp$NAME,pwp$TierName,sep="-")
    pwp=pwp%>%
      mutate(Points=replace_na(Points,0))
    
    
    ## Summarizing data by county-tier designation
    NRMPcounty=pwp%>%st_drop_geometry%>%
      group_by(StCtyTier)%>%
      summarise(Pts=sum(Points))
    
    
    ## see what it looks like
    scd1$Point_Goal=scd1$PTGoal
    scd1$Points=as.numeric(unlist((NRMPcounty[match(scd1$StCtyTier,NRMPcounty$StCtyTier),"Pts"])))
    scd1$Points[is.na(scd1$Points)]=0
    scd1$Points_Needed=ifelse(scd1$Point_Goal==0,"No ERS",
                              ifelse(scd1$Point_Goal<=scd1$Points,"Goal met",
                                     as.numeric(scd1$Point_Goal-unlist(scd1$Points))))
    
    
    ctymet=length(which(scd1$Points_Needed=="Goal met"))
    ctyn=length(which(scd1$Point_Goal!=0))
    
    valueBox(
      ctymet, paste("# of counties where point goal was met out of ",ctyn), icon = icon("face-smile"),
      color = "olive"
    )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########################
  ###
  ###  Map tab version 2
  ###
  ###########################
  
  ####
  ### Map issues tab components
  ####
  # Create foundational leaflet map
  # and store it as a reactive expression
  foundational.map2 <- reactive({
    
    data <- ersdata
    statmap1=statmaps[which(statmaps$STATE_FIPS%in%rabiestates$Fips),]
    ctypts1=ctypts
    
    df=data[,c("IDNUMBER","LONGITUDE","LATITUDE","ERSCATEGORY","Category","SPECIES","COUNTY","STATE","STCO","RABIESBRAINRESULTS")]
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      statmap1=statmaps[statmaps$STATE_NAME==input$staten,]
      ctypts1=ctypts[ctypts$STATE_NAME==input$staten,]
    }
    df=df[which(!is.na(df$LONGITUDE)),]
    if(dim(df)[1]>0){
      loccols=turbo(7)[as.numeric(df$ERSCATEGORY)]
      
      ## Get spatial ERS data
      dfsp=st_as_sf(df,coords = c('LONGITUDE', 'LATITUDE'),crs = 4326)
      
      ##  heat map setup
      ## Create kernel density output
      kde <- bkde2D(as.matrix(df[,c("LONGITUDE","LATITUDE")]),
                    bandwidth=c(.15, .15), gridsize = c(1000,1000))
      # Create Raster from Kernel Density output
      KernelDensityRaster <- raster::raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))
      #set low density cells as NA so we can make them transparent with the colorNumeric function
      KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.05)] <- NA
      palRaster <- colorBin("RdGy",reverse = TRUE, bins = 10, domain = KernelDensityRaster@data@values, na.color = "transparent")
      palRaster <- colorBin("YlGnBu",reverse = TRUE, bins = 10, domain = KernelDensityRaster@data@values, na.color = "transparent")
      
    }
    
    
    labelscty <- paste(
      "<b>", ctypts1$NAME,"County","</b>",
      "<br>Tier: ",ctypts1$TierName,
      "<br>Subset: ",ctypts1$Subset,
      "<br>Point Goal: ", ctypts1$PTGoal,
      "<br>Points: ", ctypts1$Pts) %>%
      lapply(htmltools::HTML)
    
    bboxa <- st_bbox(statmap1) %>% 
      as.vector()
    
    
    l2= leaflet()%>%
      addTiles(group="OpenStreetMap")%>%
      addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery")%>%
      addPolygons(data=ctypts1$geometry,color="black",fillColor = "grey",fillOpacity = 0,opacity = 1,weight = 0.2)%>%
      addPolygons(data=statmap1$geometry,color="black",fillColor = "grey",fillOpacity = 0,opacity = 1,weight = 0.2)%>%
      addPolygons(data = orvhatch,color = "black",fillColor = "grey",
                  fillOpacity = 0.3,opacity = 0.9,weight = 1.5,group="Show ORV")%>%
      addPolygons(data = orv$geometry,color = "black",fillColor = "grey",
                  fillOpacity = 0,opacity = 0.9,weight = 1.5,group="Show ORV")%>%
      addLayersControl(baseGroups = c("OpenStreetMap","Esri.WorldImagery"),
                       overlayGroups=c("Show ORV"), 
                       options = layersControlOptions(collapsed = FALSE))%>%
      fitBounds(bboxa[1], bboxa[2], bboxa[3], bboxa[4])
    
    
    
    if(input$mapheat=="Heat map"){
      ## Redraw the map
      if(dim(df)[1]>0){
        l2=l2%>%
          addRasterImage(KernelDensityRaster, 
                         colors = palRaster, 
                         opacity = .9) %>%
          addLegend_decreasing(pal = palRaster, 
                               values = KernelDensityRaster@data@values,
                               title = "Density of Points (pts/km2)",decreasing = TRUE)
      }
    }
    if(input$mapheat=="County assessment"){
      l2=l2%>%
        addPolygons(data = ctypts1, 
                    color = "black",
                    opacity = 0.9,
                    fillOpacity = c(.75,0.1,rep(0.75,7))[ctypts1$PtLevs2+5],
                    fillColor = classcolors2[ctypts1$PtLevs2+5],
                    weight  = 0.25,
                    label =   ~labelscty)%>%
        addLegend_decreasing("topright", colors = rev(classcolors2),
                             #labels = c("No ERS","Need 50+ points","Need 10+ points","Need 1+ points","Goal met","Good job","Great job","Awesome job!"),
                             labels = c("Awesome job!","Great job","Goal met","Need 1+ points","Need 10+ points","No ERS","Bonus"),
                             title = "ERS assessment",
                             opacity = 1,decreasing = TRUE)
      
    }
    
    if(input$ShowPts=="Yes"){
      if(dim(df)[1]>0){
        l2<-l2 %>% 
          addCircleMarkers(
            data = dfsp,color = "black",
            popup = ~get_popup_content(erssf),opacity = 0.9,radius = 0.05)
      }
    }
    
    l2
    
    
  }) # end of foundational.map()
  output$mapx2<-renderLeaflet({
    
    # call reactive map
    foundational.map2()
    
  })
  
  ### Option to save the map
  # end of creating user.created.map()
  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a png
  output$dl2 <- downloadHandler(
    filename = function(){paste0(input$staten,"_ERS_mapv2.png")}
    
    , content = function(file) {
      req(foundational.map2())
      
      Sys.sleep(2)
      
      progress <- shiny::Progress$new()
      progress$set(message = "Please be patient. This takes a few minutes... ")
      on.exit(progress$close())
      
      
      mapshot(foundational.map2(),file=file,cliprect="viewport")
      
    } # end of content() function
  ) # end of downloadHandler() function
  
  
  ###
  ### Table of county evaluations shown next to map
  ###
  output$tablex2 <- renderDataTable({
    data<-ersdata
    scd1=ctyorv
    
    df=data[,c("IDNUMBER","LONGITUDE","LATITUDE","ERSCATEGORY","Category","SPECIES","COUNTY","STATE","STCO","RABIESBRAINRESULTS")]
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      scd1=scd1[scd1$STATE_NAME==input$staten,]
    }
    df=df[which(!is.na(df$LONGITUDE)),]
    df$Points=ersnames[match(df$ERSCATEGORY,ersnames$Num),"Points"]
    
    ## Need to calculate the points overlapping with polygons as counties are split
    ##  between high and low priority areas
    dfsf=st_as_sf(df,coords = c("LONGITUDE","LATITUDE"),crs=4326)
    
    ## Trying a join 
    pwp=st_join(dfsf,scd1)
    ## 
    pwp$TierName=ifelse(is.na(pwp$Tier),"None",
                        ifelse(pwp$Tier==1,"High","Low"))
    pwp$CtyTier=paste(pwp$NAME,pwp$TierName,sep="-")
    pwp=pwp%>%
      mutate(Points=replace_na(Points,0))
    
    
    ## Summarizing data by county-tier designation
    NRMPcounty=pwp%>%st_drop_geometry%>%
      group_by(StCtyTier)%>%
      summarise(Pts=sum(Points))
    
    
    ## see what it looks like
    scd1$Point_Goal=scd1$PTGoal
    scd1$Points=as.numeric(unlist((NRMPcounty[match(scd1$StCtyTier,NRMPcounty$StCtyTier),"Pts"])))
    scd1$Points[is.na(scd1$Points)]=0
    scd1$Points_Needed=ifelse(scd1$Point_Goal<=scd1$Points,"Goal met",
                              as.numeric(scd1$Point_Goal-unlist(scd1$Points)))
    
    
    dast=scd1%>%st_drop_geometry()%>%
      mutate(Subset2=ifelse(is.na(Subset)," ",as.character(Subset)),
             County_Tier=paste(NAME,TierName,Subset2,sep=" "))%>%
      arrange(STATE_NAME,County_Tier)%>%
      dplyr::select(STATE_NAME,County_Tier,Point_Goal,Points,Points_Needed)%>%
      rename(State=STATE_NAME,Goal=Point_Goal,Needed=Points_Needed)
    dast
    
  })
  
  
  ###
  ### Info boxes
  ###
  output$totsampsinf <- renderInfoBox({
    df<-ersdata
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      scd1=ctyorv[ctyorv$STATE_NAME==input$staten,]
    }
    ctymet=dim(df)[1]
    
    infoBox(
      "Samples collected", ctymet, icon = icon("brain"),
      color = "navy"
    )
    
  })
  
  output$countyerrorinf <- renderInfoBox({
    data<-ersdata
    scd1=ctyorv
    
    df=data[,c("IDNUMBER","LONGITUDE","LATITUDE","ERSCATEGORY","Category","SPECIES","COUNTY","STATE","STCO","RABIESBRAINRESULTS")]
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      scd1=ctyorv[ctyorv$STATE_NAME==input$staten,]
    }
    df=df[which(!is.na(df$LONGITUDE)),]
    
    df$Points=ersnames[match(df$ERSCATEGORY,ersnames$Num),"Points"]
    
    ## Need to calculate the points overlapping with polygons as counties are split
    ##  between high and low priority areas
    dfsf=st_as_sf(df,coords = c("LONGITUDE","LATITUDE"),crs=4326)
    
    ## Trying a join 
    pwp=st_join(dfsf,scd1)
    ## 
    pwp$TierName=ifelse(is.na(pwp$Tier),"None",
                        ifelse(pwp$Tier==1,"High","Low"))
    pwp$CtyTier=paste(pwp$NAME,pwp$TierName,sep="-")
    pwp=pwp%>%
      mutate(Points=replace_na(Points,0))
    
    
    ## Summarizing data by county-tier designation
    NRMPcounty=pwp%>%st_drop_geometry%>%
      group_by(StCtyTier)%>%
      summarise(Pts=sum(Points))
    
    
    ## see what it looks like
    scd1$Point_Goal=scd1$PTGoal
    scd1$Points=as.numeric(unlist((NRMPcounty[match(scd1$StCtyTier,NRMPcounty$StCtyTier),"Pts"])))
    scd1$Points[is.na(scd1$Points)]=0
    scd1$Points_Needed=ifelse(scd1$Point_Goal<=scd1$Points,"Goal met",
                              as.numeric(scd1$Point_Goal-unlist(scd1$Points)))
    
    
    dast=scd1%>%st_drop_geometry()%>%
      filter(Points_Needed!="Goal met")%>%
      dplyr::select(STATE_NAME,NAME,TierName,Point_Goal,Points,Points_Needed)%>%
      arrange(STATE_NAME,NAME,TierName)%>%
      rename(State=STATE_NAME,County=NAME,ERS_Tier=TierName,Goal=Point_Goal,
             Needed=Points_Needed)
    dast
    
    
    ctymet=pmin(1,sum(dast$Points)/sum(dast$Goal))*100
    
    infoBox(
      "Progress to point goal", paste0(round(ctymet,0), "%"), icon = icon("list-check"),
      color = "yellow"
    )
    
  })
  
  output$countymetinf <- renderInfoBox({
    data<-ersdata
    scd1=ctyorv
    
    df=data[,c("IDNUMBER","LONGITUDE","LATITUDE","ERSCATEGORY","Category","SPECIES","COUNTY","STATE","STCO","RABIESBRAINRESULTS")]
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      scd1=ctyorv[ctyorv$STATE_NAME==input$staten,]
    }
    df=df[which(!is.na(df$LONGITUDE)),]
    df$Points=ersnames[match(df$ERSCATEGORY,ersnames$Num),"Points"]
    
    ## Need to calculate the points overlapping with polygons as counties are split
    ##  between high and low priority areas
    dfsf=st_as_sf(df,coords = c("LONGITUDE","LATITUDE"),crs=4326)
    
    ## Trying a join 
    pwp=st_join(dfsf,scd1)
    ## 
    pwp$TierName=ifelse(is.na(pwp$Tier),"None",
                        ifelse(pwp$Tier==1,"High","Low"))
    pwp$CtyTier=paste(pwp$NAME,pwp$TierName,sep="-")
    pwp=pwp%>%
      mutate(Points=replace_na(Points,0))
    
    
    ## Summarizing data by county-tier designation
    NRMPcounty=pwp%>%st_drop_geometry%>%
      group_by(StCtyTier)%>%
      summarise(Pts=sum(Points))
    
    
    ## see what it looks like
    scd1$Point_Goal=scd1$PTGoal
    scd1$Points=as.numeric(unlist((NRMPcounty[match(scd1$StCtyTier,NRMPcounty$StCtyTier),"Pts"])))
    scd1$Points[is.na(scd1$Points)]=0
    scd1$Points_Needed=ifelse(scd1$Point_Goal<=scd1$Points,"Goal met",
                              as.numeric(scd1$Point_Goal-unlist(scd1$Points)))
    scd1=scd1|>filter(Point_Goal>0)
    
    
    ctymet=length(which(scd1$Points_Needed=="Goal met"))
    ctyn=length(which(!is.na(scd1$Point_Goal)))
    
    
    infoBox(
      "% of county goals met", paste0(round(ctymet/ctyn*100,0), "%"), icon = icon("clipboard-check"),
      color = "olive"
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################################
  ###
  ###  Summary tab
  ###
  ##################################
  
  ###
  ### Summary pie plots
  ###
  output$PieSampPlots<-renderPlotly({
    validate(
      need(input$staten != " ", "Please select a state to view summary plots")
    )
    df<-ersdata
    df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
    
    df$Points=ersnames[match(df$Category,ersnames$Name),"Points"]
    catdf=df%>%
      mutate(Points=ifelse(ERSCATEGORY==1,14,ifelse(ERSCATEGORY==2,20,
                                                    ifelse(ERSCATEGORY==3,4,1))),
             Category=factor(Category,ordered=TRUE,levels=ersnames$Name))%>%
      group_by(Category)%>%
      summarise(TotalSamples=n(),
                Pts=sum(Points),
                .groups="drop")%>%
      complete(Category,fill=list(TotalSamples=0,Pts=0))%>%ungroup()%>%
      mutate(SamplePer=round(TotalSamples/sum(TotalSamples)*100,0),
             PointPer=round(Pts/sum(Pts)*100,0)) %>% 
      mutate(across(everything(),~replace_na(.x,0)))
    
    ## Trying plotly options
    piecolors=viridis(nrow(catdf),option="C",direction = -1)
    totpie2=plot_ly(catdf,labels=~Category,values=~TotalSamples,type="pie",
                    textinfo="label+value",marker=list(colors=piecolors)) %>% 
      layout(showlegend=FALSE,
             xaxis=list(categoryorder = "array",categoryarray=levels(catdf$Category)))
    
    
    totpie2
    
  })
  ### Summary pie plots
  ###
  output$PiePtsPlots<-renderPlotly({
    validate(
      need(input$staten != " ", "Please select a state to view summary plots")
    )
    df<-ersdata
    df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
    
    df$Points=ersnames[match(df$Category,ersnames$Name),"Points"]
    catdf=df%>%
      mutate(Points=ifelse(ERSCATEGORY==1,14,ifelse(ERSCATEGORY==2,20,
                                                    ifelse(ERSCATEGORY==3,4,1))),
             Category=factor(Category,ordered=TRUE,levels=ersnames$Name))%>%
      group_by(Category)%>%
      summarise(TotalSamples=n(),
                Pts=sum(Points),
                .groups="drop")%>%
      complete(Category,fill=list(TotalSamples=0,Pts=0))%>%ungroup()%>%
      mutate(SamplePer=round(TotalSamples/sum(TotalSamples)*100,0),
             PointPer=round(Pts/sum(Pts)*100,0)) %>% 
      mutate(across(everything(),~replace_na(.x,0)))
    catdf=data.frame(catdf)
    
    ## Trying plotly options
    piecolors=viridis(nrow(catdf),option="C",direction = -1)
    
    ptspie2=plot_ly(catdf,labels=~Category,values=~PointPer,type="pie",
                    textinfo="label+percent",marker=list(colors=piecolors)) %>% 
      layout(showlegend=FALSE,
             xaxis=list(categoryorder = "array",categoryarray=levels(catdf$Category))) 
    
    print(ptspie2)
  })
  
  ###
  ### Summary bar county area plots
  ###
  output$CountyBars<-renderPlotly({
    validate(
      need(input$staten != " ", "Please select a state to view summary plots")
    )
    ctypts1=ctypts[ctypts$STATE_NAME==input$staten,]
    
    ###
    ### Getting and displaying the top five counties and the bottom five
    ###
    sortcty=ctypts1%>%st_drop_geometry()%>%
      arrange(-PtDiff)%>%
      mutate(Collected=pmin(PTGoal,Pts),
             Needed=ifelse(PtDiff>0,0,abs(PtDiff)),
             Above=pmax(0,PtDiff),
             Subset=as.character(Subset),
             Subset=replace_na(Subset," "),
             CtyTier=paste(CtyTier,Subset,sep=" "))%>%
      dplyr::select(STATE_NAME,CtyTier,TierName,Collected,Needed,Above)
    
    
    ### Take All counties and split them to show counties over in one plot and under in another
    abovecty=sortcty %>% filter(TierName!="None",Needed==0) %>% 
      dplyr::select(-STATE_NAME,-TierName)%>%
      pivot_longer(cols = -CtyTier,names_to = "Type",values_to = "Points")%>%
      mutate(Type=factor(Type,ordered=TRUE,levels=c("Above","Needed","Collected")))
    abvplsolo=ggplot(abovecty,aes(x=reorder(CtyTier,Points),y=Points,fill=Type))+
      geom_bar(position="stack", stat = "identity",)+
      scale_fill_manual(values = c("#FECF49","#7084E5","black"),name="Progress towards goal")+
      theme_classic()+ggtitle("Counties - point goals met")+
      coord_flip()+xlab("County - ERS tier")
    abvpl=ggplot(abovecty,aes(x=reorder(CtyTier,Points),y=Points,fill=Type))+
      geom_bar(position="stack", stat = "identity",)+
      scale_fill_manual(values = c("#FECF49","#7084E5","black"),name="Progress towards goal")+
      theme_classic()+
      coord_flip()+xlab("County - ERS tier")
    
    abvplsolo2=ggplotly(abvplsolo)
    abvpl2=ggplotly(abvpl)
    
    ### Take All counties and split them to show counties over in one plot and under in another
    belowcty=sortcty %>% filter(TierName!="None",Needed>0) %>% 
      dplyr::select(-STATE_NAME,-TierName)%>%
      pivot_longer(cols = -CtyTier,names_to = "Type",values_to = "Points")%>%
      mutate(Type=factor(Type,ordered=TRUE,levels=c("Above","Needed","Collected")))
    blplsolo=ggplot(belowcty,aes(x=reorder(CtyTier,Points),y=Points,fill=Type))+
      geom_bar(position="stack", stat = "identity",)+
      scale_fill_manual(values = c("#FECF49","#7084E5","black"),name="Progress towards goal")+
      theme_classic()+ggtitle("Counties - points needed")+
      coord_flip()+xlab("County - ERS tier")
    blpl=ggplot(belowcty,aes(x=reorder(CtyTier,Points),y=Points,fill=Type))+
      geom_bar(position="stack", stat = "identity",)+
      scale_fill_manual(values = c("#FECF49","#7084E5","black"),name="Progress towards goal")+
      theme_classic()+
      coord_flip()+xlab("County - ERS tier")
    
    blplsolo2=ggplotly(blplsolo) %>% layout(showlegend=FALSE)
    blpl2=ggplotly(blpl) %>% layout(showlegend=FALSE)
    
    if(dim(abovecty)[1]==0){
      blplsolo2
    }else{
      if(dim(belowcty)[1]==0){
        abvplsolo2
      }else{
        subplot(abvpl2,blpl2,nrows=1,margin = .2)|>
          add_annotations(
            x = c(.25, .75),
            y = 1,
            xref = "paper",
            yref = "paper",
            text = c("Counties - point goals met", "Counties - points needed"),
            showarrow = F,
            xanchor = "center",
            yanchor = "bottom",
            font = list(size = 16)
          )
      }
    }
    
  })
  
  ###
  ### Summary bar county area plots
  ###
  output$CountyBarsNeed<-renderPlotly({
    validate(
      need(input$staten != " ", "Please select a state to view summary plots")
    )
    ctypts1=ctypts[ctypts$STATE_NAME==input$staten,]
    
    ###
    ### Getting and displaying the top five counties and the bottom five
    ###
    sortcty=ctypts1%>%st_drop_geometry()%>%
      arrange(-PtDiff)%>%
      mutate(Collected=pmin(PTGoal,Pts),
             Needed=ifelse(PtDiff>0,0,abs(PtDiff)),
             Above=pmax(0,PtDiff),
             Subset=as.character(Subset),
             Subset=replace_na(Subset," "),
             CtyTier=paste(CtyTier,Subset,sep=" "))%>%
      dplyr::select(STATE_NAME,CtyTier,TierName,Collected,Needed,Above)
    
    
    ### Take All counties and split them to show counties over in one plot and under in another
    belowcty=sortcty %>% filter(TierName!="None",Needed>0) %>% 
      dplyr::select(-STATE_NAME,-TierName)%>%
      pivot_longer(cols = -CtyTier,names_to = "Type",values_to = "Points")%>%
      mutate(Type=factor(Type,ordered=TRUE,levels=c("Above","Needed","Collected")))
    blpl=ggplot(belowcty,aes(x=reorder(CtyTier,Points),y=Points,fill=Type))+
      geom_bar(position="stack", stat = "identity",)+
      scale_fill_manual(values = c("#FECF49","#7084E5","black"),name="Progress towards goal")+
      theme_classic()+ggtitle("Counties where points are still needed")+
      coord_flip()+xlab("County - ERS tier")
    
    
    blpl2=ggplotly(blpl)
    blpl2
    
  })
  
  output$tableerror<- renderDataTable({
    ### How many samples needed by county
    validate(
      need(input$staten != " ", "Please select a state to view summary table")
    )
    
    data<-ersdata
    scd1=ctyorv
    
    df=data[,c("IDNUMBER","LONGITUDE","LATITUDE","ERSCATEGORY","Category",
               "SPECIES","COUNTY","STATE","STCO","RABIESBRAINRESULTS")]
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      scd1=ctyorv[ctyorv$STATE_NAME==input$staten,]
    }
    df=df[which(!is.na(df$LONGITUDE)),]
    df$Points=ersnames[match(df$ERSCATEGORY,ersnames$Num),"Points"]
    
    ## Need to calculate the points overlapping with polygons as counties are split
    ##  between high and low priority areas
    dfsf=st_as_sf(df,coords = c("LONGITUDE","LATITUDE"),crs=4326)
    
    ## Trying a join 
    pwp=st_join(dfsf,scd1)
    ## 
    pwp$TierName=ifelse(is.na(pwp$Tier),"None",
                        ifelse(pwp$Tier==1,"High","Low"))
    pwp$CtyTier=paste(pwp$NAME,pwp$TierName,sep="-")
    pwp=pwp%>%
      mutate(Points=replace_na(Points,0))
    
    
    ## Summarizing data by county-tier designation
    NRMPcounty=pwp%>%st_drop_geometry%>%
      group_by(StCtyTier)%>%
      summarise(Pts=sum(Points))
    
    
    ## see what it looks like
    scd1$Point_Goal=scd1$PTGoal
    scd1$Points=as.numeric(unlist((NRMPcounty[match(scd1$StCtyTier,NRMPcounty$StCtyTier),"Pts"])))
    scd1$Points[is.na(scd1$Points)]=0
    scd1$Points_Needed=ifelse(scd1$Point_Goal<=scd1$Points,"Goal met",
                              as.numeric(scd1$Point_Goal-unlist(scd1$Points)))
    
    
    dast=scd1%>%st_drop_geometry()%>%dplyr::select(STATE_NAME,NAME,TierName,Subset,Point_Goal,Points,Points_Needed)%>%
      #filter(Points_Needed!="Goal met")%>%
      arrange(STATE_NAME,NAME,TierName)
    names(dast)=c("State","County","ERS_Tier","Subset","Point_Goal","Points","Points_Needed")
    dast$Points_Needed[is.na(dast$Points_Needed)]="No ERS needed"
    data.table(dast)
    
  })
  
  
  
  
  
  
  
  
  
  ##################################
  ###
  ###  Time trend tab
  ###
  ##################################
  
  ###
  ### Time trend plot
  ###
  output$ProgressPlot<-renderPlotly({
    validate(
      need(input$staten != " ", "Please select a state to view time plots")
    )
    ##
    oldstate=old%>%filter(STATE_NAME==input$staten)
    
    ## Make summary data set
    ossum=oldstate %>% 
      group_by(Year,TilNow) %>% 
      summarise(Samples=n(),
                Points=sum(Point)) 
    
    stgoal=statsamp$PointGoal[which(statsamp$STATE_NAME==input$staten)]
    
    ossum$TilNow=factor(ossum$TilNow, ordered=TRUE,levels=c("Total","Year To Date"))
    
    validate(
      need(dim(ossum)[1]!=0, "No current or historic ERS data")
    )
    ### Plot this out
    tplt=ggplot(ossum,aes(x=as.numeric(Year),y=Points,fill=TilNow))+
      geom_col()+
      scale_fill_manual(values = c("grey","royalblue4"),name=" ")+
      theme_classic()+xlab("Year")+
      ylim(0,max(max(ossum$Points),stgoal))+
      xlim(2006,2025)+
      geom_hline(yintercept = stgoal,color="red",lwd=1.5)
    
    tplt2=ggplotly(tplt)
    tplt2
    
  })
  
  
  ###
  ### Time trend plot
  ###
  output$SpatialProgressPlot<-renderPlotly({
    validate(
      need(input$staten != " ", "Please select a state to view time plots")
    )
    
    ## Visualize old data for a state
    stoldsum=oldstsum %>% filter(STATE_NAME==input$staten) %>% 
      mutate(New=factor(ifelse(Year=="2025","2025","Prior CY"),ordered = TRUE,
                        levels=c("Prior CY","2025")))
    
    validate(
      need(sum(stoldsum$Nummet)!=0, "No current or historic ERS data")
    )
    
    sppl=ggplot(stoldsum,aes(x=as.numeric(Year),y=PercentMet,fill=New))+
      geom_col(show.legend = FALSE)+
      ylab("Percent of county areas with goals met")+
      ylim(c(0,100))+xlab("Year")+
      xlim(2006,2025)+
      scale_fill_manual(values=c("grey25","aquamarine"))+
      theme_classic()+
      geom_hline(yintercept = 100,color="red",lwd=1.5)
    
    sppl2=ggplotly(sppl)
    sppl2
    
  })
  
  
  
  
  
  
  
  
  #########################################
  ###
  ### State summary tab
  ###
  #########################################
  
  ### State results tab
  output$stateneed <- renderDataTable({
    ### How many samples needed by state
    ctyorv1=ctyorv%>%st_drop_geometry()%>%
      dplyr::select(STATE_NAME,PTGoal,StrgA_N,FndDd_N,Rdkll_N,SrvTr_N,NWCO_Nd)
    names(ctyorv1)=c("STATE_NAME","PTGoal", "StrageAct_Needed","FoundDead_Needed",
                     "Roadkill_Needed","SurvTrap_Needed","NWCO_Needed")
    statsamp=ctyorv1%>%group_by(STATE_NAME)%>%
      summarise(PointGoal=sum(PTGoal,na.rm = TRUE),
                StrageAct_Needed=sum(StrageAct_Needed,na.rm = TRUE),
                FoundDead_Needed=sum(FoundDead_Needed,na.rm = TRUE),
                Roadkill_Needed=sum(Roadkill_Needed,na.rm = TRUE),
                SurvTrap_Needed=sum(SurvTrap_Needed,na.rm = TRUE),
                NWCO_Needed=sum(NWCO_Needed,na.rm = TRUE))
    data.table(statsamp)
  })
  
  
  
  
  
  ########################################
  ###
  ### Genetic tab elements v1
  ###
  ########################################
  output$gensampsprev <- renderValueBox({
    if(input$staten!=" "){
      gen=gen[which(gen$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
    }
    ctypre=dim(gen)[1]
    valueBox(
      ctypre, "# of archived genetic samples", 
      icon = icon("thumbtack"),
      color = "olive"
    )
  })
  
  
  ## Box for the new samples
  output$gensampsnow <- renderValueBox({
    df<-ersdata
    if(input$staten!=" "){
      df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      scd1=ctyorv[ctyorv$STATE_NAME==input$staten,]
    }
    
    ctymet=length(which(df$DNASAMPLE=="YES"))
    
    valueBox(
      value=ctymet,subtitle= "# new genetics samples collected", 
      icon = icon("vials"),
      color = "purple"
    )
  })
  
  
  ## Box for the needed samples
  output$genneeded <- renderValueBox({
    ctyorv1=ctyorv
    if(input$staten!=" "){
      ctyorv1=ctyorv[ctyorv$STATE_NAME==input$staten,]
    }
    
    ndcyt=sum(pmax(0,ctyorv1$Samples_needed-ctyorv1$GenSampsNew))
    
    valueBox(
      value=ndcyt,subtitle= "# genetics samples needed", 
      icon = icon("person-hiking"),
      color = "fuchsia"
    )
  })
  
  ####
  ### Map issues tab components
  ####
  # Create foundational leaflet map
  # and store it as a reactive expression
  genetic.map <- reactive({
    ### 
    statmap1=statmaps[which(statmaps$STATE_FIPS%in%rabiestates$Fips),]
    ctyorv1=ctyorv
    capc1=capc
    gen1=gen
    ### Reduce data to state of interest
    if(input$staten!=" "){
      gen1=gen[which(gen$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      statmap1=statmaps[statmaps$STATE_NAME==input$staten,]
      ctyorv1=ctyorv[ctyorv$STATE_NAME==input$staten,]
      capc1=capc[capc$STATE_NAME==input$staten,]
    }
    capc1=st_transform(capc1,st_crs(ctyorv))
    
    if(dim(gen1)[1]>0){
      ##
      ##  heat map setup
      ## Create kernel density output
      kdeg <- bkde2D(as.matrix(gen1[,c("LONGITUDE","LATITUDE")]),
                     bandwidth=c(.15, .15), gridsize = c(1000,1000))
      # Create Raster from Kernel Density output
      KernelDensityRasterg <- raster::raster(list(x=kdeg$x1 ,y=kdeg$x2 ,z = kdeg$fhat))
      #set low density cells as NA so we can make them transparent with the colorNumeric function
      KernelDensityRasterg@data@values[which(KernelDensityRasterg@data@values < 0.02)] <- NA
      palRasterg <- colorBin("RdGy",reverse = TRUE, bins = 10, domain = KernelDensityRasterg@data@values, na.color = "transparent")
      
      
      #
    }
    bboxg <- st_bbox(statmap1) %>% 
      as.vector()    
    
    labelsgencty <- paste(
      "<b>", ctyorv1$NAME,"County","</b>",
      "<br>Tier: ",ctyorv1$TierName,
      "<br>Subset: ",ctyorv1$Subset,
      "<br># of new samples needed: ", ctyorv1$Samples_needed,
      "<br># of new samples collected: ", ctyorv1$GenSampsNew,
      "<br># of archived samples: ", ctyorv1$GenSampsArchived,
      "<br>Notes: ",ctyorv1$Notes) %>%
      lapply(htmltools::HTML)
    
    ### Get leaflet basemap for genetics
    g2= leaflet()%>%
      addTiles(group="OpenStreetMap")%>%
      addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery")%>%
      addPolygons(data=ctyorv1$geometry,color = "black",weight=0.7,fillOpacity = 0)%>%
      addPolygons(data=statmap1$geometry,color="black",fillColor = "grey",fillOpacity = 0,opacity = 1,weight = 0.2)%>%
      addPolygons(data = orvhatch,color = "black",fillColor = "grey",
                  fillOpacity = 0.3,opacity = 0.9,weight = 1.5,group="Show ORV")%>%
      addPolygons(data = orv$geometry,color = "black",fillColor = "grey",
                  fillOpacity = 0,opacity = 0.9,weight = 1.5,group="Show ORV")%>%
      addLayersControl(baseGroups = c("OpenStreetMap","Esri.WorldImagery"),
                       overlayGroups=c("Show ORV"), 
                       options = layersControlOptions(collapsed = FALSE))%>%
      fitBounds(bboxg[1], bboxg[2], bboxg[3], bboxg[4])
    
    
    if(input$mapheatgen=="Heat map"){
      ## Redraw the map
      if(dim(gen1)[1]>0){
        g2=g2%>%
          addRasterImage(KernelDensityRasterg, 
                         colors = palRasterg, 
                         opacity = .9) %>%
          addLegend_decreasing(pal = palRasterg, 
                               values = KernelDensityRasterg@data@values,
                               title = "Density of samples (samples/km2)",decreasing = TRUE)
      }
      
    }
    if(input$mapheatgen=="County assessment"){
      g2=g2%>%
        addPolygons(data = ctyorv1, 
                    color = rev(c("#2BC8F1","#3F3315","white"))[ctyorv1$GenPriority],
                    opacity = 0.9,
                    fillOpacity = 0.7,
                    fillColor = rev(gencolors)[ctyorv1$GenPtLev],
                    weight  = c(1,2,2)[as.numeric(ctyorv1$GenPriority)],
                    label =   ~labelsgencty)%>%
        addLegend_decreasing("topright", colors = rev(gencolors),
                             labels = c("10+ samples needed","1+ samples needed",
                                        "Genetic goal met","No goal"),
                             title = "Genetic evaluation",
                             opacity = 1,decreasing = FALSE) %>% 
        addLegendCustom("topright",
                        colors="#E0D9CA", 
                        labels = c("Priority 1","Priority 2","No Priority"),
                        sizes = 20, 
                        shapes = "square", 
                        borders = (c("#2BC8F1","#3F3315","white")))
      
      
    }
    if(input$mapheatgen=="Spatial gaps"){
      if(dim(gen1)[1]>0){
        g2=g2 %>% 
          addPolygons(data=capc1,
                      color = "steelblue",
                      opacity = 0.8,
                      fillOpacity=0.8,
                      weight=0.25) 
      }
      
      
    }
    if(input$ShowPtsgen=="Yes"){
      g2<-g2 %>% 
        addCircleMarkers(
          data = gencsf,
          color = ifelse(gencsf$Sample=="New","red","black"),
          popup = ~get_genetic_popup_content(gencsf),opacity = 0.9,radius = 0.05)%>%
        addLegend("topright",colors=c("red","black"),labels=c("New","Archived"),
                  title="Genetics samples",opacity=1)
    }
    
    g2
    
    
  }) # end of foundational.map()
  output$mapgen<-renderLeaflet({
    
    # call reactive map
    genetic.map()
    
  })
  
  
  ## Summary table by county for genetic samples
  output$tablegen <- renderDataTable({
    
    statmap1=statmaps[which(statmaps$STATE_FIPS%in%rabiestates$Fips),]
    ctyorv1=ctyorv
    
    ### Reduce data to state of interest
    if(input$staten!=" "){
      gen=gen[which(gen$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      statmap1=statmaps[statmaps$STATE_NAME==input$staten,]
      ctyorv1=ctyorv[ctyorv$STATE_NAME==input$staten,]
      gen1=gen[which(gen$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      
    }
    
    ## Summarizing data by county-tier designation
    gencounty=ctyorv1%>%st_drop_geometry() %>% 
      mutate(Subset=as.character(Subset),
             Subset=(replace_na(Subset," ")),
             ERS_Tier=paste(TierName,Subset,sep=" ")) %>% 
      dplyr::select(STATE_NAME,NAME,ERS_Tier,GenSampsArchived,
                    Samples_needed,GenSampsNew,GenLevel)%>%
      rename(State=STATE_NAME,County=NAME,Archived=GenSampsArchived,
             Goal=Samples_needed,Samples=GenSampsNew,Status=GenLevel)
    
    gencounty
  })
  
  
  output$gendl <- downloadHandler(
    filename = function(){paste0(input$staten,"_genetic_map.png")}
    
    , content = function(file) {
      req(genetic.map())
      
      progress <- shiny::Progress$new()
      progress$set(message = "Please be patient. This takes a few minutes... ")
      on.exit(progress$close())
      
      mapshot(genetic.map(),file=file,cliprect="viewport")
    } # end of content() function
  ) # end of downloadHandler() function
  
  
  
  
  
  
  
  
  
  
  
  ##########################################
  ###
  ###  Report generator code
  ###
  ##########################################
  
  ############
  ###
  ### Download data button information for report
  output$reportX <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function(){paste0(input$staten,"_report.doc")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report2024.Rmd")
      file.copy("report2024.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(df= ersdata,state=input$staten)
      
      # Dynamically select which report to generate depending on state selection
      rmd_file<-if(input$staten%in%ers_states){
        "report2024.Rmd"
      }else{
        "NonERSreport.Rmd"
      }
      
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      library(rmarkdown)
      # withProgress(message = 'This takes a few minutes',{
      progress <- shiny::Progress$new()
      progress$set(message = "Please be patient. This takes a few minutes... ")
      on.exit(progress$close())
      out <- render(input=rmd_file,output_format= word_document(), params=params,output_file = "MyReport.doc",envir=new.env())
      # })
      file.copy(out,file)
    }
  )
  
  observeEvent(input$staten, {
    if (input$staten == " ")
      shinyjs::hide("reportX")
    else
      shinyjs::show("reportX")
  })
  
  session$onSessionEnded(stopApp)
  
}

# Run the application 
shinyApp(ui = ui, server = server)



