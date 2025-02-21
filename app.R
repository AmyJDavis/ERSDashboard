########################################################################
########################################################################
###
### Shiny app for Rabies quarterly reports as a dashboard
###   - This version uses the ERS area instead of county boundaries only to show point needs
###   - Using the actual update ERS area!!! Sent from Jordona 11/20/2024
###
### Amy J Davis
### June 6, 2024, updated January 8, 2025
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
library(shinyWidgets)
library(hablar)
library(sf)
library(mapview)
library(webshot2)
library(KernSmooth)
library(patchwork)
require(tigris)
library(bslib)

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

col_types=c('date','text','text','text','text','text','text','text','text','text','text','text','date','text','numeric','text','text','text',
            'numeric','numeric','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text',
            'text','text','text','text','text','text','text','text','text','text','text','text','numeric','text','text','text','text','text',
            'text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','date','text',
            'text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text','text',
            'text','text')
ersnames=data.frame(Num=1:6,Name=c("Strange Acting","Found Dead","Roadkill",
                                   "Surveillance Trapped","NWCO/Other","Unknown"),
                    Points=c(14,20,4,1,1,1))
### Set the rabies states
rabiestates=data.frame(StateName=state.name,StateAbb=state.abb)[c(1,7,8,10,17,19:21,22,24,29,30,32,33,35,38:40,42,45,46,48),]
rabiestates$Fips=tidycensus::fips_codes[match(rabiestates$StateAbb,tidycensus::fips_codes$state),"state_code"]

### Color palette options
classcolors=c("#034521","#F2EFE9","#353264","#5456AA","#7084E5","white","#FFF7E1","#FEED89","#FECF49")

# Options for Spinner
options(spinner.color="#0C1936", spinner.color.background="#ffffff", spinner.size=2)
options(shiny.maxRequestSize = 30*1024^2)


###
## Read in the ORV and Archived county ERS shapefiles
orv=read_sf("www/CY2023ORV_merged.shp")
orv=st_transform(orv,crs=4326)

orvhatch <- HatchedPolygons::hatched.SpatialPolygons(orv, density = c(20), angle = c(45, 135))

# ctyorv=read_sf("C:/Users/apamydavis/OneDrive - USDA/Documents/Rabies/ORV shapefiles/ctyorv.shp")
ctyorv=read_sf("www/ctyallERSv2.shp")
ctyorv=st_transform(ctyorv,crs=4326)
ctyorv$FIPS=paste0(ctyorv$STATEFP,ctyorv$COUNTYFP)

## Read in the current dataset 
#ersdata <- read_excel("www/2023_Complete_ERS.xlsx",col_types = col_types)
ersdata <- read_excel("www/2024 ERS_19Feb2025.xlsx",col_types = col_types)
ersdata$DATE2=as.POSIXct(ersdata$DATE,format="%Y-%m-%d")
## If the ERSCATEGORY has been left blank, this will take the information from the FREETEXT
ersdata$ERSCATEGORY=ifelse(is.na(ersdata$ERSCATEGORY)|ersdata$ERSCATEGORY=="null",gsub("\\;.*","",ersdata$FREETEXT),ersdata$ERSCATEGORY)
ersdata$ERSCATEGORY=ifelse(nchar(ersdata$ERSCATEGORY)==1,ersdata$ERSCATEGORY,
                           ifelse(grepl("ERS=",toupper(ersdata$ERSCATEGORY)),gsub("ERS=","",toupper(ersdata$ERSCATEGORY)),NA))
ersdata=ersdata[!is.na(ersdata$ERSCATEGORY),]
ersdata$Category=ersnames[match(ersdata$ERSCATEGORY,ersnames$Num),"Name"]

## Remove non rabies states
ersdata=ersdata[which(ersdata$STATE%in%rabiestates$StateAbb),]

## Specific fix for DeKalb county Alabama and Dona Ana county New Mexico
ersdata$COUNTY[ersdata$COUNTY=="DE KALB"]="DEKALB"
ersdata$COUNTY[ersdata$COUNTY=="DONA ANA"]="DOÑA ANA"
ersdata$COUNTY[ersdata$COUNTY=="LA SALLE"&ersdata$STATE=="LA"]="LASALLE"
ersdata$StateName=rabiestates[match(ersdata$STATE,rabiestates$StateAbb),"StateName"]
ersdata$STCO=tolower(paste(ersdata$StateName,ersdata$COUNTY,sep=","))
ersdata$Points=ersnames[match(ersdata$ERSCATEGORY,ersnames$Num),"Points"]
ersdata=ersdata[which(ersdata$SPECIES=="RACCOONS"),]

# Just county information for genetics plotting
uscd=sf::read_sf("www/cb_2018_us_county_5m.shp")
uscd=st_transform(uscd,crs=4326)
uscd=uscd[which(uscd$STATEFP%in%rabiestates$Fips),]
uscd$STATEID=rabiestates[match(uscd$STATEFP,rabiestates$Fips),"Fips"]
uscd$STATE_NAME=rabiestates[match(uscd$STATEFP,rabiestates$Fips),"StateName"]
uscd$FIPS=paste0(uscd$STATEFP,uscd$COUNTYFP)
uscd=sf::st_transform(uscd,crs=4326)
uscd$STCO=paste(tolower(uscd$STATE_NAME),tolower(uscd$NAME),sep=",")
uscd$HighPriority=rbinom(dim(uscd)[1],1,0.05)

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
  mutate(Points=replace_na(Points,0))

## Summarizing data by county-tier designation
NRMPcounty=pwp%>%st_drop_geometry%>%
  group_by(StCtyTier)%>%
  summarise(Pts=sum(Points))


########
###
###   Checking with the data from 2023
###
########
## ERS area by county tier information
ctypts=left_join(ctyorv,NRMPcounty,by=c("StCtyTier"))
ctypts=ctypts%>%
  mutate(Pts=replace_na(Pts,0),
         TierName=replace_na(TierName,"None"),
         PtDiff=Pts-PTGoal,
         PtLevs=ifelse(Tier==0,ifelse(Pts>0,-5,-4),
                       ifelse(PtDiff<(-50),-3,
                              ifelse(PtDiff<(-10),-2,
                                     ifelse(PtDiff<0,-1,
                                            ifelse(PtDiff==0,0,
                                                   ifelse(PtDiff<10,1,
                                                          ifelse(PtDiff<50,2,3))))))))

#######
### 
### Get new genetic samples from the ERS data
# Starting simple only DNASAMPLE==YES
ersgen=ersdata|>filter(DNASAMPLE=="YES")|>
  dplyr::select(DATE,STATE,COUNTY,LATITUDE,LONGITUDE,SPECIES,IDNUMBER,SEX,
                RELATIVEAGE,FATE,METHOD)|>
  mutate(Sample="New")


### Read in the data from Matt that has the genetic data up to 2024
gen=read.csv("www/GeneticsDataFromMatt.csv")
gen=gen[!is.na(gen$LONGITUDE),]
gen$DATE=as.POSIXct(gen$DATE,format="%m/%d/%Y")
gen$FATE=" "
gen$METHOD=" "
gen$Sample="Archived"
gen=rbind(gen,ersgen)

## State maps
statmaps=sf::read_sf("www/states.shp")
statmap1=statmaps[which(statmaps$STATE_FIPS%in%rabiestates$Fips),]

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
                                                     "ERS Data Collection Dashboard")))),
  
  # Sidebar  
  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 100px}"),
    width=350,
    title=tags$h2(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Data selection",align='left'),
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
    conditionalPanel(condition = 'input.tabs=="maptab"',
                     tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Mapping options",align='left'),
                     radioButtons(inputId = "mapheat",label = "Show density of surveillance samples or county points assessment?",
                                  choices = c("Heatmap","County assessment","Neither"),selected = "County assessment"),
                     radioButtons(inputId = "ShowPts",label = "Display surveillance sample locations?",
                                  choices = c("Yes","No"),selected="No")),
    conditionalPanel(condition = 'input.tabs=="gentab"',
                     tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Genetic mapping options",align='left'),
                     radioButtons(inputId = "mapheatgen",label = "Show density of genetic samples or county assessment?",
                                  choices = c("Heatmap","County assessment","Neither"),selected = "County assessment"),
                     radioButtons(inputId = "ShowPtsgen",label = "Display genetic sample locations?",
                                  choices = c("Yes","No"),selected="No"))
  ),
  
  # Show output
  dashboardBody(
    tabsetPanel(id="tabs",selected="guidetab",
                tabPanel(title = "User Guide",value="guidetab",icon=icon("info"),
                         box(width=12,title=span("How to use this dashboard",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             column(11,p("Welcome to the ERS Data Collection Dashboard.  The intent of this dashboard is to empower rabies biologists and state directors by providing a clear, user-friendly way to visualize and interact with the data you collect. The goal of this tool is to help gain insights into program performance, easily track metrics of success, and efficiently generate reports that can be shared with stakeholders to demonstrate impact and progress. ",style="font-size:130%;"),
                                    p("This dashboard is designed to be a resource that enhances the great work that in done in support of the National Rabies Management Program. This dashboard is aimed at helping rabies biologists make your work more impactful and to showcase the results of your efforts to other.",style="font-size:130%;"),
                                    p("You can toggle through the different tabs to see different aspects of the data. To use many features of this dashboard, you need to select a state of interest (using the panel on the left). Once a state is selected you can see different tables and figures that this dashboard producces and you can download a report of your state's data. Below are descriptions of the tabs and how to use them.",style="font-size:130%;")),
                             column(11,        
                                    p("     •	",strong("Summary Report")," – This tab summarizes the ERS data in your quarterly report. There is a pie chart that shows the number of samples by ERS category and the number of points by category. ",style="font-size:130%;"),
                                    p("     •	",strong("Distribution Map")," – This tab has an interactive map that lets you see the data. The default plot shows the county-ERS tier evaluations (has the county goal be met based on the number of samples collected to date). There is an option to show a heatmap to visualize the density of samples and where they were collected. You can also choose to include the actual point data, to see where all samples have been collected. As you scroll over the map it will tell you which county your cursor is in. If you click on a point, an info box will pop up that tells you the IDNUMBER, SPECIES, COUNTY, and ERSCATEGORY on that MIS record. You can also visualize the samples by category.",style="font-size:130%;"),
                                    p("     •	",strong("Some other fun information")," – What this tab does is a mystery but hopefully it will be something interesting.",style="font-size:130%;"),
                             )
                         ),
                         box(width=12,title=span("Trouble-shooting",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             # 
                             column(11,p("",style="font-size:130%;"),
                                    p("We have tried to make this app as user-friendly as possible. However, we know that issues may arise. ",style="font-size:130%;"), 
                                    p("     •	",strong("Email someone if you have issues. "),style="font-size:130%;")
                             )
                         )
                ),
                tabPanel(title = "Points Overview",value="overviewtab",icon = icon("bar-chart"),
                         box(width=12,title=span("Summary ",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             # varImp Plot
                             column(10,plotOutput('PiePlots'))
                         ),
                         box(width=12,title=span("Points needed and points collected by county",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             #confusion matrix, model accuracy metrics
                             column(10,withSpinner(dataTableOutput(outputId="tableerror")))
                         )
                ),
                tabPanel(title = "Distribution Map",value="maptab",icon = icon("map"),
                         box(width=12,title=span("It's a map!",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             # 
                             column(11,p("Some neat information about the map ",style="font-size:130%;"),
                             )
                         ),
                         box(width=12,title=span("Summary of location info",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             #tags$h4(class="primary-subtitle", style='margin-top:8px;margin-left:15px;',"Warning there may be issues if locations are close to county boundaries but the location will be correct.  This is more of a reminder to double check the locations than a confirmation that there are issues. ",align='left'),
                             
                             # 
                             # Dynamic valueBoxes
                             valueBoxOutput("totsamps"),
                             valueBoxOutput("countyerror"),
                             valueBoxOutput("countymet")
                         ),
                         column(width=7,box(width=6.8,title=span("Map of MIS samples",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            withSpinner(leafletOutput(outputId = "mapx",height = 600),color = "#0C1936"),
                                            downloadButton( outputId = "dl",label = "Save the map?")
                         )),
                         column(width=5,box(width=4.8,title=span("Table of point goals, point collected, and points needed by county. Only counties where the goals were not met are shown.",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            dataTableOutput('tablex')
                         ))
                ),     
                tabPanel(title = "General State Needs",value="statetab",icon = icon("readme"),
                         box(width=12,title=span("Info",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             # 
                             column(11,p("The table below shows the point goals per state. Since different types of surveillance categories are worth more points than others, the number of samples needed would change if you collected all of your sample from a particular category  If all of your samples are from NWCO you would need considerably more samples to achive the target point value than if you collected higher value samples (e.g., strange acting, found dead, or roadkill). ",style="font-size:130%;")),
                             column(11,p("These target numbers are to give a general idea on the surveillance needs.  However, the locations of samples is also important. In the Distribution Map tab of this dashboard, you can get a since of how well your state is doing on collecting samples throughout the ERS high priority area within your state. ",style="font-size:130%;"),
                             )
                         ),
                         box(width=11,title=span("Number of samples needed by state shown by category. Fewer samples are needed if they are of higher point vales.",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             dataTableOutput('stateneed')
                         )),
                tabPanel(title = "Genetics Samples",value="gentab",icon = icon("dna"),
                         box(width=12,title=span("Genetic data",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                             # 
                             column(11,p("Matt Hopken is able to use genetic data to determine population structure of raccoons which helps determine raccoon movement and identify translocation events. This section can include any type of discriptors that we want. The points are color coded for if they are previously archived samples (i.e., from the file Matt provided) and the samples that say new are any sample from the ERS data that had DNASample equalling YES.  We can make other classifiers in there. We can do a lot of different things with the plots. Currently the map just looks like the ERS map but with only the genetic data. I set a target number of genetic samples of 10 for each county just to start, which is obviously simplistic and does not consider areas of higher interest. Play around with this and think about what would changes would be helpful.",style="font-size:130%;"),
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
                         column(width=5,box(width=4.8,title=span("Table of target numbers and total genetic samples by county.",style="color:white;font-size:28px"),solidHeader = TRUE,status="primary",
                                            dataTableOutput('tablegen')
                         ))
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
  
  
  ####
  ### Map issues tab components
  ####
  # Create foundational leaflet map
  # and store it as a reactive expression
  foundational.map <- reactive({
    
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
    loccols=turbo(7)[as.numeric(df$ERSCATEGORY)]
    
    ## Get spatial ERS data
    dfsp=st_as_sf(df,coords = c('LONGITUDE', 'LATITUDE'),crs = 4326)
    
    ##  Heatmap setup
    ## Create kernel density output
    kde <- bkde2D(as.matrix(df[,c("LONGITUDE","LATITUDE")]),
                  bandwidth=c(.15, .15), gridsize = c(1000,1000))
    # Create Raster from Kernel Density output
    KernelDensityRaster <- raster::raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))
    #set low density cells as NA so we can make them transparent with the colorNumeric function
    KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 0.05)] <- NA
    palRaster <- colorBin("RdGy",reverse = TRUE, bins = 10, domain = KernelDensityRaster@data@values, na.color = "transparent")
    palRaster <- colorBin("YlGnBu",reverse = TRUE, bins = 10, domain = KernelDensityRaster@data@values, na.color = "transparent")
    
    
    # Create leaflet
    lngmin=min(df$LONGITUDE[df$LONGITUDE<0],na.rm = TRUE)-0.01
    lngmax=max(df$LONGITUDE[df$LONGITUDE<0],na.rm = TRUE)+0.01
    latmin=min(df$LATITUDE[df$LATITUDE>0],na.rm = TRUE)-0.01
    latmax=max(df$LATITUDE[df$LATITUDE>0],na.rm = TRUE)+0.01
    
    labelscty <- paste(
      "<b>", ctypts1$NAME,"County","</b>",
      "<br>Tier: ",ctypts1$TierName,
      "<br>Subset: ",ctypts1$Subset,
      "<br>Point Goal: ", ctypts1$PTGoal,
      "<br>Points: ", ctypts1$Pts) %>%
      lapply(htmltools::HTML)
    
    bboxa <- st_bbox(statmap1) %>% 
      as.vector()
    
    ### Get leaflet basemap for genetics
    basemap <- leaflet() %>%
      # add different provider tiles
      addProviderTiles(
        "OpenStreetMap",
        # give the layer a name
        group = "OpenStreetMap"
      ) %>%
      addProviderTiles(
        "Esri.WorldImagery",
        group = "Esri.WorldImagery"
      ) %>%
      # add a layers control
      addLayersControl(
        baseGroups = c(
          "OpenStreetMap", "Esri.WorldImagery"
        ),
        # position it on the topleft
        position = "topleft"
      )
    
    l2= basemap|>
      addPolygons(data=ctypts1$geometry,color="black",fillColor = "grey",fillOpacity = 0,opacity = 1,weight = 0.2)%>%
      addPolygons(data = orvhatch,color = "black",fillColor = "grey",fillOpacity = 0.3,opacity = 0.9,weight = 1.5)%>%
      addPolygons(data = orv$geometry,color = "black",fillColor = "grey",fillOpacity = 0,opacity = 0.9,weight = 1.5)%>%
      fitBounds(bboxa[1], bboxa[2], bboxa[3], bboxa[4])
    
    
    
    if(input$mapheat=="Heatmap"){
      ## Redraw the map
      l2=l2%>%
        addRasterImage(KernelDensityRaster, 
                       colors = palRaster, 
                       opacity = .9) %>%
        addLegend_decreasing(pal = palRaster, 
                             values = KernelDensityRaster@data@values,
                             title = "Density of Points (pts/km2)",decreasing = TRUE)
      
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
    
    if(input$ShowPts=="Yes"){
      l2<-l2 %>% 
        addCircleMarkers(
          data = dfsp,color = "black",
          popup = ~get_popup_content(erssf),opacity = 0.9,radius = 0.05)
    }
    
    l2
    
    
  }) # end of foundational.map()
  output$mapx<-renderLeaflet({
    
    # call reactive map
    foundational.map()
    
  })
  
  
  # end of creating user.created.map()
  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a PDF
  output$dl <- downloadHandler(
    filename = function(){paste0(input$staten,"_ERS_map.pdf")}
    
    , content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      # withProgress(message = 'This takes a few minutes',{
      progress <- shiny::Progress$new()
      progress$set(message = "Please be patient. This takes a few minutes... ")
      on.exit(progress$close())
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(foundational.map(), "ERS_state_map.html", selfcontained = TRUE)
      #webshot("ERS_state_map.html", file = file, cliprect = "viewport")
      mapshot(foundational.map(),file=file,cliprect="viewport")
      ### using mapshot we can substitute the above two lines of code
      # mapshot(foundational.map(), file = file, cliprect = "viewport")
    } # end of content() function
  ) # end of downloadHandler() function
  
  
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
    
    
    dast=scd1%>%st_drop_geometry()%>%dplyr::select(STATE_NAME,NAME,TierName,Subset,Point_Goal,Points,Points_Needed)%>%
      filter(Points_Needed!="Goal met")%>%
      arrange(STATE_NAME,NAME,TierName)
    names(dast)=c("State","County","ERS_Tier","Subset","Point_Goal","Points","Points_Needed")
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
      ctymet, "# of county-tier polygons needing addition surveillance", icon = icon("exclamation-triangle"),
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
    scd1$Points_Needed=ifelse(scd1$Point_Goal<=scd1$Points,"Goal met",
                              as.numeric(scd1$Point_Goal-unlist(scd1$Points)))
    
    
    ctymet=length(which(scd1$Points_Needed=="Goal met"))
    ctyn=length(which(!is.na(scd1$Point_Goal)))
    
    valueBox(
      ctymet, paste("# of county-tier polygons where point goal was met out of ",ctyn), icon = icon("face-smile"),
      color = "olive"
    )
  })
  
  ###
  ### Summary pie plots
  ###
  output$PiePlots<-renderPlot({
    validate(
      need(input$staten != " ", "Please select a state to view summary plots")
    )
    df<-ersdata
    df=df[which(df$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
    
    df$Points=ersnames[match(df$Category,ersnames$Name),"Points"]
    catdf=df%>%group_by(Category)%>%
      summarise(TotalSamples=n(),
                Pts=sum(Points))%>%ungroup()%>%
      mutate(SamplePer=round(TotalSamples/sum(TotalSamples)*100,0),
             PointPer=round(Pts/sum(Pts)*100,0))
    catdf$Category=factor(catdf$Category,ordered=TRUE,levels=ersnames$Name)
    
    
    totpie=ggplot(catdf, aes(x="",y=TotalSamples,fill=Category))+
      geom_bar(width = 1, stat = "identity")+
      geom_text(aes(x=1.8,label = paste(SamplePer,"%")), 
                size = 6,position = position_stack(vjust = 0.5))+
      coord_polar("y")+
      theme_void() +
      theme(axis.text.x=element_blank(),legend.text=element_text(size=20),
            legend.title =element_text(size=30),
            plot.title = element_text(size = 30, face = "bold"))+
      ggtitle("Samples")
    
    ptspie=ggplot(catdf, aes(x="",y=Pts,fill=Category))+
      geom_bar(width = 1, stat = "identity")+
      geom_text(aes(x=1.8,vjust=c(rep(0,(length(Category)-1)),1),label = paste(PointPer,"%")), 
                size = 6,position = position_stack(vjust = 0.5))+
      coord_polar("y")+
      theme_void() +
      theme(axis.text.x=element_blank(),legend.text=element_text(size=20),
            legend.title=element_text(size=30),
            plot.title = element_text(size = 30, face = "bold"))+
      ggtitle("Points")
    
    totpie + ptspie +
      plot_layout(guides = "collect")+
      plot_annotation(tag_levels = 'A')
  })
  
  
  output$tableerror<- renderDataTable({
    ### How many samples needed by county
    validate(
      need(input$staten != " ", "Please select a state to view summary table")
    )
    
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
    dast$Points_Needed[is.na(dast$Points_Needed)]="No ERS needed"
    data.table(dast)
    
  })
  
  ### Error results tab
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
  
  
  
  
  
  ######
  ###
  ### Genetic tab elements
  ###
  ######
  output$gensampsprev <- renderValueBox({
    if(input$staten!=" "){
      gen=gen[which(gen$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
    }
    ctypre=dim(gen)[1]
    valueBox(
      ctypre, "# of previous genetic samples", 
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
    if(input$staten!=" "){
      gen=gen[which(gen$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
    }
    gencounty=gen|>
      group_by(COUNTY)%>%
      summarise(PreSamples=length(which(Sample=="Processed")), 
                NewSamples=length(which(Sample=="New")), 
                Needed=ifelse((10-n())>0,10-n(),0))
    ndcyt=sum(gencounty$Needed)
    
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
    uscd1=uscd
    
    ### Reduce data to state of interest
    if(input$staten!=" "){
      gen=gen[which(gen$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      statmap1=statmaps[statmaps$STATE_NAME==input$staten,]
      uscd1=uscd[uscd$STATE_NAME==input$staten,]
    }
    
    # if(staten!=" "){
    #   gen=gen[which(gen$STATE==rabiestates[which(rabiestates$StateName==staten),"StateAbb"]),]
    #   statmap1=statmaps[statmaps$STATE_NAME==staten,]
    #   uscd1=uscd[uscd$STATE_NAME==staten,]
    # }
    
    gensf=st_as_sf(gen,coords=c("LONGITUDE","LATITUDE"),crs = 4326)
    uscd1=st_transform(uscd1,crs = 4326)
    statmap1=st_transform(statmap1,crs=4326)
    
    ## Combine the county shapefile and the genetic points data
    ctygen=st_intersects(uscd1,gensf)
    uscd1$GenPts=sapply(ctygen,length)
    uscd1$PtLevs=ifelse(uscd1$GenPts==0,0,
                        ifelse(uscd1$GenPts<4,1,
                               ifelse(uscd1$GenPts<10,2,
                                      ifelse(uscd1$GenPts<30,3,4))))
    uscd1$TargetGen=10
    
    ##
    ##  Heatmap setup
    ## Create kernel density output
    kdeg <- bkde2D(as.matrix(gen[,c("LONGITUDE","LATITUDE")]),
                   bandwidth=c(.15, .15), gridsize = c(1000,1000))
    # Create Raster from Kernel Density output
    KernelDensityRasterg <- raster::raster(list(x=kdeg$x1 ,y=kdeg$x2 ,z = kdeg$fhat))
    #set low density cells as NA so we can make them transparent with the colorNumeric function
    KernelDensityRasterg@data@values[which(KernelDensityRasterg@data@values < 0.02)] <- NA
    palRasterg <- colorBin("RdGy",reverse = TRUE, bins = 10, domain = KernelDensityRasterg@data@values, na.color = "transparent")
    
    bboxg <- st_bbox(statmap1) %>% 
      as.vector()
    #
    labelsgencty <- paste(
      "<b>", uscd1$NAME,"County","</b>",
      "<br>Target # of samples: ", uscd1$TargetGen,
      "<br># of samples: ", uscd1$GenPts) %>%
      lapply(htmltools::HTML)
    
    ### Get leaflet basemap for genetics
    g2= leaflet()|>
      addTiles(group="OpenStreetMap")|>
      addProviderTiles("Esri.WorldImagery",group = "Esri.WorldImagery")|>
      addPolygons(data=uscd1$geometry,color = "black",weight=0.7,fillOpacity = 0)|>
      addPolygons(data=statmap1$geometry,color="black",fillColor = "grey",fillOpacity = 0,opacity = 1,weight = 0.2)|>
      addPolygons(data = orvhatch,color = "black",fillColor = "grey",
                  fillOpacity = 0.3,opacity = 0.9,weight = 1.5,group="Show ORV")%>%
      addPolygons(data = orv$geometry,color = "black",fillColor = "grey",
                  fillOpacity = 0,opacity = 0.9,weight = 1.5,group="Show ORV")%>%
      addLayersControl(baseGroups = c("OpenStreetMap","Esri.WorldImagery"),
                       overlayGroups=c("Show ORV"), 
                       options = layersControlOptions(collapsed = FALSE))|>
      fitBounds(bboxg[1], bboxg[2], bboxg[3], bboxg[4])
    
    
    if(input$mapheatgen=="Heatmap"){
      ## Redraw the map
      g2=g2%>%
        addRasterImage(KernelDensityRasterg, 
                       colors = palRasterg, 
                       opacity = .9) %>%
        addLegend_decreasing(pal = palRasterg, 
                             values = KernelDensityRasterg@data@values,
                             title = "Density of samples (samples/km2)",decreasing = TRUE)
      
    }
    if(input$mapheatgen=="County assessment"){
      g2=g2%>%
        addPolygons(data = uscd1, 
                    color = c("black","red")[uscd1$HighPriority+1],
                    opacity = 0.9,
                    fillOpacity = 0.7,
                    fillColor = viridis(5,option="E")[uscd1$PtLevs+1],
                    weight  = c(0.25,2)[uscd1$HighPriority+1],
                    label =   ~labelsgencty)%>%
        addLegend_decreasing("topright", colors = rev(viridis(5,option="E")),
                             labels = c(">30 samples","10-29 samples","4-9 samples","1-3 samples","No samples"),
                             title = "Genetic samples collected",
                             opacity = 1,decreasing = TRUE)
      
    }
    
    if(input$ShowPtsgen=="Yes"){
      g2<-g2 %>% 
        addCircleMarkers(
          data = gensf,
          color = ifelse(gensf$Sample=="New","red","black"),
          popup = ~get_genetic_popup_content(gensf),opacity = 0.9,radius = 0.05)|>
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
    uscd1=uscd
    
    ### Reduce data to state of interest
    if(input$staten!=" "){
      gen=gen[which(gen$STATE==rabiestates[which(rabiestates$StateName==input$staten),"StateAbb"]),]
      statmap1=statmaps[statmaps$STATE_NAME==input$staten,]
      uscd1=uscd[uscd$STATE_NAME==input$staten,]
    }
    
    
    ## Summarizing data by county-tier designation
    gencounty=gen|>
      group_by(COUNTY)%>%
      summarise(Prepro_Samples=length(which(Sample=="Processed")),
                New_Samples=length(which(Sample=="New")),
                Needed=ifelse((10-n())>0,10-n(),0))
    
    gencounty
    
  })
  
  
  
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
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      library(rmarkdown)
      # withProgress(message = 'This takes a few minutes',{
      progress <- shiny::Progress$new()
      progress$set(message = "Please be patient. This takes a few minutes... ")
      on.exit(progress$close())
      out <- render(input='report2024.Rmd',output_format= word_document(), params=params,output_file = "MyReport.doc",envir=new.env())
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



