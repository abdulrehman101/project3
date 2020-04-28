############

#Cleaning the movies.list data to only hold movie names


##################
library(dplyr)
library(tidyr)
library(plyr)
library(xlsx)
library(stringr)
library(lubridate)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)


movies <- read.csv(file = "movies.list",sep = "",header=FALSE,fill = TRUE)
# Removing the first line
movies <- movies[2:4697741,]

# Finding the col with '#'
tv<- with(movies,movies[grepl('\\(#',V2)|grepl('\\(#',V3)|grepl('\\(#',V4)|grepl('\\(#',V5)|grepl('\\(#',V6)|grepl('\\(#',V7)|grepl('\\(#',V8)|grepl('\\(#',V9)|grepl('\\(#',V10)|grepl('\\(#',V11)|grepl('\\(#',V12)|grepl('\\(#',V13)|grepl('\\(#',V14),])

# names of those entries who have '#'
distinctNamesFrame <- data.frame(tv$V1)
distinctNames <- (distinctNamesFrame[!duplicated(distinctNamesFrame),])

# movies with all the '#' entries removed
movies2 <- movies[!movies$V1 %in% distinctNames,]

#Remove ones with TV
tv2<- with(movies2,movies2[grepl('\\(TV\\)',V2)|grepl('\\(TV\\)',V3)|grepl('\\(TV\\)',V4)|grepl('\\(TV\\)',V5)|grepl('\\(TV\\)',V6)|grepl('\\(TV\\)',V7)|grepl('\\(TV\\)',V8)|grepl('\\(TV\\)',V9)|grepl('\\(TV\\)',V10)|grepl('\\(TV\\)',V11)|grepl('\\(TV\\)',V12)|grepl('\\(TV\\)',V13)|grepl('\\(TV\\)',V14),])

# movies with entries '(TV)'
distinctNamesFrame2 <- data.frame(tv2$V1)
distinctNames2 <- (distinctNamesFrame2[!duplicated(distinctNamesFrame2),])

# movies with all '(TV)' removed
movies3 <- movies2[ ! movies2$V1 %in% distinctNames2, ]

#Remove ones with V
tv3<- with(movies3,movies3[grepl('\\(V\\)',V2)|grepl('\\(V\\)',V3)|grepl('\\(V\\)',V4)|grepl('\\(V\\)',V5)|grepl('\\(V\\)',V6)|grepl('\\(V\\)',V7)|grepl('\\(V\\)',V8)|grepl('\\(V\\)',V9)|grepl('\\(V\\)',V10)|grepl('\\(V\\)',V11)|grepl('\\(V\\)',V12)|grepl('\\(V\\)',V13)|grepl('\\(V\\)',V14),])

distinctNamesFrame3 <- data.frame(tv3$V1)
distinctNames3 <- (distinctNamesFrame3[!duplicated(distinctNamesFrame3),])

movies4 <- movies3[ ! movies3$V1 %in% distinctNames3, ]

#Remove ????
#tv4<- with(movies4,movies4[grepl('????',V2)|grepl('????',V3)|grepl('????',V4)|grepl('????',V5)|grepl('????',V6)|grepl('????',V7)|grepl('????',V8)|grepl('????',V9)|grepl('????',V10)|grepl('????',V11)|grepl('????',V12)|grepl('????',V13)|grepl('????',V14),])
tv4<- with(movies4,movies4[grepl('????',V2,fixed=TRUE)|grepl('????',V3,fixed=TRUE)|grepl('????',V4,fixed=TRUE)|grepl('????',V5,fixed=TRUE)|grepl('????',V6,fixed=TRUE)|grepl('????',V7,fixed=TRUE)|grepl('????',V8,fixed=TRUE)|grepl('????',V9,fixed=TRUE)|grepl('????',V10,fixed=TRUE)|grepl('????',V11,fixed=TRUE)|grepl('????',V12,fixed=TRUE)|grepl('????',V13,fixed=TRUE)|grepl('????',V14,fixed=TRUE),])

distinctNamesFrame4 <- data.frame(tv4$V1)
distinctNames4 <- (distinctNamesFrame4[!duplicated(distinctNamesFrame4),])

movies5 <- movies4[ ! movies4$V1 %in% distinctNames4, ]

##########
#Remove VG
tv5<- with(movies5,movies5[grepl('\\(VG\\)',V2)|grepl('\\(VG\\)',V3)|grepl('\\(VG\\)',V4)|grepl('\\(VG\\)',V5)|grepl('\\(VG\\)',V6)|grepl('\\(VG\\)',V7)|grepl('\\(VG\\)',V8)|grepl('\\(VG\\)',V9)|grepl('\\(VG\\)',V10)|grepl('\\(VG\\)',V11)|grepl('\\(VG\\)',V12)|grepl('\\(VG\\)',V13)|grepl('\\(VG\\)',V14),])

distinctNamesFrame5 <- data.frame(tv5$V1)
distinctNames5 <- (distinctNamesFrame5[!duplicated(distinctNamesFrame5),])

movies6 <- movies5[ ! movies5$V1 %in% distinctNames5, ]




#STUPID
movies5 <- movies6

##########


merged <- unite(movies5,V1, sep = " ")

# Keep Before (
merged$V2 <- gsub("\\(.*","", merged$V1)

# Keep After
merged$V3 <- gsub(".*\\(", "", merged$V1)


merged$V4 <- gsub("\\).*","", merged$V3)

merged <- (merged[!duplicated(merged$V2),])
merged <- subset(merged, merged$V2!="")

# For release
release <- read.csv(file = "release-dates.list",sep = "",header=FALSE,fill = TRUE)
release <-release[2:5397202,]
relMerged <- unite(release,V1, sep = " ")
relMerged$V2 <- gsub("\\(.*","", relMerged$V1)



delete <- merged$V2
delete2 <- relMerged[relMerged$V2 %in% delete,]

delete2 <- subset(delete2, delete2$V2!="")
delete2 <- (delete2[!duplicated(delete2$V2),])

tv5<- with(delete2,delete2[grepl('\\(internet\\)',V1),])
distinctNamesFrame5 <- data.frame(tv5$V1)
distinctNames5 <- (distinctNamesFrame5[!duplicated(distinctNamesFrame5),])

delete3 <- delete2[ ! delete2$V1 %in% distinctNames5, ]

delete3$V3 <- gsub(".*\\)", "", delete3$V1)
delete3$V3 <- gsub(".*\\:", "", delete3$V1)

# For Genre
genre <- read.csv(file = "genres.list",sep = "",header=FALSE,fill = TRUE)
genre <-genre[2:2658955,]
genre <- unite(genre,V1, sep = " ")

genre$V2<- gsub(".*\\(", "", genre$V1)
genre$V3<- gsub(".*\\)", "", genre$V2)

genre$V4 <- gsub("\\(.*","", genre$V1)

deleteG1 <- delete3$V2
deleteG2 <- genre[genre$V4 %in% deleteG1,]
deleteG2 <- (deleteG2[!duplicated(deleteG2$V4),])

# FOR COMBINING GENRE + DATE + NAME
delete4 <- delete3
deleteG4 <- deleteG2$V4
deleteG5 <- delete4[delete4$V2 %in% deleteG4,]
deleteG5$V4 <- deleteG2$V3

deleteG5 <- arrange(deleteG5, deleteG5$V2)
deleteG5$V3 <- gsub("\\(.*","", deleteG5$V3)

# DELETEG5 IS THE FINAL FILE WITH GENRE + DATE + NAME

#FILTERING GENRE


# RATINGS
ratings <- read.csv(file = "ratings.list",sep = "",header=FALSE,fill = TRUE)
ratings <-ratings[2:2658955,]
ratings2 <- unite(ratings,V4, V4:V50, sep = " ")
ratings2$V5 <- gsub("\\(.*","", ratings2$V4)
ratings2 <- arrange(ratings2, ratings2$V5)

deleteR1 <- deleteG5$V2
deleteR2 <- ratings2[ratings2$V5 %in% deleteR1,]
deleteR2 <- (deleteR2[!duplicated(deleteR2$V5),])

##########
delete4 <- deleteG5
deleteR4 <- deleteR2$V5
deleteR5 <- delete4[delete4$V2 %in% deleteR4,]

deleteR5$V5 <- deleteR2$V3 
deleteR6 <- deleteR5
deleteR6$V3 <- gsub("\\(.*","", deleteR6$V3)

#Final table: R6

# Running Times
runningTime <- read.csv(file = "running-times.list",sep = "",header=FALSE,fill = TRUE)
runningTime <- runningTime[2:1517432,]
runningTime2 <- unite(runningTime,V1, sep = " ")
runningTime2$V2 <- gsub("\\(.*","", runningTime2$V1)
runningTime2 <- arrange(runningTime2, runningTime2$V2)
runningTime2 <- subset(runningTime2, runningTime2$V2!="")

deleteRT1 <- deleteR6$V2
deleteRT2 <- runningTime2[runningTime2$V2 %in% deleteRT1,]
deleteRT2 <- (deleteRT2[!duplicated(deleteRT2$V2),])

#$V3 <- gsub(".*\\)", "", deleteRT2$V1)
deleteRT2$V3 <- gsub("\\s*\\([^\\)]+\\)","",as.character(deleteRT2$V1))
deleteRT2$V3 <- trimws(deleteRT2$V3)

# Basically after these 3 lines, you should have a column with runtimes:
# Just keep the last 3 digits
deleteRT2$V3 <- str_sub(deleteRT2$V3,-3,-1)
# Keep everything after the space
deleteRT2$V3 <- sub(".*? ", "",deleteRT2$V3)
#Keep everything after the :
deleteRT2$V3 <- sub(".*?:", "", deleteRT2$V3)


deleteRT2$V3 <- as.numeric(deleteRT2$V3)


# Time has to be > 60
deleteRT2<- deleteRT2[deleteRT2[, "V3"] >= 60,]

# arrange
deleteRT2 <- arrange(deleteRT2, deleteRT2$V2)

# now keep everything in R6 if its in RT2
inRT2List <- deleteRT2$V2
deleteR7 <- deleteR6[deleteR6$V2 %in% inRT2List,]
deleteR7 <- (deleteR7[!duplicated(deleteR7$V2),])

deleteRT2$V2 <- trimws(deleteRT2$V2)
deleteR7$V2 <- trimws(deleteR7$V2)
inR7List <- deleteR7$V2

deleteRT5 <- deleteRT2[deleteRT2$V2 %in% inR7List,]
deleteRT5 <- (deleteRT5[!duplicated(deleteRT5$V2),])

# RT5 IS FINAL WITH RUNTIME, R7 IS FINAL WITH EVERYTHING ELSE

# ATTACH RUNTIME TO R7
deleteR7$V6 <- deleteRT5$V3

# R7 has: garbage, name, year, genre, rating, runtime

#deleteR5$V4 <- deleteG2$V3
##########


#movies4 <- movies3[!is.na(movies3$V1), ]
keywordsNum <- read.csv(file = "keywordsNum.list",sep = "",header=FALSE,fill = TRUE)

# FOR GETTING A DATAFRAME with keyword and quantity
keywordsNum2 <- unite(keywordsNum,V1, sep = " ")
keywordsNum3 <- data.frame(keywordsNum2=unlist(strsplit(as.character(keywordsNum2$V1),")")))
keywordsNum3$V2 <- gsub(".*\\(", "", keywordsNum3$keywordsNum2)
keywordsNum3$V3 <- gsub("\\(.*","", keywordsNum3$keywordsNum2)

keywordsNum3$V3 <- trimws(keywordsNum3$V3)
keywordsNum3$V2 <- trimws(keywordsNum3$V2)

keywordsNum3 <- subset(keywordsNum3, keywordsNum3$V3!="")
keywordsNum3$V2 <- as.numeric(keywordsNum3$V2)

# keywords above 20
keywordsNum3 <- subset(keywordsNum3, keywordsNum3$V2 >= 20)

#turn it into a list
keywordsList <- keywordsNum3$V3

#load keywords
keywords <- read.csv(file = "keywords.list",sep = "",header=FALSE,fill = TRUE)
keywords <- keywords[2:7480280,]
keywords2 <- unite(keywords,V1, sep = " ")

keywords2$V2 <- gsub("\\(.*","", keywords2$V1)
keywords2$V3 <- gsub(".*\\)", "", keywords2$V1)
keywords2$V3 <- trimws(keywords2$V3)
keywords2$V2 <- trimws(keywords2$V2)

# If the keyword is in the list, keep the row
keywords2 <- keywords2[keywords2$V3 %in% keywordsList,]

#keyword 2 only has the filtered keywords

# Keep title if its in R7
inR7List <- deleteR7$V2
keywords2 <- keywords2[keywords2$V2 %in% inR7List,]
keywords2 <- (keywords2[!duplicated(keywords2$V2),])
keywords2 <- arrange(keywords2, keywords2$V2)

# keep in r7 if in keyword 2
inKeywords2List <- keywords2$V2
deleteR8 <- deleteR7[deleteR7$V2 %in% inKeywords2List,]
deleteR8 <- (deleteR8[!duplicated(deleteR8$V2),])
deleteR8 <- arrange(deleteR8, deleteR8$V2)

# get the keywords and add it to R8
deleteR8$V7 <- keywords2$V3

# Certificates
certificates <- read.csv(file = "certificates.list",sep = "",header=FALSE,fill = TRUE)
certificates <- certificates[2:924920,]
certificates2 <- unite(certificates,V1, sep = " ")
certificates2$V2 <- gsub("\\s*\\([^\\)]+\\)","",as.character(certificates2$V1))
certificates2$V3 <- sub(".*?USA:", "", certificates2$V2)
# Keep everything before the first space
certificates2$V3 <- gsub(" .*","", certificates2$V3)

#ONLY KEEP USA: ones
certificates2<- with(certificates2,certificates2[grepl('USA:',V2),])

#Get rid of the TV ones
certificates2 <- certificates2[ grep("TV-", certificates2$V3, invert = TRUE) , ]

#Get rid of not rated
certificates2 <- certificates2[ grep("Not", certificates2$V3, invert = TRUE) , ]


certificates2$V2 <- gsub("\\(.*","", certificates2$V1)
certificates2$V2 <- trimws(certificates2$V2)
certificates2$V3 <- trimws(certificates2$V3)

# At this point, certificate only has USA ones...

# Now, delete stuff thats in certificate but not in our main R7
deleteC1 <- deleteR7$V2
deleteC2 <- certificates2[certificates2$V2 %in% deleteC1,]
deleteC2 <- (deleteC2[!duplicated(deleteC2$V2),])
deleteC2 <- arrange(deleteC2, deleteC2$V2)


# keep everything in R7 thats in certificate
inC2 <- deleteC2$V2
deleteR9 <- deleteR8[deleteR8$V2 %in% inC2,]
deleteR9 <- (deleteR9[!duplicated(deleteC2$V2),])
deleteR9 <- arrange(deleteR9, deleteR9$V2)

# Add certificates to the rest of the table
deleteR9$V8 <- deleteC2$V3
deleteR9$V2 <- trimws(deleteR9$V2)
deleteR9$V3 <- trimws(deleteR9$V3)
deleteR9$V4 <- trimws(deleteR9$V4)
deleteR9$V5 <- trimws(deleteR9$V5)
deleteR9$V6 <- trimws(deleteR9$V6)
deleteR9$V7 <- trimws(deleteR9$V7)
deleteR9$V8 <- trimws(deleteR9$V8)

deleteR9$V9 <- dmy(deleteR9$V3)
#deleteR9$V10 <- years(deleteR9$V3)
#deleteR9$V11 <- as.numeric(substr(deleteR9$V11, 1, 4));

#deleteRT2$V12 <- str_sub(deleteRT2$V3,-3,-1)


deleteR9$V10 <- str_sub(deleteR9$V3,-4,-1)

deleteR9$V4 <- gsub(" .*","", deleteR9$V4)

r10 <- deleteR9

r10$V12 <- paste(r10$V10, r10$V11)
r10$V5 <- as.numeric(r10$V5)
r10$V6 <- as.numeric(r10$V6)
r10$V10 <- as.numeric(r10$V10)


r10 <- subset(r10, r10$V8!="")
r10 <- subset(r10, r10$V8!="E10+")





ui <- dashboardPage(
  skin = "black",
  
  #Dashboard Header
  dashboardHeader(
    title = "Movies Visualization"
    
  ),
  
  #Dashboard Sidebar
  dashboardSidebar(
    sidebarMenu(
      
      sliderInput(
        inputId = "decadeSelect", label = h3("Decade Selection"), min = 1900, max = 2020, step = 10, 
        value = c(1900, 2017), sep=""
      ),
      
      sliderInput(
        inputId = "yearSelect", label = h3("Year Selection"), min = 1900, max = 2017, 
        value = 2017, sep = ""
      ),
      
      sliderInput(
        inputId = "getN", label = h3("Number of Keywords"), min = 0, max = 20,
        value = 20
      ),
      
      actionButton("aboutPage", "About"
      )
      
      
    )
  ),
  
  #Dashboard Body
  dashboardBody(
    fluidRow(
      #Box displaying Averages
      
      box(
        width = 3,
        title = "Avg Films Released Year", background = "red", solidHeader = TRUE,
        textOutput("avgNumFilmsYears")
      ),
      box(
        width = 3,
        title = "Avg Films Released Month", background = "red", solidHeader = TRUE,
        textOutput("avgNumFilmsMonths")
      ),
      box(
        width = 3,
        title = "Avg Running Time", background = "red", solidHeader = TRUE,
        textOutput("avgRunningTime")
      )
      
      
      
    ),#End of fluid row #1
    
    
    fluidRow(
      #Box displaying number of Films released
      tabBox(
        title = "Number of Films Released", 
        id = "numOfFilms", width = 12,
        tabPanel(title = "Per Year", plotlyOutput("filmsPerYear")), #Done
        tabPanel(title = "Per Year Table",  DT::dataTableOutput("filmsPerYearTable")), #Done
        tabPanel(title = "Per Month", plotlyOutput("filmsPerMonth")), #Done
        tabPanel(title = "Per Month Table",  DT::dataTableOutput("filmsPerMonthTable")) #Done
      )
      
    ),#End of fluid row #2
    
    fluidRow(
      #Box displaying the distributions
      tabBox(
        title = "Distributions",
        id = "distribution", width = 12,
        tabPanel(title = "Running Times", plotlyOutput("runTimes")),#Done
        tabPanel(title = "Run Time Table",  DT::dataTableOutput("runTimeTable")),#Done
        tabPanel(title = "Certificates", plotlyOutput("certif")), #Done
        tabPanel(title = "Certificates Table", DT::dataTableOutput("certifTable")), 
        tabPanel(title = "Genres", plotlyOutput("genres")), #Done
        tabPanel(title = "Genres Table", DT::dataTableOutput("genresTable")), 
        tabPanel(title = "Keywords", plotlyOutput("keyWords")), #Done
        tabPanel(title = "Keywords Table",  DT::dataTableOutput("keyWordsTable")) #Done
        
      )
      
    )
    
  )
  
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  #n <- 10
 
  observeEvent(input$getN,{
    
#n <- input$getN
    
    countKeyword <- countKeyword[1:input$getN,]

    
    #Keywords distribution
    output$keyWords <- renderPlotly({
      #number <-
      ggplot(data=countKeyword, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Keywords") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
    })
    
    output$keyWordsTable <- renderDataTable({
      countKeyword
    })
    
  })
  
  observeEvent(input$decadeSelect,{
    #To update the hurricane dropdown box to contain hurricane names of that specifc year
    
   
    
    decade = input$decadeSelect
    
    
    
    r10<-r10[((r10$V10) >= input$decadeSelect[1]),]
    r10<-r10[((r10$V7) <= input$decadeSelect[2]),]
    
    #####
    #Holds the Years in movieYears$V3
    #deleteR5 holds the year released
    movieYears <- select(r10, V2, V3)
    movieYears$V3 <- trimws(movieYears$V3)
    # Keep After
    movieYears$V4 <- gsub(".*\\s","", movieYears$V3)
    
    #Movies per year table
    moviesPerYear <- unlist(movieYears$V4)
    moviesPerYearTable <- table(moviesPerYear)
    moviesPerYearDF <- as.data.frame(moviesPerYearTable)
    moviesPerYearDF$moviesPerYear<- strtoi(moviesPerYearDF$moviesPerYear)
    moviesPerYearDF<- subset(moviesPerYearDF, moviesPerYearDF$moviesPerYear!="")
    moviesPerYearDF<- subset(moviesPerYearDF, moviesPerYearDF$moviesPerYear <= 2017)
    
    
    
    
    #HOLDS THE MOVIE MONTHS IN movieMonths$V4
    movieMonths<- select(r10, V2, V3)
    movieMonths$V3 <- trimws(movieMonths$V3)
    #Dates Before
    movieMonths$V4 <- gsub("\\d","", movieMonths$V3)
    movieMonths <- subset(movieMonths, movieMonths$V4!="")
    movieMonths$V4 <- trimws(movieMonths$V4)
    
    
    #Movies per month table
    
    monthList = c("December", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "December")
    moviesPerMonth <- unlist(movieMonths$V4)
    moviesPerMonthTable <- table(moviesPerMonth)
    moviesPerMonthDF <- as.data.frame(moviesPerMonthTable)
    moviesPerMonthDF <- subset(moviesPerMonthDF, moviesPerMonth %in% monthList)
    
    
    
    
    
    # Count for certificates
    countCertificate <- count(r10$V8)
    
    # Count for genre
    countGenre <- count(r10$V4)
    
    # Count for top10 key
    countKeyword <- count(r10$V7)
    countKeyword <- subset(countKeyword, countKeyword$x!="")
    attach(countKeyword)
    countKeyword <- countKeyword[order(-freq),]
    detach(countKeyword)
    
    observeEvent(input$getN,{
      
      #n <- input$getN
      
      countKeyword <- countKeyword[1:input$getN,]
      
      
      #Keywords distribution
      output$keyWords <- renderPlotly({
        #number <-
        ggplot(data=countKeyword, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Keywords") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
      })
      
      output$keyWordsTable <- renderDataTable({
        countKeyword
      })
      
    })
   
  
    
    # Count for runtime
    countRuntime <- count(r10$V6)
    countRuntime <- subset(countRuntime, countRuntime$x!="")
    attach(countRuntime)
    countRuntime <- countRuntime[order(-freq),]
    detach(countRuntime)
    countRuntime <- countRuntime[1:10,]
    
    
    
    #####
    
    
    
    {
      
      #Run Times Distribution
      output$runTimes <- renderPlotly({
        ggplot(data=countRuntime, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Top 10 Runtime") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
      })
      output$runTimeTable <- renderDataTable({
        countRuntime
      })
      
      
      #Certificates Distribution 
      output$certif <- renderPlotly({
        ggplot(data=countCertificate, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Certificate") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
      })
      output$certifTable <- renderDataTable({
        countCertificate
      })
      
      #Genres distribution
      output$genres <- renderPlotly({
        ggplot(data=countGenre, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Genre") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
      })
      output$genresTable <- renderDataTable({
        countGenre
      })
    }
    
    
    
    
  })
  
  
  observeEvent(input$yearSelect,{
    #To update the hurricane dropdown box to contain hurricane names of that specifc year
    
    decade = input$yearSelect
    
    
    r10<-r10[((r10$V10) == input$yearSelect),]
    
    #####
    #Holds the Years in movieYears$V3
    #deleteR5 holds the year released
    movieYears <- select(r10, V2, V3)
    movieYears$V3 <- trimws(movieYears$V3)
    # Keep After
    movieYears$V4 <- gsub(".*\\s","", movieYears$V3)
    
    #Movies per year table
    moviesPerYear <- unlist(movieYears$V4)
    moviesPerYearTable <- table(moviesPerYear)
    moviesPerYearDF <- as.data.frame(moviesPerYearTable)
    moviesPerYearDF$moviesPerYear<- strtoi(moviesPerYearDF$moviesPerYear)
    moviesPerYearDF<- subset(moviesPerYearDF, moviesPerYearDF$moviesPerYear!="")
    moviesPerYearDF<- subset(moviesPerYearDF, moviesPerYearDF$moviesPerYear <= 2017)
    
    
    
    
    #HOLDS THE MOVIE MONTHS IN movieMonths$V4
    movieMonths<- select(r10, V2, V3)
    movieMonths$V3 <- trimws(movieMonths$V3)
    #Dates Before
    movieMonths$V4 <- gsub("\\d","", movieMonths$V3)
    movieMonths <- subset(movieMonths, movieMonths$V4!="")
    movieMonths$V4 <- trimws(movieMonths$V4)
    
    
    #Movies per month table
    
    monthList = c("December", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "December")
    moviesPerMonth <- unlist(movieMonths$V4)
    moviesPerMonthTable <- table(moviesPerMonth)
    moviesPerMonthDF <- as.data.frame(moviesPerMonthTable)
    moviesPerMonthDF <- subset(moviesPerMonthDF, moviesPerMonth %in% monthList)
    
    
    
    
    
    # Count for certificates
    countCertificate <- count(r10$V8)
    
    # Count for genre
    countGenre <- count(r10$V4)
    
    # Count for top10 key
    countKeyword <- count(r10$V7)
    countKeyword <- subset(countKeyword, countKeyword$x!="")
    attach(countKeyword)
    countKeyword <- countKeyword[order(-freq),]
    detach(countKeyword)
    
    observeEvent(input$getN,{
      
      #n <- input$getN
      
      countKeyword <- countKeyword[1:input$getN,]
      
      
      #Keywords distribution
      output$keyWords <- renderPlotly({
        #number <-
        ggplot(data=countKeyword, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Keywords") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
      })
      
      output$keyWordsTable <- renderDataTable({
        countKeyword
      })
      
    })
    
    # Count for runtime
    countRuntime <- count(r10$V6)
    countRuntime <- subset(countRuntime, countRuntime$x!="")
    attach(countRuntime)
    countRuntime <- countRuntime[order(-freq),]
    detach(countRuntime)
    countRuntime <- countRuntime[1:10,]
    
    
    
    #####
    
    
    
    {
      
      #Run Times Distribution
      output$runTimes <- renderPlotly({
        ggplot(data=countRuntime, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Top 10 Runtime") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
      })
      output$runTimeTable <- renderDataTable({
        countRuntime
      })
      
      
      #Certificates Distribution 
      output$certif <- renderPlotly({
        ggplot(data=countCertificate, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Certificate") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
      })
      output$certifTable <- renderDataTable({
        countCertificate
      })
      
      #Genres distribution
      output$genres <- renderPlotly({
        ggplot(data=countGenre, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Genre") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
      })
      output$genresTable <- renderDataTable({
        countGenre
      })
      
      
      
    }
    
    
    
    
  })
  
  
  #####
  {
    #Holds the Years in movieYears$V3
    #deleteR5 holds the year released
    movieYears <- select(r10, V2, V3)
    movieYears$V3 <- trimws(movieYears$V3)
    # Keep After
    movieYears$V4 <- gsub(".*\\s","", movieYears$V3)
    
    #Movies per year table
    moviesPerYear <- unlist(movieYears$V4)
    moviesPerYearTable <- table(moviesPerYear)
    moviesPerYearDF <- as.data.frame(moviesPerYearTable)
    moviesPerYearDF$moviesPerYear<- strtoi(moviesPerYearDF$moviesPerYear)
    moviesPerYearDF<- subset(moviesPerYearDF, moviesPerYearDF$moviesPerYear!="")
    moviesPerYearDF<- subset(moviesPerYearDF, moviesPerYearDF$moviesPerYear <= 2017)
    
    
    
    
    #HOLDS THE MOVIE MONTHS IN movieMonths$V4
    movieMonths<- select(r10, V2, V3)
    movieMonths$V3 <- trimws(movieMonths$V3)
    #Dates Before
    movieMonths$V4 <- gsub("\\d","", movieMonths$V3)
    movieMonths <- subset(movieMonths, movieMonths$V4!="")
    movieMonths$V4 <- trimws(movieMonths$V4)
    
    
    #Movies per month table
    
    monthList = c("December", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "December")
    moviesPerMonth <- unlist(movieMonths$V4)
    moviesPerMonthTable <- table(moviesPerMonth)
    moviesPerMonthDF <- as.data.frame(moviesPerMonthTable)
    moviesPerMonthDF <- subset(moviesPerMonthDF, moviesPerMonth %in% monthList)
    
    
    
    
    
    # Count for certificates
    countCertificate <- count(r10$V8)
    
    # Count for genre
    countGenre <- count(r10$V4)
    
    # Count for top10 key
    countKeyword <- count(r10$V7)
    countKeyword <- subset(countKeyword, countKeyword$x!="")
    attach(countKeyword)
    countKeyword <- countKeyword[order(-freq),]
    detach(countKeyword)
    
    n = 10
    countKeyword <- countKeyword[1:n,]
    
    # Count for runtime
    countRuntime <- count(r10$V6)
    countRuntime <- subset(countRuntime, countRuntime$x!="")
    attach(countRuntime)
    countRuntime <- countRuntime[order(-freq),]
    detach(countRuntime)
    countRuntime <- countRuntime[1:10,]
  }
  

  
  
  #####
  
  
  
  
  
  
  
  
  
  
  
  
  ###############################
  #START AVERAGES RENDER TEXT
  #Per year
  output$avgNumFilmsYears <- renderText({
    "32.8017241379 ~ 32 Films"
  })
  
  output$avgNumFilmsMonths <- renderText({
    "2.733477011491667 ~ 2-3 Films"
  })
  
  output$avgRunningTime <- renderText({
    "96.7802891 ~ 96 Minutes"
  })
  
  #END OF AVERAGES PLOTLY
  ##################################
  
  
  
  ##################################
  #NUMBER OF FILMS RELEASED START
  
  ######
  
  
  
  
  #Number of Films Per YEAR
  output$filmsPerYear <- renderPlotly({
    ggplot(data=moviesPerYearDF, aes(x=moviesPerYear, y=Freq)) + geom_bar(stat="identity") + xlab("Movies per year") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
  })
  output$filmsPerYearTable <- renderDataTable({
    moviesPerYearDF
  })
  
  #Number of Films Per MONTH
  output$filmsPerMonth <- renderPlotly({
    ggplot(data=moviesPerMonthDF, aes(x=moviesPerMonth, y=Freq)) + geom_bar(stat="identity") + xlab("Movies per Month") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
  })
  output$filmsPerMonthTable <- renderDataTable({
    moviesPerMonthDF
  })
  
  #NUMBER OF FILMS RELEAESD END
  ################################
  
  
  
  ################################
  #DISTRIBUTIONS START
  
  
  
{
  
  #Run Times Distribution
  output$runTimes <- renderPlotly({
    ggplot(data=countRuntime, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Top 10 Runtime") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
  })
  output$runTimeTable <- renderDataTable({
    countRuntime
  })
  
  
  #Certificates Distribution 
  output$certif <- renderPlotly({
    ggplot(data=countCertificate, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Certificate") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
  })
  output$certifTable <- renderDataTable({
    countCertificate
  })
  
  #Genres distribution
  output$genres <- renderPlotly({
    ggplot(data=countGenre, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Genre") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
  })
  output$genresTable <- renderDataTable({
    countGenre
  })
  
  #Keywords distribution
  output$keyWords <- renderPlotly({
    #number <-
    ggplot(data=countKeyword, aes(x=x, y=freq)) + geom_bar(stat="identity") + xlab("Keywords") + ylab("Frequency") +  geom_bar(stat="identity", fill = "#FF6666")
  })
  
  output$keyWordsTable <- renderDataTable({
    countKeyword
  })
  
  
}
  
  
  #DISTRIBUTIONS END
  ################################
  
  
  
  
  
  observeEvent(input$aboutPage, 
               {
                 showNotification("Authors: Imaad, Abdul, Jaoudat 
          Data Originated from IMDb 2017 Dataset
          Libraries Used: shiny, dplyr, plotly, shinyWidgets
         ")
               })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)