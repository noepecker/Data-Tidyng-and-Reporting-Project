library(readr)
library(shinythemes)
library(DT)
library(plotly)
library(ggplot2)
library(devtools)
library(kaggler)
library(fmsb)
library(tidyverse)
library(shiny)
library(shinyjs)

library(RCurl)
url <- getURL("https://raw.githubusercontent.com/noepecker/Data-Tidyng-and-Reporting-Project/master/Seasons_Stats.csv")
#seasons_stats <- read.csv(text=url)
########################### CLEANING

#Import data

seasons_stats <- read.csv(text=url)
seasons_stats[,1]<- NULL

#Remove blank columns
stats=seasons_stats
stats=stats[,-c(21,26)]

#Move name to the front
stats=select(stats, Player, everything())

#Filter to get data only since 2000
stats=stats %>% filter(Year>2000)

#Filter to get data only from the 5 main positions
statsC =stats %>% filter(Pos == 'C')
statsPF =stats %>% filter(Pos == 'PF')
statsSF =stats %>% filter(Pos == 'SF')
statsSG =stats %>% filter(Pos == 'SG')
statsPG =stats %>% filter(Pos == 'PG')

stats=rbind(statsC, statsPF, statsSF, statsSG, statsPG)

#Filter players with less than 30 macthes which are useless for statistics analysis
stats=stats %>% filter(G>30)

### Filtering for the Radar Plot
pstats= stats[,c(10,13:19)]
pstats=pstats[,1:8]
rstats=cbind(stats[,c(1:2,5)],pstats)

#Join two variables to make it easier
rstats=unite(rstats, "Player_Team", c("Player","Tm"))

#Complete cases to filter NAs
stats=stats %>% drop_na()

#Arrange the column for an easy search
rstats= rstats %>% arrange(rstats$Player_Team)

rstats_2017=rstats %>% filter(Year == 2017)

#New dataset to get unique row names
dstats=unite(stats, "Pl_Tm_Yr", c("Player","Tm", "Year"))
dstats=cbind(dstats[,1],stats[,c(1:50)])
row.names(dstats)=dstats$Pl_Tm_Yr
dstats=dstats[,2:51]


###################################### SHINY APP
require(shiny)
require(tidyverse)
require(shinyjs)
require(fmsb)

######## INPUTS 

#Lists to select
list_variables <-  c(colnames(stats))
list_position <- unique(stats$Pos)[!is.na(unique(stats$Pos))]
list_category <-  unique(c(colnames(stats[6:length(stats[1,])])))
stats2=stats %>% select(-contains('%'))
list_category2 <-  unique(c(colnames(stats2[6:length(stats2[1,])])))
list_year <- unique(stats$Year)[!is.na(unique(stats$Year))]
list_player <- unique(stats$Player)[!is.na(unique(stats$Player))]
list_team <- unique(stats$Tm)[!is.na(unique(stats$Tm))]
list_player_team_2017 <- unique(rstats_2017$Player_Team)[!is.na(unique(rstats_2017$Player_Team))]
#Adapt the dataset to plot
stats2017=stats %>% filter(Year==2017)
list_player2017 <- stats2017$Player


################## ui

ui=navbarPage("NBA features",
              tabPanel("Position Comparision",
                       fluidPage(theme = shinytheme("flatly"),
                                 sidebarLayout(
                                   sidebarPanel(("Control Panel"),
                                                selectInput("category", label = h3("Select Category"), 
                                                            choices = character(0),
                                                            selected = 1),
                                                selectInput("year", label = h3("Select Year"), 
                                                            choices = character(0),
                                                            selected = 1)
                                                
                                   ), # sidebarPanel
                                   mainPanel(h3("Position player Comparision", align = "center"),
                                             plotOutput(outputId = "boxplot")
                                   )# mainPanel
                                 ),# sidebarLayout
                                 sidebarLayout(
                                   sidebarPanel(("Top NBA"),
                                                selectInput("category2", label = h3("Select Category"), 
                                                            choices = character(0),
                                                            selected = 1),
                                                sliderInput("year2", label = h4("Select Year"), 
                                                            min=2000, max=2017, value=2017),
                                                fluidRow(
                                                  h3(style = "margin-left: 20px; margin-bottom: 0px;", h4("Position")),
                                                  column(3,
                                                         div( checkboxInput("all_players", label = "All players", value = TRUE))
                                                  ),
                                                  column(10, 
                                                         selectInput("position", label= "" ,
                                                                     choices = character(0),
                                                                     selected = 1)
                                                  )
                                                )
                                   ), #sidebarpanel 2
                                   mainPanel(h3("NBA Top 10",align = "center"),
                                             fluidRow(
                                               column(12,align="center",
                                                      tableOutput(outputId = "topten"
                                                      )
                                               ) #column
                                             )#FluidRow    
                                   )# mainPanel
                                 )# sidebarLayout 2
                       )# fluidPage
              ), #  tabPanel
              tabPanel("1 vs 1",
                       fluidPage(theme = shinytheme("flatly"), 
                                 sidebarLayout(
                                   sidebarPanel(("Control Panel"),
                                                selectInput("player_team", label = h4("Select Player - Team"), 
                                                            choices = character(0),
                                                            selected = 1),
                                                selectInput("player_team2", label = h4("Select Player - Team"), 
                                                            choices = character(0),
                                                            selected = 1)
                                                
                                   ), # sidebarPanel
                                   
                                   mainPanel(h3("1 vs 1 in 2017"),
                                             h5("Attributes that measure the influence in their teams when they are in the floor"),
                                             #Radar plot
                                             plotOutput(outputId = "radarplot"),
                                             
                                             #Glossary to explain the graph
                                             
                                             fluidRow(
                                               column(1,
                                                      h5(strong("TS%:"))
                                               ),
                                               column(11,
                                                      p("Shooting efficiency that takes into account 2-point field goals
                                                        3-point field goals and free throws.", align="left")
                                                      )
                                               ),#FluidRow
                                             fluidRow(
                                               column(1,
                                                      h5(strong("ORB%:"))
                                               ),
                                               column(11,
                                                      p("Percentage of available offensive rebounds a player grabbed while 
                                                        he was on the floor.", align="left")
                                                      )
                                               ),#FluidRow
                                             fluidRow(
                                               column(1,
                                                      h5(strong("DRB%:"))
                                               ),
                                               column(11,
                                                      p("Percentage of available defensive rebounds a player grabbed while 
                                                        he was on the floor.", align="left")
                                                      )
                                               ),#FluidRow
                                             fluidRow(
                                               column(1,
                                                      h5(strong("TRB%:"))
                                               ),
                                               column(11,
                                                      p("Percentage of available rebounds a player grabbed while
                                                        he was on the floor.", align="left")
                                                      )
                                               ),#FluidRow
                                             fluidRow(
                                               column(1,
                                                      h5(strong("AST%:"))
                                               ),
                                               column(11,
                                                      p("Percentage of teammate field goals a player assisted while
                                                        he was on the floor.", align="left")
                                                      )
                                               ),#FluidRow
                                             fluidRow(
                                               column(1,
                                                      h5(strong("STL%:"))
                                               ),
                                               column(11,
                                                      p("Percentage of opponent possesions that end with a steal by the player while
                                                        he was on the floor.", align="left")
                                                      )
                                               ),#FluidRow
                                             fluidRow(
                                               column(1,
                                                      h5(strong("BLK%:"))
                                               ),
                                               column(11,
                                                      p("Percentage of opponent 2-point field goal attempts blocked by the player while 
                                                        he was on the floor.", align="left")
                                                      )
                                               ),#FluidRow
                                             fluidRow(
                                               column(1,
                                                      h5(strong("TOV%:"))
                                               ),
                                               column(11,
                                                      p("Turnovers commited per 100 plays", align="left")
                                               )
                                             )#FluidRow
                                             )# mainPanel
                                             )# sidebarLayout
              )# fluidPage
              ), #  tabPanel
              tabPanel("Best Performance",
                       fluidPage(theme = shinytheme("flatly"), 
                                 titlePanel(""),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("category3", label = h3("Select Variable"), 
                                                 choices = character(0),
                                                 selected = 1)
                                     #textInput("Player", "Player Name")
                                     
                                   ), # sidebarPanel
                                   mainPanel(h4("Performance in a specific area vs total points in 2017", align="center"),
                                             #textOutput(outputId = "info"),
                                             plotlyOutput("dplot"),
                                             p(style = "margin-top: 50px;", "In case you are wondering about the meaning of stat acronyms in the dataset, have a look at the source page, e.g."),
                                             uiOutput("tab")
                                   )# mainPanel
                                 )# sidebarLayout
                       )# fluidPage
              )#  tabPanel
              )

################ server

server <- function(input, output, session) {
  
  # Select variable for description
  updateSelectInput(session, "name",
                    choices = list_variables,
                    selected = head(list_variables, 1)
  );
  
  # Select position for top10
  updateSelectInput(session, "position",
                    choices = list_position,
                    selected = head(list_position, 1)
  );
  
  # Select category to visualize boxplot
  updateSelectInput(session, "category",
                    choices = list_category,
                    selected = tail(list_category, 1)
  );
  
  # Select category to visualize plotly
  updateSelectInput(session, "category3",
                    choices = list_category,
                    selected = tail(list_category, 1)
  );
  
  # Select category to visualize top10
  updateSelectInput(session, "category2",
                    choices = list_category2,
                    selected = tail(list_category2, 1)
  );
  
  # Select year to visualize boxplot
  updateSelectInput(session, "year",
                    choices = list_year,
                    selected = tail(list_year, 1)
  );
  # Select year to visualize top10
  updateSelectInput(session, "year2",
                    choices = list_year,
                    selected = tail(list_year, 1)
  );
  
  # Select Player to visualize boxplot
  updateSelectInput(session, "player",
                    choices = list_player,
                    selected = head(list_player, 1)
  );
  
  # Select Team to visualize boxplot
  updateSelectInput(session, "team",
                    choices = list_team,
                    selected = head(list_team, 1)
  );
  
  # Select Player-Team to visualize Radarplot
  updateSelectInput(session, "player_team",
                    choices = list_player_team_2017,
                    selected = head(list_player_team_2017, 1)
  );
  
  # Select Player-Team to visualize Radarplot
  updateSelectInput(session, "player_team2",
                    choices = list_player_team_2017,
                    selected = head(list_player_team_2017, 1)
  );
  
  # Select Player-Team to visualize Radarplot
  updateSelectInput(session, "player2017",
                    choices = list_player2017,
                    selected = head(list_player2017, 1)
  );
  
  #Boxplot
  
  output$boxplot <- renderPlot({
    varname <- input$category
    symname <- rlang::sym(varname)
    quoname <- enquo(symname)
    
    ggplot(stats %>% filter(Year==input$year),
           aes(x=Pos, y=!!quoname, colour = Pos)) +
      geom_violin(trim = FALSE, fill="black", alpha=0.1,size = 0.8)+
      geom_jitter(width = 0.15)+
      stat_boxplot(geom ='errorbar', width = 0.1)+
      geom_boxplot(width = 0.4, alpha=0.4)+
      ylab(input$category)
  });
  
  #Radar plot
  
  output$radarplot <- renderPlot({
    if(input$player_team != ""){
      # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
      pstats=rbind(c(1,20,40,30,60,5,11,37) , rep(0,8) , pstats)
      
      #Stats for each selected player
      astats=rstats_2017 %>% 
        filter(`Player_Team` == input$player_team)
      bstats=rstats_2017 %>% 
        filter(`Player_Team` == input$player_team2)
      
      #We join them to plot the radar
      sstats=rbind(astats,bstats)
      
      #Add max and min
      sstats=sstats[,3:10]
      sstats=rbind(c(1,20,40,30,60,5,11,37) , rep(0,8) , sstats)
      
      #Colors
      colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
      colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
      
      radarchart( sstats  , axistype=1 , 
                  #custom polygon
                  pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                  #custom labels
                  vlcex=0.8 
      )
      legend(x=0.2, y=1.4, legend = c(input$player_team, input$player_team2), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
      
    }
  });
  
  #NBA Top
  
  #To see the overall top or distinguish by position
  observe(if(input$all_players) disable("position") else enable("position") )
  
  output$topten = renderTable(
    
    if(input$all_players){
      stats %>% arrange_(input$category2) %>%
        filter(Year==input$year2) %>%
        select(Player,Pos, input$category2) %>%
        tail(10)
      
    }else{
      stats %>% arrange_(input$category2) %>%
        filter(Year==input$year2) %>%
        filter(Pos==input$position) %>%
        select(Player,Pos, input$category2) %>%
        tail(10)
    }
  );
  #Plotly Boxplot Not used
  
  output$plot <- renderPlotly({ 
    dstats %>% filter(Year==2017) %>%
      plot_ly(
        x = ~Pos,
        y = ~PF,
        split = ~Pos,
        type = 'violin',
        points = "all", jitter = 0.4, pointpos = 0, 
        opacity= 0.75,
        #ids=,
        #customdata=,
        #hoverinfo=~PF,
        fillcolor=~Pos,
        box = list(visible = T),
        meanline = list(visible = T)) %>% 
      layout(
        xaxis = list(title = "Position"),
        yaxis = list(title = "PF",zeroline = F)
      )
    
  });
  
  #Plotly Plot
  
  output$dplot <- renderPlotly({
    varname2 <- input$category3
    symname2 <- rlang::sym(varname2)
    quoname2 <- enquo(symname2)
    key= list_player2017
    p <- ggplot(stats2017, 
                aes(x = PTS, y = !!quoname2 , colour = Tm, key = key)) + 
      geom_point(alpha=0.5)+
      ylab(input$category3)+
      xlab("Total Points")
    ggplotly(p) %>% layout(dragmode = "select")
  });
  
  url <- a("Kobe Bryant example", href="https://www.basketball-reference.com/players/b/bryanko01.html")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
