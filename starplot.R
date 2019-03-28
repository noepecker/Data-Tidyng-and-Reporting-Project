require(shiny)
require(tidyverse)
require(shinyjs)
require(fmsb)
library(readr)

########################### CLEANING



seasons_stats <- read_csv("Seasons_Stats.csv", col_types = cols(
  X1 = col_skip(), `3P` = col_double(), 
  `3P%` = col_double(), `3PA` = col_double(), 
  `3PAr` = col_double(), `AST%` = col_double(), 
  BLK = col_double(), `BLK%` = col_double(), 
  BPM = col_double(), DBPM = col_double(), 
  DRB = col_double(), `DRB%` = col_double(), 
  GS = col_double(), MP = col_double(), 
  OBPM = col_double(), ORB = col_double(), 
  `ORB%` = col_double(), PER = col_double(), 
  STL = col_double(), `STL%` = col_double(), 
  TOV = col_double(), `TOV%` = col_double(), 
  TRB = col_double(), `TRB%` = col_double(), 
  `USG%` = col_double(), VORP = col_double(), 
  `WS/48` = col_double(), blank2 = col_double(), 
  blanl = col_double()))
#Remove blank columns
stats=seasons_stats
stats=stats[,-c(21,26)]

#Move name to the front
stats=select(stats, Player, everything())

#Filter to get data only since 2000
stats=stats %>% filter(Year>2000)

#Filter to get data from the 5 main positions
statsC =stats %>% filter(Pos == 'C')
statsPF =stats %>% filter(Pos == 'PF')
statsSF =stats %>% filter(Pos == 'SF')
statsSG =stats %>% filter(Pos == 'SG')
statsPG =stats %>% filter(Pos == 'PG')

stats=rbind(statsC, statsPF, statsSF, statsSG, statsPG)

#Filter players with less than 30 macthes
stats=stats %>% filter(G>30)

### Filtering for the Radar Plot
pstats= stats %>% select(ends_with("%"))
pstats=pstats[,1:8]
rstats=cbind(stats[,c(1:2,5)],pstats)

#Join two variables to make it easier
rstats=unite(rstats, "Player_Team", c("Player","Tm"))

#Complete cases to filter NAs
stats=stats %>% drop_na()

#Arrange the column for an easy search
rstats= rstats %>% arrange(rstats$Player_Team)

rstats_2017=rstats %>% filter(Year == 2017)





###################################### SHINY APP


######## INPUTS 

list_variables <-  c(colnames(stats))
list_category <-  unique(c(colnames(stats[6:length(stats[1,])])))
list_year <- unique(stats$Year)[!is.na(unique(stats$Year))]
list_player <- unique(stats$Player)[!is.na(unique(stats$Player))]
list_team <- unique(stats$Tm)[!is.na(unique(stats$Tm))]
list_player_team_2017 <- unique(rstats_2017$Player_Team)[!is.na(unique(rstats_2017$Player_Team))]



################## ui

ui=navbarPage("NBA features",
              tabPanel("Position Comparision",
                       fluidPage( 
                         sidebarLayout(
                           sidebarPanel(("Control Panel"),
                            selectInput("category", label = h3("Select Category"), 
                                       choices = character(0),
                                       selected = 1),
                            selectInput("year", label = h3("Select Year"), 
                                        choices = character(0),
                                        selected = 1)
                           
                         ), # sidebarPanel
                           mainPanel(("Box Plot (Per-Game)"),
                                     plotOutput(outputId = "boxplot")
                           )# mainPanel
                         )# sidebarLayout
                       )# fluidPage
              ), #  tabPanel
              tabPanel("Features of Players",
                       fluidPage( 
                         sidebarLayout(
                           sidebarPanel(("Control Panel"),
                                        selectInput("player_team", label = h4("Select Player - Team"), 
                                                    choices = character(0),
                                                    selected = 1),
                                        selectInput("player_team2", label = h4("Select Player - Team"), 
                                                    choices = character(0),
                                                    selected = 1)

                           ), # sidebarPanel
                           mainPanel(h3("1 vs 1 attributes on a radar plot"),
                                     plotOutput(outputId = "radarplot")
                           )# mainPanel
                         )# sidebarLayout
                       )# fluidPage
              ), #  tabPanel
              tabPanel("Description of the variables",
                       fluidPage( 
                         titlePanel("Description of the variables"),
                         sidebarLayout(
                           sidebarPanel(
                             selectInput("name", label = h3("Select Variable"), 
                                         choices = character(0),
                                         selected = 1)
                             
                           ), # sidebarPanel
                           mainPanel(("Description of the variables"),
                           textOutput(outputId = "info")
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
                    selected = tail(list_variables, 1)
  );
  
  
  # Select category to visualize boxplot
  updateSelectInput(session, "category",
                    choices = list_category,
                    selected = tail(list_category, 1)
  );
  
  # Select year to visualize boxplot
  updateSelectInput(session, "year",
                    choices = list_year,
                    selected = tail(list_year, 1)
  );
  # Select year to visualize boxplot
  updateSelectInput(session, "year2",
                    choices = list_year,
                    selected = tail(list_year, 1)
  );
  
  # Select Player to visualize boxplot
  updateSelectInput(session, "player",
                    choices = list_player,
                    selected = tail(list_player, 1)
  );
  
  # Select Team to visualize boxplot
  updateSelectInput(session, "team",
                    choices = list_team,
                    selected = tail(list_team, 1)
  );
  
  # Select Player-Team to visualize Radarplot
  updateSelectInput(session, "player_team",
                    choices = list_player_team_2017,
                    selected = tail(list_player_team_2017, 1)
  );
  
  # Select Player-Team to visualize Radarplot
  updateSelectInput(session, "player_team2",
                    choices = list_player_team_2017,
                    selected = tail(list_player_team_2017, 1)
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

      astats=rstats_2017 %>% 
        filter(`Player_Team` == input$player_team)
      bstats=rstats_2017 %>% 
        filter(`Player_Team` == input$player_team2)
      
      sstats=rbind(astats,bstats)
      #Add max and min
      sstats=sstats[,3:10]
      sstats=rbind(c(1,20,40,30,60,5,11,37) , rep(0,8) , sstats)
      
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

  
  output$info =renderText(input$name)
  
}


# Run the application 
shinyApp(ui = ui, server = server)
