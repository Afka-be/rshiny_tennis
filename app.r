library(shiny)
#library(shinydashboard)
library(shiny.semantic)
library(semantic.dashboard)
library(plotly) #Spiderchart
library(robservable)
library(tidyverse) #Needed for read_csv (not to be confused with read.csv)




ui <- dashboardPage(
  margin = FALSE,
  dashboardHeader(title = tags$div(class = 'goat_logo',
                            tags$a(href='#',
                            tags$img(src='images/logo_app.svg')),
                          ),
                  logo_path = "images/logo.svg",
                  logo_align = "right",
                  inverted = TRUE,
                  dropdownMenu(type = "notifications",
                    taskItem("Project progress...", 50.777, color = "red")
                  )
                ),

  dashboardSidebar(
    size = "wide",
    inverted = TRUE,
    sidebarMenu(
      menuItem(tabName = "homepage", "Homepage", icon = icon("dashboard")),
      menuItem(tabName = "general", "General overview", icon = icon("dashboard")),
      menuItem(tabName = "player", "Player", icon = icon("address card"))
    )
  ),
  dashboardBody(
    # include the CSS file
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "/style/custom.css"),
    ),
    tabItems(
      selected = 1,
      tabItem(
        tabName = "homepage",
        box(h1("Welcome"), title = "How to use", width = 16, color = "orange")
      ),
      tabItem(
        tabName = "general",
          fluidRow(
            box(
              width = 16,
              h1("General overview"),
              robservableOutput("chart", height = NULL)
            ), #box
          ), #fluidRow
      ),
      tabItem(tabName = "player",
          fluidRow(
            box(
              width = 16,
              h1("Choose a player"),
              div(
                class = "choose_player_window",
                selectInput(
                  inputId = "nom",
                  label = "Choose a name",
                  width = 450,
                  choices = c("Roger Federer", "Raphael Nadal", "Novak Djokovic"),
                  multiple = FALSE),
                actionButton(inputId = "clicks",
                              label = "Update")
              ), #div
            ), #box
          ), #fluidRow
          fluidRow(
            box(
              title = "Resume", status = "warning",
              solidHeader = TRUE,
              width = 4,
              class = "player_card",
              imageOutput("playerimage", height = NULL, width = NULL),
              htmlOutput("nom_text") , htmlOutput("nationalite_text"),  htmlOutput("age_text")
            ), #box
            column(4,
            class = "player_numbers",
              infoBoxOutput("Grandslams"),
              infoBoxOutput("Masters1000"),
              infoBoxOutput("Olympicmedals"),
              infoBoxOutput("Weeksnr1")
            ), #column
            column(8,
              plotlyOutput("playerPlot")
            )
          ) #fluidRow
        ) #tabName player
    ) #tabItems
  )
)


server <- function(input, output, session) {

var_playername  <- eventReactive(input$clicks, {
  input$nom})

output$playerimage <- renderImage({
  #Replace the space with an underscore for the image src
  var_playerimage <- sub(" ", "_", var_playername())
  return(list(
    width = "200px",
    src = paste("players_pictures/", var_playerimage, ".png", sep = ""),
    filetype = "image/png",
    alt = paste(var_playername())
  ))
}, deleteFile = FALSE)

#Only use for debug purposes
output$debugtext <- renderText({
  paste("players_pictures/", var_playername(), ".png", sep = "")
})

chelemsCSV <- read.csv(file = "csv/chelems.csv", header = TRUE, sep = ";")


output$Grandslams <- renderInfoBox({
playerchelems <- subset(chelemsCSV, Nom == var_playername())
a = playerchelems[, 2];
b = playerchelems[, 3];
c = playerchelems[, 4];
d = playerchelems[, 5];


  infoBox(
    "Grand slams", a+b+c+d,icon = icon("check circle"),
    color = "purple"
  )
})

output$Masters1000 <- renderInfoBox({
  playerchelems <- subset(chelemsCSV, Nom == var_playername())  
  e = playerchelems[, 6];
 
  infoBox(
    "Masters 1000", e, icon = icon("check"),
    color = "orange"
  )
})

output$Olympicmedals <- renderInfoBox({
  playerchelems <- subset(chelemsCSV, Nom == var_playername())  
  f = playerchelems[, 7];
  infoBox(
    "Olympic medals", f, icon = icon("trophy"),
    color = "green"
  )
})

output$Weeksnr1 <- renderInfoBox({
  playerchelems <- subset(chelemsCSV, Nom == var_playername())  
  g = playerchelems[, 8];
  infoBox(
    "Weeks nr 1 world", g, icon = icon("calendar check"),
    color = "blue"
  )
})


output$playerPlot <- renderPlotly({
  # Create data: note in High school for several students
  fig <- plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  ) 
  fig <- fig %>%
    add_trace(
      r = c(39, 28, 8, 7, 28, 39),
      theta = c('A','B','C', 'D', 'E', 'A'),
      name = 'Group A'
    )
  fig <- fig %>%
    add_trace(
      r = c(1.5, 10, 39, 31, 15, 1.5),
      theta = c('A','B','C', 'D', 'E', 'A'),
      name = 'Group B'
    )
  fig <- fig %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,50)
        )
      )
    )

  fig
})

output$chart <- renderRobservable({
  d <- read.csv(file = 'csv/ere open 3.csv', header = TRUE, sep = ";")
  robservable(
    "https://observablehq.com/@juba/bar-chart-race",
    include = c("viewof date", "chart", "draw", "styles"),
    hide = "draw",
    input = list(
      data = d,
      title = "Cumulative number of grand slams in career over time ?",
      subtitle = "Cumulative number of grand slams in career over time",
      source = "Source : ATP Tour",
      tickDuration = 200,
      # top_n = 10,
      color_scheme = "schemeSet2"
    ),
    width = 700,
    height = 600,
  )
})


output$nom_text <- renderText({
  playerchelems <- subset(chelemsCSV, Nom == var_playername())  
  h = playerchelems[, 9];
  i = playerchelems[, 10];
  paste("<b>Nom:</b>", var_playername())
  })

output$nationalite_text <- renderText({
  playerchelems <- subset(chelemsCSV, Nom == var_playername())  
  h = playerchelems[, 9];
  i = playerchelems[, 10];
  paste("<b>Nationalité:</b>",h)
})

output$age_text <- renderText({
  playerchelems <- subset(chelemsCSV, Nom == var_playername())  
  h = playerchelems[, 9];
  i = playerchelems[, 10];
  paste("<b>Age:</b>", i,"years")
})


}

shinyApp(ui, server)