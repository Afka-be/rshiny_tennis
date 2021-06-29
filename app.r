
library(shiny)
library(shinydashboard)
library(robservable)
library(tidyverse) #Needed for read_csv (not to be confused with read.csv)




ui <- dashboardPage(

    dashboardHeader(title = " Who is the goat?"),

    dashboardSidebar(sidebarMenu(id = "sidebarid",
      menuItem("Homepage", tabName = "homepage", icon = icon("dashboard")),
      menuItem("General overview", tabName = "general", icon = icon("dashboard")),
      menuItem("Player", tabName = "player",  icon = icon("address-card")),
      conditionalPanel(
      'input.sidebarid == "player"',
      selectInput(inputId = "nom",
                  label = "Choose a name",
                  choices = c("Roger Federer","Raphael Nadal","Novak Djokovic"),
                  multiple = FALSE),
      actionButton(inputId = "clicks",
                  label = "Update")
      ))
    ), # dashboardSidebar

    dashboardBody(
      # include the CSS file
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "/style/custom.css")
      ),
      tabItems(
        tabItem(tabName = "homepage",
                fluidPage(img(src = 'images/logo.png', height = '600px', width = '600px'))
        ),
        tabItem(tabName = "general",
                robservableOutput("chart", width = 600)
        ),
        tabItem(tabName = "player",
              fluidRow(
                box(
                  title = "Resume", status = "warning",
                  solidHeader = TRUE,
                  width = 4,
                  fluidRow(column(width = 8, imageOutput("playerimage", height = NULL)),
                          column(width = 4,  htmlOutput("nom_text") , htmlOutput("nationalite_text"),  htmlOutput("age_text")))
                ),
                column(8,
                  fluidRow(
                    column(6, infoBoxOutput("Grandslams", width = NULL)),
                    column(6, infoBoxOutput("Masters1000", width = NULL))
                  ),
                  fluidRow(
                    column(6, infoBoxOutput("Olympicmedals", width = NULL)),
                    column(6, infoBoxOutput("Weeksnr1", width = NULL))
                  )
                )
              ), #fluidRow
              fluidRow(
                column(6,
                tabBox(id = "Grand slams",
                      title = "Grand slams", width = "800px",
                      height = "250px",
                      selected = "Grand chelems",
                      tabPanel("Australian Open", ),
                      tabPanel("Rolland Garros",  "Tab content Rolland Garros "),
                      tabPanel("Wimbeldon",  "Tab cotent Wimbeldon"),
                      tabPanel("US Open",  "Tab cotent US Open"))),

                column(6,
                tabBox(id = "Masters 1000",
                      title = "Masters 1000",
                      width = "800px",
                      height = "250px",
                      selected = "Grand chelems",
                      tabPanel("Australian Open", ),
                      tabPanel("Rolland Garros",  "Tab content Rolland Garros "),
                      tabPanel("Wimbeldon",  "Tab cotent Wimbeldon"),
                      tabPanel("US Open",  "Tab cotent US Open")))
              ), #fluidRow
              fluidRow(
                column(6,
                tabBox(id = "Olympic Medals",
                        title = "Olympic Medals",
                        width = "800px",
                        height = "250px",
                        selected = "Grand chelems",
                        tabPanel("Australian Open", ),
                        tabPanel("Rolland Garros",  "Tab content Rolland Garros "),
                        tabPanel("Wimbeldon",  "Tab cotent Wimbeldon"),
                        tabPanel("US Open",  "Tab cotent US Open"))),

                column(6,
                tabBox(id = "Weeks nr 1 world",
                        title = "Weeks nr 1 world",
                        width = "800px",
                        height = "250px",
                        selected = "Grand chelems",
                        tabPanel("Australian Open", ),
                        tabPanel("Rolland Garros",  "Tab content Rolland Garros "),
                        tabPanel("Wimbeldon",  "Tab cotent Wimbeldon"),
                        tabPanel("US Open",  "Tab cotent US Open")))
              ) #fluidRow
        ) #tabName player
      ) # tabItems
    ) # dashboardBody
  ) # dashboardPage


server <- function(input, output) {

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
    "Grand slams", a+b+c+d,icon = icon("check-double", lib = "font-awesome"),
    color = "purple"
  )
})

output$Masters1000 <- renderInfoBox({
  playerchelems <- subset(chelemsCSV, Nom == var_playername())  
  e = playerchelems[, 6];
 
  infoBox(
    "Masters 1000", e, icon = icon("check", lib = "font-awesome"),
    color = "orange"
  )
})

output$Olympicmedals <- renderInfoBox({
  playerchelems <- subset(chelemsCSV, Nom == var_playername())  
  f = playerchelems[, 7];
  infoBox(
    "Olympic medals", f, icon = icon("medal", lib = "font-awesome"),
    color = "green"
  )
})

output$Weeksnr1 <- renderInfoBox({
  playerchelems <- subset(chelemsCSV, Nom == var_playername())  
  g = playerchelems[, 8];
  infoBox(
    "Weeks nr 1 world", g, icon = icon("calendar-check",lib = "font-awesome"),
    color = "maroon"
  )
})


output$chart <- renderRobservable({
  d <- read_csv2(file = 'csv/ere open 3.csv')
  robservable(
    "https://observablehq.com/@juba/bar-chart-race",
    include = c("viewof date", "chart", "draw", "styles"),
    hide = "draw",
    input = list(
      data = d,
      title = "Cumulative number of grand slams in career over time?",
      #subtitle = "Cumulative number of grand slams in career over time",
      source = "Source : ATP Tour"
      # tickDuration = 500,         # top_n = 10,
      # color_scheme = "schemeSet3"
    ),
    width = 700,
    height = 400,
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