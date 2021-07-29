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
              global_slams_ui("players")
            ), #box
          ), #fluidRow
      ),
      tabItem(
        tabName = "player",
          fluidRow(
            player_select_ui("select")
          ), #fluidRow
          fluidRow(
            player_card_ui("select"),
            player_stats_ui("select"),
            player_spider_ui("select")
          ) #fluidRow
        ) #tabName player
    ) #tabItems
  )
)