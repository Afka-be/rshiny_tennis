# Generates player stats inside infoboxes
player_stats_ui <- function(id) {
    ns <- NS(id)

    column(4,
        class = "player_numbers",
        infoBoxOutput(ns("Grandslams")),
        infoBoxOutput(ns("Masters1000")),
        infoBoxOutput(ns("Olympicmedals")),
        infoBoxOutput(ns("Weeksnr1"))
    ) #column
}

player_stats_server <- function(id) {

    # Get the player name from the dropdown (eventReactive)
    get_playername <- player_select_server("select")
    # Store the chelems CSV and use it inside server function
    chelemsCSV <- csv_select("chelems")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            output$Grandslams <- renderInfoBox({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())
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
                playerchelems <- subset(chelemsCSV, Nom == get_playername())  
                e = playerchelems[, 6];
                infoBox(
                    "Masters 1000", e, icon = icon("check"),
                    color = "orange"
                )
            })
            output$Olympicmedals <- renderInfoBox({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())  
                f = playerchelems[, 7];
                infoBox(
                    "Olympic medals", f, icon = icon("trophy"),
                    color = "green"
                )
            })
            output$Weeksnr1 <- renderInfoBox({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())  
                g = playerchelems[, 8];
                infoBox(
                    "Weeks nr 1 world", g, icon = icon("calendar check"),
                    color = "blue"
                )
            })
        }
    )
}

#----------------------------------
# Generates spiderchart for player stats
player_spider_ui <- function(id) {
    ns <- NS(id)

    column(8,
        plotlyOutput(ns("playerPlot"), height = NULL)
    ) #column
}

player_spider_server <- function(id) {

    # Get the player name from the dropdown (eventReactive)
    get_playername <- player_select_server("select")
    # Store the chelems CSV and use it inside server function
    chelemsCSV <- csv_select("chelems")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            output$playerPlot <- renderPlotly({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())
                a = playerchelems[, 2];
                b = playerchelems[, 3];
                c = playerchelems[, 4];
                d = playerchelems[, 5];
                # Create data: note in High school for several students
                fig <- plot_ly(
                    type = 'scatterpolar',
                    fill = 'toself'
                )
                fig <- fig %>%
                    add_trace(
                    r = c(a, b, c, d),
                    theta = c('Australian Open','Rolland Garros','Wimbeldon', 'US Open'),
                    name = 'Chelems'
                    )
                fig <- fig %>%
                    layout(
                    polar = list(
                        radialaxis = list(
                        visible = T,
                        range = c(0,14)
                        )
                    )
                    )
                fig
            })
        }
    )
}