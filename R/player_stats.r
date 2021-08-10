# Generates player stats inside infoboxes
player_stats_ui <- function(id) {
    ns <- NS(id)

    column(4,
        class = "player_numbers",
        infoBoxOutput(ns("Grandslams")),
        infoBoxOutput(ns("Masters1000")),
        infoBoxOutput(ns("Olympicmedals")),
        infoBoxOutput(ns("Weeksnr1")),
        infoBoxOutput(ns("DavisCup"))
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
            output$DavisCup <- renderInfoBox({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())  
                h = playerchelems[, 11];
                infoBox(
                    "Davis Cup", h, icon = icon("trophy",lib = "font-awesome"),
                    color = "yellow"
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
        class = "playerspidercharts_tabset",
        tabBox(title = "Tournaments Stats", collapsible = FALSE,
            tabs = list(
                list(menu = "Grand Slams", content = plotlyOutput(ns("spider_playerSlams"), height = "310px")),
                list(menu = "Masters 1000", content = plotlyOutput(ns("spider_playerMasters1000"), height = "310px")),
                list(menu = "Olympics", content = plotlyOutput(ns("spider_playerOlympics"), height = "310px"))
            ) #list
        ) #tabset
    ) #column
}

player_spider_server <- function(id) {

    # Get the player name from the dropdown (eventReactive)
    get_playername <- player_select_server("select")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            output$spider_playerSlams <- renderPlotly({
                # Select CSV and store it.
                chelemsCSV <- csv_select("chelems")

                playerchelems <- subset(chelemsCSV, Nom == get_playername())
                a = playerchelems[, 2];
                b = playerchelems[, 3];
                c = playerchelems[, 4];
                d = playerchelems[, 5];

                fig <- plot_ly(
                    type = 'scatterpolar',
                    mode = 'markers',
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

            output$spider_playerMasters1000 <- renderPlotly({

                # Select the mastersStack Xls and player's sheet
                db <- xlsx_select("masters1000_total", get_playername())

                # Because some players did not compete at every master,
                # we need to search for each master1000 name in the sheet
                # if we have no match, then the player did not play this master
                playermasterselect <- function(mastersName) {
                    subset(db, Annee == 2021 & Type == mastersName)
                }

                # convert the dbl value into a num with as.numeric and as.character
                a = as.numeric(as.character(playermasterselect('Indian Wells')[, 3]));
                b = as.numeric(as.character(playermasterselect('Miami Open')[, 3]));
                c = as.numeric(as.character(playermasterselect('Monte Carlo Masters')[, 3]));
                d = as.numeric(as.character(playermasterselect('Madrid Open')[, 3]));
                e = as.numeric(as.character(playermasterselect('Rome Open')[, 3]));
                f = as.numeric(as.character(playermasterselect('Canadian Open')[, 3]));
                g = as.numeric(as.character(playermasterselect('Cincinnati Masters')[, 3]));
                h = as.numeric(as.character(playermasterselect('Shanghai Masters')[, 3]));
                i = as.numeric(as.character(playermasterselect('Paris Masters')[, 3]));
                masters1000Vector <- c(a, b, c, d, e, f, g, h, i)
                # if the master is missing, value is NA
                # for the scatterpolar, we need 0 and not NA to create and fill the graph
                # Convert NA value in the vector into 0
                masters1000Vector[is.na(masters1000Vector)] <- 0

                fig <- plot_ly(
                    type = 'scatterpolar',
                    mode = 'markers',
                    fill = 'toself'
                )
                fig <- fig %>%
                    add_trace(
                    r = masters1000Vector,
                    theta = c('Indian Wells','Miami Open','Monte Carlo Masters', 'Madrid Open', 'Rome Open', 'Canadian Open', 'Cincinnati Masters', 'Shanghai Masters', 'Paris Masters' ),
                    name = 'Masters 1000'
                    )
                fig <- fig %>%
                    layout(
                    polar = list(
                        radialaxis = list(
                        visible = T,
                        range = c(0,11)
                        )
                    )
                    )
                fig
            })

            output$spider_playerOlympics <- renderPlotly({
                # Select the Olympics Xls and player's sheet
                db <- xlsx_select("olympics", get_playername())
                df <- data.frame(db) #convert to dataframe, better for maths
                df[is.na(df)] <- 0 #convert NA values to 0 to avoid errors

                # "Simple" vector. Count Medals for each grade
                a = length(which(df$Simple_en == "bronze"));
                b = length(which(df$Simple_en == "silver"));
                c = length(which(df$Simple_en == "gold"));

                # "Double" vector. Sum Medals for each grade
                d = length(which(df$Double_en == "bronze"));
                e = length(which(df$Double_en == "silver"));
                f = length(which(df$Double_en == "gold"));


                fig <- plot_ly(
                    type = 'scatterpolar',
                    mode = 'markers',
                    fill = 'toself'
                )
                fig <- fig %>%
                    add_trace(
                    r = c(a, b, c),
                    theta = c('Bronze','Argent','Or'),
                    name = 'Simple'
                    )
                fig <- fig %>%
                    add_trace(
                    r = c(d, e, f),
                    theta = c('Bronze','Argent','Or'),
                    name = 'Double'
                    )
                fig <- fig %>%
                    layout(
                    polar = list(
                        radialaxis = list(
                        visible = T,
                        range = c(0,3),
                        dtick = 1
                        )
                    )
                    )
                fig
            })
        }
    )
}