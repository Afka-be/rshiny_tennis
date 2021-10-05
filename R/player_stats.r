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
            get_globalstats <- reactive({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())
                #Append values to the global list parameters for markdown report
                player_params <<- append(player_params, list(globalstats = playerchelems))

                return(playerchelems)
            })

            output$Grandslams <- renderInfoBox({
                a = get_globalstats()[, 2];
                b = get_globalstats()[, 3];
                c = get_globalstats()[, 4];
                d = get_globalstats()[, 5];
                infoBox(
                    "Grand slams", a+b+c+d,icon = icon("check circle"),
                    color = "purple"
                )
            })
            output$Masters1000 <- renderInfoBox({
                e = get_globalstats()[, 6];
                infoBox(
                    "Masters 1000", e, icon = icon("check"),
                    color = "orange"
                )
            })
            output$Olympicmedals <- renderInfoBox({
                f = get_globalstats()[, 7];
                infoBox(
                    "Olympic medals", f, icon = icon("trophy"),
                    color = "green"
                )
            })
            output$Weeksnr1 <- renderInfoBox({
                g = get_globalstats()[, 8];
                infoBox(
                    "Weeks nr 1 world", g, icon = icon("calendar check"),
                    color = "blue"
                )
            })
            output$DavisCup <- renderInfoBox({
                h = get_globalstats()[, 11];
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
    # Select CSV and store it.
    chelemsCSV <- csv_select("chelems")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            get_globalstats <- reactive({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())
                return(playerchelems)
            })

            get_masters1000stats <- reactive({
                # Select the mastersStack Xls and player's sheet
                db <- xlsx_select("masters1000_total", get_playername())

                # Because some players did not compete at every master,
                # we need to search for each master1000 name in the sheet
                # if we have no match, then the player did not play this master
                playermasterselect <- function(mastersName) {
                    subset(db, Annee == 2021 & Type == mastersName)
                }

                # convert the dbl value into a num with as.numeric
                a = as.numeric(playermasterselect('Indian Wells')[, 3]);
                b = as.numeric(playermasterselect('Miami Open')[, 3]);
                c = as.numeric(playermasterselect('Monte Carlo Masters')[, 3]);
                d = as.numeric(playermasterselect('Madrid Open')[, 3]);
                e = as.numeric(playermasterselect('Rome Open')[, 3]);
                f = as.numeric(playermasterselect('Canadian Open')[, 3]);
                g = as.numeric(playermasterselect('Cincinnati Masters')[, 3]);
                h = as.numeric(playermasterselect('Shanghai Masters')[, 3]);
                i = as.numeric(playermasterselect('Paris Masters')[, 3]);

                masters1000Vector <- c(a, b, c, d, e, f, g, h, i)
                # if the master is missing, value is NA
                # for the scatterpolar, we need 0 and not NA to create and fill the graph
                # Convert NA value in the vector into 0
                masters1000Vector[is.na(masters1000Vector)] <- 0

                #Append values to the global list parameters for markdown report
                player_params <<- append(player_params, list(masters1000 = masters1000Vector))

                return(masters1000Vector)
            })

            get_olympicstats_simple <- reactive({
                # Select the Olympics Xls and player's sheet
                db <- xlsx_select("olympics", get_playername())
                df <- data.frame(db) #convert to dataframe, better for maths
                df[is.na(df)] <- 0 #convert NA values to 0 to avoid errors

                # "Simple" vector. Count Medals for each grade
                a = length(which(df$Simple_en == "bronze"));
                b = length(which(df$Simple_en == "silver"));
                c = length(which(df$Simple_en == "gold"));

                olympicsimpleVector <- c(a, b, c)

                #Append values to the global list parameters for markdown report
                player_params <<- append(player_params, list(olympicsimple = olympicsimpleVector))

                return(olympicsimpleVector)
            })

            get_olympicstats_double <- reactive({
                # Select the Olympics Xls and player's sheet
                db <- xlsx_select("olympics", get_playername())
                df <- data.frame(db) #convert to dataframe, better for maths
                df[is.na(df)] <- 0 #convert NA values to 0 to avoid errors

                # "Double" vector. Sum Medals for each grade
                d = length(which(df$Double_en == "bronze"));
                e = length(which(df$Double_en == "silver"));
                f = length(which(df$Double_en == "gold"));

                olympicdoubleVector <- c(d, e, f)

                #Append values to the global list parameters for markdown report
                player_params <<- append(player_params, list(olympicdouble = olympicdoubleVector))

                return(olympicdoubleVector)
            })


            output$spider_playerSlams <- renderPlotly({

                playerchelems <- subset(chelemsCSV, Nom == get_playername())
                a = get_globalstats()[, 2];
                b = get_globalstats()[, 3];
                c = get_globalstats()[, 4];
                d = get_globalstats()[, 5];

                fig <- plot_ly(
                    type = 'scatterpolar',
                    mode = 'markers',
                    fill = 'toself'
                )
                fig <- fig %>%
                    add_trace(
                    r = c(a, b, c, d),
                    theta = c('Australian Open','Roland Garros','Wimbeldon', 'US Open'),
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
                fig <- plot_ly(
                    type = 'scatterpolar',
                    mode = 'markers',
                    fill = 'toself'
                )
                fig <- fig %>%
                    add_trace(
                    r = get_masters1000stats(),
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

            #load the tab on pageload even without clicking on it.
            #Need it to store values for the rmarkdown in the getter functions above
            outputOptions(output, "spider_playerMasters1000", suspendWhenHidden = FALSE)

            output$spider_playerOlympics <- renderPlotly({               
                fig <- plot_ly(
                    type = 'scatterpolar',
                    mode = 'markers',
                    fill = 'toself'
                )
                fig <- fig %>%
                    add_trace(
                    r = get_olympicstats_simple(),
                    theta = c('Bronze','Argent','Or'),
                    name = 'Simple'
                    )
                fig <- fig %>%
                    add_trace(
                    r = get_olympicstats_double(),
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

            #load the tab on pageload even without clicking on it.
            #Need it to store values for the rmarkdown in the getter functions above
            outputOptions(output, "spider_playerOlympics", suspendWhenHidden = FALSE)
        }
    )
}