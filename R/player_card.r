# Create the player card with his/her informations
player_card_ui <- function(id) {
    ns <- NS(id)
    box(
        title = "Resume", status = "warning", collapsible = FALSE,
        solidHeader = TRUE,
        width = 4,
        class = "player_card",
        imageOutput(ns("playerimage"), height = NULL, width = NULL),
        htmlOutput(ns("nom_text")),
        htmlOutput(ns("nationalite_text")),
        htmlOutput(ns("age_text"))
    ) #box
}

player_card_server <- function(id) {

    # Get the player name from the dropdown (eventReactive)
    get_playername <- player_select_server("select")
    # Store the chelems CSV and use it inside server function
    chelemsCSV <- csv_select("chelems")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            get_nationality <- reactive({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())
                nationality = playerchelems[, 9];

                #Append values to the global list parameters for markdown report
                player_params <<- append(player_params, list(name = get_playername()))
                player_params <<- append(player_params, list(nationality = nationality))
                return(nationality)
            })

            get_age <- reactive({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())
                age = playerchelems[, 10];

                #Append this value to the global list parameters for markdown report
                player_params <<- append(player_params, list(age = age))
                return(age)
            })

            # Create circle image Avatar
            output$playerimage <- renderImage({
                #Replace the space with an underscore for the image src
                var_playerimage <- tolower(sub(" ", "_",  get_playername()))
                filename <- normalizePath(file.path('players_pictures', 
                                            paste(var_playerimage, '.png', sep='')))
                return(list(
                    width = "200px",
                    src = filename,
                    filetype = "image/png",
                    alt = paste(get_playername())
                ))
            }, deleteFile = FALSE)

            # Outputs player name
            output$nom_text <- renderText({
                paste("<b>Nom:</b>", get_playername())
            })

            # Outputs player nationality
            output$nationalite_text <- renderText({
                paste("<b>Nationalité:</b>", get_nationality())
            })

            # Outputs player age
            output$age_text <- renderText({
                paste("<b>Age:</b>", get_age(), "years")
            })
        }
    )
}