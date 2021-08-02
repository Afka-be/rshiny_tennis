# Create the player card with his/her informations
player_card_ui <- function(id) {
    ns <- NS(id)
    box(
        title = "Resume", status = "warning",
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
            # Create circle image Avatar
            output$playerimage <- renderImage({
                #Replace the space with an underscore for the image src
                var_playerimage <- sub(" ", "_",  get_playername())
                return(list(
                    width = "200px",
                    src = paste("players_pictures/", var_playerimage, ".png", sep = ""),
                    filetype = "image/png",
                    alt = paste(get_playername())
                ))
            }, deleteFile = FALSE)

            # Outputs player name
            output$nom_text <- renderText({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())
                h = playerchelems[, 9];
                i = playerchelems[, 10];
                paste("<b>Nom:</b>", get_playername())
            })

            # Outputs player nationality
            output$nationalite_text <- renderText({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())
                h = playerchelems[, 9];
                i = playerchelems[, 10];
                paste("<b>Nationalité:</b>", h)
            })

            # Outputs player age
            output$age_text <- renderText({
                playerchelems <- subset(chelemsCSV, Nom == get_playername())
                h = playerchelems[, 9];
                i = playerchelems[, 10];
                paste("<b>Age:</b>", i, "years")
            })
        }
    )
}