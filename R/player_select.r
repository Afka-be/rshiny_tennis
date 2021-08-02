# Create a dropdown to select a player
# Make it eventReactive with the action button
player_select_ui <- function(id) {
    ns <- NS(id)

    #Define choices for players
    var_choices <- list(
        "Roger Federer",
        "Rafael Nadal",
        "Novak Djokovic"
    )
    box(
        width = 16,
        h1("Choose a player"),
        div(
        class = "choose_player_window",
        selectInput(
            inputId = ns("nom"),
            label = "Choose a name",
            width = 450,
            choices = var_choices,
            multiple = FALSE),
        actionButton(inputId = ns("clicks"),
                        label = "Update")
        ), #div
    ) #box
}

# Stores a player via dropdown 
# Make it eventReactive with the "update" button
player_select_server <- function(id) {
    moduleServer(
        id = id,
        module = function(input, output, session) {
            # Wait for the actionButton click to update data
            # Most of the player data are dependant on this value
            playername  <- eventReactive(input$clicks, {
                return(input$nom)
            })
        }
    )
}