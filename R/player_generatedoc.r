#' Generates Report for the selected player (UI SIDE)
#' UI Side -> Creates the download button
player_generatedoc_ui <- function(id) {
    ns <- NS(id)

    downloadButton(ns("documentGenerator"),
                    "Generate report",
                    class = "button_generate_doc")

}

#' Generates Report for the selected player (SERVER SIDE)
player_generatedoc_server <- function(id) {
    get_playername <- player_select_server("select")

    player_stats_server("select")
    moduleServer(
        id = id,
        module = function(input, output, session) {
            output$documentGenerator <- downloadHandler(
                # For PDF output, change this to "report.pdf"
                filename = "report.html",
                content = function(file) {
                    # Copy the report file to a temporary directory before processing it, in
                    # case we don't have write permissions to the current working dir (which
                    # can happen when deployed).
                    tempReport <- file.path(tempdir(), "report.Rmd")
                        file.copy("report.Rmd", tempReport, overwrite = TRUE)


                    #Because we use a temp directory, we need to bring the assets with us
                    #So we have to include the external player's picture to use it in the markdown
                    #take the playername => lowercase, replace " " with "_" and add .png
                    get_playerpicture <- paste0(tolower(sub(" ", "_", get_playername())), ".png")

                    tempReport2 <- file.path(tempdir(), get_playerpicture)
                        file.copy(paste0("players_pictures/", get_playerpicture), tempReport2, overwrite = TRUE)

                    # Set up parameters to pass to Rmd document
                    # params <- list(
                    #     n = get_playername()
                    # )

                    # Knit the document, passing in the `params` list, and eval it in a
                    # child of the global environment (this isolates the code in the document
                    # from the code in this app).
                    rmarkdown::render(tempReport, output_file = file,
                    params = player_params,
                    envir = new.env(parent = globalenv())
                    )
                }
            )
        }
    )
}