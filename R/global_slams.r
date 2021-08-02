# Creates an win history bar evolving through time for every players
global_slams_ui <- function(id) {
    ns <- NS(id)

    robservableOutput(ns("chart"), height = NULL)
}

global_slams_server <- function(id) {

    # Store slams CSV
    slamsCSV <- csv_select("ereopen")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            output$chart <- renderRobservable({
                robservable(
                    "https://observablehq.com/@juba/bar-chart-race",
                    include = c("viewof date", "chart", "draw", "styles"),
                    hide = "draw",
                    input = list(
                    data = slamsCSV,
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
        }
    )
}