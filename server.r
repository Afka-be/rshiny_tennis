server <- function(input, output, session) {

# Player Tab
player_card_server("select")
player_stats_server("select")
player_spider_server("select")


# Overview Tab
global_slams_server("players")

}