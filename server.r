server <- function(input, output, session) {


# Player Tab
player_generatedoc_server("select")
player_card_server("select")
player_stats_server("select")
player_spider_server("select")
player_slams_server("select")
player_masters1000_server("select")
player_olympics_server("select")
player_weeksnr1_server("select")
player_daviscups_server("select")


# Overview Tab
global_slams_server("players")

}