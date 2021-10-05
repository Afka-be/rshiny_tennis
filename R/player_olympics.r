#' Create a panel
#' Gives Olympic's games medals stats for selected player
player_olympics_ui <- function(id) {
    ns <- NS(id)
    column(8,
        class = "playerstats_tabset",
        box(
            title = "Olympic Medals", 
            status = "warning",
            collapsible = FALSE,
            solidHeader = TRUE,
            leafletOutput(ns("mymap"))
        ) #box
    ) #column
}

player_olympics_server <- function(id) {

    # Get the player name from the dropdown (eventReactive)
    get_playername <- player_select_server("select")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            output$mymap <- renderLeaflet({
  
                jo <- xlsx_select("olympics", get_playername())
                attach(jo)

                josimple <- jo %>% filter(Simple!=0)
                jodouble <- jo %>% filter(Double!=0)
                
                medalIcons <- iconList(
                    gold = makeIcon("www/images/gold.png", "gold.png", 24, 30),
                    silver = makeIcon("www/images/silver.png", "silver.png", 24, 30),
                    bronze = makeIcon("www/images/bronze.png", "bronze.png", 24, 30)
                )

                leaflet(josimple) %>% addTiles() %>%
                addMarkers(~Long, ~Lat, label = ~htmlEscape(paste(City, Year,":", Simple_en,  "medal in simple")),icon = ~medalIcons[Simple])  %>%
                addMarkers(data=jodouble,~Long, ~Lat, label = ~htmlEscape(paste(City, Year,":",Double_en, "medal in double")),icon = ~medalIcons[Double]) %>%
                addProviderTiles("CartoDB.DarkMatter") %>%
                setView(lng = 2.80, lat = 46.80, zoom = 2)
            })
        }
    )
}