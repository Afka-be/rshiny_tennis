#' Create a panel with tabs
#' Gives Masters 1000 competitions stats for selected player
player_masters1000_ui <- function(id) {
    ns <- NS(id)

    column(8,
        class = "playerstats_tabset",
        tabBox(title = "Masters 1000", collapsible = FALSE,
            tabs = list(
                list(menu = "All", content = plotOutput(ns("Plot_All_Masters"))),
                list(menu = "Indian Wells", content = plotOutput(ns("Plot_Indian_Wells"))),
                list(menu = "Miami Open", content = plotOutput(ns("Plot_Miami_Open"))),
                list(menu = "Monte-Carlo Masters", content = plotOutput(ns("Plot_Monte_Carlo_Masters"))),
                list(menu = "<< Madrid Open >>", content = plotOutput(ns("Plot_Madrid_Open"))
                    %>% add_prompt(
                        message = "This fourth slot was the Hamburg Masters from 1990 to 2008 and the Madrid Open (clay) from 2009 to now",
                        position = "left", type = "error", 
                        size = "medium", rounded = TRUE
                    )
                ),
                list(menu = "Rome Open", content = plotOutput(ns("Plot_Rome_Open"))),
                list(menu = "Canadian Open", content = plotOutput(ns("Plot_Canadian_Open"))),
                list(menu = "Cincinnati Masters", content = plotOutput(ns("Plot_Cincinnati_Masters"))),
                list(menu = "<< Shanghai Masters >>", content = plotOutput(ns("Plot_Shanghai_Masters"))
                    %>% add_prompt(
                        message = "This eighth  slot was the Stockholm Open from 1990 to 1994, the Eurocard Open from 1995 to 2001, the Madrid_Open (hard) from 2002 to 2008 and the Shanghai Masters from 2009 to now",
                        position = "left", type = "error", 
                        size = "medium", rounded = TRUE
                    )
                ),
                list(menu = "Paris Masters", content = plotOutput(ns("Plot_Paris_Masters")))
            ) #list
        ) #tabset
    ) #column
}

player_masters1000_server <- function(id) {

    # Get the player name from the dropdown (eventReactive)
    get_playername <- player_select_server("select")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            #' Generate specific Masters 1000 chart
            #' @param master Master's 1000 name (title)
            Plot_Masters_1000 <- function(master) {

                # Select xlsx and store it.
                data_Masters_1000 <- xlsx_select("masters1000_unit", get_playername())

                # Hamburg_Masters then Madrid Open (clay)
                # Stockholm_Open Eurocard_Open Madrid_Open (hard) Shanghai Masters
                attach(data_Masters_1000);
                n_max <- max(Indian_Wells,Miami_Open,Monte_Carlo_Masters,Madrid_Open,Rome_Open,Canadian_Open, Cincinnati_Masters,Shanghai_Masters, Paris_Masters,na.rm = TRUE);
                annee_min <- annee_minimale(data_Masters_1000);
                name <- sub("_", " ", deparse(substitute(master)));

                ggplot(data= data_Masters_1000)+
                geom_bar(stat="identity", aes(x=Annee, y=master, fill=master), show.legend = FALSE)+
                geom_text(stat="identity", aes(x=Annee, y=master , label=master), vjust=-0.5, size=4, face="bold", color="white")+
                ggtitle(paste("Evolution du nombre de",name,"au cours du temps"))+
                ylab(name)+
                xlab("Annee")+  
                scale_x_continuous(breaks=seq(annee_min,2021,1))+
                coord_cartesian(xlim=c(annee_min,2021))+
                scale_y_continuous(breaks=seq(0,n_max,1), limits = c(0, n_max))+
                scale_fill_gradient(low="blue", high="red")+
                theme_classic()+
                theme(
                plot.background = element_rect(fill = "#0d1524", color = "#0d1524", size = 0),
                panel.background = element_rect(fill = "#0d1524", color = "#0d1524", size = 0),
                panel.border = element_blank(),
                plot.title = element_text(face="bold.italic", color="white", size=16)  , #family=... pour changer le font
                axis.title.x = element_text(color="white", size=12, face="bold"),
                axis.title.y = element_text(color="white", size=12, face="bold"),
                axis.text.x = element_text(face="bold", color="white", size=12, angle=0))
            }

            #' Generate Global GRANDSLAMS chart
            Plot_All_Masters <- function() {

                # Select xlsx and store it.
                Matstersstack <- xlsx_select("masters1000_total", get_playername())

                attach(Matstersstack); 
                annee_min <- annee_minimale(Matstersstack);

                ggplot(data=Matstersstack, aes(x=Annee, y=Value, fill=Type))+
                geom_bar(stat="identity", position="stack", show.legend = TRUE)+
                geom_text(stat="identity", aes(x=Annee, y=Sum , label=Sum), vjust=-0.5, size=4, face="bold", color="white")+
                ggtitle(paste("Evolution du nombre de Masters 1000 au cours du temps"))+
                ylab("Nombre de Masters 1000")+
                xlab("Annee")+
                scale_x_continuous(breaks=seq(annee_min,2021,1))+
                coord_cartesian(xlim=c(annee_min,2021))+
                scale_y_continuous(breaks=seq(0,36,1), limits = c(0, 36))+
                theme_classic()+
                theme(
                plot.background = element_rect(fill = "#0d1524", color = "#0d1524", size = 0),
                panel.background = element_rect(fill = "#0d1524", color = "#0d1524", size = 0),
                panel.border = element_blank(),
                plot.title = element_text(face="bold.italic", color="white", size=16)  , #family=... pour changer le font
                axis.title.x = element_text(color="white", size=12, face="bold"),
                axis.title.y = element_text(color="white", size=12, face="bold"),
                axis.text.x = element_text(face="bold", color="white", size=12, angle=0))
            }

            output$Plot_All_Masters <- renderPlot({
                Plot_All_Masters()
            })

            output$Plot_Indian_Wells <- renderPlot({
                Plot_Masters_1000(Indian_Wells)
            })

            output$Plot_Miami_Open <- renderPlot({
                Plot_Masters_1000(Miami_Open)
            })

            output$Plot_Monte_Carlo_Masters <- renderPlot({
                Plot_Masters_1000(Monte_Carlo_Masters)
            })

            output$Plot_Madrid_Open <- renderPlot({
                Plot_Masters_1000(Madrid_Open)
            })

            output$Plot_Rome_Open <- renderPlot({
                Plot_Masters_1000(Rome_Open)
            })

            output$Plot_Canadian_Open <- renderPlot({
                Plot_Masters_1000(Canadian_Open)
            })

            output$Plot_Cincinnati_Masters <- renderPlot({
                Plot_Masters_1000(Cincinnati_Masters)
            })

            output$Plot_Shanghai_Masters <- renderPlot({
                Plot_Masters_1000(Shanghai_Masters)
            })

            output$Plot_Paris_Masters <- renderPlot({
                Plot_Masters_1000(Paris_Masters)
            })
        }
    )
}