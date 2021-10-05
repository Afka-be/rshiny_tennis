######## HELPERS ########

#' Scale maths according to win numbers
annee_minimale <- function(tab) {
    attach(tab)
    tag = 0;
    for (i in 1:length(Annee)) {
        for (j in 2:length(tab[1, ])) {
            if (tab[i, j] == 1) {
                annee_min = Annee[i];
                tag = 1
                break;
            }
        }
        if (tag == 1)  {
            break;
        }
    }
    return(annee_min)
}

######## SHINY MODULE ########

#' Create a panel with tabs
#' Gives Grand Slams competitions stats for selected player
player_slams_ui <- function(id) {
    ns <- NS(id)
    column(8,
        class = "playerstats_tabset",
        tabBox(title = "Grandslams", collapsible = FALSE,
            tabs = list(
                list(menu = "All", content = plotOutput(ns("Plot_All_Slams"))),
                list(menu = "Australian Open", content = plotOutput(ns("Plot_Australian_Open"))),
                list(menu = "Roland Garros", content = plotOutput(ns("Plot_Roland_Garros"))),
                list(menu = "Wimbeldon", content = plotOutput(ns("Plot_Wimbeldon"))),
                list(menu = "US Open", content = plotOutput(ns("Plot_US_Open")))
            ) #list
        ) #tabset
    ) #column
}

player_slams_server <- function(id) {

    # Get the player name from the dropdown (eventReactive)
    get_playername <- player_select_server("select")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            get_slamstats <- reactive({
                Grandslams <- xlsx_select("grandslams_total", get_playername())
                #Append values to the global list parameters for markdown report
                player_params <<- append(player_params, list(slamstats = Grandslams))

                return(Grandslams)
            })

            #' Generate specific GRANDSLAM chart
            #' @param slam Grandslam's name (title)
            Plot_Grand_Slams <- function(slam) {

                # Select xlsx and store it.
                Grandslams <- xlsx_select("grandslams_unit", get_playername())

                attach(Grandslams);
                n_max <- max(Australian_Open,Roland_Garros,Wimbeldon,US_Open);
                annee_min <- annee_minimale(Grandslams);
                name <- sub("_", " ", deparse(substitute(slam)));

                ggplot(data= Grandslams)+
                geom_bar(stat="identity", aes(x=Annee, y=slam, fill=slam), show.legend = FALSE)+
                geom_text(stat="identity", aes(x=Annee, y=slam , label=slam), vjust=-0.5, size=4, face="bold", color="white")+
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
            Plot_All_Slams <- function() {

                # Select xlsx and store it.
                Grandslams <- get_slamstats()

                attach(Grandslams);
                annee_min <- annee_minimale(Grandslams);

                ggplot(data= Grandslams, aes(x=Annee, y=Value, fill=Type))+
                geom_bar(stat="identity", position="stack", show.legend = TRUE)+
                geom_text(stat="identity", aes(x=Annee, y=Sum , label=Sum), vjust=-0.5, size=4, face="bold", color="white" )+
                ggtitle(paste("Evolution du nombre de grand chelems au cours du temps"))+
                ylab("Nombre de grands chelems")+
                xlab("Annee")+
                scale_x_continuous(breaks=seq(annee_min,2021,1))+
                coord_cartesian(xlim=c(annee_min,2021))+
                scale_y_continuous(breaks=seq(0,20,1), limits = c(0, 20))+
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

            output$Plot_All_Slams <- renderPlot({
                Plot_All_Slams()
            })

            output$Plot_Australian_Open <- renderPlot({
                Plot_Grand_Slams(Australian_Open)
            })

            output$Plot_Roland_Garros <- renderPlot({
                Plot_Grand_Slams(Roland_Garros)
            })

            output$Plot_Wimbeldon <- renderPlot({
                Plot_Grand_Slams(Wimbeldon)
            })

            output$Plot_US_Open <- renderPlot({
                Plot_Grand_Slams(US_Open)
            })
        }
    )
}