#' Create a panel
#' Gives DavisCups stats for selected player
player_daviscups_ui <- function(id) {
    ns <- NS(id)
    column(8,
        class = "playerstats_tabset",
        box(
            title = "Davis Cups",
            status = "warning",
            collapsible = FALSE,
            solidHeader = TRUE,
            plotOutput(ns("Plot_All_Davis"))
        ) #box
    ) #column
}

player_daviscups_server <- function(id) {

    # Get the player name from the dropdown (eventReactive)
    get_playername <- player_select_server("select")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            output$Plot_All_Davis <- renderPlot({
  
                data_davis <- xlsx_select("daviscups_total", get_playername())
                attach(data_davis);
                annee_min <- annee_minimale(data_davis);
                ggplot(data= data_davis)+
                geom_bar(stat="identity", aes(x=Annee, y=Davis, fill=Davis), show.legend = FALSE)+
                geom_text(stat="identity", aes(x=Annee, y=Davis , label=Davis), vjust=-0.5, size=4, face="bold", color="white")+
                ggtitle(paste("Evolution du nombre de Davis Cups au cours du temps"))+
                ylab("Number of Davis Cups")+
                xlab("Annee")+  
                scale_x_continuous(breaks=seq(annee_min,2021,1))+
                coord_cartesian(xlim=c(annee_min,2021))+
                scale_y_continuous(breaks=seq(0,5,1), limits = c(0, 5))+
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
            })
        }
    )
}