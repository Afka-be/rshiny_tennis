#' Create a panel
#' Gives Weeks nr1 stats for selected player
player_weeksnr1_ui <- function(id) {
    ns <- NS(id)
    column(8,
        class = "playerstats_tabset",
        box(
            title = "Weeks nr 1 World",
            status = "warning",
            collapsible = FALSE,
            solidHeader = TRUE,
            plotOutput(ns("Plot_weeks"))
        ) #box
    ) #column
}

player_weeksnr1_server <- function(id) {

    # Get the player name from the dropdown (eventReactive)
    get_playername <- player_select_server("select")

    moduleServer(
        id = id,
        module = function(input, output, session) {
            output$Plot_weeks <- renderPlot({
  
                Weeks <- xlsx_select("weeksnr1", get_playername())
                attach(Weeks); 
                ggplot(data=Weeks, aes(x=Annee, y=Total))+geom_line(linetype = "dashed", color="orange", size=1)+geom_point(color="orange", size=4)+
                geom_text(stat="identity", aes(x=Anneetext, y=Totaltext , label=Totaltext), vjust=-0.5, size=4, face="bold", color="white")+
                ggtitle(paste("Evolution du nombre de semaines nr1 mondial au cours du temps"))+
                ylab("Nombre de semaines")+
                xlab("Temps")+
                scale_x_datetime(breaks = "1 year",labels=date_format("%Y"))+ #
                #   scale_x_discrete(breaks=seq(2003,2021,1))+
                #   coord_cartesian(xlim=c(01-01-03,01-07-21))+
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