library(shiny)
library(wehoop)
library(hoopR)
library(tidyverse)
library(bslib)
library(lubridate)
########################################################################
#NBA Tab
nba_tab<- nav_panel(
  title = "NBA",
  layout_sidebar(
    title = "Conditional sidebar",
    id = "nba_main_view",
    sidebar = sidebar(
      width=250,
      navset_pill_list(
        id="nba_page_selection",
        well = FALSE,
        nav_panel(title = "Overview",value = "nba_overview_val"),
        nav_panel(title = "Stats Hub",value = "nba_stats_hub_val"),
        nav_panel(title = "Other",value = "nba_other_val")
      )
    ),
    navset_hidden(
      id="nba_main_disp",
      nav_panel_hidden(
        value = "nba_overview_val",
        dateInput(inputId = "nba_game_date",label = h3("Date input"),value = Sys.Date()),
        uiOutput("stacked_rows_container")
      ),
      nav_panel_hidden(
        value = "nba_stats_hub_val"
      ),
      nav_panel_hidden(
        value = "nba_other_val"
      )
    )
    
  )
  
)
########################################################################
#WNBA Tab
wnba_tab<-nav_panel(
  title = "WNBA",
  card("This is where your tables go.")
)
########################################################################
#UI Function Call
ui <- shinyUI(
  page_navbar(
    title = "Basketball Overview",
    nba_tab,
    wnba_tab
  )
)
########################################################################
#Server Function Call
server <- function(input, output, session) {
# NBA Tab Selection Logic
  observeEvent(input$nba_page_selection, {
    nav_select("nba_main_disp", input$nba_page_selection)
  })
# NBA Game output Logic 
  nba_schedule_game_selections<-reactive({
    seasons_selected<-c(as.numeric(substr(input$nba_game_date,1,4)),as.numeric(substr(input$nba_game_date,1,4))-1)
    load_nba_schedule(seasons = seasons_selected)%>%
      filter(grepl(input$nba_game_date,date))
  })%>%bindEvent(input$nba_game_date)
  
  output$stacked_rows_container <- renderUI({
    nba_schedule_game_selections()%>%
      nrow()->nba_game_count
    
    #Creates a card for each game
    nba_game_list <- lapply(seq_len(nba_game_count), function(i) {
      card(
        card_header(paste("Game #", i)),
        p("Game output"),
        
      )
    })
    do.call(
      layout_column_wrap,
      c(nba_game_list,width = 1,gap = "15px")
    )
  })
}

shinyApp(ui, server)%>%print()


