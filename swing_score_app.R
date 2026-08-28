library(shiny)
library(wehoop)
library(hoopR)
library(tidyverse)
library(bslib)
library(lubridate)
library(dqshiny)

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
        nav_panel(title = "Player Search",value = "nba_player_search_val"),
        nav_panel(title = "Other",value = "nba_other_val")
      )
    ),
    navset_hidden(
      id="nba_main_disp",
      nav_panel_hidden(
        value = "nba_overview_val",
        dateInput(inputId = "nba_game_date",label = h3("Date input"),value = Sys.Date()),
        uiOutput("nba_game_list")
      ),
      nav_panel_hidden(
        value = "nba_player_search_val",
        autocomplete_input(
          id="nba_player_search",
          label = "Search a Player",
          options = NULL,
          max_options = 10
        ),
        autocomplete_input(
          id="nba_player_search_season_select",
          label = "Select a Season",
          value=most_recent_nba_season(),
          options = c(2026,2025,2024),
          max_options = 10
        )
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
  card("WNBA tab info")
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
  
  nba_player_search_season_data <- eventReactive(input$nba_player_search_season_select, {
    req(input$nba_player_search_season_select)
    load_nba_player_box(seasons = as.numeric(input$nba_player_search_season_select))
  })
  observe({
    update_autocomplete_input(
      session = session,
      id = "nba_player_search",
      options = nba_player_search_season_data() %>%
        distinct(athlete_display_name) %>%
        arrange(athlete_display_name) %>%
        pull(athlete_display_name)
    )
  })
# NBA Game output Logic 
  nba_schedule_game_selections<-reactive({
    if (as.numeric(substr(input$nba_game_date,1,4))==most_recent_mbb_season()){
      #If the current season is selected only include current year
      seasons_selected<-c(as.numeric(substr(input$nba_game_date,1,4)))
      
    }
    else{
      seasons_selected<-c(as.numeric(substr(input$nba_game_date,1,4)),
                          as.numeric(substr(input$nba_game_date,1,4))+1)
    }
    load_nba_schedule(seasons = seasons_selected)%>%
      filter(grepl(input$nba_game_date,date))
  })%>%bindEvent(input$nba_game_date)
  
  output$nba_game_list <- renderUI({
    nba_schedule_game_selections()->nba_schedule_game_selections_df
    nba_schedule_game_selections_df%>%
      nrow()->nba_game_count
    
    if (nba_game_count==0){
      p("No Games Today")
    }
    else{
      #Creates a card for each game
      nba_game_list <- lapply(seq_len(nba_game_count), function(i) {
        card(
          card_header(paste("Game #", i)),
          card_body(
            layout_column_wrap(
              width = 1/3,
              p(strong(nba_schedule_game_selections_df$home_display_name[i]),
                class = "mx-auto my-3",
                style = "font-size: 30px;"),
              p(),
              p(strong(nba_schedule_game_selections_df$away_display_name[i]),
                class = "mx-auto my-3",
                style = "font-size: 30px;"),
              card_image(file = nba_schedule_game_selections_df$home_logo[i],
                         height = "150px",
                         width = "150px",
                         class = "mx-auto my-3"),
              p("@",
                style = "font-size: 50px;",
                class = "mx-auto my-3"),
              card_image(file = nba_schedule_game_selections_df$away_logo[i],
                         height = "150px",
                         width = "150px",
                         class = "mx-auto my-3"),
              p(nba_schedule_game_selections_df$home_score[i],
                class = "mx-auto my-3",
                style = "font-size: 40px;"),
              p(),
              p(nba_schedule_game_selections_df$away_score[i],
                class = "mx-auto my-3",
                style = "font-size: 40px;")
          ))
        ) 
      })
      do.call(
        layout_column_wrap,
        c(nba_game_list,width = 1,gap = "15px")
      )
    }
  })
}


shinyApp(ui, server)%>%print()

