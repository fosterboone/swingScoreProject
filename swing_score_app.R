library(shiny)
library(wehoop)
library(hoopR)
library(tidyverse)
library(bslib)
library(lubridate)
library(dqshiny)

########################################################################
#NBA Tab----
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
        layout_column_wrap(
          width=1/2,
          card(
            autocomplete_input(
              id="nba_player_search",
              label = "Search a Player",
              options = NULL,
              max_options = 10
            ),
            class = "mx-auto my-3"
          ),
          card(
            selectInput(
              inputId = "nba_player_search_season_select",
              label = "Select a Season",
              choices = rev(seq(2002, most_recent_nba_season())),
              selected = most_recent_nba_season()
            ),
            class = "mx-auto my-3"
          )
          
        ),
        card_body(
          uiOutput("nba_player_stats_ui")
        )
        
      ),
      nav_panel_hidden(
        value = "nba_other_val"
      )
    )
    
  )
  
)
########################################################################
#WNBA Tab----
wnba_tab<-nav_panel(
  title = "WNBA",
  card("WNBA tab info")
)
########################################################################
#UI Function Call----
ui <- shinyUI(
  page_navbar(
    title = "Basketball Overview",
    nba_tab,
    wnba_tab
  )
)
########################################################################
#Server Function Call----
server <- function(input, output, session) {
  
# Tab Selection (Server)----
# NBA Tab Selection Logic
  observeEvent(input$nba_page_selection, {
    nav_select("nba_main_disp", input$nba_page_selection)
  })

# Player Search (Server)----

  nba_player_search_season_data <- eventReactive(input$nba_player_search_season_select, {
    req(input$nba_player_search_season_select)#requires a season input
    load_nba_player_box(seasons = as.numeric(input$nba_player_search_season_select))%>%
      rename(player = "athlete_display_name",
             pts = "points",
             fgm = "field_goals_made", fga = "field_goals_attempted",
             fg3m = "three_point_field_goals_made",
             ftm = "free_throws_made", fta = "free_throws_attempted", 
             oreb = "offensive_rebounds", dreb = "defensive_rebounds",
             ast = "assists", stl = "steals", blk = "blocks",
             pf = "fouls", tov = "turnovers")%>%
      nba_add_advanced_metrics()
  })#When the nba season dropdown menu input changes, use it to filter the box 
  #score data based on the selected season
  
  observe({
    req(nba_player_search_season_data())#requires the filtered season data to exist
    update_autocomplete_input(
      session = session,
      id = "nba_player_search",
      options = nba_player_search_season_data() %>%
        distinct(player) %>%
        arrange(player) %>%
        pull(player)
    )
  })#Uses the season filtered box score data to populate plaeyer selection options
  
  nba_selected_player_data <- reactive({
    req(input$nba_player_search) # requires a player search input value
    nba_player_search_season_data() %>%
      filter(player == input$nba_player_search)
  })#filters the season data based on player name
  
  output$nba_player_stats_ui<-renderUI({
    req(nrow(nba_selected_player_data()) > 0)#Waits to make sure player is selected
    page_fluid(
      layout_column_wrap(
        style = paste0("border-radius: 8px; border: 4px solid #f0f4f8;
                     background: linear-gradient(135deg, #",
                       nba_selected_player_data()$team_color[1],
                       " 49.9%, #ffffff 50%, #ffffff 50.5%, #",
                       nba_selected_player_data()$team_alternate_color[1],
                       " 50.6%);"),#Creates split line coloring with team colors
        width = 1/4,
        card_image(file=nba_selected_player_data()$athlete_headshot_href[1]),
        card(
          class = "text-white",
          style = "background: transparent; border: none; box-shadow: none;",
          card_body(
            style = "background: transparent;",
            p(strong(nba_selected_player_data()$player[1]),
              style = "font-family: Tahoma; font-size: 25px"),
            p(strong(paste0("#",nba_selected_player_data()$athlete_jersey[1])),
              style = "font-family: Tahoma; font-size: 15px")
          )
        ),
      )
    )
  })#Creates a UI based on the selected player and season

#Schedule Output (Server)----
  # NBA Game output Logic 
  nba_schedule_game_selections<-reactive({
    if (as.numeric(substr(input$nba_game_date,1,4))==most_recent_mbb_season()){
      #If the current season is selected only include current year
      seasons_selected<-c(as.numeric(substr(input$nba_game_date,1,4)))
      
    }
    else{
      #Include the range of seasons
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


shinyApp(ui, server)


