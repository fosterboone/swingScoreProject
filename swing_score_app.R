library(shiny)
library(wehoop)
library(hoopR)
library(tidyverse)
library(bslib)
library(lubridate)
library(dqshiny)
library(DT)
library(ggimage)

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
    header = tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Bebas+Neue&family=Rajdhani:wght@500;700&display=swap"
      )
    ),
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
  
  nba_selected_player_pbp<-eventReactive(input$nba_player_search_season_select,{
    load_nba_pbp(seasons = input$nba_player_search_season_select)%>%
      select(shooting_play,scoring_play,coordinate_x,coordinate_y,athlete_name_1)%>%
      filter(shooting_play,
             athlete_name_1=="Tre Johnson")
  })
  output$nba_player_stats_ui <- renderUI({
    req(nrow(nba_selected_player_data()) > 0) # Waits to make sure player is selected
    
    page_fluid(
      fluidRow(
        style = paste0("border-radius: 8px; border: 4px solid #f0f4f8;
                     background: linear-gradient(135deg, #",
                       nba_selected_player_data()$team_color[1],
                       " 49.9%, #f0f4f8 50%, #f0f4f8 50.5%, #",
                       "ffffff",
                       " 50.6%);"), # Creates split line coloring with team colors
        column(
          width = 3,
          card_image(file = nba_selected_player_data()$athlete_headshot_href[1],
                     height = "220px")#Show player headshot
        ),
        column(
          width = 3,
          card(
            class = "text-white",
            style = "background: transparent; border: none; box-shadow: none;",
            card_body(
              style = "background: transparent;",
              p(strong(nba_selected_player_data()$player[1]),
                style="font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 35px;"),
              p(strong(paste0(nba_selected_player_data()$team_display_name[1],
                              " | #", nba_selected_player_data()$athlete_jersey[1])),
                style="font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 23px;")
            )
          )
        ),#Shows the player's name and jersey number
        column(width = 1),#Empty spacing column
        column(
          width=5,
          fluidRow(
            column(
              width = 4,
              class = "d-flex flex-column justify-content-center align-items-center",
              card(
                style = "background: transparent; border: none; box-shadow: none;",
                card_body(
                  style = "background: transparent;",
                  p(""),
                  p(""),
                  p("PPG",style="font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 35px; letter-spacing: 1px;"),
                  p(nba_selected_player_data()%>%
                             filter(!is.na(pts))%>%
                             summarise(avg=round(mean(pts),2))%>%
                             pull(avg),
                    style="font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 25px; letter-spacing: 1px;",
                    class = "text-center")
                )
              )
            ),#Shows Points per game 
            column(
              width = 4,
              class = "d-flex flex-column justify-content-center align-items-center",
              card(
                style = "background: transparent; border: none; box-shadow: none;",
                card_body(
                  style = "background: transparent;",
                  p(""),
                  p(""),
                  p("RPG",style="font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 35px; letter-spacing: 1px;"),
                  p(nba_selected_player_data()%>%
                             filter(!is.na(oreb),
                                    !is.na(dreb))%>%
                             summarise(avg=round(mean(oreb+dreb),digits = 2))%>%
                             pull(avg),
                    style="font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 25px; letter-spacing: 1px;",
                    class = "text-center")
                )
              )
            ),#Shows Rebounds per game
            column(
              width = 4, 
              class = "d-flex flex-column justify-content-center align-items-center",
              card(
                style = "background: transparent; border: none; box-shadow: none;",
                card_body(
                  style = "background: transparent;",
                  p(""),
                  p(""),
                  p("APG",style="font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 35px; letter-spacing: 1px;"),
                  p(mean(nba_selected_player_data()%>%
                                  filter(!is.na(ast))%>%
                                  summarise(avg=round(mean(ast),2))%>%
                                  pull(avg)
                                ),
                    style="font-family: 'Rajdhani', sans-serif; font-weight: 700; font-size: 25px; letter-spacing: 1px;",
                    class = "text-center")
                )
              )
            )#Shows Assists per game
          )
        )
      ),
      navset_tab(
        nav_panel("Box Score",
                  DT::renderDataTable(nba_selected_player_data()%>%
                                    select(c(39,3,4,12:29,46:48,52,57:63))%>%
                                    rename(`Team Name`="team_display_name",
                                           `Season Type`="season_type",
                                           `Date`="game_date",
                                           `Minutes`="minutes",
                                           `Field Goals Made`="fgm",
                                           `Field Goals Attempted`="fga",
                                           `3P Field Goals Made`="fg3m",
                                           `3P Field Goals Attempted`="three_point_field_goals_attempted",
                                           `Free Throws Made`="ftm",
                                           `Free Throws Attempted`="fta",
                                           `Offensive Rebounds`="oreb",
                                           `Defensive Rebounds`="dreb",
                                           `Rebounds`="rebounds",
                                           `Assists`="ast",
                                           `Steals`="stl",
                                           `Blocks`="blk",
                                           `Turnovers`="tov",
                                           `Personal Fouls`="pf",
                                           `Plus Minus`="plus_minus",
                                           `Points`="pts",
                                    )%>%
                                      datatable(class = 'stripe hover compact cell-border')
                                    )),
        nav_panel("Shot Chart"
                  ),
        nav_panel("title 3")
      )
    )
  })

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

load_nba_pbp(seasons=2026)%>%
  select(shooting_play,scoring_play,coordinate_x,coordinate_y,athlete_name_1)%>%
  filter(shooting_play,
         athlete_name_1=="Tre Johnson")%>%
  ggplot(aes(abs(coordinate_x),coordinate_y))+
  geom_point(aes(colour = scoring_play))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )->x


  ggbackground(x,background = "shot_chart.png")


