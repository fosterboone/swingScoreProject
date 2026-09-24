library(shiny)
library(wehoop)
library(hoopR)
library(tidyverse)
library(bslib)
library(lubridate)
library(dqshiny)
library(DT)
library(ggimage)
library(ggpubr)
library(shinycssloaders)

########################################################################


stat_card <- function(label, value) {
  column(
    width = 4,
    class = "d-flex flex-column justify-content-center align-items-center",
    card(
      style = "background: transparent; border: none; box-shadow: none;",
      card_body(
        style = "background: transparent;",
        p(""),
        p(""),
        p(label, style = paste0("font-family: 'Rajdhani', sans-serif; font-weight: 700; letter-spacing: 1px;", "font-size: 35px;")),
        p(value, style = paste0("font-family: 'Rajdhani', sans-serif; font-weight: 700; letter-spacing: 1px;", "font-size: 25px;"), class = "text-center")
      )
    )
  )
}

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
  
  nba_player_search_season_pbp<-eventReactive(input$nba_player_search_season_select,{
    load_nba_pbp(seasons = as.integer(input$nba_player_search_season_select))
  })#Uses selected season to filter play by play data
  
  nba_selected_player_pbp<-reactive({
    req(input$nba_player_search)
    nba_player_search_season_pbp()%>%
      filter(shooting_play,
             athlete_name_1==input$nba_player_search)
  })#Uses selected player to filter play by play data
  
  nba_data_player_search_season<-eventReactive(input$nba_player_search_season_select,{
    nba_playergamelogs(
      season = paste0(as.numeric(input$nba_player_search_season_select)-1,
                      "-",
                      as.numeric(input$nba_player_search_season_select) %% 100),player_id = "")
  })
  nba_data_selected_player_shot_data <- reactive({
    req(input$nba_player_search)
    req(nba_data_player_search_season())
    
    season_input <- input$nba_player_search_season_select
    
    player_teams <- nba_data_player_search_season()$PlayerGameLogs %>%
      mutate(PLAYER_NAME_CLEAN = stringi::stri_trans_general(PLAYER_NAME, "Latin-ASCII")) %>%
      filter(PLAYER_NAME_CLEAN == input$nba_player_search) %>%
      distinct(TEAM_ID, PLAYER_ID)
    
    req(nrow(player_teams) > 0)
    
    chosen_season <- paste0(as.numeric(season_input) - 1, "-", as.numeric(season_input) %% 100)
    
    shot_data <- lapply(seq_len(nrow(player_teams)), function(i) {
      nba_shotchartdetail(
        player_id = player_teams$PLAYER_ID[i],
        team_id   = player_teams$TEAM_ID[i],
        season    = chosen_season
      )$Shot_Chart_Detail
    }) %>%
      bind_rows()
    
    shot_data
  }) %>%
    bindEvent(input$nba_player_search, input$nba_player_search_season_select)
  # Uses NBA gamelog to get shot chart for selected player
  
  # ---- Player stats UI 
  output$nba_player_stats_ui <- renderUI({
    d <- nba_selected_player_data()
    req(nrow(d) > 0) # Waits to make sure player is selected
    
    page_fluid(
      fluidRow(
        style = paste0("border-radius: 8px; border: 4px solid #f0f4f8;
                     background: linear-gradient(135deg, #",
                       d$team_color[1],
                       " 49.9%, #f0f4f8 50%, #f0f4f8 50.5%, #",
                       "ffffff",
                       " 50.6%);"), # Creates split line coloring with team colors
        column(
          width = 3,
          card_image(file = d$athlete_headshot_href[1],
                     height = "220px")#Show player headshot
        ),
        column(
          width = 3,
          card(
            class = "text-white",
            style = "background: transparent; border: none; box-shadow: none;",
            card_body(
              style = "background: transparent;",
              p(strong(d$player[1]),
                style = paste0("font-family: 'Rajdhani', sans-serif; font-weight: 700; letter-spacing: 1px;", "font-size: 35px;")),
              p(strong(paste0(d$team_display_name[1], " | #", d$athlete_jersey[1])),
                style = paste0("font-family: 'Rajdhani', sans-serif; font-weight: 700; letter-spacing: 1px;", "font-size: 23px;"))
            )
          )
        ),#Shows the player's name and jersey number
        column(width = 1),#Empty spacing column
        column(
          width = 5,
          fluidRow(
            stat_card("PPG", round(mean(d$pts, na.rm = TRUE), 2)),
            stat_card("RPG", round(mean(d$oreb + d$dreb, na.rm = TRUE), 2)),
            stat_card("APG", round(mean(d$ast, na.rm = TRUE), 2))
          )
        )
      ),
      fluidRow(
        style = "border-radius: 8px; border: 4px solid #f0f4f8;",
        column(
          width = 2,
          card(
            #Drop Down to filter chart
          )
        ),
        column(
          width = 10,
          card(
            # CHANGED: placeholder + spinner. The plot itself is rendered below.
            withSpinner(plotOutput("nba_shot_chart"), type = 6, color = "#6c757d")
          )
        ),
        column(
          width = 12,
          card(
            h3(strong("LAST 5 GAMES")),
            # CHANGED: placeholder + spinner. The table is rendered below.
            withSpinner(DT::DTOutput("nba_boxscore"), type = 6, color = "#6c757d")
          )
        )
      )
    )
  })
  
  # ---- Shot chart ----
  output$nba_shot_chart <- renderPlot({
    shots <- nba_data_selected_player_shot_data()
    req(nrow(shots) > 0)
    
    shots %>%
      mutate(LOC_X = as.numeric(LOC_X),
             LOC_Y = as.numeric(LOC_Y)) %>%
      ggplot(aes(x = LOC_X, y = LOC_Y))+
      #Outerbox
      geom_rect(xmin = -250,xmax = 250, ymin=-50,ymax = 420, 
                color = "black", linewidth = 0.8,fill = NA)+
      #Paint
      geom_rect(xmin=-80,xmax =80, ymin= -50, ymax= 140, 
                color = "black", linewidth = 0.8,fill = NA)+
      #3pt line side
      geom_segment(x= 220,xend= 220, y = -50, yend = 89.48, color = "black")+
      geom_segment(x = -220,xend= -220,y =-50, yend = 89.48, color = "black")+
      #3pt line curved
      geom_function(
        fun = function(x) { sqrt(237.5^2 - x^2) },
        xlim = c(-220, 220),
        color = "black"
      )+
      geom_point(aes(color = factor(SHOT_MADE_FLAG)), alpha = 0.5, size = 2) +
      scale_y_continuous(limits = c(420, -50))+ 
      scale_x_continuous(limits = c(-250, 250))+
      scale_color_manual(values = c("0" = "#C93636", "1" = "#609E3F"))+
      coord_fixed()+
      theme_void()+
      theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
      labs(title = paste0(nba_selected_player_data()$player[1]," Shot Chart"))
  })
  
  # ---- Box score ----
  output$nba_boxscore <- DT::renderDataTable({
    req(nrow(nba_selected_player_data()) > 0)
    
    nba_selected_player_data() %>%
      head(5) %>%
      select(team_display_name, season_type, game_date, minutes,
             fgm, fga, fg3m, three_point_field_goals_attempted,
             ftm, fta, oreb, dreb, rebounds, ast, stl, blk, tov, pf,
             plus_minus, pts, starter, home_away, team_winner,
             team_score, opponent_team_display_name, opponent_team_score,
             ts_pct, efg_pct, ft_rate, tov_pct, ast_to, game_score) %>%
      rename(`Team`="team_display_name",
             `Season`="season_type",
             `Date`="game_date",
             `Minutes`="minutes",
             `FGM`="fgm",
             `FGA`="fga",
             `3PFGM`="fg3m",
             `3PFGA`="three_point_field_goals_attempted",
             `FTM`="ftm",
             `FTA`="fta",
             `ORB`="oreb",
             `DRB`="dreb",
             `REB`="rebounds",
             `AST`="ast",
             `STL`="stl",
             `BLK`="blk",
             `TOV`="tov",
             `PF`="pf",
             `+/-`="plus_minus",
             `PTS`="pts",
             `START`="starter",
             `H/A`="home_away",
             `WON`="team_winner",
             `TM SCR`="team_score",
             `OPP TEAM`="opponent_team_display_name",
             `OPP SCR`="opponent_team_score",
             `TS%`="ts_pct",
             `eFG%`="efg_pct",
             `FT RT`="ft_rate",
             `TOV%`="tov_pct",
             `AST/TOV`="ast_to",
             `GMSC`="game_score") %>%
      mutate(`TS%`=round(`TS%`*100,2),
             `eFG%`=round(`eFG%`*100,2),
             `FT RT`=round(`FT RT`*100,2),
             `TOV%`=round(`TOV%`,2),
             `GMSC`=round(`GMSC`,2),
             `AST/TOV`=round(`AST/TOV`,2)) %>%
      datatable(class = 'stripe hover compact cell-border',
                options = list(dom = 't'),
                rownames = FALSE)
  })
  
  #Schedule Output (Server)----
  # NBA Game output Logic 
  nba_schedule_game_selections<-reactive({
    # CHANGED: was most_recent_mbb_season(); this is the NBA tab
    if (as.numeric(substr(input$nba_game_date,1,4))==most_recent_nba_season()){
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
