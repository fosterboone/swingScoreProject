library(shiny)
library(wehoop)
library(hoopR)
library(tidyverse)
library(bslib)
library(lubridate)

#NBA Tab
nba_tab<- nav_panel(
  title = "NBA",
  layout_sidebar(
    sidebar = sidebar(
      nav_panel(
        title = "Overview", 
        
      ),
      nav_panel(
        title = "Stats Hub", 
        card(
          card_header("Main Data Stream Configuration"),
          p("Set your bearer tokens and endpoints inside this workspace container.")
        )
      ),
    ),
    uiOutput("stacked_rows_container")
  )
)
#WNBA Tab
wnba_tab<-nav_panel(
  title = "WNBA",
  card("This is where your tables go.")
)

ui <- shinyUI(
  page_navbar(
    title = "Basketball Overview",
    nba_tab,
    wnba_tab
  )
)

server <- function(input, output, session) {
  
  output$stacked_rows_container <- renderUI({
    load_nba_schedule(seasons = most_recent_nba_season())%>%
      filter(grepl("2025-12-31",date))%>%
      nrow()->nba_game_count
    
    # 1. Create a list of full-width rows (cards) using a loop
    nba_game_list <- lapply(seq_len(nba_game_count), function(i) {
      card(
        card_header(paste("Row Layer #", i)),
        p("This row extends across the full container width. It will stack cleanly on mobile or desktop."),
        # Add a custom height rule to each row card
        height = "140px" 
      )
    })
    
    # 2. Use layout_column_wrap to force a full-width vertical stack
    # width = 1 means each item takes up 100% of the horizontal space (1 fraction)
    do.call(
      layout_column_wrap,
      c(
        nba_game_list,
        width = 1,          # Force full-width for every single item
        gap = "15px"         # Clean spacing between the rows
      )
    )
  })
  
  

}

shinyApp(ui, server)%>%print()

load_nba_schedule(seasons = most_recent_nba_season())%>%
  filter(grepl("2025-12-31",date))%>%
  nrow()

