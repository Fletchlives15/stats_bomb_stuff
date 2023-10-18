pacman::p_load(shiny, tidytable, data.table, lubridate, runner, roll, ggplot2, lubridate, stringr,
               thematic, shinythemes, htmlwidgets, RCurl, httr, rsconnect, shinydashboard, rsconnect, 
               shinymanager)

events_data <- fread("2023_2024_bundesliga.csv")

#################
################# ui set up
#################

header <- dashboardHeader(title = "Bundesliga Shiny",
                          tags$li(a(img(src = "bundesliga-logo.png", height = "30px"), 
                                    style = "padding-top:10px; padding-bottom:10px;"), 
                                  class = "dropdown"))

# need to add menu item for tab to show
sidebar <- dashboardSidebar(width = 230, 
                            sidebarMenu(
                              id = 'sidebar', 
                              style = "position: relative;
                              overflow: visible;", 
                              menuItem("Player Heatmaps", tabName = "play_heatmaps")))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "play_heatmaps", 
            tags$style(".selectize-dropdown-content{
                     text-size = 50px;
                     background-color: #FFFFFF;}
                     
                     /* logo */
        .skin-blue .main-header .logo {
        background-color: #000000;
        
        }
        
        .skin-blue .main-header .logo:hover {
          background-color: #000000;
        
        }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
        background-color: #D3010C;
        }   
        
        /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #000000;
                                }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #000000;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #000000;
                              }"
            ),
        selectizeInput(inputId = "select_player", 
                       label = "Player Name:",
                       choices = sort(unique(events_data$player_name_id))),  
        selectizeInput(inputId = "metric", 
                       label = "Stat:", 
                       choices = sort(unique(events_data$type_name))),
        br(),
        plotOutput("heatmaps")
    )
  ))


ui <- dashboardPage(header, sidebar, body)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  player_react <- reactive({
    
    events_data %>% 
      filter(player_name_id %in% input$select_player)
    
  })
  
  
  metric_react <- reactive({
    
    player_react() %>% 
      mutate(location_x = case_when(location_x > 120 ~ 120, 
                                    location_x < 0 ~ 0,
                                    TRUE ~ location_x), 
             location_y = case_when(location_y > 80 ~ 80, 
                                    location_y < 0 ~ 0, 
                                    TRUE ~ location_y)) %>% 
      filter(type_name %in% input$metric) %>% 
      mutate(x_bins = cut(location_x, breaks = seq(from=0, to=120, by = 20),include.lowest=TRUE), 
             y_bins = cut(location_y, breaks = seq(from=0, to=80, by = 20),include.lowest=TRUE)) %>% 
      mutate(total_event = n(), 
             .by = c(player_name_id, match_date_opp)) %>% 
      summarise(total_event = max(total_event), 
                bin_event = n(), 
                location_x = median(location_x), 
                location_y = median(location_y),
                .by = c(player_name_id, match_date_opp, x_bins, y_bins)) %>% 
      drop_na(player_name_id) %>% 
      mutate(bin_pct = bin_event / total_event, 
             .by = c(player_name_id, match_date_opp, x_bins, y_bins)) 
    
  }) 
  
  
  output$heatmaps <- renderPlot({
    
    metric_react() %>%    
      ggplot(aes(x = location_x, y = location_y, fill = bin_pct, group = bin_pct)) +
      geom_bin2d(binwidth = c(20, 20), position = "identity", linewidth = 0.9) + #2
      annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", linewidth = 0.6) +
      annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", linewidth = 0.6) +
      annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", linewidth = 0.6) +
      annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", linewidth = 0.6) +
      annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", linewidth = 0.6) +
      annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", linewidth = 0.6) +
      annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", linewidth = 0.6) +
      annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", linewidth = 0.6) +
      annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", linewidth = 0.6)+
      annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", linewidth = 0.6)+
      annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", linewidth = 0.6)+
      theme(rect = element_blank(), line = element_blank()) +
      annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) + # add penalty spot right
      annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) +
      annotate("path", colour = "white", linewidth = 0.6, x=60+10*cos(seq(0,2*pi,length.out=2000)),
               y=40+10*sin(seq(0,2*pi,length.out=2000)))+ # add centre spot
      annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) +
      annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), linewidth = 0.6,
               y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
      annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), linewidth = 0.6,
               y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") + 
      theme(axis.text.x=element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y=element_blank(),
            legend.title = element_blank(), 
            legend.direction = "vertical") + 
      scale_y_reverse() + 
      coord_fixed(ratio = 95/100) +
      facet_wrap(~ match_date_opp) +
      theme(strip.text = element_text(size = 7))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
