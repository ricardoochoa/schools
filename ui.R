# ui.R

dashboardPage(skin = "black",
  dashboardHeader(title = tags$a(href='http://islaurbana.org', tags$img(src='logo.png',height='40',width='200'))),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calculator", tabName = "tool_tab", icon = icon("calculator")),
#       menuItem("Help", tabName = "help_tab", icon = icon("commenting"), 
#         menuSubItem("Water harvesting", tabName = "help_cosecha"), 
#         menuSubItem("Pre-filter", tabName = "help_prefiltro"), 
#         menuSubItem("Storage", tabName = "help_almacenamiento"), 
#         menuSubItem("Filter", tabName = "help_Filtro")), 
      # In the 1.0 Beta version this section was named "Assumptions". 
      # In the following versions it will be renamed to "Configuration".
      menuItem("Assumptions", tabName = "tool_configuration", icon = icon("commenting"), #briefcase
          menuSubItem("Tanks", tabName = "tool_tanks"), 
          menuSubItem("Holidays", tabName = "tool_holidays"),
          menuSubItem("Pre-filter coefficients", tabName = "tool_prefilters"), 
          menuSubItem("Runoff coefficients", tabName = "tool_runoff"),
          menuSubItem("Use factors", tabName = "tool_usefactors")), 
      menuItem("About us", tabName = "tool_about", icon = icon("child"))
    )),
  
dashboardBody(
tabItems(
      # First tab content
  tabItem(tabName = "tool_tab",
  # INTRODUCCIÓN
  fluidRow(column(12, 
            HTML(paste(tags$span(style="color: #16b3eb", 
                          tags$h2(tags$b("Water harvesting"), 
                          tags$sup(tags$span(style="font-size: 65%", "beta 1.0")), 
                          sep = "")))),
            p("This tool is intended to assist in the planning and design stages of Rain Water Harvesting (RWH) systems in Mexico. 
               It was developed with the aim of supporting the analysis of RWH feasibility and performing basic system calculations."))), 
  br(),
  
  fluidRow(
    column(12, h4(tags$b("Location")), p("Please select the State and Municipality where your project is located.")),
    column(6,
           # SELECT STATE
           selectInput("my.state", label = "State", 
                       choices = all_states$NOM_ENT, 
                       selected = 1)
    ),
    column(6,
           # SELECT MUNICIPALITY
           uiOutput("Box1"))
  ), hr(),
  fluidRow(
    column(12, h4(tags$b("Analysis")), 
               p("Please fill out the following information for further analysis of the system. The project also makes some assumptions 
                  in relation to tank sizes, school holidays, pre-filter coefficients, runoff coefficients and use factors. Click on the 
                  'Assumptions' menu for more information.")), # If you need more information on how to do this, click on the the titles. 
    column(3,
           # SELECT CATCHMENT AREA
           numericInput("my.catchmentarea", label = HTML(paste("Area (m", tags$sup("2"), ")", sep="")) #a(HTML(paste("Area (m", tags$sup("2"), ")", sep="")), href="http://www.google.com", target="_blank")
                        , value = 400),
           # SELECT TANK
           selectInput("my.storage", label = "Tank volume (L)" #a("Expected use", href="http://www.google.com", target="_blank")
                       , 
                       choices = as.list(tanks_table$size),
                       selected = 2)),
#            numericInput("my.storage", label = "Tank volume (L)" #a("Tank volume (L)", href="http://www.google.com", target="_blank")
#                         , value = 450)),
    column(3,
           # SELECT NUMBER OF USERS
           numericInput("my.users", label = "Number of users" #a("Number of users", href="http://www.google.com", target="_blank")
                        , value = 60),
           # SELECT EXPECTED USE
           selectInput("my.use", label = "Expected use" #a("Expected use", href="http://www.google.com", target="_blank")
                       , 
                       choices = as.list(use_table$use),
                       selected = 2)),
    column(3, 
           # SELECT PREFILTER
           selectInput("my.prefilter", label = "Pre-filter" #a("Pre-filter", href="http://www.google.com", target="_blank")
                       , 
                       choices = as.list(prefilter_table$prefilter), 
                       selected = 1), 
           # SELECT FILTER
           selectInput("my.filter", label = "Filter" #a("Filter", href="http://www.google.com", target="_blank")
                       , 
                       choices = as.list(filter_table$filter), #list("UV" = 1, "Ozone" = 2, "Ceramic" = 3), 
                       selected = 2)),
    column(3,
           # SELECT ROOF MATERIAL
           selectInput("my.roofmaterial", label = "Material" #a("Material", href="http://www.google.com", target="_blank")
                       , 
                       choices = as.list(runoff_table$material),
                       selected = 1), 
           br(),
           # SELECT SLOPE
           checkboxInput("my.slope", label = "Select if the slope is 5% or over" #a("Select if the slope is 5% or over", href="http://www.google.com", target="_blank")
                         , value = FALSE))#¿cuál es el punto de la búsqueda?
    ),

  hr(), 
  
  fluidRow(column(12, h4(tags$b("Results")))), 
  
   fluidRow(
    # RESULTADOS
    infoBoxOutput("supplyBox"),
    infoBoxOutput("rainBox"),
    infoBoxOutput("costsBox"),
    tabBox(
             title = "Results",
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "tabset1",
             tabPanel("Consumption", box(width = 6), plotlyOutput("plot_main")),
             tabPanel("Precipitation", box(width = 6), plotlyOutput("plot_monthly"))
             #tabPanel("Precipitación (día)", plotlyOutput("plot_daily"))
    ), 
    tabBox(
      title = "Assessment",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset2",
      tabPanel("Use", plotlyOutput("plot_summary")),
      tabPanel("Costs", p("Cost breakdown"), tableOutput('the_costs_table'))
    )),

  fluidRow(
    column(12,
           p("Developed by", a("Capital Sustentable", href="http://www.capitalsustentable.com", target="_blank"), ", 2016.", align = "center")))
  ), 
  
#   # Second tab content
#   tabItem(tabName = "help_tab"),
#   
#   # Second - 1st subtab content
#   tabItem(tabName = "help_cosecha", h2("Work in progress"),
#     fluidRow(
#       column(12,
#         p("Developed by", a("Capital Sustentable", href="http://www.capitalsustentable.com", target="_blank"), ", 2016.", align = "center")))
#           ), 
#   
#   # Second - 2nd subtab content
#   tabItem(tabName = "help_prefiltro", h2("Work in progress"),
#     fluidRow(
#       column(12,
#         p("Developed by", a("Capital Sustentable", href="http://www.capitalsustentable.com", target="_blank"), ", 2016.", align = "center")))), 
#   
#   # Second - 3rd subtab content
#   tabItem(tabName = "help_almacenamiento", h2("Work in progress"),
#     fluidRow(
#      column(12,
#       p("Developed by", a("Capital Sustentable", href="http://www.capitalsustentable.com", target="_blank"), ", 2016.", align = "center")))), 
# 
#   # Second - 4th subtab content
#   tabItem(tabName = "help_Filtro", h2("Work in progress"),
#     fluidRow(
#      column(12,
#       p("Developed by", a("Capital Sustentable", href="http://www.capitalsustentable.com", target="_blank"), ", 2016.", align = "center")))), 


  # Third tab content
  tabItem(tabName = "tool_configuration"),

  # Third - 1st subtab content
  tabItem(tabName = "tool_tanks", h2("Tanks"),
          p("There are diverse types of storage devices for water. However, in the calculations carried out by this tool it is assumed that water 
            tanks are used by the school. The following table describes the prices in British Pounds (£) for commercial water tanks in Mexico. 
            This data is used to estimate the total project costs."),
          hr(),
          fluidRow(
            column(3), 
            column(6, dataTableOutput('the_tanks_table')))), 

  # Third - 2nd subtab content
  tabItem(tabName = "tool_holidays", h2("Holidays"),
          p("School calendars are an important consideration in the calculations carried out by this tool, as it was assumed that there is no water 
             demand when there are no students at school. The following calendars show in blue the days with expected water demand and in gray the 
             days with no water demand, such as weekends and school holidays (winter holidays, easter and summer holidays). For example, it can be 
             seen that in July there is no water demand due to the summer holidays."),
#     HTML(paste(tags$span(
#                   tags$p(tags$b("Water harvesting"), 
#                   tags$p(tags$span(style="color: #16b3eb", "beta 1.0")), 
#                   sep = "")))),
          hr(),
          plotOutput("plot_holidays")), 

  # Third - 3rd subtab content
  tabItem(tabName = "tool_prefilters", h2("Pre-filter coefficients"), 
          p("The pre-filter or first flush diverter selection affects the total amount of wasted water and investment costs. Pre-filters are a very 
             important part of a rainwater harvesting system. Most of the pre-filters options perform in a very similar manner. However, in order 
             to provide more precise results, several pre-filter options were included in this tool. The following table shows the main assumptions 
             that were made."),
          hr(),
          fluidRow(
            column(1), 
            column(10, dataTableOutput('the_prefilters_table')))), 

  # Third - 4th subtab content
  tabItem(tabName = "tool_runoff", h2("Runoff coefficients"), 
          p("Different types of roofs have different surface runoffs. The following table shows the runoff coefficients for different types of roof 
            materials."),
          hr(),
          fluidRow(
            column(1), 
            column(10, dataTableOutput('the_runoff_table')))), 

  # Third - 5th subtab content
  tabItem(tabName = "tool_usefactors", h2("Use factors"), 
          p("The following table shows the main assumptions for per capita water demand."),
          hr(),
          fluidRow(
            column(3),
            column(6, dataTableOutput('the_use_table')))), 

#   # Third - 5th subtab content
#   tabItem(tabName = "tool_filters", h2("Filters"), 
#         p("This tool considers that all filters will perform similarly. However, it considers too that filtering technologies have different investment costs. The following table shows the different filters and costs."),
#         dataTableOutput('the_use_table')), 

  
  # Fourth tab content
  tabItem(tabName = "tool_about", 
          HTML(paste(tags$span(style="color: #16b3eb", 
                               tags$h2(tags$b("Water harvesting"), 
                                       tags$sup(tags$span(style="font-size: 65%", "beta 1.0")), 
                                       sep = "")))),
      hr(),    
      p("This tool is the result of a project carried out by", a("Isla Urbana", href="http://islaurbana.org/", target="_blank"), ",",
        a("Caminos de Agua", href="http://caminosdeagua.org/", target="_blank"), ",",
        a("Fundación Cántaro Azul", href="http://en.cantaroazul.org/", target="_blank"), "and",
        a("University College London", href="http://www.cege.ucl.ac.uk/", target="_blank"), ".", align = "center"),
      br(),
      p("This tool was developed by ", a("Capital Sustentable", href="http://www.capitalsustentable.com", target="_blank"), 
        ". This is a trial version, if you encounter any errors or bugs, please do not hesitate to contact us at ", 
        a("rochoa@capitalsustentable.com", href="mailto:rochoa@capitalsustentable.com", target="_blank"), "or at ",
        a("margarita.garfias@gmail.com", href="mailto:margarita.garfias@gmail.com", target="_blank"), ".", align = "center" ),
      hr(), hr(),
      fluidRow(
        column(2),
        column(3, align = "center",
          tags$a(href='http://islaurbana.org', tags$img(src='logo.png', height='60', width='300'))),
        column(2),
        column(3, align = "center",
               tags$a(href='http://caminosdeagua.org', tags$img(src='caminos.png', height='60', width='375')))),      
      hr(), 
      fluidRow(
        column(2),
        column(3,align = "center",
          tags$a(href='http://en.cantaroazul.org', tags$img(src='cantaro.png', height='60', width='255'))),
        column(2),
        column(3, align = "center",
               tags$a(href='http://www.cege.ucl.ac.uk', tags$img(src='ucl.png', height='60', width='203')))),
      hr(),
      fluidRow(
        column(4), 
        column(4, align = "center", 
          tags$a(href='http://www.capitalsustentable.com', tags$img(src='LOGO_CAPSUS.png', height='71', width='200'))))
)
  
  )

)
)