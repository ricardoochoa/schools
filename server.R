#server.R

function(input, output, session) {
  # subset municipalities from state selection
    output$Box1 = renderUI({selectInput(inputId = "my.municipality", 
                                               label = "Municipality", 
                                               choices = subset(my.data, NOM_ENT == input$my.state)$NOM_MUN, 
                                               selected = 1)})
    # plot_main
    output$plot_main <- 
      renderPlotly({
        temp_table <- 
          get_someanswers(precipitation_file = read_precipitation_file(nom_ent = input$my.state, nom_mun =  input$my.municipality),
                          area = input$my.catchmentarea, 
                          material = input$my.roofmaterial, 
                          slope = input$my.slope, 
                          # filter = input$my.filter, 
                          prefilter = input$my.prefilter, 
                          users =  input$my.users, 
                          use = input$my.use, 
                          tank = as.numeric(input$my.storage))
        
        temp_table <- ddply(temp_table, c("m"), summarise,
                            "Rainwater" = sum(usedrw), 
                            "Other source" = sum(demand - usedrw))
        
        temp_table <- melt(temp_table,
                           id.vars=c("m"), 
                           variable.name="Source", 
                           value.name="Volume")
        
        p <- ggplot(temp_table) + 
          geom_bar(aes(x = m, weight = Volume, fill = Source), position = "stack", width = 0.2) +
          scale_x_continuous(name = "Month", breaks = 1:12, labels = c("J","F","M","A","M","J","J","A","S","O","N","D"))+
          scale_y_continuous(labels = comma, name = HTML(paste("Expected water use (m", tags$sup("3"), ")", sep=""))) +
          scale_fill_manual(values = c("#16b3ebff", "grey")) +
          theme_bw()+
          theme(legend.title=element_blank(),
                axis.text=element_text(size=7),
                axis.title=element_text(vjust=1, hjust=1))
        
        ggplotly(p)
      })

    # plot_monthly
    output$plot_monthly <- 
      renderPlotly({
      p <- ggplot(read_precipitation_file(nom_ent = input$my.state, nom_mun = input$my.municipality)) + 
          stat_summary(aes(x=m, y=p), fun.y = "sum", colour = "#16b3ebff", size = 2, geom = "line")+
          stat_summary(aes(x=m, y=p), fun.y = "sum", colour = "#16b3ebff", size = 4, geom = "point")+
          theme_bw()+
          theme(axis.text=element_text(size=7),
                axis.title=element_text(vjust=1, hjust=1))+
          scale_x_continuous(name = "Month", breaks = 1:12, labels = c("J","F","M","A","M","J","J","A","S","O","N","D"))+
          scale_y_continuous(name = "Precipitation (mm)")
      
      ggplotly(p)
  })

  # plot_daily
    output$plot_daily <- renderPlotly({
      p <- ggplot(read_precipitation_file(nom_ent = input$my.state, nom_mun = input$my.municipality)) + 
        geom_jitter(aes(x=m, y=p), colour = "#16b3ebff")+
        theme_bw()+
        theme(axis.text=element_text(size=7),
              axis.title=element_text(vjust=1, hjust=1))+
        scale_x_continuous(name = "Month", breaks = 1:12, labels = c("J","F","M","A","M","J","J","A","S","O","N","D"))+
        scale_y_continuous(name = "Precipitation (mm)") +
        ggtitle("Diaria")
      ggplotly(p)
    })
    
  # plot_holidays
    output$plot_holidays <- renderPlot(function() {
      p <- ggplot(holidays_table) +
        geom_tile(aes(x = as.factor(weekday_number), y = -week_number, fill = as.factor(active)), 
                  colour = "white") +
        scale_fill_manual(values=c("grey", "#16b3ebff"), 
                          name="Experimental\nCondition",
                          breaks=c("ctrl", "trt1"),
                          labels=c("Control", "Treatment 1")) +
        scale_y_continuous(breaks=NULL, name = NULL) +
        scale_x_discrete(breaks=1:7,
                         labels=c("S", "M", "T", "W", "T", "F", "S"), 
                         name = NULL) + 
        facet_wrap(~month_name) +
        theme_bw()
      
      print(p)
    })
    
  # plot_summary
    output$plot_summary <- renderPlotly({
      temp_table <- get_optimaltanksize(precipitation_file = read_precipitation_file(nom_ent = input$my.state, nom_mun =  input$my.municipality),
                                    area = input$my.catchmentarea, 
                                    material = input$my.roofmaterial, 
                                    slope = input$my.slope, 
                                    # filter = input$my.filter, 
                                    prefilter = input$my.prefilter, 
                                    users =  input$my.users, 
                                    use = input$my.use)

      p <- 
        ggplot(temp_table) + 
        geom_bar(aes(x=tank, weight = Volume, fill = Source), position = "stack")+
        scale_fill_manual(values=c("#16b3eb", "#bebebe"))+
        scale_y_continuous(labels = comma, name = HTML(paste("Expected water use (m", tags$sup("3"), "/month)", sep="")))+
        scale_x_discrete(name = "Tank size (L)")+
        theme_bw()+
        theme(legend.title=element_blank()) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.text=element_text(size=7),
              axis.title=element_text(vjust=1, hjust=1))

      ggplotly(p)      
    })

    output$supplyBox <- renderInfoBox({
      infoBox(title = "Rainwater supply", 
              value = paste0(get_waterdemand(precipitation_file = read_precipitation_file(nom_ent = input$my.state, nom_mun = input$my.municipality), 
                                             area = input$my.catchmentarea, 
                                             material = input$my.roofmaterial, 
                                             slope = input$my.slope, 
                                             #filter = input$my.filter, 
                                             prefilter = input$my.prefilter, 
                                             users =  input$my.users, 
                                             use = input$my.use, 
                                             tank = as.numeric(input$my.storage)
                                            ), "%"), icon = icon("pie-chart"), fill = TRUE)
    })
    
    output$rainBox <- renderInfoBox({
      infoBox(title = "Rainwater use", 
              value = paste0(get_rain(precipitation_file = read_precipitation_file(nom_ent = input$my.state, nom_mun = input$my.municipality), 
                                             area = input$my.catchmentarea, 
                                             material = input$my.roofmaterial, 
                                             slope = input$my.slope, 
                                             #filter = input$my.filter, 
                                             prefilter = input$my.prefilter, 
                                             users =  input$my.users, 
                                             use = input$my.use, 
                                             tank = as.numeric(input$my.storage)
              ), 
              
              " m3/year"

          #  HTML(paste('m',tags$sup(3),'/year',sep=""))

              ), icon = icon("tint"), fill = TRUE, color = "purple")
    })

    output$costsBox <- renderInfoBox({
      infoBox("Expected cost", paste0(get_costs(input$my.prefilter, input$my.storage, input$my.catchmentarea, input$my.filter, input$my.users), "£"), icon = icon("money"), fill = TRUE, color = "yellow")
    })
    
    output$the_tanks_table <- renderDataTable(tanks_table_P)
    output$the_runoff_table <- renderDataTable(runoff_table_P)
    output$the_prefilters_table <- renderDataTable(prefilter_table_P)
    output$the_use_table <- renderDataTable(use_table_P)
    output$the_costs_table <- renderTable(get_costs(input$my.prefilter, input$my.storage, input$my.catchmentarea, input$my.filter, input$my.users, tabular = TRUE))
    
}



# 
# 
# 