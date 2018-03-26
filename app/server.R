#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(plotly)
library(crosstalk)
library(ggplot2)
library(stats)
library(dplyr)
library(leaflet)


# First read the datasets
triathletes <- readRDS("data/triathletes2016.rds")
countries <- readRDS("data/countries.rds")
#tibble for my DT later
m <- triathletes %>% 
  tibble::rownames_to_column()

##################### Start of Server code ###################################
shinyServer(function(input, output) {

  ## HISTOGRAM TAB
  d <- SharedData$new(m, ~rowname)
  # highlight selected rows in the scatterplot
  x_reactive <- reactive({
    
    switch(input$selected_data,  # Look at the value of input$selected_data...
           "BMI" = triathletes[,11],          
           "Height" = triathletes[,5],
           "Weight" = triathletes[,6],     
           "Age"=triathletes[,4])
    
  })
  output$histoPlot <- renderPlot({
    values <- x_reactive()
    bins <- seq(min(values), max(values), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    histo <- hist(values, breaks = bins, col = 'skyblue', border = 'skyblue', xlab = "Attribute", ylab = "Number of trathletes", main = "Histogram of Attribute Selected")
    if(input$addmean) {  # If the addmean box is checked...
      # Add a vertical line at the mean of x
      histo %>% abline(v = mean(values),
                       lwd = 2,      # Thickness
                       lty = 2, col="red", text("Mean"))      # Dashed line
      
    } # close if statement
  })
  
  #DATA TABLE TAB
  
  output$x2 <- renderPlotly({
    
    s <- input$x1_rows_selected
    
    if (!length(s)) {
      p <- d %>%
        plot_ly(x = ~Age, y = ~BMI, type = "scatter", mode = "markers", color = ~Sex, name = 'Unselected',colors="Set1") %>%
        layout(showlegend = T) %>% 
        highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Selected'))
    } else if (length(s)) {
      pp <- m %>%
        plot_ly() %>% 
        add_trace(x = ~Age, y = ~BMI, type = "scatter", mode = "markers", color = ~Sex, name = 'Selected',colors="Set1") %>%
        layout(showlegend = T)
      #selected data
      pp <- add_trace(pp, data = m[s, , drop = F], x = ~Age, y = ~BMI, type = "scatter", mode = "markers", 
                      color = I('green'), name = 'Selected')
    }
    
  })
  
  # highlight selected rows in the table
  output$x1 <- DT::renderDT({
    m2 <- m[d$selection(),]
    dt <- DT::datatable(m)
    if (NROW(m2) == 0) {
      dt 
    } else {
      DT::formatStyle(dt, "rowname", target = "row",
                      color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                      backgroundColor = DT::styleEqual(m2$rowname, rep("black", length(m2$rowname))))
    }
    
  }
  )
  
  # download the filtered data
  output$x3 <- downloadHandler('triathletes-filtered.csv', content = function(file) {
    s <- input$x1_rows_selected
    if (length(s)) {
      write.csv(m[s, , drop = FALSE], file)
    } else if (!length(s)) {
      write.csv(m[d$selection(),], file)
    }
  })
  
  #COUNTRIES TAB
  output$piechart <- renderPlotly({
    
    p <- triathletes %>%
      group_by(Nationality) %>%
      summarise(count = n()) %>%
      plot_ly(labels = ~Nationality, values = ~count,textinfo = "none") %>%
      add_pie(hole = 0.6) %>%
      layout(title = "Nationalities of Triathletes",  showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$value <- renderText({ req(input$obs) })
  #Let user pick how many countries to display
  #but pick at random
  points <- eventReactive(req(input$obs), {
    if(input$obs < 1) sample_n(countries, 1)
    else if(input$obs < nrow(countries)) sample_n(countries, input$obs)
    else countries
  }, ignoreNULL = FALSE)
  
  output$trimap <- renderLeaflet({
    points() %>% leaflet() %>% addTiles() %>%
      addMarkers(~long, ~lat, popup = ~Nationality, label = ~Nationality)
  })
  #EXTRAS TAB
  # output$input <- renderText({
  #   paste0("You are ", "<strong>",input$yourmass, " kg", " @ ", input$yourheight, " m", "</strong>")
  # })
  
  output$YourInput <- renderText({
    paste0("You are ", "<strong>",input$yourmass, " kg", " @ ", input$yourheight, " m", "</strong>")
  })
  
  
  
  resultprint_next <- eventReactive(input$Calculate, {
    
    bmi <-input$yourmass / (input$yourheight^2)
    
    if      (bmi <  15.0) info = "<span style='color: red'>Very severely underweight</span>"
    else if (bmi <= 16.0) info = "<span style='color: red'>Severely underweight</span>"
    else if (bmi <= 18.5) info = "<span style='color: orange'>Underweight</span>"
    else if (bmi <= 25.0) info = "<span style='color: green'>Normal (healthy weight)</span>"
    else if (bmi <= 30.0) info = "<span style='color: orange'>Overweight</span>"
    else if (bmi <= 35.0) info = "<span style='color: red'>Obese Class I (Moderately obese)</span>"
    else if (bmi <= 40.0) info = "<span style='color: red'>Obese Class II (Severely obese)</span>"
    else                  info = "<span style='color: red'>Obese Class III (Very severely obese)</span>"
    
    paste0("Your BMI is ", "<code>", round(bmi, 2), "</code>", ", which is: ", info)
    # p <- plot_ly(triathletes, x = ~Age, y = ~BMI, name = 'Olympic Triathletes', type = 'scatter', mode = 'markers') %>%
    #   add_trace(y = bmi, name = 'Your BMI', mode = 'lines') 
    
  })
  output$BMIresult <- renderText({
    resultprint_next()
    
  })
  resultplot_next <- eventReactive(input$Calculate, {
    
    p <- plot_ly(triathletes, x = ~Age, y = ~BMI, name = 'Olympic Triathletes', type = 'scatter', mode = 'markers') %>%
      add_trace(y = input$yourmass / (input$yourheight^2), name = 'Your BMI', mode = 'lines')
    
  })
  output$BMIplot <- renderPlotly({
    resultplot_next()
  })
  
  
  
  # 
})
