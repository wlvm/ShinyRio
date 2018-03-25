##
library(shiny)
library(DT)
library(plotly)
library(crosstalk)
library(ggcorrplot)
library(ggplot2)
library(stats)


# First read the datasets
athletes <- readRDS("data/athletes2016.rds")
triathletes <- readRDS("data/triathletes2016.rds")
m <- triathletes %>% 
  tibble::rownames_to_column()

ui <- fluidPage(
  
  # Application title
  #h4("W. Mathie, 25th March 20"),
  sidebarLayout(
    sidebarPanel(
      HTML('<center><img src="2016_1.png"></center>'),
      br(),
      helpText("BMI = Body Mass Index"),
      helpText("Height [m]"),
      helpText("Weight [kg]"),
      helpText("Age at Rio [years]"),
      selectInput("selected_data",
                  label = "Select an attribute for Histogram",
                  choices = c("BMI", "Height", "Weight", "Age"),
                  selected = "BMI"),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      # Add a new checkboxInput to your app
      checkboxInput("addmean",
                    label = "Show mean line?",
                    value = TRUE),
      
      
      HTML('<center><img src="rio.png"></center>'),
      br(),
      br(),
    p("The data and images were sourced from the official ",
      a("Rio 2016 website.", 
        href = "https://www.rio2016.com/")),
      br(),
      width=3),

    # Show a plot of the generated distribution
    mainPanel(
      titlePanel(h1("Triathletes at the 2016 Rio Olympic Games")),
      tabsetPanel(type = "tabs",
                  tabPanel("About", 
                           br(),
                           HTML('<center><img src="logo-90.png"></center>'),
                           br(),
                           p("This is my first Shiny app, and I am planning to include other sports and make it more interactive as I get more time to do 
                             so. Hopefully you enjoyed it!"),
                           br(),
                           h3("Usage:"),
                           p("Use the side panel to control the histogram displayed, and click on the tabs to explore some stats. Use the Data Table tab 
                              to see, search and sort data. You can also filter data by selecting rows with your moust. Lastly, you can 
                              export the selected rows to a csv file
                              by clicking on the button at the bottom of the data table."), 
                           h3("Data:"),
                           p("The data and images were sourced from the official ",
                             a("Rio 2016 website.", 
                               href = "https://www.rio2016.com/")),
                           p("Currently I am only displaying the data of 110 triathletes with known height and weight."),
                           br(),
                           HTML('<center><img src="Gwen.png"></center>')
                           ),
                  tabPanel("Histogram", plotOutput("histoPlot")),  
                  tabPanel("Countries", 
                           br(),
                           plotlyOutput("piechart"),
                           br(),
                           helpText("Hover over the Olympic Ring! Scroll down the legend to see all countries.")),
                  tabPanel("Data Table",
                          #data table and scatter graph with selection
                           br(),
                           p("The Olympic triathlon: 1500m swim, 40km bike (drafting allowed), 10km run. What does it take to get to the Olympics? Here is 
                             the data!"),
                           br(),
                           plotlyOutput("x2"),
                           helpText("Select a row with your mouse. Selected athlete is shown as a green data point."),
                           DT::DTOutput("x1"),
                           fluidRow(
                             p(class = 'text-center', downloadButton('x3', 'Download Selected Data'))
                           )
                           )                        
      )
      
    )
  )
)



server <- function(input, output) {
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
}
# Run the application 
shinyApp(ui, server)

