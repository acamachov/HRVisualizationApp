library(shiny)
library(shinyWidgets)
library('tidyverse')
library('ggplot2')

data <- read.csv("HRDataset_v14.csv")
data$Sex <- as.factor(data$Sex)
resp_varH <- c('Salary')
pred_varH <- c('Sex', 'RaceDesc','MaritalDesc')

data$DateofHire <- as.POSIXct(data$DateofHire,
                              format = "%m/%d/%Y")
data$DateofTermination <- as.POSIXct(data$DateofTermination,
                                     format = "%m/%d/%Y")

ActualDate ="2/28/2019"
ActualDate <- as.POSIXct(ActualDate, format = "%m/%d/%Y")
data$DateofTermination[is.na(data$DateofTermination)] <- ActualDate

data$Seniority <- (data$DateofTermination - data$DateofHire)/365

data$SalarySplit <- cut(data$Salary, 
                        breaks=c(-Inf, 55000, 63000, 70000, 90000, Inf), 
                        labels=c("0-55k","55k-63k","63k-70k", "70k-90k", "90k+"))

data.barchart <- subset(data, select= c(Salary, Sex, Position, RaceDesc, Department, ManagerName, EmpSatisfaction, Absences))

# Define UI ----
ui <- fluidPage(
  titlePanel("Human Resources Visualization Tool"),
  
  navbarPage("NavBar",
             
             tabPanel("About",
                      sidebarLayout(
                        sidebarPanel(
                          fluidPage(
                            h2("Hotel booking demand app"),
                            p("The main goal of this app is to help an analyst to find some insights about the demand of hotels."),
                            
                            p("The analyst could be the regional manager of a major hotel chain that uses reservation information to make the best decisions regarding adjustments to its facilities, marketing strategies to attract more guests, and everything else needed to offer the best possible experience to its guests."),
                            
                            br(),
                            br(),
                            p('These visualizations aim to help the analyst to find aswers to three different questions:'),
                            p(strong('1.- Demand distribution throughout the year')),
                            
                          )
                          ,width = 3),
                        
                        
                        mainPanel(
                          h1("Introducing Hotel booking demand dataset"),
                          p("The dataset represents information regarding the demand for two types of hotels,
        one is a resort-type hotel and the other is an urban hotel, it contains 32 attributes
        that include information such as when the reservation was made, length of stay,
        number of adults , children and/or babies, the number of available parking spaces,
        among other things. and approximately 119,000 observations where each observation 
        represents a hotel reservation from July 1, 2015 to August 31, 2017, including reservations 
        that actually arrived and those that were cancelled."),
                          
                        ) # mainPanel
                        
                      ) # sidebarLayout
             ),             
             
             tabPanel("Salary Discrimination",
                      titlePanel("Is there any notable difference in salaries between people of
different sex, race, or marital status?"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput(inputId = 'xvar', choices = pred_varH, label = "Social discriminant")
                          
                          , width = 3), 
                        
                        mainPanel(
                          plotOutput("cormap", width = "95%")
                          
                        ) # mainPanel
                        
                      ) # sidebarLayout
             ), # tabPanel
             
             tabPanel("Salary Evolution",
                      titlePanel("How is the evolution over the years of the salary of the employees? "),
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("select", 
                                      label = "Choose a department to be filtered by",
                                      choices = c("No filtered",
                                                  "Admin Offices", 
                                                  "Executive Office",
                                                  "IT/IS",
                                                  "Production       ",
                                                  "Sales",
                                                  "Software Engineering"),
                                      selected = "No filtered")), 
                        
                        mainPanel(
                          plotOutput("scatterplot", width = "95%",
                                     dblclick = "plot1_dblclick",
                                     brush = brushOpts(
                                       id = "plot1_brush",
                                       resetOnNew = TRUE))
                          
                        ) # mainPanel
                        
                      ) # sidebarLayout
             ), # tabPanel
             
             tabPanel("Satisfaction",
                      titlePanel("How is employee satisfaction affected by internal aspects of our company?"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Use the following box to change the filter you want to compare satisfaction with, The plot will show the
               average satisfaction for all employees according to the filter selected"),
                          
                          selectInput("barChartFilter", 
                                      label = "Choose a filter.",
                                      choices = c("Department", 
                                                  "Salary",
                                                  "Position",
                                                  "Manager",
                                                  "State",
                                                  "Race",
                                                  "Sex"),
                                      selected = "Department"),
                        ),
                        
                        
                        mainPanel(
                          plotOutput("barchart")
                          
                        ) # mainPanel
                        
                      ) # sidebarLayout
             ) # tabPanel
             
  ) # navbarPage
) # fluidpage

# Define server logic ----
server <- function(input, output) {
  
  # SALARY DISCRIMIANTION TAB
  output$cormap <- renderPlot({ 
    ggplot(data, aes(x = get(input$xvar), y = Salary, fill = get(input$xvar))) +
      geom_boxplot() +
      ggtitle('Salary ~ Social discriminant') +
      ylab('Salary') +
      xlab('Social discriminant') +
      theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none')
  })
  
  output$lmsummary <- renderPrint({
    print("******************Summary*********************")
    summary(lm(reformulate(input$xvar, input$yvar), data = data))
    
  })
  
  # SALARY EVOLUTION TAB
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  depSelect <- reactive(input$select)
  
  observe({
    dep <- depSelect()
    if(dep == "No filtered")
      depFiltered <- data
    else
      depFiltered<- data[data$Department %in% c(dep), ]
    
    
    output$scatterplot <- renderPlot({ 
      ggplot(depFiltered, aes(x = Seniority, y = Salary, color= Department)) +
        geom_point() +
        ggtitle('Salary evolution') +
        ylab('Salary') +
        xlab('Time employees have been in the Company(Years)') +
        theme(plot.title = element_text(face = 'bold', size = 14, hjust = 0.5), legend.position = 'none')+
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = TRUE)
    })
    observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
  })
  
  # SATISFACTION TAB
  output$barchart <- renderPlot({
    
    if (input$barChartFilter != "State") {
      
      if (input$barChartFilter == "Department"){data.aux <- aggregate(data$EmpSatisfaction, list(data$Department), mean)}
      else if (input$barChartFilter == "Salary"){data.aux <- aggregate(data$EmpSatisfaction, list(data$SalarySplit), mean)}
      else if (input$barChartFilter == "Position"){data.aux <- aggregate(data$EmpSatisfaction, list(data$Position), mean)}
      else if (input$barChartFilter == "Manager"){data.aux <- aggregate(data$EmpSatisfaction, list(data$ManagerName), mean)}
      else if (input$barChartFilter == "Race"){data.aux <- aggregate(data$EmpSatisfaction, list(data$Race), mean)}
      else if (input$barChartFilter == "Sex"){data.aux <- aggregate(data$EmpSatisfaction, list(data$Sex), mean)}
      
      
      data.aux$x <- round(data.aux$x, digits=3)
      colnames(data.aux) <- c('Filter', 'Average_Satisfaction')
      
      
      ggplot(data=data.aux, aes(x=reorder(Filter, Average_Satisfaction), y=Average_Satisfaction, fill=reorder(Filter, Average_Satisfaction))) +
        geom_bar(stat="identity", fill='steelblue')+
        geom_text(aes(label=Average_Satisfaction), hjust=1.5, size=4)+
        scale_fill_brewer(palette="Dark2") + xlab(input$barChartFilter) + ylab('Average Satisfaction') +
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
        theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) + 
        theme(legend.position = 'none') +
        #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        #theme(axis.text.y = element_text(angle = -90, vjust = 0.5, hjust=1)) +
        coord_flip() +
        theme(text = element_text(size = 15))  
      
    }
    else{
      data.states <- data
      data.states$State <- state.name[match(data$State,state.abb)]
      data.states$State <- tolower(data.states$State)
      
      ##agg
      data.states <- aggregate(data.states$EmpSatisfaction, list(data.states$State), mean)
      colnames(data.states) <- c('State', 'Average_Satisfaction')
      data.states
      
      ##print map
      states_map <- map_data("state")
      states_map <- merge(states_map, data.states, by.x = "region", by.y = "State", all.x = TRUE)
      states_map <- arrange(states_map, group, order)
      
      ggplot(states_map, aes(x = long, y = lat, group = group, fill=Average_Satisfaction)) +
        geom_polygon(colour = "black") + coord_map("polyconic") +
        expand_limits(x = states_map$long, y = states_map$lat) +
        scale_fill_gradient2(low = "#559999", mid = "grey90", high = "#cc6600", name="Avg. Satisfaction") 
      
    }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)