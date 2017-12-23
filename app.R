library(shiny)
library(ggplot2)
library(shinythemes)
library(plotly)
library(dplyr)
library(matrixStats)
library(GGally)
library(DT)
library(gridExtra)
library(ggfortify)
library(seplyr)




dataw <- read.csv("data/tsCD38allDatav2.csv")

cleandata <- function(data, name, var){
  name <- filter(data, Region == var)
  name <- name[colSums(!is.na(name)) >0]
  name[is.na(name)] <- 0
  return(name)
}

datawcd28 <- cleandata(dataw, datawcd28, "CD28")
datawcd38 <- cleandata(dataw, datawcd38, "CD38")
datawcd3 <- cleandata(dataw, datawcd3, "CD3")
datawconstant <- cleandata(dataw, datawconstant, "Constant")



ui <- fluidPage(
  navbarPage("Characterization Developability Analytics - tsCD38 ", inverse=TRUE,
             tabPanel("Summary",
                      fluidRow(
                          sidebarLayout(
                            sidebarPanel(width=2,
                                         h4("Select attribute to plot:"),
                                         selectInput('chattrPlot', "Select Attribute",
                                                     choices=colnames(dataw[5:length(colnames(dataw))]),
                                                     selected = dataw[5]),
                                         radioButtons('yscale', "Choose y-scale",
                                                      choices = c(
                                                        "Normalized" = "fixed",
                                                        "Individual" = "free_y"
                                                      )),
                                         h4("Adjust font sizes"),
                                         numericInput("titlefont", "Title", value = 20, width = "60%"),
                                         numericInput("striptextfont", "Strip", value = 18, width = "60%"),
                                         numericInput("xtextfont", "X Labels", value = 12, width = "60%"),
                                         numericInput("ytextfont", "Y Labels", value = 16, width = "60%"),
                                         numericInput("legendtitlefont", "Legend Title", value = 18, width = "60%"),
                                         numericInput("legendtextfont", "Legend Text", value = 18, width = "60%")
                            ),
                            mainPanel(width = 9, plotOutput('attrPlot', height="550px"))
                          )
                                  )
                     
             ),
            
             tabPanel("Correlation analysis",
                      fluidRow(
                        sidebarLayout(
                          sidebarPanel(width = 2,
                                       radioButtons('corData', "Select Region",
                                                    choices = c(
                                                      "CD28" = "CD28",
                                                      "CD38" = "CD38",
                                                      "CD3" = "CD3")
                                                    ),
                                       br(),
                                       checkboxGroupInput('constructs', "Select constructs to include in correlation",
                                                          choices = as.vector(unique(dataw$Construct))),
                                       br(),
                                       actionButton('run', "Generate Matrix"),
                                       br(),
                                       br(),
                                       p("Choose desired region and click 'Genreate Matrix'. A multivariate analysis will be performed on all possible
                                         combinations. The output will be a plot matrix with scatterplots in the lower diagonal, densities on the diagonal and 
                                         correlations written in the upper diagonal"),
                                       br(),
                                       p("Use the interactive plot below to interrogate correlations in greater depth")
                                       ),
                          mainPanel(h2("Correlation matrix plot"),
                                    h4("Wait for plot to load after submitting"),
                                    p(style ="color:red", "This may take time to run depending on how many varialbes you are comparing - please be patient"),
                            plotOutput('corPlot', height = "800px", width = "100%")
                            )
                        )
                      ),
                    br(),
                    fluidRow(
                      tabsetPanel(type="pills",
                        tabPanel("CD28",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                h2("CD28 Modifications"),
                                                h3("Variable Plotting"),
                                                selectInput('x28', "X variable",
                                                            choices = colnames(datawcd28[5:length(colnames(datawcd28))]),
                                                            selected = colnames(datawcd28[5])),
                                                selectInput('y28', "Y variable",
                                                            choices = colnames(datawcd28[5:length(colnames(datawcd28))]),
                                                            selected = colnames(datawcd28[6]))
                                   ),
                                   mainPanel(plotlyOutput('varPlot28', height="550px"))
                                 )
                        ),
                        tabPanel("CD38",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                h2("CD38 Modifications"),
                                                h3("Variable Plotting"),
                                                selectInput('x38', "X variable",
                                                            choices = colnames(datawcd38[5:length(colnames(datawcd38))]),
                                                            selected = colnames(datawcd38[5])),
                                                selectInput('y38', "Y variable",
                                                            choices = colnames(datawcd38[5:length(colnames(datawcd38))]),
                                                            selected = colnames(datawcd38[6]))
                                   ),
                                   mainPanel(plotlyOutput('varPlot38', height="550px"))
                                 )
                        ),
                        tabPanel("CD3",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                h2("CD3 Modifications"),
                                                h3("Variable Plotting"),
                                                selectInput('x3', "X variable",
                                                            choices = colnames(datawcd3[5:length(colnames(datawcd3))]),
                                                            selected = colnames(datawcd3[5])),
                                                selectInput('y3', "Y variable",
                                                            choices = colnames(datawcd3[5:length(colnames(datawcd3))]),
                                                            selected = colnames(datawcd3[6]))
                                   ),
                                   mainPanel(plotlyOutput('varPlot3', height="550px"))
                                 )
                        ),
                        tabPanel("Constant",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                h2("Constant Modifications"),
                                                h3("Variable Plotting"),
                                                selectInput('xConstant', "X variable",
                                                            choices = colnames(datawconstant[5:length(colnames(datawconstant))]),
                                                            selected = colnames(datawconstant[5])),
                                                selectInput('yConstant', "Y variable",
                                                            choices = colnames(datawconstant[5:length(colnames(datawconstant))]),
                                                            selected = colnames(datawconstant[6]))
                                   ),
                                   mainPanel(plotlyOutput('varPlotConstant', height="550px"))
                                 )
                        )
                      ))
                      ),
             tabPanel("Linear Regression"),
             tabPanel("PCA",
                      h1("PCA analysis"),
                      tags$hr(),
                      fluidRow(
                        sidebarLayout(
                          sidebarPanel(width=3,
                                       h3("Select regions to run PCA analysis"),
                                       p("This principal component analysis uses all available attributes as dimensions for Eigenvectors.
                                         It is designed to show relationships within constructs, buffers, and regions. 
                                         All empty data will be replaced with a numeric zero."),
                                       radioButtons('pcachoices', "Select Region for PCA",
                                                    choices = c(
                                                      "CD28" = "CD28",
                                                      "CD38" = "CD38",
                                                      "CD3" = "CD3",
                                                      "Constant" = "Constant",
                                                      "All" = "All"))
                                       ),
                          mainPanel(plotlyOutput('pcaPlot', height="600px", width="100%")))
                      )
               
             ),
             navbarMenu("Tables and plots",
                          tabPanel("Data table",
                                   h2("Data used for this analysis"),
                                   h4("Click region below to view data"),
                                   br(),
                                   tags$style(HTML("
                                                   .tabbable > .nav > li > a                  {background-color: black;  color:white}
                                                   .tabbable > .nav > li[class=active]    > a {background-color: #4286f4; color:white}
                                                   ")),
                                   tabsetPanel(
                                     tabPanel("CD28",
                                              br(),
                                              downloadButton("downloadCD28", "Download table below"),
                                              br(),
                                              br(),
                                              dataTableOutput("table28")),
                                     tabPanel("CD38",
                                              br(),
                                              downloadButton("downloadCD38", "Download table below"),
                                              br(),
                                              br(),
                                              dataTableOutput("table38")),
                                     tabPanel("CD3",
                                              br(),
                                              downloadButton("downloadCD3", "Download table below"),
                                              br(),
                                              br(),
                                              dataTableOutput("table3")),
                                     tabPanel("Constant",
                                              br(),
                                              downloadButton("downloadConstant", "Download table below"),
                                              br(),
                                              br(),
                                              dataTableOutput("tableConstant")),
                                     tabPanel("All",
                                              br(),
                                              downloadButton("downloadAll", "Download table below"),
                                              br(),
                                              br(),
                                              dataTableOutput("tableAll"))
                                   )),
                          tabPanel("Plots by attribute",
                                   tags$h2("Under construction"))
             )
             
  )
  
)


server <- function(input, output) {
  
  mypltTheme <- theme_minimal() +
    theme(
      panel.background = element_rect(fill="#f1f1f1", colour="black"),
      plot.title = element_text(size=18, face="bold"),
      axis.title.y = element_text(size=14, face="bold"),
      axis.title.x = element_text(size=14, face="bold"),
      axis.text.x = element_text(size=12, face="bold"),
      axis.text.y = element_text(face="bold", size = 12),
      legend.title = element_text(size=16, face="bold"),
      legend.text = element_text(size=14, face="bold")
    )
  
  pcadata <- reactive({
    dat <- filter(dataw, Region == input$pcachoices)
    dat <- dat[colSums(!is.na(dat)) > 0]
    dat <- mutate(dat,
                  name = paste(Construct, ".", Time, ".", Buffer))
    row.names(dat) <- dat$name
    dat
    })
  
  allpcadata <- mutate(dataw, name = paste(Construct, ".", Time, ".", Buffer, ".", Region))
  row.names(allpcadata) <- allpcadata$name
  
  pcadims <- reactive({
    dims <- filter(dataw, Region == input$pcachoices)
    dims <- dims[colSums(!is.na(dims)) > 0]
    dims <- dims[5:length(colnames(dims))]
    dims[is.na(dims)] <- 0
    dims
  })
  
  pcaTheme <- theme(
    plot.title =element_text(colour = 'white', size=22, hjust = 0),
    plot.background = element_rect(fill='black'),
    panel.border = element_rect(colour = "white", fill=NA, size=2),
    panel.background = element_rect(fill='black', color='#9eb9e5', size=10),
    axis.text.x = element_text(colour='white'),
    axis.text.y = element_text(colour='white'),
    axis.title.x = element_text(colour='white'),
    axis.title.y = element_text(colour='white'),
    legend.background = element_rect(fill='black'),
    legend.text = element_text(colour='white'),
    legend.key = element_rect(fill = "black", colour="white")
  )
  output$pcaPlot <- renderPlotly({
    
    if (input$pcachoices == "All"){
      alldims <- dataw[5:length(colnames(dataw))]
      alldims[is.na(alldims)] <- 0 
      p <- autoplot(prcomp(alldims), data = allpcadata, colour="Buffer", label = TRUE, shape=FALSE, label.size=3.2) + 
        labs(title="PCA Analysis")+
        theme_dark() + pcaTheme
        
    } else {
      p <- autoplot(prcomp(pcadims()), data = pcadata(), colour="Buffer", label = TRUE, shape=FALSE, label.size=3.2) + 
        labs(title="PCA Analysis")+
        theme_dark() + pcaTheme
    }
    
    
    ggplotly(p)
  })
  
  
  
  output$attrPlot <- renderPlot({
    dataw$Time <- factor(dataw$Time)
    p <- ggplot(dataw, aes_string(x = "Construct", y = input$chattrPlot, fill = "Time")) +
      geom_bar(stat = "identity", position = position_dodge(), color = "black") +
      geom_hline(yintercept = 0, color="black") +
      labs(x=NULL, title=input$chattrPlot, y = cat(paste(input$chattrPlot,"", sep='\n'))) +
      theme_linedraw() +
      theme(
        strip.text.x = element_text(size=input$striptextfont, face="bold", color="white"),
        strip.background = element_rect(fill="#595959"),
        panel.background = element_rect(fill="#f9f9f9"),
        panel.grid.major = element_line(color="grey"),
        panel.grid.minor = element_line(color="grey"),
        axis.title.y = element_text(size = 18, face="bold"),
        axis.text.x = element_text(size=input$xtextfont, face="bold"),
        axis.text.y = element_text(size=input$ytextfont, face="bold"),
        legend.title = element_text(size=input$legendtitlefont, face="bold"),
        legend.text = element_text(size=input$legendtextfont, face="bold"),
        plot.title = element_text(size=input$titlefont, face="bold")
      )
    
    p + facet_wrap(~Buffer, scales = input$yscale)
    
  })
  
  output$varPlot28 <- renderPlotly({
    p <- ggplot(datawcd28, aes_string(x=input$x28, y=input$y28, colour="Construct")) +
      geom_point(aes(text=paste("\nTime (weeks): ", Time, "\nBuffer:", Buffer)), size=2) + mypltTheme
    ggplotly(p)
  })
  
  output$varPlot38 <- renderPlotly({
    p <- ggplot(datawcd38, aes_string(x=input$x38, y=input$y38, colour="Construct")) +
      geom_point(aes(text=paste("\nTime (weeks): ", Time, "\nBuffer:", Buffer)), size=2) + mypltTheme
    ggplotly(p)
  })
  
  output$varPlot3 <- renderPlotly({
    p <- ggplot(datawcd3, aes_string(x=input$x3, y=input$y3, colour="Construct")) +
      geom_point(aes(text=paste("\nTime (weeks): ", Time, "\nBuffer:", Buffer)), size=2) + mypltTheme
    ggplotly(p)
  })
  
  output$varPlotConstant <- renderPlotly({
    p <- ggplot(datawconstant, aes_string(x=input$xConstant, y=input$yConstant, colour="Construct")) +
      geom_point(aes(text=paste("\nTime (weeks): ", Time, "\nBuffer:", Buffer)), size=2) + mypltTheme
    ggplotly(p)
  })
  
  cordata <- eventReactive(input$run, {
    cordat <- dataw %>%
                filter(Region == input$corData &
                       Construct %in% input$constructs)
    cordat <- cordat[colSums(!is.na(cordat)) > 0]
    cordat[is.na(cordat)] <- 0
    cordat
  })
  
  output$corPlot <- renderPlot({
    ggpairs(
      cordata(), 5:length(colnames(cordata())), mapping = aes_string(colour="Buffer"),
      upper = list(continuous = wrap("cor", size = 4, alignPercent = 1)),
      lower = list(continuous = wrap("points", size=3)), 
      diag = list(continuous = wrap(ggally_diagAxis,
                                    labelSize = 4,
                                    gridLabelSize=2)),
      title = "Correlation Matrix",
      showStrips = F
    ) + theme_bw(base_size = 12)
  })
    
  # cordata <- eventReactive(input$run, {
  #     cordat <- filter(dataw, Region == input$corData)
  #     cordat <- cordat[colSums(!is.na(cordat)) > 0]
  #     cordat <- cordat[4:length(colnames(cordat))]
  #     cordat[is.na(cordat)] <- 0
  #     cordat
  #   })
  # 
  # output$corPlot <- renderPlot({
  #   ggpairs(cordata(), columns = 2:length(colnames(cordata())), aes_string(colour="Buffer"),
  #           upper = list(
  #             continuous = wrap("cor", size = 4, alignPercent = 1)
  #           )
  #           ) +
  #     theme_bw()
  # })
  
  output$tableAll <- DT::renderDataTable(dataw, options = list(
    pageLength=50
  ))
  output$table28 <- DT::renderDataTable(datawcd28, options = list(
    pageLength=50
  ))
  output$table38 <- DT::renderDataTable(datawcd38, options = list(
    pageLength=50
  ))
  output$table3 <- DT::renderDataTable(datawcd3, options = list(
    pageLength=50
  ))
  output$tableConstant <- DT::renderDataTable(datawconstant, options = list(
    pageLength=50
  ))
  
  output$downloadCD28 <- downloadHandler(
    filename = function() {
      paste("CD28data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datawcd28, file, row.names = FALSE)
    }
  )
  
  output$downloadCD38 <- downloadHandler(
    filename = function() {
      paste("CD38data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datawcd38, file, row.names = FALSE)
    }
  )
  
  output$downloadCD3 <- downloadHandler(
    filename = function() {
      paste("CD3data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datawcd3, file, row.names = FALSE)
    }
  )
  output$downloadConstant <- downloadHandler(
    filename = function() {
      paste("Constantdata", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datawconstant, file, row.names = FALSE)
    }
  )
  output$downloadAll <- downloadHandler(
    filename = function() {
      paste("Alldata", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataw, file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

