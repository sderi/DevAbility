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
library(grid)

source("funcsApp.R")

molRegions <- as.character(unique(dataw$Region))
molConstructs <- as.character(unique(dataw$Construct))

datawcd28 <- cleandata(dataw, datawcd28, molRegions[1])
datawcd38 <- cleandata(dataw, datawcd38, molRegions[2])
datawcd3 <- cleandata(dataw, datawcd3, molRegions[3])
datawconstant <- cleandata(dataw, datawconstant, molRegions[4])



ui <- fluidPage(
  navbarPage("Characterization Developability Analytics - tsCD38 ", inverse=TRUE,
             tabPanel("Modifications",
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
                                                    choices = as.vector(unique(dataw$Region))
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
                                    tabPanel(molRegions[1],
                                             sidebarLayout(
                                               sidebarPanel(width = 3,
                                                            h2(paste(molRegions[1], "Modifications")),
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
                                    tabPanel(molRegions[2],
                                             sidebarLayout(
                                               sidebarPanel(width = 3,
                                                            h2(paste(molRegions[2],"Modifications")),
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
                                    tabPanel(molRegions[3],
                                             sidebarLayout(
                                               sidebarPanel(width = 3,
                                                            h2(paste(molRegions[3], "Modifications")),
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
                                    tabPanel(molRegions[4],
                                             sidebarLayout(
                                               sidebarPanel(width = 3,
                                                            h2(paste(molRegions[4], "Modifications")),
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
             navbarMenu("Linear Regression",
                        tabPanel("Summary",
                                 fluidRow(
                                   sidebarLayout(
                                     sidebarPanel(width = 2,
                                                  h4("Changes plots and tables"),
                                                  radioButtons('linregion', "Select Region",
                                                               choices = unique(dataw$Region)),
                                                  radioButtons('norm', "Select Data Transformation",
                                                               choices = c(
                                                                 "Raw" = 0,
                                                                 "Log2" = 1
                                                               ),
                                                               selected = 0),
                                                  br(), hr(),
                                                  h4("Changes plots only"),
                                                  selectInput('linmods', "Select Modifications",
                                                              choices = colnames(dataw[5:length(colnames(dataw))])),
                                                  h4("Change color scale"),
                                                  radioButtons('direction', "Data Direction",
                                                               choices = c(
                                                                 "Increasing" = -1,
                                                                 "Decreasing" = 1
                                                               ),
                                                               selected = -1)
                                     ),
                                     mainPanel(width = 10, plotOutput('linSum', height = "600px"))
                                   )
                                 ),
                                 h2("Data tables"),
                                 textInput('textRegion', label = NULL),
                                 textInput('textData', label = NULL),
                                 tabsetPanel(
                                   tabPanel("Linear Regression",
                                            br(),
                                            downloadButton("downloadLinReg", "Download table below"),
                                            br(), br(),
                                            dataTableOutput("tableLinReg")),
                                   tabPanel("Slopes R-squared",
                                            br(),
                                            downloadButton("downloadSR2", "Download table below"),
                                            br(), br(),
                                            div(dataTableOutput("tableSR2"), style = 'width:900px'))
                                 )
                        ),
                        tabPanel("Heat maps")
             ),
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
                                                      molRegions,
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
                      tabPanel(molRegions[1],
                               br(),
                               downloadButton('dtreg1', "Download table below"),
                               br(),
                               br(),
                               dataTableOutput("tablereg1")),
                      tabPanel(molRegions[2],
                               br(),
                               downloadButton("dtreg2", "Download table below"),
                               br(),
                               br(),
                               dataTableOutput("tablereg2")),
                      tabPanel(molRegions[3],
                               br(),
                               downloadButton("dtreg3", "Download table below"),
                               br(),
                               br(),
                               dataTableOutput("tablereg3")),
                      tabPanel(molRegions[4],
                               br(),
                               downloadButton("dtreg4", "Download table below"),
                               br(),
                               br(),
                               dataTableOutput("tablereg4")),
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


server <- function(input, output, session) {
  
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
      upper = list(continuous = wrap("cor", size = 5, alignPercent = 1)),
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
  lrTheme <- theme_linedraw() +
    theme(
      plot.title = element_text(size=16, face='bold'),
      strip.background = element_rect(fill="#595959"),
      panel.background = element_rect(fill="#f9f9f9"),
      panel.grid.major = element_line(color="grey"),
      panel.grid.minor = element_line(color="grey"),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=16, face='bold'),
      axis.text.y = element_text(size=16, face='bold'),
      strip.text.x = element_text(size=18, face='bold'),
      legend.text = element_text(size=16, face='bold'),
      legend.title = element_text(size=16, face='bold'),
      legend.position = "top"
    )
  
  hmTheme <- theme_linedraw() +
    theme(
      plot.title = element_text(size=16, face='bold'),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=12, face='bold'),
      axis.text.y = element_text(size=16, face='bold'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.text = element_text(size=14, face='bold'),
      legend.title = element_text(size=14, face='bold')
    )
  
  linregdf <- reactive({
    df <- cleanDF(dataw, input$linregion, input$norm)
    df
  })
  
  hmdf <- reactive({
    df <- linReg(dataw, input$linregion, input$norm)
    df <- filter(df, Attribute == input$linmods)
    df
  })
  
  output$linSum <- renderPlot({
    
    dataw$Time <- factor(dataw$Time)
    bp <- ggplot(dataw, aes_string(x = "Construct", y = input$linmods, fill = "Time")) +
      geom_bar(stat = "identity", position = position_dodge(), color = "black") +
      geom_hline(yintercept = 0, color="black") +
      labs(x=NULL, title= "Raw Data") +
      lrTheme
    barp <- bp + facet_wrap(~Buffer)
    
    print(head(linregdf()))
    lr <- ggplot(linregdf(), aes_string("Time", input$linmods, color = "Construct")) +
      geom_point(size=2.0) +
      geom_smooth(method = "lm", se =F, size = 1.2) +
      labs(title="Linear Regression", color = NULL) +
      lrTheme
    lrp <- lr + facet_grid(. ~ Buffer)
    
    hp <- ggplot(hmdf(), aes(Construct, Buffer)) + 
      geom_tile(aes(fill = Slope), color = "black") +
      geom_text(aes(label=paste("Slope: ",round(Slope,3))), fontface='bold', size = 6, show.legend = F) +
      geom_text(aes(label=paste("\n" , "\n" , "R2: ", round(R.squared,3))), fontface='bold', size = 6, show.legend = F) +
      scale_fill_distiller(palette = "Spectral", direction = input$direction) +
      labs(title="Heat Map", x=NULL, y=NULL) +
      hmTheme
    
    grid.arrange(lrp, hp,
                 #arrangeGrob(barp, lrp, nrow = 2), hp, 
                 ncol = 2,
                 top=textGrob(paste(input$linmods, "\n"),gp=gpar(fontsize=36,font=2)))
    
  })
  
  observe({
    if (input$norm == 0){
      dtext <- "Raw"
    } else if (input$norm == 1) {
      dtext <- "Log2 Normalized"
    } else
      dtext <- "unknown data type"
    
    regText <- input$linregion
    
    updateTextInput(session, "textRegion", value = paste("Selected region:", regText))
    updateTextInput(session, 'textData', value = paste("Data type selected:", dtext))
  })
  
  output$tableLinReg <- DT::renderDataTable(linregdf(), options = list(
    pageLength=50
  ))
  
  output$downloadLinReg <- downloadHandler(
    filename = function() {
      paste("LinearRegression", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(linregdf(), file, row.names = FALSE)
    }
  )
  
  hmtdf <- reactive({
    df <- linReg(dataw, input$linregion, input$norm)
    df$Slope <- format(round(df$Slope, 4), nsmall=4)
    df$R.squared <- format(round(df$R.squared, 4), nsmall=4)
    df
  })
  
  output$tableSR2 <- DT::renderDataTable(hmtdf(), options = list(
    autoWidth = FALSE,
    pageLength=50
  ))
  
  output$downloadSR2 <- downloadHandler(
    filename = function() {
      paste("LinearRegression", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(hmtdf(), file, row.names = FALSE)
    }
  )
  
  output$tableAll <- DT::renderDataTable(dataw, options = list(
    pageLength=50
  ))
  output$tablereg1 <- DT::renderDataTable(datawcd28, options = list(
    pageLength=50
  ))
  output$tablereg2 <- DT::renderDataTable(datawcd38, options = list(
    pageLength=50
  ))
  output$tablereg3 <- DT::renderDataTable(datawcd3, options = list(
    pageLength=50
  ))
  output$tablereg4 <- DT::renderDataTable(datawconstant, options = list(
    pageLength=50
  ))
  
  output$dtreg1 <- downloadHandler(
    filename = function() {
      paste(molRegions[1], "data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(paste("download", molRegions[1], sep = "_"), file, row.names = FALSE)
    }
  )
  
  output$dtreg2 <- downloadHandler(
    filename = function() {
      paste(molRegions[2], "data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(paste("download", molRegions[2], sep = "_"), file, row.names = FALSE)
    }
  )
  
  output$dtreg3 <- downloadHandler(
    filename = function() {
      paste(molRegions[3], "data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(paste("download", molRegions[3], sep = "_"), file, row.names = FALSE)
    }
  )
  output$dtreg4 <- downloadHandler(
    filename = function() {
      paste(molRegions[4], "data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(paste("download", molRegions[4], sep = "_"), file, row.names = FALSE)
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

