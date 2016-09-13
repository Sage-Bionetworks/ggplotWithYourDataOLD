
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Hello User!"),
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(
        tabPanel("Inputs", 
                 tagList(
                   singleton(tags$head(tags$script(src='//cdn.datatables.net/1.10.7/js/jquery.dataTables.min.js',
                                                   type='text/javascript'))),
                   singleton(tags$head(tags$script(src='//cdn.datatables.net/tabletools/2.2.4/js/dataTables.tableTools.min.js',
                                                   type='text/javascript'))),
                   singleton(tags$head(tags$script(src='//cdn.datatables.net/colreorder/1.1.3/js/dataTables.colReorder.min.js',type='text/javascript'))),
                   singleton(tags$head(tags$script(src='colvis.js',type='text/javascript'))),
                   singleton(tags$head(tags$script(src='//cdn.datatables.net/tabletools/2.2.4/js/ZeroClipboard.min.js',type='text/javascript'))),
                   singleton(tags$head(tags$link(href='//cdn.datatables.net/tabletools/2.2.4/css/dataTables.tableTools.css',rel='stylesheet',type='text/css'))),
                   singleton(tags$script(HTML("if (window.innerHeight < 400) alert('Screen too small');"))),
                   tags$head( tags$style(HTML("
                                              .cvclear{
                                              text-align:right}"))) 
                 ),
                 
                 #checkboxInput('example', 'Use Example Data',value=TRUE),
                 
                 textInput("tableId", "Input a Synapse ID of a Table", value=NULL),
                 actionButton('getTable', 'Get Table'),
                 #  uiOutput("dilution"),
                 #   uiOutput("keyvar"),
                 #  uiOutput("stratify"),
                 # uiOutput("stratifyvar"),
                 # uiOutput("samplefraction"),
                 
                 
                 uiOutput("ycol"),
                 uiOutput("xcol"),
                 tabsetPanel(id = "filtercategorize",
                             tabPanel("Filters", 
                                      uiOutput("maxlevels"),
                                      uiOutput("filtervar1"),
                                      uiOutput("filtervar1values"),
                                      uiOutput("filtervar2"),
                                      uiOutput("filtervar2values"),
                                      uiOutput("filtervar3"),
                                      uiOutput("filtervar3values"),
                                      uiOutput("filtervarcont1"),
                                      uiOutput("fslider1"),
                                      uiOutput("filtervarcont2"),
                                      uiOutput("fslider2"),
                                      uiOutput("filtervarcont3"),
                                      uiOutput("fslider3")
                             ),
                             tabPanel("Categorize", 
                                      uiOutput("catvar"),
                                      uiOutput("ncuts"),
                                      uiOutput("catvar2"),
                                      uiOutput("catvar3"),
                                      uiOutput("ncuts2")
                             ),
                             
                             tabPanel("Combine Variables", 
                                      uiOutput("pastevar")
                             )
                             
                 ),
                 hr()
        ), # tabpanel
        tabPanel("Graph Options",
                 column(6,
                        checkboxInput('logy', 'Log Y axis', value = FALSE) ,
                        checkboxInput('logx', 'Log X axis', value = FALSE),
                        hr()),
                 column(6,
                        conditionalPanel(condition = "!input.logy" ,
                                         checkboxInput('scientificy', 'Comma separated Y axis ticks', value = FALSE) ),
                        conditionalPanel(condition = "!input.logx" ,
                                         checkboxInput('scientificx', 'Comma separated X axis ticks', value = FALSE))
                        ,hr())
                 ,
                 textInput('ylab', 'Y axis label', value = "") ,
                 textInput('xlab', 'X axis label', value = "") ,
                 selectInput('backgroundcol', label ='Background Color',
                             choices=c("Gray" ="gray97","White"="white","Dark Gray"="grey90"),
                             multiple=FALSE, selectize=TRUE,selected="gray97"),
                 selectInput('legendposition', label ='Legend Position',
                             choices=c("left", "right", "bottom", "top"),
                             multiple=FALSE, selectize=TRUE,selected="bottom"),
                 #selectInput('facetscales','Facet Scales:',c("fixed","free_x","free_y","free")),
                 uiOutput("facetscales"),
                 selectInput('facetspace' ,'Facet Spaces:',c("fixed","free_x","free_y","free")),
                 checkboxInput('facetwrap', 'Use facet_wrap ?'),
                 conditionalPanel(condition = "input.facetwrap" ,
                                  numericInput("wrapncol",label = "N columns",value = 2,min=1,max =10) ,
                                  numericInput("wrapnrow",label = "N rows",value = 2,min=1,max=10) 
                 ),
                 h4("Reference Lines ?"),
                 checkboxInput('identityline', 'Identity Line')    ,   
                 checkboxInput('horizontalzero', 'Horizontol Zero Line'),
                 checkboxInput('customline1', 'Vertical Line'),
                 conditionalPanel(condition = "input.customline1" , 
                                  numericInput("vline",label = "",value = 1) ),
                 checkboxInput('customline2', 'Horizontal Line'),
                 conditionalPanel(condition = "input.customline2" , 
                                  numericInput("hline",label = "",value = 1) ),
                 h4("Additional Themes Options ?"),
                 checkboxInput('themebw', 'Use ggplot Black and White Theme ?')   , 
                 checkboxInput('themeaspect', 'Use custom aspect ratio ?')   ,  
                 conditionalPanel(condition = "input.themeaspect" , 
                                  numericInput("aspectratio",label = "Y/x ratio",value = 1) ),
                 checkboxInput('sepguides', 'Separate Legend Guides for Median/PI ?',value = TRUE),       
                 checkboxInput('labelguides', 'Hide the Names of the Guides ?',value = FALSE)       
        ),
        
        tabPanel("How To",
                 h5("1. Upload your data file in CSV format. R default options for read.csv will apply except for missing values where both (NA) and dot (.) are treated as missing. If your data has columns with other non-numeric missing value codes then they be treated as factors."),
                 h5("2. It is assumed that your data is ready for ggplot (long format)."),                                   h5("3. x and y variable(s) input allow numeric and factor variables."),
                 h5("4. Changed ! Sliders for x and y variables were removed. Use the Filter variables tab to select up to 6 different filters."),  
                 h5("5. New ! You can now select more than one y variable. The data will be automatically stacked using tidyr::gather and result in yvars and yvalues variables. if you select factor and continuous variables data will be transformed to factor. The internal variable yvalues is used for y variable mapping and you can select yvars for facetting. The app automatically select additional row split as yvars and set Facet Scales to free_y. To change Facet Scales and many other options go to Graph Options tab."),
                 h5("6. The UI is dynamic and changes depending on your choices of options, x ,y, filter, group and so on."),
                 h5("7. There is now six slots for Filter variables. Use these to apply exclusions on your data."),
                 h5("8. New ! Filter variables 1, 2,3,4,5 and 6 are applied sequentially. Values shown for filter variable 2 will depend on your selected data exclusions using filter variable 1 and so on. The first three filters accept numeric and non numeric columns while the last three are sliders and only work with numeric variables."),
                 h5("9. If you want to change a numerical variable to categorical include it in the list of Categorical variables and then choose a number of cuts (default is 3). This helps when you want to group or color by cuts of a continuous variable."),
                 h5("10. You can simply change a numeric variable to factor without binning in the treat as categories slot."),
                 h5("11. New ! You can cut a numeric variable to factor using specified cutoffs, by default the min, median, max are used."),
                 h5("12. New ! You can additional smoothing functions added including linear fit and logistic user needs to make sure that data is compatible with the smoothing used i.e for logistic a 0/1 variable is expected."),
                 h5("13. Initial support to include boxplots and allow facet wrap is now on. More work needed. Especially that grouping does not work when x axis variable is not continuous and that facet wrap breaks when formula has dots."),
                 h5("14. Download the plot using the options on the 'Download' tab. This section is based on code from Mason DeCamillis ggplotLive app."),
                 h5("15. Visualize the data table in the 'Data' tab. You can reorder the columns, filter and much more."),
                 p(),
                 h5("Samer Mouksassi 2016"),
                 h5("Contact me @ samermouksassi@gmail.com for feedback/Bugs/features requests!")
                 
        )# tabpanel 
      )
    ), #sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel("Plot"  , 
                 plotOutput('plot',  width = "100%" ,click = "plot_click",
                            hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                            brush = brushOpts(id = "plot_brush")),
                 #verbatimTextOutput("plotinfo"),
                 hr(),
                 uiOutput("clickheader"),
                 tableOutput("plot_clickedpoints"),
                 uiOutput("brushheader"),
                 tableOutput("plot_brushedpoints"),
                 #actionButton("plotButton", "Update Plot"),
                 uiOutput("optionsmenu") ,
                 
                 conditionalPanel(
                   condition = "input.showplottypes" , 
                   
                   fluidRow(
                     
                     column (12, hr()),
                     column (3,
                             radioButtons("Points", "Points/Jitter:",
                                          c("Points" = "Points",
                                            "Jitter" = "Jitter",
                                            "None" = "None")),
                             conditionalPanel( " input.Points!= 'None' ",
                                               sliderInput("pointstransparency", "Points Transparency:", min=0, max=1, value=c(0.5),step=0.01))
                     ),
                     column(3,
                            conditionalPanel( " input.Points!= 'None' ",
                                              sliderInput("pointsizes", "Points Size:", min=0, max=4, value=c(1),step=0.1),
                                              numericInput('pointtypes','Points Type:',16, min = 1, max = 25)
                            )),
                     
                     column (3,
                             
                             radioButtons("line", "Lines:",
                                          c("Lines" = "Lines",
                                            "None" = "None"),selected="None"),
                             conditionalPanel( " input.line== 'Lines' ",
                                               sliderInput("linestransparency", "Lines Transparency:", min=0, max=1, value=c(0.5),step=0.01),
                                               checkboxInput('lineignorecol', 'Ignore Mapped Color')
                             )
                             
                     ),
                     column(3,
                            conditionalPanel( " input.line== 'Lines' ",
                                              sliderInput("linesize", "Lines Size:", min=0, max=4, value=c(1),step=0.1),
                                              selectInput('linetypes','Lines Type:',c("solid","dotted")),
                                              conditionalPanel( " input.lineignorecol ",
                                                                selectInput('colline', label ='Lines Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") 
                                              )
                            ),
                            checkboxInput('boxplotaddition', 'Add a Boxplot ? (makes sense if x variable is categorical and you Group By using it, other features might not work on top of boxplot e.g loess)')
                            
                     ),
                     column (12, h6("Points and Lines Size will apply only if Size By: in the Color Group Split Size Fill Mappings are set to None"))
                     
                   )#fluidrow
                 ) ,
                 conditionalPanel(
                   condition = "input.showfacets" , 
                   fluidRow(
                     column (12, hr()),
                     column (3, uiOutput("colour"),uiOutput("group")),
                     column(3, uiOutput("facet_col"),uiOutput("facet_row")),
                     column (3, uiOutput("facet_col_extra"),uiOutput("facet_row_extra")),
                     column (3, uiOutput("pointsize"),uiOutput("fill")),
                     column (12, h6("Make sure not to choose a variable that is in the y variable(s) list otherwise you will get an error Variable not found. These variables are stacked and become yvars and yvalues." ))
                     
                   )
                 ),
                 
                 
                 
                 
                 #rqss quantile regression
                 conditionalPanel(
                   condition = "input.showrqss" , 
                   
                   fluidRow(
                     column(12,hr()),
                     column(3,
                            checkboxInput('Tauvalue', 'Dynamic and Preset Quantiles', value = FALSE),
                            h5("Preset Quantiles"),
                            checkboxInput('up', '95%'),
                            checkboxInput('ninetieth', '90%'),
                            checkboxInput('mid', '50%', value = FALSE),
                            checkboxInput('tenth', '10%'),
                            checkboxInput('low', '5%')
                     ),
                     column(5,
                            sliderInput("Tau", label = "Dynamic Quantile Value:",
                                        min = 0, max = 1, value = 0.5, step = 0.01)  ,
                            sliderInput("Penalty", label = "Spline sensitivity adjustment:",
                                        min = 0, max = 10, value = 1, step = 0.1)  ,
                            selectInput("Constraints", label = "Spline constraints:",
                                        choices = c("N","I","D","V","C","VI","VD","CI","CD"), selected = "N")
                     ),
                     column(3,
                            checkboxInput('ignorecolqr', 'Ignore Mapped Color'),
                            checkboxInput('ignoregroupqr', 'Ignore Mapped Group',value = TRUE),
                            checkboxInput('hidedynamic', 'Hide Dynamic Quantile'),
                            selectInput('colqr', label ='QR Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                     )
                     
                   )#fluidrow
                 ),
                 
                 conditionalPanel(
                   condition = "input.showSmooth" , 
                   
                   fluidRow(
                     column(12,hr()),
                     column (3, 
                             radioButtons("Smooth", "Smooth:",
                                          c("Smooth" = "Smooth",
                                            "Smooth and SE" = "Smooth and SE",
                                            "None" = "None"),selected="None")
                     ),
                     column (3, 
                             conditionalPanel( " input.Smooth!= 'None' ",
                                               selectInput('smoothmethod', label ='Smoothing Method',
                                                           choices=c("Loess" ="loess","Linear Fit"="lm","Logistic"="glm"),
                                                           multiple=FALSE, selectize=TRUE,selected="loess"),
                                               
                                               sliderInput("loessens", "Loess Span:", min=0, max=1, value=c(0.75),step=0.05)
                             ) 
                     ),
                     
                     
                     
                     
                     
                     column (3,  conditionalPanel( " input.Smooth!= 'None' ",
                                                   checkboxInput('ignorecol', 'Ignore Mapped Color'),
                                                   uiOutput("weight")
                     )
                     ),
                     column (3, conditionalPanel( " input.Smooth!= 'None' ",
                                                  checkboxInput('ignoregroup', 'Ignore Mapped Group',value = TRUE),
                                                  selectInput('colsmooth', label ='Smooth Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black")
                     ) )
                     
                   )#fluidrow
                 )
                 ,
                 ### Mean CI section
                 conditionalPanel(
                   condition = "input.showMean" , 
                   
                   fluidRow(
                     column(12,hr()),
                     column (3, 
                             radioButtons("Mean", "Mean:",
                                          c("Mean" = "Mean",
                                            "Mean (95% CI)" = "Mean (95% CI)",
                                            "None" = "None") ,selected="None") 
                     ),
                     column (3,
                             
                             conditionalPanel( " input.Mean== 'Mean (95% CI)' ",
                                               sliderInput("CI", "CI %:", min=0, max=1, value=c(0.95),step=0.01),
                                               numericInput( inputId = "errbar",label = "CI bar width:",value = 2,min = 1,max = NA)      
                             )
                             
                     )
                     ,
                     column (3,
                             conditionalPanel( " input.Mean!= 'None' ",
                                               checkboxInput('meanpoints', 'Show points') ,
                                               checkboxInput('meanlines', 'Show lines', value=TRUE),
                                               checkboxInput('meanignorecol', 'Ignore Mapped Color') ,
                                               conditionalPanel( " input.meanignorecol ",
                                                                 selectInput('colmean', label ='Mean Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") )
                                               
                             ) ),
                     
                     
                     column(3,
                            conditionalPanel( " input.Mean!= 'None' ",
                                              checkboxInput('meanignoregroup', 'Ignore Mapped Group',value = TRUE),
                                              sliderInput("meanlinesize", "Mean(s) Line(s) Size:", min=0, max=3, value=1,step=0.05)
                            ) 
                     )
                   ) #fluidrow
                 ), # conditional panel for mean
                 
                 ### median PI section
                 
                 
                 conditionalPanel(
                   condition = "input.showMedian" , 
                   fluidRow(
                     column(12,hr()),
                     column (3,
                             radioButtons("Median", "Median:",
                                          c("Median" = "Median",
                                            "Median/PI" = "Median/PI",
                                            "None" = "None") ,selected="None") 
                     ),
                     column (3,
                             conditionalPanel( " input.Median== 'Median' ",
                                               checkboxInput('medianpoints', 'Show points') ,
                                               checkboxInput('medianlines', 'Show lines',value=TRUE)),
                             conditionalPanel( " input.Median== 'Median/PI' ",
                                               sliderInput("PI", "PI %:", min=0, max=1, value=c(0.95),step=0.01),
                                               sliderInput("PItransparency", "PI Transparency:", min=0, max=1, value=c(0.2),step=0.01)
                             )
                     ),
                     column (3,
                             conditionalPanel( " input.Median!= 'None' ",
                                               checkboxInput('medianignorecol', 'Ignore Mapped Color'),
                                               conditionalPanel( " input.medianignorecol ",
                                                                 selectInput('colmedian', label ='Median Color', choices=colors(),multiple=FALSE, selectize=TRUE,selected="black") )
                                               
                             ) ),
                     column (3,
                             conditionalPanel( " input.Median!= 'None' ",
                                               
                                               checkboxInput('medianignoregroup', 'Ignore Mapped Group',value = TRUE),
                                               sliderInput("medianlinesize", "Median(s) Line(s) Size:", min=0, max=4, value=c(1),step=0.1)
                                               
                             )
                     )
                     
                   )#fluidrow
                 )
                 ### median PI section
                 
        ),#tabPanel1
        tabPanel("Download", 
                 selectInput(
                   inputId = "downloadPlotType",
                   label   = h5("Select download file type"),
                   choices = list("PDF"  = "pdf","BMP"  = "bmp","JPEG" = "jpeg","PNG"  = "png")),
                 
                 # Allow the user to set the height and width of the plot download.
                 h5(HTML("Set download image dimensions<br>(units are inches for PDF, pixels for all other formats)")),
                 numericInput(
                   inputId = "downloadPlotHeight",label = "Height (inches)",value = 7,min = 1,max = 100),
                 numericInput(
                   inputId = "downloadPlotWidth",label = "Width (inches)",value = 7,min = 1,max = 100),
                 # Choose download filename.
                 textInput(
                   inputId = "downloadPlotFileName",
                   label = h5("Enter file name for download")),
                 
                 # File downloads when this button is clicked.
                 downloadButton(
                   outputId = "downloadPlot", 
                   label    = "Download Plot")
        ),
        
        tabPanel('Data',  dataTableOutput("mytablex") 
        )#tabPanel2
      )#tabsetPanel
    )#mainPanel
  )#sidebarLayout
)#fluidPage
)
