
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output, session) {
  filedata <- eventReactive(input$getTable, {
    
    tableId <- input$tableId
    
    if (is.null(tableId)) {
      # User has not input a table id
      return(NULL)
    }
    tblObj <- synTableQuery(sprintf('select * from %s', tableId))
    tblObj@values
    
  })
  
  
  
  myData <- reactive({
    df=filedata()
    if (is.null(df)) return(NULL)
  })
  
  output$optionsmenu <-  renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    fluidRow(
      column (12, h6("Select the checkbox(es) for the options to be showed")),
      hr(),
      column(4,checkboxInput('showplottypes','Plot types, Points, Lines (?)',
                             value = TRUE)),
      column(4,checkboxInput('showfacets','Color/Group/Split/Size/Fill Mappings (?)',
                             value = TRUE) ),
      column(4,checkboxInput('showrqss', 'Quantire Regresssion (?)',
                             value = TRUE)),
      column(4,checkboxInput('showSmooth', 'Smooth SE (?)', value = TRUE)),
      column(4,checkboxInput('showMean' , 'Mean CI (?)', value = FALSE)),
      column(4,checkboxInput('showMedian','Median PIs (Show ?)', value = FALSE))
    )
  })
  
  
  output$dilution <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    checkboxInput('dilutionin', 'Sub-Sample data ?', value = FALSE)
    
  })
  output$stratify <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    conditionalPanel(condition = "input.dilutionin" ,
                     checkboxInput('stratifyon', 'Stratify On ?', value = FALSE) )
    
  })
  output$samplefraction <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    conditionalPanel(condition = "input.dilutionin" ,
                     sliderInput("samplefractionin", "Fraction to Keep ?", min=1, max=100, value=c(10),step=1)
    )                     
  })
  
  
  
  output$keyvar <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    conditionalPanel(condition = "input.dilutionin" ,
                     selectizeInput(  "keyvarin", "Use this variable as a sampling unit:", choices = NAMESTOKEEP2,multiple=TRUE,
                                      options = list(
                                        maxItems = 1 ,
                                        placeholder = 'Please select one variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      )
                     ))
  })
  
  output$stratifyvar <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    conditionalPanel(condition = "input.stratifyon" ,
                     selectizeInput(  "stratifyvarin", "Stratify on up to three categorical variables:", choices = NAMESTOKEEP2,multiple=TRUE,
                                      options = list(
                                        maxItems = 3 ,
                                        placeholder = 'Please select some variables',
                                        onInitialize = I('function() { this.setValue(""); }')
                                      ))
    )
  })
  
  #       samplefilterdata  <- reactive({
  #         if (is.null(filedata())) return(NULL)
  #        df <- filedata()
  #        if (is.null(df)) return(NULL)
  #        if(is.null(input$dilutionin)) {
  #          df <-  df 
  #     }
  #      if(input$dilutionin) {
  # df <-  resample_df(dataplot,key_cols=input$stratifyvarin)
  #                          n=100, replace = FALSE)
  #       }
  #      if(input$dilutionin&input$stratifyon) {
  #           df <-  resample_df(dataplot,key_cols=input$keyvarin, strat_cols=input$stratifyvarin)
  #                          n=100, replace = FALSE)
  #     }
  # 
  #  df
  #      })
  
  
  output$ycol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("y", "y variable(s):",choices=items,selected = items[1],multiple=TRUE,selectize=TRUE)
  })
  
  output$xcol <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("x", "x variable:",items,selected=items[2])
    
  })
  
  output$maxlevels <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    numericInput( inputId = "inmaxlevels",label = "Max number of unique values for filter variables (1,2,3):",value = 500,min = 1,max = NA)
    
  })
  
  
  output$filtervar1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    selectInput("infiltervar1" , "Filter variable (1):",c('None',NAMESTOKEEP ) )
  })
  
  output$filtervar1values <- renderUI({
    df <-filedata()
    validate(       need(!is.null(df), "Please select a data set"))
    
    if (is.null(df)) return(NULL)
    if(input$infiltervar1=="None") {return(NULL)}
    if(input$infiltervar1!="None" )  {
      choices <- levels(as.factor(df[,input$infiltervar1]))
      selectInput('infiltervar1valuesnotnull',
                  label = paste("Select values", input$infiltervar1),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=FALSE)   
    }
  })  
  
  
  subsetdata  <- reactive({
    df <- filedata()
    #     if (is.null(df)) return(NULL)
    #     xvariable<- input$x
    #     yvariable<- input$y
    #         
    #     if(!is.null(input$inSlidercat)& !is.numeric(df[,xvariable]) ) {
    #  df <-  df [ is.element(df[,input$x],input$inSlidercat),]
    #     }
    #     if(!is.null(input$inSlidercat2)& !is.numeric(df[,yvariable])) {
    #  df <-  df [ is.element(df[,input$y],input$inSlidercat2),]
    #     }
    #    
    #     if(!is.null(input$inSlider)&is.numeric(df[,xvariable])) {
    #     if(is.numeric( input$inSlider[1]) & is.numeric(df[,input$x]))
    #       {df <- df [!is.na(df[,input$x]),]
    #        df <- df [df[,input$x] >= input$inSlider[1]&df[,input$x] <= input$inSlider[2],]
    #       }
    #     }
    #     if(!is.null(input$inSlider2)&is.numeric(df[,yvariable])) {
    #     if(is.numeric( input$inSlider2[1])& is.numeric(df[,input$y]) ) {
    #       df<- df [!is.na(df[,input$y]),]
    #       df <-  df [df[,input$y] >= input$inSlider2[1]&df[,input$y] <= input$inSlider2[2],]
    #     }  
    #     }
    
    df
  }) 
  output$filtervar2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    NAMESTOKEEP<-  NAMESTOKEEP[ NAMESTOKEEP!=input$infiltervar1 ]
    
    selectInput("infiltervar2" , "Filter variable (2):",c('None',NAMESTOKEEP ) )
  })
  
  output$filtervar3 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  [ NUNIQUEDF  < input$inmaxlevels ]
    NAMESTOKEEP<-  NAMESTOKEEP[ NAMESTOKEEP!=input$infiltervar1 ]
    NAMESTOKEEP<-  NAMESTOKEEP[ NAMESTOKEEP!=input$infiltervar2 ]
    
    selectInput("infiltervar3" , "Filter variable (3):",c('None',NAMESTOKEEP ) )
  })
  
  
  output$filtervarcont1 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont1" , "Filter continuous (1):",c('None',NAMESTOKEEP ) )
  })
  output$filtervarcont2 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont2" , "Filter continuous (2):",c('None',NAMESTOKEEP ) )
  })
  output$filtervarcont3 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    NUNIQUEDF <- sapply(df, function(x) length(unique(x)))
    NAMESTOKEEP<- names(df)  
    NAMESTOKEEP<- NAMESTOKEEP[ is.element ( NAMESTOKEEP,names(df[sapply(df,is.numeric)]))]
    selectInput("infiltervarcont3" , "Filter continuous (3):",c('None',NAMESTOKEEP ) )
  })
  
  filterdata  <- reactive({
    if (is.null(filedata())) return(NULL)
    df <- subsetdata()
    if (is.null(df)) return(NULL)
    if(is.null(input$infiltervar1)) {
      df <-  df 
    }
    if(!is.null(input$infiltervar1)&input$infiltervar1!="None") {
      
      df <-  df [ is.element(df[,input$infiltervar1],input$infiltervar1valuesnotnull),]
    }
    
    df
  })
  
  output$filtervar2values <- renderUI({
    df <- filterdata()
    if (is.null(df)) return(NULL)
    if(input$infiltervar2=="None") {
      selectInput('infiltervar2valuesnull',
                  label ='No filter variable 2 specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$infiltervar2!="None"&!is.null(input$infiltervar2) )  {
      choices <- levels(as.factor(as.character(df[,input$infiltervar2])))
      selectInput('infiltervar2valuesnotnull',
                  label = paste("Select values", input$infiltervar2),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=TRUE)   
    }
  })
  
  filterdata2  <- reactive({
    df <- filterdata()
    if (is.null(df)) return(NULL)
    if(!is.null(input$infiltervar2)&input$infiltervar2!="None") {
      df <-  df [ is.element(df[,input$infiltervar2],input$infiltervar2valuesnotnull),]
    }
    if(input$infiltervar2=="None") {
      df 
    }
    df
  })
  output$filtervar3values <- renderUI({
    df <- filterdata2()
    if (is.null(df)) return(NULL)
    if(input$infiltervar3=="None") {
      selectInput('infiltervar3valuesnull',
                  label ='No filter variable 2 specified', 
                  choices = list(""),multiple=TRUE, selectize=FALSE)   
    }
    if(input$infiltervar3!="None"&!is.null(input$infiltervar3) )  {
      choices <- levels(as.factor(as.character(df[,input$infiltervar3])))
      selectInput('infiltervar3valuesnotnull',
                  label = paste("Select values", input$infiltervar3),
                  choices = c(choices),
                  selected = choices,
                  multiple=TRUE, selectize=TRUE)   
    }
  })
  
  filterdata3  <- reactive({
    df <- filterdata2()
    if (is.null(df)) return(NULL)
    if(!is.null(input$infiltervar3)&input$infiltervar3!="None") {
      df <-  df [ is.element(df[,input$infiltervar3],input$infiltervar3valuesnotnull),]
    }
    if(input$infiltervar3=="None") {
      df 
    }
    df
  })  
  
  output$fslider1 <- renderUI({ 
    df <-  filterdata3()
    if (is.null(df)) return(NULL)
    xvariable<- input$infiltervarcont1
    if(input$infiltervarcont1=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont1!="None" ){
      sliderInput("infSlider1", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  filterdata4  <- reactive({
    df <- filterdata3()
    if (is.null(df)) return(NULL)
    if(input$infiltervarcont1!="None" ){
      if(is.numeric( input$infSlider1[1]) & is.numeric(df[,input$infiltervarcont1])) {
        df <- df [!is.na(df[,input$infiltervarcont1]),]
        df <-  df [df[,input$infiltervarcont1] >= input$infSlider1[1]&df[,input$infiltervarcont1] <= input$infSlider1[2],]
      }
    }
    
    df
  })
  output$fslider2 <- renderUI({ 
    df <-  filterdata4()
    if (is.null(df)) return(NULL)
    xvariable<- input$infiltervarcont2
    if(input$infiltervarcont2=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont2!="None" ){
      sliderInput("infSlider2", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  
  
  filterdata5  <- reactive({
    df <- filterdata4()
    if (is.null(df)) return(NULL)
    if(input$infiltervarcont2!="None" ){
      if(is.numeric( input$infSlider2[1]) & is.numeric(df[,input$infiltervarcont2])) {
        df<- df [!is.na(df[,input$infiltervarcont2]),]
        df<-df [df[,input$infiltervarcont2] >= input$infSlider2[1]&df[,input$infiltervarcont2] <= input$infSlider2[2],]
      }
    }
    
    df
  })
  
  output$fslider3 <- renderUI({ 
    df <-  filterdata5()
    if (is.null(df)) return(NULL)
    xvariable<- input$infiltervarcont3
    if(input$infiltervarcont3=="None" ){
      return(NULL)  
    }
    if (!is.numeric(df[,xvariable]) ) return(NULL)
    if(input$infiltervarcont3!="None" ){
      sliderInput("infSlider3", paste("Select",xvariable,"Range"),
                  min=min(df[,xvariable],na.rm=T),
                  max=max(df[,xvariable],na.rm=T),
                  value=c(min(df[,xvariable],na.rm=T),max(df[,xvariable],na.rm=T)) 
      )
    }             
  })
  
  
  filterdata6  <- reactive({
    df <- filterdata5()
    if (is.null(df)) return(NULL)
    if(input$infiltervarcont3!="None" ){
      if(is.numeric( input$infSlider3[1]) & is.numeric(df[,input$infiltervarcont3])) {
        df<- df [!is.na(df[,input$infiltervarcont3]),]
        df<-df [df[,input$infiltervarcont3] >= input$infSlider3[1]&df[,input$infiltervarcont3] <= input$infSlider3[2],]
      }
    }
    
    df
  })
  
  
  output$catvar <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    selectInput('catvarin',label = 'Recode into Binned Categories:',choices=NAMESTOKEEP2,multiple=TRUE)
  })
  
  
  output$ncuts <- renderUI({
    if (length(input$catvarin ) <1)  return(NULL)
    sliderInput('ncutsin',label = 'N of Cut Breaks:', min=2, max=10, value=c(3),step=1)
  })
  
  output$catvar2 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    if (length(input$catvarin ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarin) ]
    }
    
    selectInput('catvar2in',label = 'Treat as Categories:',choices=NAMESTOKEEP2,multiple=TRUE)
    
  })
  
  output$catvar3 <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [ MODEDF ]
    if (length(input$catvarin ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvarin) ]
    }
    if (length(input$catvar2in ) >=1) {
      NAMESTOKEEP2<-NAMESTOKEEP2 [ !is.element(NAMESTOKEEP2,input$catvar2in) ]
    }
    selectizeInput(  "catvar3in", 'Custom cuts of this variable, defaults to min, median, max before any applied filtering:',
                     choices =NAMESTOKEEP2 ,multiple=FALSE,
                     options = list(    placeholder = 'Please select a variable',
                                        onInitialize = I('function() { this.setValue(""); }')
                     )
    )
  })
  output$ncuts2 <- renderUI({
    df <-filedata()
    if (length(input$catvar3in ) <1)  return(NULL)
    if ( input$catvar3in!=""){
      textInput("xcutoffs", label =  paste(input$catvar3in,"Cuts"),
                value = as.character(paste(
                  min(df[,input$catvar3in] ,na.rm=T),
                  median(df[,input$catvar3in],na.rm=T),
                  max(df[,input$catvar3in],na.rm=T) ,sep=",")
                )
      )
    }
    
  })
  outputOptions(output, "ncuts", suspendWhenHidden=FALSE)
  outputOptions(output, "ncuts2", suspendWhenHidden=FALSE)
  outputOptions(output, "catvar3", suspendWhenHidden=FALSE)
  outputOptions(output, "catvar2", suspendWhenHidden=FALSE)
  outputOptions(output, "fslider3", suspendWhenHidden=FALSE)
  
  recodedata1  <- reactive({
    df <- filterdata6() 
    if (is.null(df)) return(NULL)
    if(length(input$catvarin ) >=1) {
      for (i in 1:length(input$catvarin ) ) {
        varname<- input$catvarin[i]
        df[,varname] <- cut(df[,varname],input$ncutsin)
        df[,varname]   <- as.factor( df[,varname])
      }
    }
    df
  })
  
  
  recodedata2  <- reactive({
    df <- recodedata1()
    if (is.null(df)) return(NULL)
    if(length(input$catvar2in ) >=1) {
      for (i in 1:length(input$catvar2in ) ) {
        varname<- input$catvar2in[i]
        df[,varname]   <- as.factor( df[,varname])
      }
    }
    df
  })
  
  recodedata3  <- reactive({
    df <- recodedata2()
    if (is.null(df)) return(NULL)
    if(input$catvar3in!="") {
      for (i in 1:length(input$catvar3in ) ) {
        varname<- input$catvar3in[i]
        xlimits <- input$xcutoffs 
        nxintervals <- length(as.numeric(unlist (strsplit(xlimits, ",")) )) -1
        df[,varname] <- cut( as.numeric ( as.character(  df[,varname])),
                             breaks=   as.numeric(unlist (strsplit(xlimits, ","))),include.lowest=TRUE)
        #df[,varname] <- as.numeric(df[,varname] ) -1
        #xaxislabels <-levels(cut( as.numeric ( as.character( df[,varname])), breaks=   as.numeric(unlist (strsplit(xlimits, ",") ))))
      }
    }
    df
  })
  
  
  
  output$pastevar <- renderUI({
    df <-recodedata3()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    MODEDF <- sapply(df, function(x) is.numeric(x))
    NAMESTOKEEP2<- names(df)  [! MODEDF ]
    selectizeInput(  "pastevarin", "Combine the categories of these two variables:", choices = NAMESTOKEEP2,multiple=TRUE,
                     options = list(
                       maxItems = 2 ,
                       placeholder = 'Please select some variables',
                       onInitialize = I('function() { this.setValue(""); }')
                     )
    )
    
  })
  
  outputOptions(output, "pastevar", suspendWhenHidden=FALSE)
  
  stackdata <- reactive({
    
    df <- recodedata3() 
    
    if (is.null(df)) return(NULL)
    if (!is.null(df)){
      validate(  need(!is.element(input$x,input$y) , "Please select a different x variable or remove the x variable from the list of y variable(s)"))
      
      if(       all( sapply(df[,as.vector(input$y)], is.numeric)) )
      {
        tidydata <- df %>%
          gather_( "yvars", "yvalues", gather_cols=as.vector(input$y) ) %>%
          mutate(combinedvariable="Choose two variables to combine first")
      }
      if(       any( sapply(df[,as.vector(input$y)], is.factor)) |
                any( sapply(df[,as.vector(input$y)], is.character)))
      {
        tidydata <- df %>%
          gather_( "yvars", "yvalues", gather_cols=as.vector(input$y) ) %>%
          mutate(yvalues=as.factor(as.factor(as.character(yvalues)) ))%>%
          mutate(combinedvariable="Choose two variables to combine first")
      } 
      
      if(       all( sapply(df[,as.vector(input$y)], is.factor)) |
                all( sapply(df[,as.vector(input$y)], is.character)))
      {
        tidydata <- df %>%
          gather_( "yvars", "yvalues", gather_cols=as.vector(input$y) ) %>%
          mutate(yvalues=as.factor(as.character(yvalues) ))%>%
          mutate(combinedvariable="Choose two variables to combine first")
      }    
      
      
    }
    
    if( !is.null(input$pastevarin)   ) {
      if (length(input$pastevarin) > 1) {
        tidydata <- tidydata %>%
          unite_("combinedvariable" , c(input$pastevarin[1], input$pastevarin[2] ),
                 remove=FALSE)
      }
    }
    
    tidydata
  })
  #outputOptions(output, "fslider1", suspendWhenHidden=FALSE)
  
  
  
  
  
  output$colour <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items #[!is.element(items,input$y)]
    selectInput("colorin", "Colour By:",c("None",items,"yvars", "yvalues","combinedvariable") )
    
  })
  
  
  output$group <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items 
    
    if (input$boxplotaddition ){
      items= c(input$x,None=".",items[items!=input$x], "yvalues","combinedvariable")    
    }
    if (!input$boxplotaddition ){
      items= c("None",items,"yvars", "yvalues","combinedvariable")    
    }
    selectInput("groupin", "Group By:",items)
  })
  
  
  output$facet_col <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items #[!is.element(items,input$y)]
    selectInput("facetcolin", "Column Split:",c(None='.',items,"yvars", "yvalues","combinedvariable"))
  })
  output$facet_row <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items #[!is.element(items,input$y)]
    selectInput("facetrowin", "Row Split:",    c(None=".",items,"yvars", "yvalues","combinedvariable"))
  })
  
  output$facet_col_extra <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items #[!is.element(items,input$y)]
    selectInput("facetcolextrain", "Extra Column Split:",c(None='.',items,"yvars", "yvalues","combinedvariable"))
  })
  output$facet_row_extra <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items #[!is.element(items,input$y)]
    if (length(input$y) > 1 ){
      items= c("yvars",None=".",items, "yvalues","combinedvariable")    
    }
    if (length(input$y) < 2 ){
      items= c(None=".",items,"yvars", "yvalues","combinedvariable")    
    }
    selectInput("facetrowextrain", "Extra Row Split:",items)
  })
  
  
  output$facetscales <- renderUI({
    if (length(input$y) > 1 ){
      items= c("free_y","fixed","free_x","free")    
    }
    if (length(input$y) < 2 ){
      items= c("fixed","free_x","free_y","free")   
    }
    selectInput('facetscalesin','Facet Scales:',items)
  })
  outputOptions(output, "facetscales", suspendWhenHidden=FALSE)
  
  
  
  output$pointsize <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items #[!is.element(items,input$y)]
    selectInput("pointsizein", "Size By:",c("None",items,"yvars", "yvalues","combinedvariable") )
    
  })
  
  output$fill <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items #[!is.element(items,input$y)]
    selectInput("fillin", "Fill By:"    ,c("None",items,"yvars", "yvalues","combinedvariable") )
  })
  
  output$weight <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    items= items #[!is.element(items,input$y)]
    selectInput("weightin", "Weight By:",c("None",items,"yvars", "yvalues","combinedvariable") )
  })
  outputOptions(output, "weight", suspendWhenHidden=FALSE)
  
  
  
  output$mytablex = renderDataTable({
    datatable( stackdata() ,#  recodedata2()
               extensions = c('ColReorder','TableTools'),
               options = list(dom = 'RMD<"cvclear"C><"clear"T>lfrtip',
                              searchHighlight = TRUE,
                              pageLength=10 ,
                              lengthMenu = list(c(5, 10, 15, -1), c('5','10', '15', 'All')),
                              colReorder = list(realtime = TRUE),
                              tableTools = list(
                                "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                                "aButtons" = list(
                                  "copy",
                                  "print",
                                  list("sExtends" = "collection",
                                       "sButtonText" = "Save",
                                       "aButtons" = c("csv","xls")
                                  )
                                )
                              )
               ), 
               filter = 'bottom',
               style = "bootstrap")
  })
  
  
  
  # `%then%` <- shiny:::`%OR%`  
  
  plotObject <- reactive({
    validate(
      need(!is.null(stackdata()), "Please select a data set") 
    )
    
    plotdata <- stackdata()
    if(!is.null(plotdata)) {
      p <- ggplot(plotdata, aes_string(x=input$x, y="yvalues")) 
      
      if (input$colorin != 'None')
        p <- p + aes_string(color=input$colorin)
      if (input$fillin != 'None')
        p <- p + aes_string(fill=input$fillin)
      if (input$pointsizein != 'None')
        p <- p  + aes_string(size=input$pointsizein)
      
      if (input$groupin != 'None' & !is.factor(plotdata[,input$x]))
        p <- p + aes_string(group=input$groupin)
      
      if (input$groupin == 'None' & is.factor(plotdata[,input$x]))
        p <- p + aes(group=1)
      
      if (input$Points=="Points"&input$pointsizein == 'None')
        p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes)  
      if (input$Points=="Points"&input$pointsizein != 'None')
        p <- p + geom_point(,alpha=input$pointstransparency,shape=input$pointtypes)
      
      if (input$Points=="Jitter"&input$pointsizein == 'None')
        p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes,size=input$pointsizes)
      if (input$Points=="Jitter"&input$pointsizein != 'None')
        p <- p + geom_jitter(,alpha=input$pointstransparency,shape=input$pointtypes)
      
      
      
      if (input$line=="Lines"&input$pointsizein == 'None')
        p <- p + geom_line(,size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes)
      if (input$line=="Lines"&input$pointsizein != 'None')
        p <- p + geom_line(,alpha=input$linestransparency,linetype=input$linetypes)
      if (input$line=="Lines"&input$pointsizein == 'None'&input$lineignorecol)
        p <- p + geom_line(,size=input$linesize,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
      if (input$line=="Lines"&input$pointsizein != 'None'& input$lineignorecol)
        p <- p + geom_line(,alpha=input$linestransparency,linetype=input$linetypes,colour=input$colline)
      
      
      if (input$boxplotaddition)
        p <- p + geom_boxplot(
          #aes_string(group=input$x)
        )
      
      
      ###### Mean section  START 
      
      
      if (!input$meanignoregroup) {
        if (!input$meanignorecol) {
          
          if (input$Mean=="Mean") {
            if(input$meanlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line")
            if(input$meanlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",size=input$meanlinesize)
            
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_single(mean, geom = "point")
            
          }
          
          if (input$Mean=="Mean (95% CI)"){
            p <- p + 
              stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI),width=input$errbar)
            if(input$meanlines&input$pointsizein != 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line")
            if(input$meanlines&input$pointsizein == 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",size=input$meanlinesize)
            if(input$meanpoints)           
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "point")
            
          }
        }
        
        
        if (input$meanignorecol) {
          meancol <- input$colmean
          if (input$Mean=="Mean") {
            if(input$meanlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancol)
            
            if(input$meanlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancol,size=input$meanlinesize)
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_single(mean, geom = "point",col=meancol)
            
          }
          
          if (input$Mean=="Mean (95% CI)"){
            p <- p + 
              stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI),width=input$errbar, col=meancol)
            if(input$meanlines&input$pointsizein != 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line", col=meancol)
            if(input$meanlines&input$pointsizein == 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line", col=meancol,size=input$meanlinesize)
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "point", col=meancol)
            
          }
        }
      }
      
      if (input$meanignoregroup) {
        if (!input$meanignorecol) {
          
          if (input$Mean=="Mean") {
            if(input$meanlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",aes(group=NULL))
            if(input$meanlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",aes(group=NULL),size=input$meanlinesize)
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_single(mean, geom = "point",aes(group=NULL))
            
          }
          
          if (input$Mean=="Mean (95% CI)"){
            p <- p + 
              stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI), width=input$errbar,aes(group=NULL))
            if(input$meanlines&input$pointsizein != 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",aes(group=NULL))
            if(input$meanlines&input$pointsizein == 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",aes(group=NULL),size=input$meanlinesize)
            if(input$meanpoints)           
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "point",aes(group=NULL))
            
          }
        }
        
        
        if (input$meanignorecol) {
          meancol <- input$colmean
          if (input$Mean=="Mean") {
            if(input$meanlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancol,aes(group=NULL))
            if(input$meanlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(mean, geom = "line",col=meancol,aes(group=NULL),size=input$meanlinesize)
            
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_single(mean, geom = "point",col=meancol,aes(group=NULL))
            
          }
          
          if (input$Mean=="Mean (95% CI)"){
            p <- p + 
              stat_sum_df("mean_cl_normal", geom = "errorbar",fun.args=list(conf.int=input$CI), width=input$errbar, col=meancol, aes(group=NULL))
            if(input$meanlines&input$pointsizein != 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",col=meancol,aes(group=NULL))
            if(input$meanlines&input$pointsizein == 'None')  
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "line",col=meancol,aes(group=NULL),size=input$meanlinesize)
            
            if(input$meanpoints)           
              p <- p + 
                stat_sum_df("mean_cl_normal", geom = "point",col=meancol,aes(group=NULL))
            
          }
        }
      }
      ###### Mean section  END 
      
      ###### Smoothing Section START
      if(!is.null(input$Smooth) ){
        familyargument <- ifelse(input$smoothmethod=="glm","binomial","gaussian") 
        
        if ( input$ignoregroup) {
          if (!input$ignorecol) {
            spanplot <- input$loessens
            if (input$Smooth=="Smooth")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,aes(group=NULL))
            
            if (input$Smooth=="Smooth and SE")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,aes(group=NULL))
            
            if (input$Smooth=="Smooth"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,aes(group=NULL))+  
                aes_string(weight=input$weightin)
            
            if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,aes(group=NULL))+  
                aes_string(weight=input$weightin)
          }
          if (input$ignorecol) {
            spanplot <- input$loessens
            colsmooth <- input$colsmooth
            if (input$Smooth=="Smooth")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,col=colsmooth,aes(group=NULL))
            
            if (input$Smooth=="Smooth and SE")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,col=colsmooth,aes(group=NULL))
            
            if (input$Smooth=="Smooth"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,col=colsmooth,aes(group=NULL))+  
              aes_string(weight=input$weightin)
            
            if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,col=colsmooth,aes(group=NULL))+  
              aes_string(weight=input$weightin)
          }
          
        }
        
        if ( !input$ignoregroup) {
          if (!input$ignorecol) {
            spanplot <- input$loessens
            if (input$Smooth=="Smooth")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot)
            
            if (input$Smooth=="Smooth and SE")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot)
            
            if (input$Smooth=="Smooth"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot)+  
                aes_string(weight=input$weightin)
            
            if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot)+  
                aes_string(weight=input$weightin)
          }
          if (input$ignorecol) {
            spanplot <- input$loessens
            colsmooth <- input$colsmooth
            if (input$Smooth=="Smooth")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,col=colsmooth)
            
            if (input$Smooth=="Smooth and SE")
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,col=colsmooth)
            
            if (input$Smooth=="Smooth"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=F,span=spanplot,col=colsmooth)+  
              aes_string(weight=input$weightin)
            
            if (input$Smooth=="Smooth and SE"& input$weightin != 'None')
              p <- p + geom_smooth(method=input$smoothmethod,
                                   method.args = list(family = familyargument),
                                   size=1.5,se=T,span=spanplot,col=colsmooth)+  
              aes_string(weight=input$weightin)
          }
          
        }
        
        ###### smooth Section END
      }
      
      
      ###### Median PI section  START  
      if (!input$medianignoregroup) {
        
        if (!input$medianignorecol) {
          
          if (input$Median=="Median") {
            if(input$medianlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line")
            
            if(input$medianlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",size=input$medianlinesize)
            
            
            if(input$medianpoints)           
              p <- p + 
                stat_sum_single(median, geom = "point")
            
          }
          
          if (input$Median=="Median/PI"&input$pointsizein == 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI) ,size=input$medianlinesize,alpha=input$PItransparency,col=NA)+ 
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI) ,size=input$medianlinesize,alpha=0)
            
            if ( input$sepguides )
              p <-   p + 
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
            
          }
          
          if (input$Median=="Median/PI"&input$pointsizein != 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI), alpha=input$PItransparency,col=NA)+
              stat_sum_df("median_hilow", geom = "smooth"  ,fun.args=list(conf.int=input$PI),alpha=0)
            
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
            
          }
        }
        
        
        
        if (input$medianignorecol) {
          mediancol <- input$colmedian
          if (input$Median=="Median") {
            if(input$medianlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancol)
            
            if(input$medianlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancol,size=input$medianlinesize)
            
            if(input$medianpoints)           
              p <- p + 
                stat_sum_single(median, geom = "point",col=mediancol)
            
          }
          
          if (input$Median=="Median/PI"&input$pointsizein == 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon", fun.args=list(conf.int=input$PI), alpha=input$PItransparency,col=NA)+
              stat_sum_df("median_hilow", geom = "smooth", fun.args=list(conf.int=input$PI), size=input$medianlinesize,col=mediancol,alpha=0)
            
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          if (input$Median=="Median/PI"&input$pointsizein != 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),alpha=input$PItransparency,col=NA)+
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),col=mediancol,
                          alpha=0)          
            
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          
          
        }
      }
      
      
      if (input$medianignoregroup) {
        
        if (!input$medianignorecol) {
          
          if (input$Median=="Median") {
            if(input$medianlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",aes(group=NULL))
            if(input$medianlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",aes(group=NULL),size=input$medianlinesize)
            
            if(input$medianpoints)           
              p <- p + 
                stat_sum_single(median, geom = "point",aes(group=NULL))
            
          }
          
          if (input$Median=="Median/PI"&input$pointsizein == 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),aes(group=NULL),size=input$medianlinesize,alpha=0)   
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          
          if (input$Median=="Median/PI"&input$pointsizein != 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=0)
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          
        }
        
        
        if (input$medianignorecol) {
          mediancol <- input$colmedian
          if (input$Median=="Median") {
            if(input$medianlines&input$pointsizein != 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancol,aes(group=NULL))
            if(input$medianlines&input$pointsizein == 'None')           
              p <- p + 
                stat_sum_single(median, geom = "line",col=mediancol,aes(group=NULL),size=input$medianlinesize)
            
            if(input$medianpoints)           
              p <- p + 
                stat_sum_single(median, geom = "point",col=mediancol,aes(group=NULL))
            
          }
          
          if (input$Median=="Median/PI"&input$pointsizein == 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),col=mediancol,aes(group=NULL),size=input$medianlinesize,alpha=0)
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          if (input$Median=="Median/PI"&input$pointsizein != 'None'){
            p <- p + 
              stat_sum_df("median_hilow", geom = "ribbon",fun.args=list(conf.int=input$PI),aes(group=NULL),alpha=input$PItransparency,col=NA)+ 
              stat_sum_df("median_hilow", geom = "smooth",fun.args=list(conf.int=input$PI),col=mediancol,aes(group=NULL),alpha=0)
            
            
            
            if ( input$sepguides )
              p <-   p +
                guides(
                  color = guide_legend(paste("Median"),
                                       override.aes = list(shape =NA,fill=NA)),
                  fill  = guide_legend(paste( 100*input$PI,"% prediction interval"),
                                       override.aes = list(shape =NA ,linetype =0,alpha=0.5 )
                  ) )
          }
          
        }
      }
      
      
      
      ###### Median PI section  END
      
      
      ###### RQSS SECTION START  
      if (!input$ignoregroupqr) {
        if (!input$ignorecolqr) {
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid", 
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))       
            }
            
            if (input$mid)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$ninetieth)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$tenth)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$up)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$low) 
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            
            
          }
        }
        if (input$ignorecolqr) {
          colqr <- input$colqr
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty)) 
            }
            
            
            if (input$mid)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$ninetieth)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$tenth)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$up)
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$low) 
              p <- p +  stat_quantile(method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            
            
          }
        }
      }
      
      
      if (input$ignoregroupqr) {
        if (!input$ignorecolqr) {
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty)) 
            }
            
            if (input$mid)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$ninetieth)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed", 
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$tenth)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed",
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$up)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed", 
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$low) 
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed", 
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
          }
        }
        if (input$ignorecolqr) {
          colqr <- input$colqr
          if (input$Tauvalue) {
            if(!input$hidedynamic){
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles =input$Tau,size=1.5,
                                      linetype="solid",col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))     
            }
            
            
            if (input$mid)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.5,size=1.5,
                                      linetype="solid", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$ninetieth)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.90,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            if (input$tenth)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.1,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$up)
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.95,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
            if (input$low) 
              p <- p +  stat_quantile(aes(group=NULL),method = "rqss",quantiles = 0.05,size=1,
                                      linetype="dashed", col=colqr,
                                      formula=y ~ qss(x, constraint= input$Constraints,
                                                      lambda=input$Penalty))
            
          }
        }
      }
      
      
      ###### RQSS SECTION END
      
      facets <- paste(input$facetrowin,'~', input$facetcolin)
      
      if (input$facetrowextrain !="."&input$facetrowin !="."){
        facets <- paste(input$facetrowextrain ,"+", input$facetrowin, '~', input$facetcolin)
      }  
      if (input$facetrowextrain !="."&input$facetrowin =="."){
        facets <- paste( input$facetrowextrain, '~', input$facetcolin)
      }  
      
      if (input$facetcolextrain !="."){
        facets <- paste( facets, "+",input$facetcolextrain)
      }  
      if (facets != '. ~ .')
        p <- p + facet_grid(facets,scales=input$facetscalesin,space=input$facetspace)
      
      if (facets != '. ~ .'&input$facetwrap) {
        # p <- p + facet_wrap(as.formula(facets),scales=input$facetscalesin,ncol=input$wrapncol,nrow=input$wrapnrow)
        p <- p + facet_wrap(    c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain ) [
          c(input$facetrowextrain ,input$facetrowin,input$facetcolin,input$facetcolextrain )!="."]
          ,scales=input$facetscalesin,ncol=input$wrapncol,nrow=input$wrapnrow)
        
      }
      
      
      
      
      if (input$logy)
        p <- p + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x))) 
      
      if (input$logx)
        p <- p + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                               labels = trans_format("log10", math_format(10^.x))) 
      
      
      
      if (input$scientificy )
        p <- p  + 
        scale_y_continuous(labels=comma )
      
      if (input$scientificx )
        p <- p  + 
        scale_x_continuous(labels=comma) 
      
      
      
      p <- p + ylab("Y variable(s)")
      if (input$ylab!="")
        p <- p + ylab(input$ylab)
      
      if (input$xlab!="")
        p <- p + xlab(input$xlab)
      
      if (input$horizontalzero)
        p <-    p+
        geom_hline(aes(yintercept=0))
      
      if (input$customline1)
        p <-    p+
        geom_vline(xintercept=input$vline)
      
      
      if (input$customline2)
        p <-    p+
        geom_hline(yintercept=input$hline)
      
      
      
      if (input$identityline)
        p <-    p+ geom_abline(intercept = 0, slope = 1)
      
      if (input$themebw)
        p <-    p+
        theme_bw()
      p <-    p+
        theme(
          legend.position=input$legendposition,
          panel.background = element_rect(fill=input$backgroundcol),
          axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          strip.text.x = element_text(size = 16),
          strip.text.y = element_text(size = 16)
        )
      if (input$labelguides)
        p <-    p+
        theme(legend.title=element_blank())
      if (input$themeaspect)
        p <-    p+
        theme(aspect.ratio=input$aspectratio)
      
      #p <- ggplotly(p)
      p
      
      
    }
  })
  
  output$plot <- renderPlot({
    plotObject()
  })
  
  
  
  
  output$plotinfo <- renderPrint({
    df<- stackdata()  
    if (is.null(df)) return(NULL)
    nearPoints( stackdata(), input$plot_click, threshold = 5, maxpoints = 5,
                addDist = TRUE) #,xvar=input$x, yvar=input$y
  })
  
  
  output$clickheader <-  renderUI({
    df <-stackdata()
    if (is.null(df)) return(NULL)
    h4("Clicked points")
  })
  
  output$brushheader <-  renderUI({
    df <-stackdata()
    if (is.null(df)) return(NULL)
    h4("Brushed points")
    
  })
  
  output$plot_clickedpoints <- renderTable({
    # For base graphics, we need to specify columns, though for ggplot2,
    # it's usually not necessary.
    df<- stackdata()  
    if (is.null(df)) return(NULL)
    
    res <- nearPoints(stackdata(), input$plot_click, input$x, "yvalues")
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    res
  })
  output$plot_brushedpoints <- renderTable({
    df<- stackdata()  
    if (is.null(df)) return(NULL)
    res <- brushedPoints(stackdata(), input$plot_brush, input$x,"yvalues")
    if (nrow(res) == 0|is.null(res))
      return(NULL)
    res
  })
  
  
  
  
  downloadPlotType <- reactive({
    input$downloadPlotType  
  })
  
  observe({
    plotType    <- input$downloadPlotType
    plotTypePDF <- plotType == "pdf"
    plotUnit    <- ifelse(plotTypePDF, "inches", "pixels")
    plotUnitDef <- ifelse(plotTypePDF, 7, 480)
    
    updateNumericInput(
      session,
      inputId = "downloadPlotHeight",
      label = sprintf("Height (%s)", plotUnit),
      value = plotUnitDef)
    
    updateNumericInput(
      session,
      inputId = "downloadPlotWidth",
      label = sprintf("Width (%s)", plotUnit),
      value = plotUnitDef)
    
  })
  
  
  # Get the download dimensions.
  downloadPlotHeight <- reactive({
    input$downloadPlotHeight
  })
  
  downloadPlotWidth <- reactive({
    input$downloadPlotWidth
  })
  
  # Get the download file name.
  downloadPlotFileName <- reactive({
    input$downloadPlotFileName
  })
  
  # Include a downloadable file of the plot in the output list.
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(downloadPlotFileName(), downloadPlotType(), sep=".")   
    },
    # The argument content below takes filename as a function
    # and returns what's printed to it.
    content = function(con) {
      # Gets the name of the function to use from the 
      # downloadFileType reactive element. Example:
      # returns function pdf() if downloadFileType == "pdf".
      plotFunction <- match.fun(downloadPlotType())
      plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
      print(plotObject())
      dev.off(which=dev.cur())
    }
  )
  
  
})
