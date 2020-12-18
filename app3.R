
library(shiny)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(reshape2)
library(plyr)
library(agricolae)
library(plotly)
#install.packages("doBy", dependencies = TRUE)
library(doBy)
library(stringr)
library(corrplot)
library(SensoMineR)
library(markdown)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(readxl)
library(ROCR)
library(caret)
# install.packages("caret", dependencies = TRUE)
library(nnet)

german1= read_excel("C:/Users/fouzai/Desktop/2eme/econometrie/german1.xls")



ui=fluidPage( theme=shinytheme("cerulean"),
              themeSelector(),navbarPage("Econometrie",
                                         tabPanel("client data",titlePanel("File Input"),
                                                  sidebarLayout(
                                                    sidebarPanel(
                                                      fileInput("file","Upload the file"), 
                                                      helpText("Default max. file size is 5MB"),
                                                      tags$hr(),
                                                      h5(helpText("Select the read.table parameters below")),
                                                      checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                                      checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                                      br(),
                                                      
                                                      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                                                      
                                                      
                                                    ),
                                                    mainPanel(
                                                      uiOutput("tb")
                                                      
                                                      
                                                      
                                                      
                                                    )
                                                    
                                                  ))
                                         ,
                                         tabPanel("Analyse descriptive",tabsetPanel(tabPanel("summary",verbatimTextOutput ("sum"),tableOutput("et"))))
                                         ,
                                         tabPanel("Analyse univariee"
                                                  ,tabsetPanel(tabPanel("Variables qualitatives",uiOutput("quali"),plotOutput("qual")),
                                                               tabPanel("Variables quantitatives",uiOutput("quanti"),uiOutput("cl"),uiOutput("cla"),plotOutput("quan")))),
                                         tabPanel("Analyse bivariee",tabsetPanel( tabPanel("Variable qualitative",uiOutput("ana_bi")),
                                                                                  tabPanel("Variable quantitative",uiOutput("ana_bi1") ))),
                                         tabPanel("regression",tabsetPanel(tabPanel("regression logistique",uiOutput("logit")),tabPanel("reestimation",uiOutput("logit2")),tabPanel("Plots",uiOutput("pl")))),
                                         tabPanel("Machine Learning",tabsetPanel(tabPanel("modele logit",uiOutput("mdl5")),tabPanel("reestimation",uiOutput("rest")),tabPanel("Plots",uiOutput("plm")))),
                                         tabPanel("Reseaux de neurones",tabsetPanel(tabPanel("modele",uiOutput("rn")),tabPanel("Plot",uiOutput("rn1"))))
                                       
                                         
              ))




Server=function(input,output){
  
  
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
  })
  
  
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  
  output$sum <- renderPrint({
    
    summary(german1)
    
  })
  
  
  output$table <- renderTable({
    if(is.null(data())){return ()}
    na.omit(data())
  })
  
  
  output$tb <- renderUI({
    
    tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", tableOutput("table")),tabPanel("Summary",verbatimTextOutput("sum1")))
  })
  #importation hedo
  ##############
  ##################"
  ################"
  ################"
  
  
  output$sum1=renderPrint({
    summary(data())
  })
  
  
  
  
  
  
  
  
  
  
  
  
  #####################
  ##################
  ##################
  ####################
  #####################
  
  
  output$quali=renderUI({
    
    selectInput("quali1","choisir la variable :",names(german1[,c(1,3,4,7,8,11,13,15,16)]))
    
  })
  
  var_ql=reactive({
    return(input$quali1)
  })
  
  output$quanti=renderUI({
    
    selectInput("quanti1","choisir la variable :",names(german1[,c(2,5,6,9,10,12,14)]))
    
  })
  var_qn=reactive({
    return(input$quanti1)
  })
  
  yl=reactive({
    if( input$quanti1 %in% names(german1[,c(2,6,9,10)])){return("Annees")}
    if( input$quanti1 %in% names(german1[,c(12,14)])){return("Nombre")}
    if( input$quanti1 %in% names(german1[,c(5)])){return("Montant")}
  })
  
  
  
  output$quan=renderPlot ({
    if (input$bool=="non"){
      hist(german1[[var_qn()]],col = "purple", border = "white",
           main = paste("Histogramme de :\n", var_qn()) ,
           xlab = paste(var_qn()), ylab = "Effectif",labels = TRUE,
           proba = TRUE)
      lines(density(german1[[var_qn()]], na.rm = TRUE), lwd = 2, col = "orange" )
    }
    
    else {   hist(german1[[var_qn()]],nclass =as.numeric(input$cla1) ,col = "purple", border = "white",
                  main = paste("Histogramme de :\n", var_qn()) ,
                  xlab = paste(var_qn()), ylab = "Effectif",labels = TRUE,
                  proba = TRUE)
      lines(density(german1[[var_qn()]], na.rm = TRUE), lwd = 2, col = "orange" ) }
  },height = 600, width = 600)
  
  
  
  output$cla=renderUI({
    if (input$bool=="oui") numericInput("cla1","nombre de classes",value = 6,min=1,max=15)
    else (return ())
  })
  
  output$cl=renderUI({
    selectInput("bool","Vous voulez definir vos classes?",c("oui","non"))
  })
  


  output$qual=renderPlot ({
    
    
    pielabels <- sprintf("%s = %3.1f%s", data_frq()[,1],100*data_frq()[,2]/sum(data_frq()[,2]), "%")
    pie(table(german1[,var_ql()]),
        labels=NA,
        clockwise=FALSE,
        col=heat.colors(nrow(data.frame(table(german1[,var_ql()])))),
        border="white",
        radius=1,
        cex=1,
        main =paste( "Pie chart de :\n",var_ql()))
    legend("bottomleft",legend=pielabels ,fill=heat.colors(nrow(data.frame(table(german1[,var_ql()])))),cex=1)
    
  })
  
  data_frq=reactive({
    a=as.data.frame(table(german1[,var_ql()]))
    somme=sum(a[,2])
    n=nrow(a)
    for (i in 1:n){
      a$pourcentage[i]=a[i,2]*100/somme
    }
    return(a)
    
  })
  
  
  
  
  output$box=renderPlotly ({
    # ggplot(german1,aes(x = as.factor(as.matrix(german1[,var_quali()])),y=as.matrix(german1[,input$aov13])))+geom_boxplot()+
    #  ggtitle("effet juge")
    
    plot_ly(german1, x = ~as.factor(as.matrix(german1[,input$aov6])), y = ~as.matrix(german1[,input$aov13]),color = ~as.factor(as.matrix(german1[,input$aov6])), type = "box")
  })
  
 
  
  var_qual=reactive({
    return(input$aov111)
  })
  
  
  
  output$aov555=renderTable({
    table(as.matrix(german1[,var_quali()]),as.matrix(german1[,var_qual()]),deparse.level=2)
  })
  
  output$aov55=renderPlot ({
    mosaicplot(as.matrix(german1[,var_quali()]) ~ as.matrix(german1[,var_qual()]), data = german1, shade = TRUE,main ="Graphique en mosaïque", las=1 )
  },height = 400, width = 600)
  
  output$chi2=renderPrint({
    library(MASS)
    base=german1
    attach(base)# load the MASS package 
    t=chisq.test(as.matrix(base[,var_quali()]),as.matrix(base[,var_qual()]))
    t
    
  })
  output$ana_bi <- renderUI({
    
    
    
    
    fluidPage(
      sidebarLayout(      
        # Define the sidebar with one input
        sidebarPanel(
          selectInput("aov11","choisir la variable(variable cible)",names(german1[,c(1,3,4,7,8,11,13,15,16)])),
          hr(),
          selectInput("aov111","choisir votre variable",names(german1[,c(1,3,4,7,8,11,13,15,16)]))
        ),
        mainPanel( 
          
          uiOutput("aov555"),
          hr(),
          plotOutput("aov55"),
          hr(),
          verbatimTextOutput("chi2")
          
        )
      )
      
    )
  })
  
  
  output$ana_bi1 <- renderUI({
    
    
    
    
    fluidPage(
      sidebarLayout(      
        # Define the sidebar with one input
        sidebarPanel(
          selectInput("aov13","choisir la variable",names(german1[,c(2,5,6,9,10,12,14)])),
          hr(),
          selectInput("aov6","choisir la variable(variable cible)",names(german1[,c(1,3,4,7,8,11,13,15,16)]))
        ),
        mainPanel( 
          
          plotlyOutput("box"),
          hr(),
          verbatimTextOutput("ttest")
          
          
        )
      )
      
    )
  })
  
  
  
  
  output$ttest=renderPrint({
    t.test(as.matrix(german1[,input$aov13]) ~as.matrix(german1[,input$aov6]), data=german1, var.equal=TRUE)
  })
  
  
  
  var_quali=reactive({
    return( input$aov11 )
  })
  
  
  
  
  ########
  
  output$logit <- renderUI({
    
    choix=colnames(german1[,-15])
    
    
    fluidPage(
      sidebarLayout(      
        # Define the sidebar with one input
        sidebarPanel(
          checkboxGroupInput("exp","choisir les variables explicatives" ,choices =choix,inline = F,selected = NULL),
          hr(),
          helpText("Modele logit")
        ),
        mainPanel( 
          
          verbatimTextOutput("mdl"),
          hr(),tableOutput("glm"),
          plotOutput("pred"),
          hr(),
          tableOutput("test1")
          
        )
      )
      
    )
  })
  
  output$mdl = renderText({
    form = paste("Statut","~", paste(c(input$exp), collapse= "+"))
    modl = as.character(unlist(form))
  })
  
  output$glm = renderTable({
    
    df<-data_logit()
    attach(df)
    
    
    #Logit = glm(df[,1]~.,data = train_data,family = "binomial")
    l<- glm(dt~.,data=df,
            family=binomial)
    l = summary(l)
    k = as.data.frame(l$coefficients)
    k
  }, rownames = T,colnames = T
  )
  
  
  
  
  
  
  
  
  data_logit=reactive({
    attach(german1)
    qq=c(2,5,6,9,10,12,14)
    t1=as.vector(paste(c(input$exp), collapse= ","))
    t=strsplit(t1,",")
    dt=as.data.frame(fastDummies::dummy_cols(german1[,15]))
    
    dt=dt[,-c(1,2)]
    
    
    # colnames(dt)="Statut"
    for (i in 1:length(t[[1]])) {
      x=which(colnames(german1)==t[[1]][i])
      
      if(x %in% qq)  dt=cbind.data.frame(dt,german1[,x])
      else {dt=cbind.data.frame(dt,as.data.frame(fastDummies::dummy_cols(german1[,x])))  
      x1=which(colnames(dt)==t[[1]][i])
      dt=dt[,-x1] }
      
      
    }
    
    
    
    return(dt)
  })
  
  
  
  
  
  
  output$test1=renderTable({
    data_logit()[1:5,]
    
    
  })
  
  
  output$logit2 <- renderUI({
    
    
    
    
    fluidPage(
      sidebarLayout(      
        # Define the sidebar with one input
        sidebarPanel(
          selectInput("corec","Choisir comment vous voulez corriger votre model",choices = c("automatique","manuel")),
          hr(),
          helpText("Modele logit"),
          uiOutput("logit3"),
          hr()
        ),
        mainPanel( 
          tableOutput("glm1")
          
        )
      )
      
    )
  })
  
  output$logit3=renderUI({
    choix=colnames(data_logit()[,-1])
    if (input$corec=="manuel") checkboxGroupInput("exp1","choisir les variables explicatives a eliminer du modele" ,choices =choix,inline = F,selected = NULL)
  })
  
  output$glm1 = renderTable({
    
    
    
    
    #Logit = glm(df[,1]~.,data = train_data,family = "binomial")
    
    if(input$corec=="automatique"){
      df<-data_logit()
      attach(df)
      l<-step(glm(dt~.,data=df,
                  family=binomial))
      l = summary(l)
      k = as.data.frame(l$coefficients)
      k }
    
    else{
      df<-data_logit2()
      attach(df)
      l<-glm(dt~.,data=df,
             family=binomial)
      l = summary(l)
      k = as.data.frame(l$coefficients)
      
      
      
      
    }
    
    
  }, rownames = T,colnames = T
  )
  
  data_logit2=reactive({
    t1=as.vector(paste(c(input$exp1), collapse= ","))
    t=strsplit(t1,",")
    for (i in 1:length(t[[1]])) {
      x=which(colnames(data_logit())==t[[1]][i])
      
      dt=data_logit()[,-x]
      
      
    }
    
    return(dt)
    
    
    
    
  })
  
  
  output$pl <- renderUI({
    
    
    
    
    fluidPage(
      sidebarLayout(      
        # Define the sidebar with one input
        sidebarPanel(
          selectInput("cou","Choisir la courbe",choices = c("ROC","LIFT"))
          
        ),
        mainPanel( 
          plotOutput("cou1")
          
        )
      )
      
    )
  })
  
  
  output$cou1=renderPlot({
    
    df<-data_logit()
    attach(df)
    l<-step(glm(dt~.,data=df,
                family=binomial))
    
    prob_g=predict(l,df,type = "response")
    pred_g <- prediction(prob_g, dt) 
    if(input$cou=="ROC"){
      
      
      perf_rocr_g <- performance(pred_g, measure = "tpr", x.measure = "fpr")
      perf_auc_g <- performance(pred_g, measure = "auc")
      plot(perf_rocr_g,main="Courbe ROC",col="blue",xlab="Taux des faux positif",ylab="Taux des vraix positif")
      abline(0,1,col="gray",lty=2)
      text(0.5,0.7,paste("AUC = ",round(perf_auc_g@y.values[[1]],digits = 3)))
    }
    
    else {
      perf_lift1_g <- performance(pred_g, measure = "lift", x.measure = "rpp") #courbe de Lift
      perf_lift2_g <- performance(pred_g, measure = "tpr", x.measure = "rpp") #courbe de Lift
      plot(perf_lift1_g,main="Courbe de Lift",xlab="RPP",ylab="Taux des Vraix Positifs")
     # plot(perf_lift2_g,main="Courbe de Lift",xlab="RPP",ylab="Taux des Vraix Positifs",col="red")
      #segments(0,0,1,1,col="blue")
      
    }
    
    
    
  })
  
  
  
  output$mdl5 <- renderUI({
    
    choix=colnames(german1[,-15])
    
    
    fluidPage(
      sidebarLayout(      
        # Define the sidebar with one input
        sidebarPanel(
          checkboxGroupInput("m1","choisir les variables explicatives" ,choices =choix,inline = F,selected = NULL),
          hr(),
          helpText("Modele logit"),
          hr(),
          numericInput("cla","Choisir le nombre de classe pour la variable age ",min=2,max=15,value=5)
        ),
        mainPanel( 
          sliderInput("sl1", label = "Choisir la taille de Train Data", min = 0, 
                      max =1, value = 0.7),
          hr(),
          verbatimTextOutput("m2"),tableOutput("tes1"),
          hr(),
          verbatimTextOutput("m3"),
          hr(),
          verbatimTextOutput("m4")
          
        )
      )
    )
    
    
  })
  
  output$m2 = renderText({
    form = paste("Statut","~", paste(c(input$m1), collapse= "+"))
    modl = as.character(unlist(form))
  })
  
  
  
  data_logit3=reactive({
    attach(german1)
    qq=c(2,5,6,9,12,14)
    t1=as.vector(paste(c(input$m1), collapse= ","))
    t=strsplit(t1,",")
    dt=as.data.frame(fastDummies::dummy_cols(german1[,15]))
    
    dt=dt[,-c(1,2)]
    
    
    # colnames(dt)="Statut"
    for (i in 1:length(t[[1]])) {
      x=which(colnames(german1)==t[[1]][i])
      
      if(x==10){
        a=as.data.frame(cut(german1$Age,input$cla))
        a1=as.data.frame(fastDummies::dummy_cols(a[,1]))
        a1=a1[,-1]
        dt=cbind.data.frame(dt,a1)
      }
      else if(x %in% qq)  dt=cbind.data.frame(dt,german1[,x])
      else {dt=cbind.data.frame(dt,as.data.frame(fastDummies::dummy_cols(german1[,x])))  
      x1=which(colnames(dt)==t[[1]][i])
      dt=dt[,-x1] }
      
      
    }
    
    
    
    return(dt)
  })
  
  
  output$tes1=renderTable({
    data_logit3()[1:5,]
  })
  
  output$m3=renderPrint({
    
    set.seed(100)
    data_ml=data_logit3()
    attach(data_ml)
    data_ml$dt=as.factor(data_ml$dt)
    trainIndex=createDataPartition(data_ml$dt,p=input$sl1,list = F)
    
    spamTrain=data_ml[trainIndex,]
    
    spamTest=data_ml[-trainIndex,]
    
    fitControl=trainControl(method = "none")
    ml_r=train(dt~.,data = spamTrain,"glm",trControl=fitControl)
    
    pred=predict(ml_r,newdata = spamTest)
    mat=confusionMatrix(
      factor(pred),
      factor(spamTest$dt)
    )
    mat
    
    
  })
  
  output$m4=renderPrint({
    set.seed(100)
    data_ml=data_logit3()
    attach(data_ml)
    data_ml$dt=as.factor(data_ml$dt)
    trainIndex=createDataPartition(data_ml$dt,p=input$sl1,list = F)
    
    spamTrain=data_ml[trainIndex,]
    
    spamTest=data_ml[-trainIndex,]
    
    fitControl=trainControl(method = "none")
    ml_r=train(dt~.,data = spamTrain,"glm",trControl=fitControl)
    summary(ml_r)
  })
  
  output$rest <- renderUI({
    
    choix=colnames(data_logit3()[,-1])
    
    
    fluidPage(
      sidebarLayout(      
        # Define the sidebar with one input
        sidebarPanel(
          checkboxGroupInput("rest1","choisir les variables explicatives a eliminer" ,choices =choix,inline = F,selected = NULL),
        
        ),
        mainPanel( 
          
          
          
          verbatimTextOutput("m5")
          
        )
      )
    )
    
    
  })
  
  data_logit4=reactive({
    t1=as.vector(paste(c(input$rest1), collapse= ","))
    t=strsplit(t1,",")
    dt=data_logit3()
    for (i in 1:length(t[[1]])) {
      x=which(colnames(dt)==t[[1]][i])
      
      dt=dt[,-x]
      
      
    }
    
    return(dt)
    
  })
  
  output$m5=renderPrint({
    set.seed(100)
    data_ml=data_logit4()
    attach(data_ml)
    data_ml$dt=as.factor(data_ml$dt)
    trainIndex=createDataPartition(data_ml$dt,p=input$sl1,list = F)
    
    spamTrain=data_ml[trainIndex,]
    
    spamTest=data_ml[-trainIndex,]
    
    fitControl=trainControl(method = "none")
    ml_r=train(dt~.,data = spamTrain,"glm",trControl=fitControl)
    summary(ml_r)
  })
  
  
  
  
  output$plm <- renderUI({
    
    
    
    
    fluidPage(
      sidebarLayout(      
        # Define the sidebar with one input
        sidebarPanel(
          selectInput("plm1","choisir la courbe" ,choices =c("ROC","LIFT"))
          
        ),
        mainPanel( 
          
          
          
          plotOutput("rcc1")
          
        )
      )
    )
    
    
  })
  
  output$rcc1=renderPlot({
    set.seed(100)
    data_ml=data_logit4()
    attach(data_ml)
    data_ml$dt=as.factor(data_ml$dt)
    trainIndex=createDataPartition(data_ml$dt,p=input$sl1,list = F)
    
    spamTrain=data_ml[trainIndex,]
    
    spamTest=data_ml[-trainIndex,]
    
    fitControl=trainControl(method = "none")
    logit=step(glm(dt~.,data = spamTrain,family = binomial),direction="both")
    ml_r=train(dt~.,data = spamTrain,"glm",trControl=fitControl)
    prob_train=predict(logit,spamTrain,type = "response")
    pred_train <- prediction(prob_train, spamTrain$dt)
    prob_test=predict(logit,spamTest,type = "response")
    pred_test <- prediction(prob_test, spamTest$dt)
    
    if(input$plm1=="ROC"){
      perf_roc_train <- performance(pred_train, measure = "tpr", x.measure = "fpr")
      auc_train <- performance(pred_train, measure = "auc") 
      
      perf_roc_test <- performance(pred_test, measure = "tpr", x.measure = "fpr") 
      auc_test <- performance(pred_test, measure = "auc") 
       plot(perf_roc_train, col="blue", main="Courbe ROC", xlab="1-Spécificité (fpr)", ylab="Sensibilité (tpr)",
           bg="white",cex.main=2,cex.lab=1,print.cutoffs.at=seq(0,1,by=0.1),lwd=3) 
      abline(0, 1,col="green",lty=3) #rajout d'une première bisectrice
      
      #rajout de la courbe ROC pour la base test
      lines(perf_roc_test@x.values[[1]],perf_roc_test@y.values[[1]],col="red",lwd=2) 
      text(1,.05,labels=paste("__ train, AUC = ",round(auc_train@y.values[[1]],digits=3),sep=""),adj=1,col = "blue")
      text(1,.15,labels=paste("__ test,  AUC = ",round(auc_test@y.values[[1]],digits=3),sep=""),adj=1,col = "red")
    }
    
    else{
      perf_lift1_train <- performance(pred_train, measure = "lift", x.measure = "rpp") #courbe de Lift
      perf_lift2_train <- performance(pred_train, measure = "tpr", x.measure = "rpp") #courbe de Lift
      perf_lift1_test <- performance(pred_test, measure = "lift", x.measure = "rpp") #courbe de Lift
      perf_lift2_test <- performance(pred_test, measure = "tpr", x.measure = "rpp") #courbe de Lift
      #mettre les deux de Lift ensemble
      plot(perf_lift2_train, col="blue", main="Courbe de Lift", xlab="RPP", ylab="Sensibilité (tpr)",
           bg="white",cex.main=1,cex.lab=1,lwd=3) 
      
      lines(perf_lift2_test@x.values[[1]],perf_lift2_test@y.values[[1]],col="red",lwd=2) 
      text(1,.25,labels="__ train",adj=1,col = "blue")
      text(1,.15,labels="__ test",adj=1,col = "red")
      
      
      
     
    }
    
  })
  
  output$rn <- renderUI({
    
    choix=colnames(german1[,-15])
    
    
    fluidPage(
      sidebarLayout(      
        # Define the sidebar with one input
        sidebarPanel(
          checkboxGroupInput("exprn","choisir les variables explicatives" ,choices =choix,inline = F,selected = NULL),
          hr(),
          helpText("resaux de neurones")
        ),
        mainPanel( 
          sliderInput("sl11", label = "Choisir la taille de Train Data", min = 0, 
                      max =1, value = 0.7),
          hr(),
          verbatimTextOutput("mdl_rn"),
          hr(),
          tableOutput("tbl_rn"),
          verbatimTextOutput("mdl_rn1")
          
          
        )
      )
      
    )
  })
  
  output$mdl_rn = renderText({
    form = paste("Statut","~", paste(c(input$exprn), collapse= "+"))
    modl = as.character(unlist(form))
  })
  
  
  data_logit5=reactive({
    attach(german1)
    qq=c(2,5,6,9,10,12,14)
    t1=as.vector(paste(c(input$exprn), collapse= ","))
    t=strsplit(t1,",")
    dt=as.data.frame(fastDummies::dummy_cols(german1[,15]))
    
    dt=dt[,-c(1,2)]
    
    
    # colnames(dt)="Statut"
    for (i in 1:length(t[[1]])) {
      x=which(colnames(german1)==t[[1]][i])
      
      if(x %in% qq)  dt=cbind.data.frame(dt,german1[,x])
      else {dt=cbind.data.frame(dt,as.data.frame(fastDummies::dummy_cols(german1[,x])))  
      x1=which(colnames(dt)==t[[1]][i])
      dt=dt[,-x1] }
      
      
    }
    
    
    
    return(dt)
  })
  
  
  output$tbl_rn=renderTable({
    head(data_logit5())
  })
  
  
  
   output$mdl_rn1=renderPrint({
     data_rn=data_logit5()
     attach(data_rn)
     data_rn$dt=as.factor(data_rn$dt)
     trainIndex=createDataPartition(data_rn$dt,p=input$sl11,list = F)
     
     data_train=data_rn[trainIndex,]
     
     data_test=data_rn[-trainIndex,]
     Neural<- nnet(dt~.,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
     
     fitNeural_train <- predict(Neural,newdata=data_train)
     fitNeural_train1 <- predict(Neural,newdata=data_train,type = "class")
     data_train$nnet_pred<-fitNeural_train 
     data_train$nnet_pred1<-fitNeural_train1 
     prednn_train = prediction( fitNeural_train, data_train$dt)
     perfnn_train = performance(prednn_train, "tpr", "fpr")
     auc_train_nn = performance(prednn_train,measure="auc")
     fitNeural <- predict(Neural, newdata=data_test)
     fitNeural1 <- predict(Neural, newdata=data_test,type="class")
     data_test$nnet_pred1<-fitNeural1 
     prednn = prediction(fitNeural, data_test$dt)
     perfnn <- performance(prednn, "tpr", "fpr")
     cf_matrix_nnet<-confusionMatrix(factor(data_test$dt),factor(data_test$nnet_pred1))
     cf_matrix_nnet
     
     
     
   })
   
   output$rn1 <- renderUI({
     
     
     
     
     fluidPage(
       sidebarLayout(      
         # Define the sidebar with one input
         sidebarPanel(
          selectInput("pl_rn","Choisir la courbe",c("ROC","LIFT","LIFT1"))  ,
           hr(),
           helpText("resaux de neurones")
         ),
         mainPanel( 
           plotOutput("pl_rn1")
           
           
           
         )
       )
       
     )
   })
   
   output$pl_rn1=renderPlot({
     
     data_rn=data_logit5()
     attach(data_rn)
     data_rn$dt=as.factor(data_rn$dt)
     trainIndex=createDataPartition(data_rn$dt,p=input$sl11,list = F)
     
     data_train=data_rn[trainIndex,]
     
     data_test=data_rn[-trainIndex,]
     Neural<- nnet(dt~.,data = data_train,size=10,maxit=500,decay=.001, linout=F, trace = F)
     
     fitNeural_train <- predict(Neural,newdata=data_train)
     fitNeural_train1 <- predict(Neural,newdata=data_train,type = "class")
     data_train$nnet_pred<-fitNeural_train 
     data_train$nnet_pred1<-fitNeural_train1 
     prednn_train = prediction( fitNeural_train, data_train$dt)
     perfnn_train = performance(prednn_train, "tpr", "fpr")
     auc_train_nn = performance(prednn_train,measure="auc")
     perf_lift2_nn_train <- performance(prednn_train, measure = "lift", x.measure = "rpp")
     perf_lift1_nn_train <- performance(prednn_train, measure = "tpr", x.measure = "rpp")
     perf_roc_nn_train <- performance(prednn_train, measure = "tpr", x.measure = "fpr")
     perf_auc_nn_train <- performance(prednn_train, measure = "auc")
     #courbe de Roc
     if (input$pl_rn=="ROC"){
     plot(perf_roc_nn_train,main="Courbe ROC Réseau de neurônes",col="blue")
     text(0.5,.7,paste("AUC - NN = ",round(perf_auc_nn_train@y.values[[1]],3)),col="blue",cex=0.75)
     segments(0,0,1,1,lty=3,col="green") }
     else if(input$pl_rn=="LIFT"){
       plot(perf_lift2_nn_train,main="Courbe de Lift Réseau de neurônes")
     }
     else {
       plot(perf_lift1_nn_train,main="Courbe de Lift Réseau de neurônes",col="blue",grid=TRUE)
       segments(0,0,1,1,lty=3)
      
       
     }
     
     
     
   })
   
 
   
   


  
  
  
}

shinyApp(ui,Server)
