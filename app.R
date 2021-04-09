library(shiny)

Install.packagesCatCVA <- function() {
  #ensures all packages are installed
  
  list.of.packages <-
    c("Matrix", "caret", "MASS", "ratte.data", "readxl", "e1071","klaR", "glmnet", "randomForest", "shinybusy", "colorspace")
  new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  
  library(caret)
  library(MASS)
  library(Matrix)
  library(readxl)
  library(e1071)
  library(klaR)
  library(glmnet)
  library(randomForest)
  library(shinybusy)
  library(colorspace)
  
  #imports all necessary biplot drawing functions
  
  BiplotFunctions <<- source("source/BiplotFunctionsClean.R")$value
  
 #imports all data sets
  
  mushroom <<- read.csv("data/mushrooms.csv")
  
}

confusion.biplot.full<-function(Zmat,groups,class.numb){
  check<-rep(1,class.numb)/class.numb
  lda.model<-lda(Zmat,grouping = groups,prior=check)
  lda.pred<-predict(lda.model, Zmat)
  lda.class <- lda.pred$class
  return(confusionMatrix(lda.class,groups))
}

Install.packagesCatCVA()

linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

ui <- tagList(
  add_busy_spinner(position = 'bottom-right', spin = "fading-circle"),
  navbarPage(

    theme = "yeti",
    title = "Categorical Biplot Comparison",
    id = "headtab",
    
    
    #IRIS panel
    tabPanel(
      "Mushrooms",
      sidebarPanel(
        "Biplot Settings",
        helpText(" ")
        ,radioButtons(
          "bipltype.iris",
          "Underlying biplot used",
          selected = "CVA H",
          inline = T,
          choices = c("CVA H", "catPCA", "CVA En")
        ),
        checkboxInput(
          "classreg.iris",
          label = strong("Class Regions"),
          value = T
        ),
        sliderInput(
          "hrank.iris",
          label = "Rank of H matrix (CVA H)",
          min = 2,
          max = 21,
          step = 1,
          value = 2
        ),
        sliderInput(
          "numvars.iris",
          label = "Number of Variables",
          min = 4,
          max = 21,
          step = 1,
          value = 21
        )
        ,
        h5(strong("Data set information:")),
        h5("Number of classes: 2"),
        h5("Number of attributes: 21 (21 nominal categorical)"),
        h5("Number of observations: 8124"),
        linebreaks(1),
        h5(strong("Data set description:")),
        h5(
          "This data set includes descriptions of hypothetical samples corresponding to 23 species of gilled mushrooms in the Agaricus and Lepiota Family. Each species is identified as edible or poisonous."
          ),
        linebreaks(1),
        tableOutput("basicerror.iris"),
        linebreaks(1)
        ,
        h5(strong("Confusion matrix:")),
        tableOutput("confusion.iris"), 
        checkboxInput("alphaincl", label = strong("Alpha-bags:"), value = TRUE),
        sliderInput("alphasize", "", min = 0, max = 99, value = 85, step = 5, post = "%"),
        checkboxInput("obsplot", label = strong("Plot observations"), value = TRUE),
        sliderInput("ticksize", "Tick label size (%)", min = 0, max = 99, value = 50, step = 5, post = "%"),
      )
      
      
    ),
    
    
    
    mainPanel(helpText(" "),
                       tabsetPanel(id = "plotarea",
                         tabPanel("Biplot", plotOutput(
                           outputId = "main",
                           height = "700px"
                         )
                         ),
                         tabPanel("Accuracy Raw Output", helpText(" "), verbatimTextOutput("errormetrics"))
                         
                       ))
    
    
    
  )  

)

server <- function(input, output,session) {
rv <<- reactiveValues()
er_rv <<- reactiveValues()

#----------- preprocessing data and using rf to get important variables


mushroom <<- mushroom[,-17]
colnames(mushroom) <- c("edibility", "cap_shape", "cap_surface", 
                        "cap_color", "bruises", "odor", 
                        "gill_attachement", "gill_spacing", "gill_size", 
                        "gill_color", "stalk_shape", "stalk_root", 
                        "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                        "stalk_color_below_ring", "veil_color", 
                        "ring_number", "ring_type", "spore_print_color", 
                        "population", "habitat")
## We make each variable as a factor
fact.fun<-function(x){
  as.factor(x)
}
mushroom<<- as.data.frame(lapply(mushroom,fact.fun))

## We redefine each of the category for each of the variables
levels(mushroom$edibility) <- c("edible", "poisonous")
levels(mushroom$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(mushroom$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                "green", "purple", "white", "yellow")
levels(mushroom$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(mushroom$bruises) <- c("no", "yes")
levels(mushroom$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushroom$gill_attachement) <- c("attached", "free")
levels(mushroom$gill_spacing) <- c("close", "crowded")
levels(mushroom$gill_size) <- c("broad", "narrow")
levels(mushroom$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                                 "pink", "green", "purple", "white", "yellow")
levels(mushroom$stalk_shape) <- c("enlarging", "tapering")
levels(mushroom$stalk_root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(mushroom$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroom$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroom$veil_color) <- c("brown", "orange", "white", "yellow")
levels(mushroom$ring_number) <- c("none", "one", "two")
levels(mushroom$ring_type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(mushroom$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                        "green", "purple", "white", "yellow")
levels(mushroom$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushroom$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

mushroom$gill_size <- factor(mushroom$gill_size, levels = c("broad", "narrow"), ordered = T)
mushroom$ring_number <- factor(mushroom$ring_number, levels = c("none", "one", "two"), ordered = T) 

rf.fit<-randomForest(edibility~.,data=mushroom)
rf.imp<-order(-rf.fit$importance)
#type <- c(rep("nom",16),"nom",rep("nom",4))
type <- c(rep("nom",7),"ord",rep("nom",8), "ord", rep("nom",4))
#---------encoding


#inputs for confusion statistics

class.num <<- NULL
groups.lda <<- NULL


#function which reads inputs and produces necessary plot

producePlot <-function(){
  
  
  renderPlot({
    bipltype.iris <<- input$bipltype.iris
    #class.reg.choice.iris <<- input$classreg.iris
    hrank.iris <<-input$hrank.iris
    #class.col.iris <<- NULL
    numvars.iris <<- input$numvars.iris
    classprior <<- "equal"
    
    require (RColorBrewer)
    colin <- c(brewer.pal(12,"Set3")[-2],brewer.pal(8,"Set2"),brewer.pal(8,"Accent"),brewer.pal(7,"Dark2"))
    
    classcols <-  gray.colors(2, start = 0.7, end = 0.9, gamma = 2.2, alpha = NULL)
    
    if(input$classreg.iris == TRUE){
      #classprior <<- "equal"
      class.col.iris <<-  classcols
    }
    else{
      #classprior <<- NULL
      class.col.iris <<- c("white","white")
    }
    
    
    choice <<- input$headtab
    
    varstoplot <- numvars.iris
    
    rf.names<-rownames(rf.fit$importance)[rf.imp][1:varstoplot]
    
    mushroom.cat.order<<-match(rf.names, colnames(mushroom))
    
    
    encoding_func<-function(x){
      as.numeric(factor(x))
    }
    
    encode_full<-as.data.frame(lapply(mushroom[,mushroom.cat.order], encoding_func))
    

  

    #-------- IRIS MAIN ------------
    
    
    if (choice == "Mushrooms") {
      class.num  <<- 2
      groups.lda <<- mushroom$edibility
      if (bipltype.iris == "CVA En") {
        
        rv <<-CVAbiplot(
          X = encode_full,
          G = indmat(mushroom[,1]),
          sample.pch = c(21, 22),
          prior.p = classprior,
          region.colours = class.col.iris, 
          ax = list(tick.label.cex = 1.5*input$ticksize/100, label.cex = 1.5*input$ticksize/100, r), 
          alpha.bags = list(which = 1:2*input$alphaincl, alpha = input$alphasize/100, col = darken(class.col.iris,0.4)),
          samples = list(cex = 1*input$obsplot)
        )
        par(xpd=TRUE)
        legend("bottom", legend=c("Edible","Poisonous"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex= 1.5*input$ticksize/100,pt.bg = classcols ,pch=c(21,22) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE)
      }
      
      
      if (bipltype.iris == "catPCA") {
        rv <<-
          CATPCAbiplot(
            X = mushroom[,mushroom.cat.order],
            Xcont = NULL,
            G = indmat(mushroom[,1]),
            sample.pch = c(21, 22),
            factor.type = type[mushroom.cat.order-1], 
            prior.p = classprior,
            region.colours = class.col.iris,
            scaled.mat = TRUE,
            ax = list(tick.label.cex = 1.5*input$ticksize/100, label.cex = 1.5*input$ticksize/100), 
            ax.nominal = list(tick.label.cex = 1.5*input$ticksize/100, label.cex = 1.5*input$ticksize/100,  tick.label.offset = 0, tick.label.pos = 0,
                              col = as.list(lighten(colin,0.5)), tick.col = darken(colin,0.5), label.col =  darken(colin,0.5)),
            ax.ordinal = list(tick.label.cex = 1.5*input$ticksize/100, label.cex = 1.5*input$ticksize/100,  tick.label.offset = 0, tick.label.pos = 0,
                              col = as.list(colin), tick.col = darken(colin,0.5), label.col =  darken(colin,0.5)),
            alpha.bags = list(which = 1:2*input$alphaincl, alpha = input$alphasize/100, col = darken(class.col.iris,0.4)),
            samples = list(cex = 1*input$obsplot)
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Edible","Poisonous"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex= 1.5*input$ticksize/100,pt.bg = classcols ,pch=c(21,22) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE)
        
      }
      

      
      
      if (bipltype.iris == "CVA H") {
        rv <<-
          CVA_H(
            X = mushroom[,mushroom.cat.order],
            Xcont = NULL,
            G = indmat(mushroom[,1]),
            sample.pch = c(21, 22),
            factor.type = type[mushroom.cat.order-1], 
            h.rank = hrank.iris,
            prior.p = classprior,
            region.colours = class.col.iris,
            scaled.mat = TRUE,
            ax = list(tick.label.cex = 1.5*input$ticksize/100, label.cex = 1.5*input$ticksize/100), 
            ax.nominal = list(tick.label.cex = 1.5*input$ticksize/100, label.cex = 1.5*input$ticksize/100,  tick.label.offset = 0, tick.label.pos = 0,
                              col = as.list(lighten(colin,0.5)), tick.col = darken(colin,0.5), label.col =  darken(colin,0.5)),
            ax.ordinal = list(tick.label.cex = 1.5*input$ticksize/100, label.cex = 1.5*input$ticksize/100,  tick.label.offset = 0, tick.label.pos = 0,
                              col = as.list(colin), tick.col = darken(colin,0.5), label.col =  darken(colin,0.5)),
            alpha.bags = list(which = 1:2*input$alphaincl, alpha = input$alphasize/100, col = darken(class.col.iris,0.4)),
            samples = list(cex = 1*input$obsplot)
          )
        par(xpd=TRUE)
        legend("bottom", legend=c("Edible","Poisonous"), box.col = "white",pt.lwd=0.5, pt.cex=1,lty=1, cex= 1.5*input$ticksize/100,pt.bg = classcols,pch=c(21,22) ,bg="white",lwd=1,bty="o",horiz=T,inset=c(0,-0.14))
        par(xpd=FALSE) 
        
      }
      
    }
    
  })

  }



#------End plotting function--------------#

basicMetrics <-function(){
  Accuracy.Metrics<-data.frame(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$overall[1]*100)
  Accuracy.Metrics<-data.frame(paste(round(Accuracy.Metrics,2),"%"))
  names(Accuracy.Metrics)<-"Accuracy:"
  return(Accuracy.Metrics)
}


#------------Output main display--------------

output$main <- producePlot()
 
  
output$errormetrics <- renderPrint({confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)})
 
output$confusion.iris <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
 
 
output$basicerror <- renderTable({basicMetrics()})


#--------generateNew calculates all biplots and relevant tables to display--------#

generateNew <- function(){
  
  updateTabsetPanel(session,inputId = "plotarea",selected = "Biplot")
  producePlot()
  output$basicerror.iris <- renderTable({basicMetrics()})
  
  
  output$confusion.iris <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)
  
  
  output$errormetrics <- renderPrint({confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)})
  
}


#-------------If biplot option is changed----------------------

observeEvent(input$bipltype.iris,{
  generateNew()
})

#-------------If class regions is pressed ---------------------

observeEvent(input$classreg.iris,{
  generateNew()
})


#--------------If H slider is moved----------------------

observeEvent(input$hrank.iris,{
  generateNew()
})

observeEvent(input$numvars.iris,{
  generateNew()
})


#------------------If tab is changed---------------------

observeEvent(input$headtab,{
  generateNew()
})

observeEvent(input$plotarea,{
  producePlot()
  output$basicerror.iris <- renderTable({basicMetrics()})


  output$confusion.iris <- renderTable(as.data.frame.matrix(confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)$table),rownames = T)


  output$errormetrics <- renderPrint({confusion.biplot.full(rv$Zmat,factor(groups.lda),class.num)})
})


  
}

shinyApp(ui, server)