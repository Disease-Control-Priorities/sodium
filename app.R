#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)

df<-read.csv("salt_df.csv", stringsAsFactors = F)%>%arrange(location)%>%na.omit()

ints<-data.frame(Interventions = c("Public food procurement", "Behavior change/media campaigns",
                                   "Low sodium salt substitutes", "Front of pack labelling",
                                   "Salt targets", "Taxation"),
                 `Salt targets` = c("Sodium outside the home", "All sources",
                                    "Discretionary salt use", "Processed foods",
                                    "All sources", "Processed foods"),
                 `Relative reduction` = c(0.06, 0.02, 0.15, 0.137, 0.15, 0.15)
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Rough demo for salt model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
          selectInput("location", "Select location", unique(df$location)),
          uiOutput("disc"),
          uiOutput("proc"),
          uiOutput("outside"),
          uiOutput("inc"),
          numericInput("institution", "Proportion of meals outside the home that come from an institution (%):", 33)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          DTOutput("dtable"),
          plotOutput("saltplot")
        )
    )
)

# https://stackoverflow.com/questions/73118218/how-to-add-checkbox-in-datatable-in-a-shiny-module
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...))
  }
  inputs
}

js <- c(
  "$('[id^=checkb]').on('click', function(){",
  "  var id = this.getAttribute('id');",
  "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
  "  var value = $(this).prop('checked');",
  "  var info = [{row: i, col: 4, value: value}];",
  "  Shiny.setInputValue('dtable_cell_edit:DT.cellInfo', info);",
  "})"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$disc<-renderUI({
    numericInput("disc", "Sodium from discretionary sources (g)", round(df%>%filter(location==input$location, year==2019)%>%pull(discretionary), 2))
  })
  
  output$proc<-renderUI({
    numericInput("proc", "Sodium from processed foods (g)", round(df%>%filter(location==input$location, year==2019)%>%pull(processed), 2))
  })
  
  output$outside<-renderUI({
    numericInput("outside", "Sodium from food consumed outside the house (g)", round(df%>%filter(location==input$location, year==2019)%>%pull(outside), 2))
  })
  
  output$inc<-renderUI({

      numericInput("inc", "Annual increase in processed food consumption (%)", round(df%>%filter(location==input$location, year==2020)%>%pull(inc)*100,2))
    
  })
  
  
  Dat <- reactiveVal(
    ints%>%mutate(Include = FALSE)
  )
  
  observeEvent(input[["dtable_cell_edit"]], { 
    info <- input[["dtable_cell_edit"]] # this input contains the info of the edit
    Dat(editData(Dat(), info))
    test<-Dat()
    print(test$Relative.reduction)
    print(test)
  })
  
  output$dtable <- renderDT({
    
    dat2 <- cbind(
      ints,
      Include = shinyInput(checkboxInput, nrow(ints), "checkb")
    )
    
    DT::datatable(dat2, 
                  rownames = TRUE,
                  escape = FALSE,
                  editable = list(target = "cell", disable = list(columns = c(1,2,4))),
                  selection = "none",
                  callback = JS(js),
                  options = list(dom = 't')
    )
  }, server = FALSE)

    output$saltplot <- renderPlot({
        
      plot<-data.frame(disc = input$disc, proc = input$proc, outside = input$outside, inc = input$inc/100)%>%
        merge(., data.frame(year=2019:2040))%>%
        mutate(inc = ifelse(year==2019,0,inc),
               inc = cumsum(inc),
               proc = proc*(1+inc),
               salt = disc+proc+outside,
               Scenario = "Baseline")
      
      interv<-Dat()
      
      disc_int<-interv%>%filter(Include==TRUE & Salt.targets%in%c("All sources", "Discretionary salt use"))%>%
        summarise(imp = 1- prod(1-Relative.reduction))%>%pull(imp)
      
      proc_int<-interv%>%filter(Include==TRUE & Salt.targets%in%c("All sources", "Processed foods"))%>%
        summarise(imp = 1- prod(1-Relative.reduction))%>%pull(imp)
      
      outside_int<-interv%>%filter(Include==TRUE & Salt.targets%in%c("All sources", "Sodium outside the home"))%>%
        summarise(imp = 1- prod(1-Relative.reduction))%>%pull(imp)*(input$institution/100)

      
      plot2<-plot%>%
        mutate(disc = ifelse(year>=2024, disc*(1-disc_int), disc),
              proc = ifelse(year>=2024, proc*(1-proc_int), proc),
              outside = ifelse(year>=2024, outside*(1-outside_int), outside),
              salt = disc+proc+outside,
              Scenario = "Intervention"
        )
      
      
      plot<-bind_rows(plot, plot2)%>%
        mutate(Scenario = factor(Scenario, levels=c("Intervention", "Baseline")))
      
      ggplot(plot, aes(x=year, y=salt, color=Scenario))+
        geom_line()+
        theme_bw()+
        ylim(0, max(plot$salt)+1)+
        xlab("Year")+
        ylab("Total sodium intake (g)")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
