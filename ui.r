shinyUI(fluidPage(
  titlePanel("Forecast"),
  fluidRow(
    column(12,
           h3("Selection Panel:-")    
    )
  ),
  fluidRow(
    column(3,
           selectInput('inRegressors', 'Inputs', c("Month" = "Dep_Month",
                                                       "Time" = "t",
                                                       "Oil Prices" = "oilprice",
                                                       "World Steel Production" = "wsp",
                                                       "Gross Output" = "go"
           ), multiple=TRUE, selected = c("Dep_Month","oilprice","t","wsp","go"),selectize=FALSE)      
           
    ),
    column(3,
           selectInput('regressand', 'Output', c("Revenue" = "Revenue",
                                                              "Volume" = "Transaction",
                                                              "Price per ticket" = "AvgPrice"
           ), multiple=FALSE, selectize=FALSE)
    ),
    column(3,
           selectInput('Country', 'Country', c(
             "ALL" = "ALL",
             "UNITED STATES" = "US",
             "FRANCE" = "FR",
             "UNITED KINGDOM" = "GB",
             "GERMANY" = "DE",
             "AUSTRALIA" = "AU",
             "SWITZERLAND" = "CH",
             "CANADA" = "CA",
             "ITALY" = "IT",
             "CHINA" = "CN",
             "BRAZIL" = "BR",
             "SPAIN" = "ES",
             "INDIA" = "IN"
           ), multiple=FALSE, selectize=FALSE)
    ),
    column(3,
           selectInput("Region_code",'Region', c(
             "ALL" = "ALL",
             "AFRICA" = "AF",
             "AUSTRALIA & PACIFIC" = "AU",
             "EUROPE" = "EU",
             "FAR EAST" = "FE",
             "MIDDLE EAST" = "ME",
             "NORTH AMERICA" = "NA",
             "SOUTH AMERICA" = "SA"
           ), multiple=FALSE, selectize=FALSE)
    )
  ),
  
  fluidRow(
    column(12,
           h3("Error Rates:-")      
    )
  ),
  fluidRow(
    column(6,
           h4("Next 1 Month"),
           verbatimTextOutput("error_list_30") ,
           h4("Next 3 Months"),
           verbatimTextOutput("error_list_90"),
           h4("Yearly"),
           verbatimTextOutput("error_list_yearly")
    ),
    column(6,
           h4("Prediction for 2016 & 2017"),
           verbatimTextOutput("pred_values")
    )
  ),
  fluidRow(
    column(12,
           h3("Diagnostic plots")      
    )
  ), 
  fluidRow(
    column(12,
           plotOutput('plot1')      
    )
  ) 
))