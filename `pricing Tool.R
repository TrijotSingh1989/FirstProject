library(shiny)
ui<- fluidPage(selectInput(inputId="num",
                           label="Select an Emirate",
                           c("Abu Dhabi","Dubai"),
                           selected = NULL),
               selectInput(inputId="num1",
                           label="Select a network",
                           c("Nw1","Nw2","Nw3","Nw4"),
                           selected = NULL),
               sliderInput(inputId="num2",
                           label="Select an Annual Limit",
                           min= 500000,max = 1000000,value=500000,step=100000,round=false),
               helpText("Annual limit are is 500K to 1Million"),
               selectInput(inputId="num4",
                           label="Select a Territorial Limit",
                           c("Worldwide","Worldwide Excluding US & Canada","Worldwide Exlcuding US , Canada & Europe","GCC"),
                           selected = NULL),
               selectInput(inputId="num3",
                           label="Select a Territorial Limit",
                           c("Worldwide","Worldwide Excluding US & Canada","Worldwide Exlcuding US , Canada & Europe","GCC"),
                           selected = NULL),
               selectInput(inputId="num4",
                           label="Select a RoomType",
                           c("SharedRoom","Deluxe Room","Super Deluxe","Suite"),
                           selected = NULL),
               selectInput(inputId="num5",
                           label="Select Reimursement of Non Network Expenses",
                           c("100%","90%","80%","70%"),
                           selected = NULL),
      textOutput("text_calc"),
      actionButton("go", "Press here to Calculate Premium")
)
server <- function(input,output,session)
{
 
   
  vals <- reactiveValues()
  observe({
    vals$A <- input$num
    vals$B <- input$num1
    vals$C <- input$num2
    vals$D <- input$num3
    vals$E <- input$num4
    vals$F <- input$num5
  })
  
  output$text_calc <- renderText({
    if (vals$A == "Abu Dhabi") {Emirate_Factor = 1}
    else if (vals$A == "Dubai") {Emirate_Factor = 1.5}
    
    if (vals$B == "Nw1") {Nw_Factor = 1000}
    else if (vals$B == "Nw2") {Nw_Factor = 900}
    else if (vals$B == "Nw3") {Nw_Factor = 850}
    else if (vals$B == "Nw4") {Nw_Factor = 700}
    
    if (vals$C == "1,000,000") {AnnualLimit_Factor = 1}
    else if (vals$C == "2,000,000") {AnnualLimit_Factor = 1.5}
    else if (vals$C == "3,000,000") {AnnualLimit_Factor = 1.75}
    else if (vals$C == "4,000,000") {AnnualLimit_Factor = 1.95}
    
    if (vals$D == "Worldwide") {Terr_Factor = 1}
    else if (vals$D == "Worldwide Excluding US & Canada") {Terr_Factor = 1.50}
    else if (vals$D == "Worldwide Exlcuding US , Canada & Europe") {Terr_Factor = 1.95}
    else if (vals$D == "GCC") {Terr_Factor = 3.0}
    
    if (vals$E == "SharedRoom") {Room_Factor = 1}
    else if (vals$E == "Deluxe Room") {Room_Factor = 1.50}
    else if (vals$E == "Super Deluxe") {Room_Factor = 1.95}
    else if (vals$E == "Suite") {Room_Factor = 3.0}
    
    
    if (vals$F == "100%") {Reimb_Factor = 1}
    else if (vals$F == "90%") {Reimb_Factor = 1.50}
    else if (vals$F == "80%") {Reimb_Factor = 1.95}
    else if (vals$F == "70%") {Reimb_Factor = 3.0}
    
    paste("The Premium is =", round(Emirate_Factor*Nw_Factor*AnnualLimit_Factor))
  })
}
shinyApp(ui=ui,server=server)






