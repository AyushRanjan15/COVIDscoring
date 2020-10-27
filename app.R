    
    library(shiny)
    library(shinyjs)
    library(leaflet)
    
    library(dplyr)
    library(magrittr)
    library(readr)
    
    
    library(dplyr)
    library(magrittr)
    library(readr)
    #####################################################
    
    
    nycounties <- rgdal::readOGR("https://raw.githubusercontent.com/tonywr71/GeoJson-Data/master/australian-suburbs.geojson")
    #nycounties <- rgdal::readOGR("Suburbs_GDA2020.geojson")
    
#    View(nycounties@data)
    
    str(nycounties, max.level = 2)
    
    nycounties@data$newrow <- sample(100, size = nrow(nycounties@data), replace = TRUE)
    
    
    #
    #STATE_NAME = c("New South Wales", "Victoria", "Queensland", "South Australia", "Western Australia", "Tasmania")
    pop = c(110, 30, 200, 1000, 85, 2000)
    
    #df = data.frame(STATE_NAME, pop)
    #df
    ##
    
    #library(dplyr)
    #nycounties@data = left_join(nycounties@data, df, by = "STATE_NAME")
    
    pal = colorNumeric(
      palette = "YlGnBu",
      domain = nycounties@data$pop
    )
    
    test = paste0("<strong>", nycounties@data$STATE_NAME, "</strong>", "<br>", "Test Number : ", nycounties@data$pop)
    
    #leaflet(data = nycounties)%>%
    #  addProviderTiles("OpenStreetMap")%>%
    #  addPolygons(fillColor = ~pal(pop), fillOpacity = 0.8, 
    #              color = "#BDBDC3", 
    #              weight = 1,
    #              popup = test)%>%setView(lat = -26.458486, lng = 133.676557, zoom = 4)
    #####################################################
    
    # Demographics: Age and Ethnicity used for calculation, gender just in shiny app demographic selectiong 
    # for data gathering purposes. All necessary values are within the AgeEthnicity Dataset.
    
    AgeEthnicity <- read_csv("AgeEthnicity.csv")
    AgeEthnicity <- na.omit(AgeEthnicity)
    
    # Get values to be used in drop down list for age
    
    ageValues <- AgeEthnicity$Age %>% unique()
    
    
    #Get values to be used in drop down list for ethnicity
    
    ethValues <- AgeEthnicity$Ethnicity %>% unique()
    
    #Functionality wise, in the Shiny app, selecting the age will take a subset of the data for that age,
    # then by selecting ethnicity it is matched to the correct row of the data. The Risk band value would 
    # then be selected. Based on that value (LOW, SOME, MODERATE, HIGH, VERy HIGH), we could either return an
    # Image with the relevant risk information, or formatted text. There is a rough sketch included in the repository
    # of what i was thinking.
    
    #Comorbidities: No dataset, just values as seen here. cmorbRates just for interest, cmorbRisk all we really need to
    # return correct risk. Same return type as demographics.
    
    comorb <- list( Asthma = 'asthma', Obesity = 'obs', Diabetes = 'diab', Chronic_Kidney_Disease = 'ckd', 
                    Severe_Obesity = 'sevObs', Corondary_Artery_Disease = 'cad', History_of_Stroke = 'sHis', 
                    No_known_conditions = 'none'
    )
    
    cmorbRates <- list(asthma = 1.5, obsR = 3, diab = 3, ckd = 4, sevObs = 4.5, cad = 4,
                       sHis = 4, condX2 = 4.5, condX3 = 5, none = 0
    )
    cmorbRisk <- list(asthma = 'MODERATE', obs = 'MODERATE', diab = 'MODERATE', ckd = 'HIGH', sevObs = 'HIGH', cad = 'HIGH',
                      sHis = 'HIGH', condX2 = 'HIGH', condX3 = 'VERY HIGH', none = 'LOW')
    #cmorbRisk["Diabetes"]
    #Occupation: Not sure if we should subset data by major occupational group, and then by occupation for dropdown lists. would
    # be easier for user to find a job to select. Same as demographic, functionality wise by a person selecting the job from the drop down
    # list, that can be used to get the correct risk band from the risk band column. same return type as demographcis. 
    
    Jobs <- read_csv("JobsCovid.csv")
    
    occGroups <- Jobs$`Major Occupational Group` %>% unique()
    
    jobsValues <- Jobs$Occupation
    
    # time function
    humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
    
    # defining user interface
    ui <- navbarPage(
      "Vulnerability Calculator",
      tabPanel("Form",
               
               #shinyWidgets::setBackgroundImage(
              #   src = "https://photos.app.goo.gl/qLJ7Sbqpjjj94avK7", shinydashboard = FALSE
               #),
               # to use shinyjs features
               useShinyjs(),
               # 1st tab
              tags$h1("Demo Form", style = "color: #4d3a7d;"),
               # Sidebar panel for inputs ----
               #Sidebar panel for Demogrpahics, Health Status and Occupation Inputs
               sidebarPanel(
                 
                 #Demogrpahic Inputs
                 "Demographics",
                 selectInput("age", "Age", c("-Select-","0-4", "5-17", "18-29", "30-39", "40-49", "50-64", "65-74", "75-84", "85+")),
                 selectInput("ethnicity", "Ethinicity", c("-Select-","Non-Hispanic American Indian or Alaska Native", "Non-Hispanic Asian or Pacific Islande", "Non-Hispanic White", "Non-Hispanic Black", "Hispanic or Latino")),
                 selectInput("gender", "Gender", c("-Select Gender-","Male", "Female")),
                 
                 #Disease and Occupation Inputs
                 "Disease",
                 selectInput("health", "Health Status", c("-Select-", "Asthama", "Obesity", "Diabetes", "Chronic_Kidney_Disease", "Severe_Obesity", 
                                                          "Coronary_Artery_Disease", "History_of_Stroke", "COPD", "Smoker", "Cancer/Immune_Compromised")),
                 
                 "Occupation",
                 selectInput("major_occ", "Major Occupation", c("-Select-", "Clerical and Admin Workers", "Community and Personal Service Workers", "Labourers", 
                                                                "Machiner Operators and Drivers", "Managers", "Professionals", "Sales Workers", "Technicians and Trade Workers")),
                 
                 selectInput("occupation", "Occupation", c("-Select-", "Courier", "Medical Receptionist", "Health Practise Manager", 
                                                           "Secretary", "Project/Program Admin", "Filing/ Registry Clerk", 
                                                           "Payroll Clerk", "Personal Assistant", "Contact Admin", "Credit Loan Officer", 
                                                           "Accounts Clerk", "Despatching/Receiving Clerk", "Officer Manager", 
                                                           "Information Officer", "General Clerk", "Bank Worker", "Purchasing Officer", 
                                                           "Data Entry Operator", "Postal Delivery Officer", "Call/Contact Center Operator", 
                                                           "Order Clerk", "Stock Clerk", "Insurance Consultor", "Hospital Orderly", 
                                                           "Dental Assistant", "Ambulance Officer", "Enrolled Nurse", "Fire Fighter", 
                                                           "Prison Officer", "Police Officer", "Personal Care Assistant", "Aged/Disabled Carer", 
                                                           "Community Worker", "Integration Aide", "Child Care Worker", "Massage Therapist", 
                                                           "Teachers' Aide", "Beauty Therapist", "Fitness Instructor", "Waiter", "Security Officer", 
                                                           "Swimmming Coach Instructor", "Barista", "Cafe worker", "Bar Attendent", "Travel Consultant",
                                                           "Commercial Housekeeper", "Commercial Keeper", "Home Improvement Installer", "Car Detailer", 
                                                           "Fast Food Cook", "Builder's Labourer", "Shelf Filler", "Garden Labourer", "Kitchen Hand", 
                                                           "Concreter", "Labourers nec", "Packers nfd", "Product Assembler", "handy Person", 
                                                           "Forklift Driver", "Truck Driver(General)", "Engineering Production Worker", "Bus Driver", 
                                                           "Taxi Driver", "Delivery Driver", "Storeperson", "Miner", "Health and Welfare Services Managers", 
                                                           "Facilities Manager", "Chief Executin/ Managing Director", "Quality Assurance Manager", 
                                                           "Transport Company Manager", "Finance Manager", "Human Resource Manager", "Engineering Manager", 
                                                           "Sales and Marketing Manager", "Construction Project Manager", "Hotel/Motel Manager", 
                                                           "ICT Managers nfd", "Supply and Distribution Manager", "Beef Cattle Farmer", "Production Manager", 
                                                           "Customer Service Manager", "Wholesaler", "Corporate General Manager", "School Principal", 
                                                           "Retail Manager", "Childcare Centre Manager", "Conference and Event Organiser", 
                                                           "Cafe or Restaurant Manager", "Clinical Psychologist", "Retail Pharmacist", 
                                                           "Early Childhood Teacher", "Registered Nurse(Mental Health)", "Social Worker", 
                                                           "Welfare Worker","Special Needs Teacher", "Registered Nurse(Surgical)", 
                                                           "Dentist", "Physiotherapist", "Primary School Teacher", "Minister of Religion","
                                                    Occupational Health and Safety Advisor", "Secondary School Teacher", 
                                                           "Vocational Education Teacher", "Private Tutors and Teaahers","Mechanial Engineer", 
                                                           "Urban and Regional Planner", "Systems Admin", "Architect Nurse Manager", 
                                                           "Medical Laboratory Scientist", "Solicitor", "Policy Analyst", "Graphic Designer", 
                                                           "Financial Investment Advisor", "Accountat(General)", "Human Resource Advisor", 
                                                           "Management Consultant", "Finance Broker", "Training and Development Professional", 
                                                           "Civil Engineer", "Public Relations Professional", "Civil Engineer", "Public Relations Professional", 
                                                           "Sales Representative", "Marketing Specialist","University Lecturer","ICT Business Analyst", 
                                                           "Pharmacy Sales Assistant","ICT Sales Assistant", "Sales Representative(Personal and Household Goods)",
                                                           "Real Estate Agent", "Retail Supervisor", "Motor Vehicle Part Interpreter", "Sales Assistant(General)",
                                                           "Real Estate Representative", "Ticket Seller", "PLumber", "Hairdresser", "Architectural Draftperson", 
                                                           "Welder", "Motor Mechanic", "Baker", "Cabinet Maker", "Electronic Equipment Trades Worker", "Fitter", 
                                                           "Fiborous Plasterer", "Telecommunications Technician", "Air Conditioning and Regfrigerating Mechanic", 
                                                           "Wall and Floor Tiler", "ICT Customer Support Officer", "Painting Trades Workers", "Building Associate", 
                                                           "Electrician", "Buthcer or Small Goods Maker", "Chef", "Carpenter", "Cook", "Bricklayer")),
                 
                 actionButton("submit", "Submit", class="btn-primary"),
                 shinyjs::hidden(
                   span(id = "submit_msg", "Submitting your responses. Please wait...")
                 )
               ),
               mainPanel(
                 fluidRow(
                   column(7,
                          h2("SCORE -1",
                            style = "font-family: 'Helvetica', cursive;
                                font-weight: 500; line-height: 1.1; 
                                color: #4d3a7d;")),
                   column(4,
                          shinydashboard::infoBoxOutput("tab1"
                          ))
                 ),
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 fluidRow(
                   column(7,
                          h2("SCORE -2",
                             style = "font-family: 'Helvetica', cursive;
                                font-weight: 500; line-height: 1.1; 
                                color: #4d3a7d;")),
                   column(4,
                          shinydashboard::infoBoxOutput("tab2")
                          )
                 ),
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 fluidRow(
                   column(7,
                          h2("SCORE -3",
                             style = "font-family: 'Helvetica', cursive;
                                font-weight: 500; line-height: 1.1; 
                                color: #4d3a7d;")),
                   column(4,
                          shinydashboard::infoBoxOutput("tab3")
                   )
                 ),
                 shinyjs::hidden(
                   div(
                     id = "thankyou_msg",
                     h3("Thanks, your response was submitted successfully!"),
                     actionLink("submit_another", "Submit another response")
                   )
                 )
                 )#main panel end
                 
               ),
      
      ################ END OF TAB 1  tableOutput("tab")
      tabPanel("Plot",
               div(
                 class="outer",
                 tags$head(includeCSS("www/style.css")),
                 leafletOutput("mymap", width="100%", height="100%")
               )
               )
      ######end of tab 2
                 
    )
    
    fieldsMandatory = c("age", "ethnicity", "health")
    fieldsAll = c("age", "ethnicity", "gender", "health", "major_occ", "occupation")# NEED TO UPDATE
    
    ## start of server
    ## start of server
    server = function(input, output, session) {
      
      observe({
        # check if all mandatory fields have a value
        mandatoryFilled <-
          vapply(fieldsMandatory,
                 function(x) {
                   !is.null(input[[x]]) && input[[x]] != "-Select-"
                 },
                 logical(1))
        mandatoryFilled <- all(mandatoryFilled)
        
        # enable/disable the submit button
        shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      })
      
      # formating data to be stored in csv
      formData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- c(data, timestamp = epochTime())
        data <- t(data)
        data
      })
      
      # method to save csv
      saveData <- function(data) {
        fileName <- sprintf("%s_%s.csv",
                            humanTime(),
                            digest::digest(data))
        
        print(data)
        
        write.csv(x = data, file = file.path("responses", fileName),
                  row.names = FALSE, quote = TRUE)
        
      }
      
      # action to take when submit button is pressed
      observeEvent(input$submit, {
        
        saveData(formData())
        
        output$tab1 = shinydashboard::renderInfoBox({
          shinydashboard::infoBox(
            paste0(filteredDF()),
            color = "purple"
          )
        })
        
        output$tab2 = shinydashboard::renderInfoBox({
          shinydashboard::infoBox( paste0(filteredOcc()),
                                   color = "purple"
          )
        })
        
        output$tab3 = shinydashboard::renderInfoBox({
          shinydashboard::infoBox( paste0(filteredDisease()),
                                   color = "purple"
          )
        })
        
      })
      
      observeEvent(input$submit_another, {
        shinyjs::reset("Form")
        shinyjs::show("form")
        shinyjs::hide("thankyou_msg")
      })
      
      
      
      
      filteredDF = reactive({
        if(input$age != '-Select' & input$ethnicity != 'Select')
        {
          res = AgeEthnicity%>%filter(Age == input$age & Ethnicity == input$ethnicity)
          res = res%>%pull("Risk Band")
          print(res)
          res
        }
        
      })
      
      filteredOcc = reactive({
        if(input$occupation != '-Select')
        {
          res = Jobs%>%filter(`Occupation` == input$occupation)
          res = res%>%pull("Risk Band by Infection Score")
          print(res)
          res
        }
      })
      
      filteredDisease = reactive({
        if(input$health != '-Select-')
        {
          temp = comorb[input$health]
          print(toString(temp))
          res = cmorbRisk[toString(temp)]
          print(res)
          res
        }
      })
      
      
      
      # hooking up map
      output$mymap = renderLeaflet({
        leaflet(data = nycounties)%>%
          addProviderTiles("OpenStreetMap")%>%
          addPolygons(fillColor = ~pal(pop), fillOpacity = 0.8, 
                      color = "#BDBDC3", 
                      weight = 1,
                      popup = test)%>%setView(lat = -26.458486, lng = 133.676557, zoom = 4)
      })
      
    }
    
    # Run the app ----
    shinyApp(ui = ui, server = server)
    
