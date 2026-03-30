#Uncomment to install libraries
#install.packages(c("shiny", "shinydashboard", "ggplot2", "DT", "randomForest"))

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(randomForest)

## Load dataset
data = read.csv("Housing.csv")

## Check Missing and Duplicated Value 

#Show sums of missing value for each column
colSums(is.na(data))

#Show sums of duplicated rows
sum(duplicated(data))

##Data Analysis 

#Convert categorical variables to factors (numerical)
data$furnishingstatus = as.factor(data$furnishingstatus)
data$mainroad = as.factor(data$mainroad)
data$guestroom = as.factor(data$guestroom)
data$basement = as.factor(data$basement)
data$hotwaterheating = as.factor(data$hotwaterheating)
data$airconditioning = as.factor(data$airconditioning)
data$prefarea = as.factor(data$prefarea)

#Get an overall summary from the dataset
summary(data)

## Build Linear Regression Model 

#Split Data
set.seed(123)
n <- nrow(data)
train_index <- sample(1:n, size = 0.8 * n)

trainData <- data[train_index, ] #gives 80% of dataset to trainData
testData <- data[-train_index, ] #gives 20% of dataset to testData

#Train Linear Regression Model
lm_model <- lm(price ~ area + bedrooms + bathrooms + stories +
                 mainroad + guestroom + basement +
                 hotwaterheating + airconditioning +
                 parking + prefarea + furnishingstatus,
               data = trainData)

#View model summary
summary(lm_model)

## Build Random Forest Regressor Model 
set.seed(123)

rf_model <- randomForest(
  price ~ .,
  data = trainData,
  ntree = 100,
  mtry = 3,
  importance = TRUE
)

summary(rf_model)

## Make Prediction 

#Linear Regression Model
pred_lm = predict(lm_model, newdata = testData)

results_lm <- data.frame(
  Actual = testData$price,
  Predicted = pred_lm
)

head(results_lm, 10)

#Random Forest Regressor Model
pred_rf <- predict(rf_model, newdata = testData)

results_rf <- data.frame(
  Actual = testData$price,
  Predicted = pred_rf
)

head(results_rf, 10)

## Evaluate Model 
# Mean Absolute Error (average error, can check how far off the error of the model is, higher = worser)
mae_lm <- mean(abs(pred_lm - testData$price))
mae_rf <- mean(abs(pred_rf - testData$price))

# Root Mean Squared Error (more sensitive, check how far off the error of the model is, higher = worser)
rmse_lm <- sqrt(mean((pred_lm - testData$price)^2))
rmse_rf <- sqrt(mean((pred_rf - testData$price)^2))

# R-squared (to check the model quality, higher = better)
r2_lm <- cor(pred_lm, testData$price)^2
r2_rf <- cor(pred_rf, testData$price)^2

## Build Dashboard

# Pre-compute metrics (reuses objects already built above)
options(scipen = 999) #Remove scientific notation

calc_metrics <- function(actual, predicted) {
  list(
    MAE  = mean(abs(predicted - actual)),
    RMSE = sqrt(mean((predicted - actual)^2)),
    R2   = cor(predicted, actual)^2
  )
}

m_lm <- calc_metrics(testData$price, pred_lm)
m_rf  <- calc_metrics(testData$price, pred_rf)


# UI
ui <- dashboardPage(
  skin = "black",
  
  # ------ Header ------
  dashboardHeader(
    title = tags$span(
      style = "font-family: 'Segoe UI', Georgia, serif; font-weight: 700; letter-spacing: 1px;",
      tags$img(src  = "https://cdn-icons-png.flaticon.com/128/5974/5974636.png",
               height = "24px",
               style  = "margin-right:8px; vertical-align:middle; filter:brightness(1.4);"),
      "House Price Dashboard"
    )
  ),
  
  # ------ Sidebar ------
  dashboardSidebar(
    tags$style(HTML("
      .main-sidebar { background: linear-gradient(180deg, #0f172a 0%, #1e293b 100%) !important; }
      .sidebar-menu > li > a {
        font-family: 'Segoe UI', Georgia, serif;
        font-size: 13px;
        letter-spacing: 0.3px;
        color: #cbd5e1 !important;
        border-left: 3px solid transparent;
        transition: all 0.3s ease;
        border-radius: 0 8px 8px 0;
      }
      .sidebar-menu > li.active > a,
      .sidebar-menu > li > a:hover {
        color: #0ea5e9 !important;
        background: rgba(14, 165, 233, 0.1) !important;
        border-left: 3px solid #0ea5e9 !important;
        box-shadow: 0 4px 12px rgba(14, 165, 233, 0.2);
      }
      .sidebar-menu > li > a > .fa { width: 20px; }
    ")),
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "s1", icon = icon("chart-line")),
      menuItem("Data Insights", tabName = "s2", icon = icon("chart-bar")),
      menuItem("Model Results", tabName = "s3", icon = icon("brain")),
      menuItem("Prediction", tabName = "s4", icon = icon("calculator"))
    )
  ),
  
  # ------ Body ------
  dashboardBody(
    
    tags$head(tags$style(HTML("
    
    /* =========================
       BASE BACKGROUND
    ========================= */
    body, .content-wrapper, .main-footer {
      background-color: linear-gradient(135deg, #0f172a 0%, #1e293b 50%, #334155 100%) !important;
      color: #e2e8f0 !important;
    }

    .content-wrapper { padding-top: 10px !important; }

    /* =========================
       SIDEBAR
    ========================= */
    .main-sidebar { 
      background: linear-gradient(180deg, #0f172a 0%, #1e293b 100%) !important;
      box-shadow: 2px 0 10px rgba(0,0,0,0.3); }

    .sidebar-menu > li > a {
      font-family: 'Segoe UI', Georgia, serif;
      font-size: 13px;
      color: #cbd5e1 !important;
      border-left: 3px solid transparent;
      transition: all 0.3s ease;
      border-radius: 0 8px 8px 0;
      margin: 2px 0;
    }

    .sidebar-menu > li.active > a,
    .sidebar-menu > li > a:hover {
      color: #0ea5e9 !important;
      background: rgba(14, 165, 233, 0.15) !important;
      border-left: 3px solid #0ea5e9 !important;
      box-shadow: 0 4px 12px rgba(14, 165, 233, 0.2);
    }

    /* =========================
       HEADER
    ========================= */
    .skin-black .main-header .navbar {
      background: linear-gradient(90deg, #0f172a 0%, #1e40af 70%, #1d4ed8 100%) !important;
    }

    .skin-black .main-header .logo {
      background: linear-gradient(90deg, #1e293b 0%, #1e40af 100%) !important;
    }
    
    /* HEADER TITLE - Pure white + strong shadow for gradient contrast */
    .main-header .logo span,
    .main-header .navbar-brand span,
    .main-header .logo,
    .navbar-brand {
      color: #ffffff !important;
      color: rgb(255, 255, 255) !important;
      text-shadow: 0 3px 12px rgba(0,0,0,0.9), 0 0 30px rgba(255,255,255,0.5) !important;
      font-weight: 900 !important;
    }

    /* Icon brightness boost */
    .main-header .logo img,
    .navbar-brand img {
      filter: brightness(2.5) contrast(1.4) drop-shadow(0 3px 8px rgba(0,0,0,0.8)) !important;
    }
    
    /* Override any inherited styles */
    .main-header .navbar .logo > span,
    .main-header .navbar-brand > span {
      color: #ffffff !important;
    }

    /* =========================
       BOXES (CARDS)
    ========================= */
    .box {
      background: rgba(30, 41, 59, 0.85) !important;
      backdrop-filter: blur(10px);
      border: 1px solid rgba(59, 130, 246, 0.2) !important;
      border-radius: 16px !important;
      box-shadow: 0 8px 32px rgba(0,0,0,0.4), 0 0 0 1px rgba(255,255,255,0.05) !important;
      transition: all 0.3s ease;
    }
    
    .box-hover {
      transform: translateY(-2px);
      box-shadow: 0 12px 40px rgba(0,0,0,0.5), 0 0 0 1px rgba(14, 165, 233, 0.3) !important;
    }

    .box-header {
      border-bottom: 1px solid rgba(71, 85, 105, 0.5) !important;
      font-family: 'Segoe UI', Georgia, serif;
      color: #f1f5f9 !important;
      background: rgba(15, 23, 42, 0.8) !important;
      border-radius: 16px 16px 0 0 !important;
    }

    .box-title {
      font-size: 15px !important;
      font-weight: 700 !important;
      color: #f8fafc !important;
      text-shadow: 0 1px 2px rgba(0,0,0,0.3);
    }

    /* =========================
       KPI CARDS
    ========================= */
    .kpi {
      background: linear-gradient(135deg, rgba(14, 165, 233, 0.15) 0%, rgba(59, 130, 246, 0.1) 100%);
      border-radius: 20px;
      padding: 25px;
      text-align: center;
      border: 1px solid rgba(14, 165, 233, 0.3);
      box-shadow: 0 8px 25px rgba(14, 165, 233, 0.15);
      transition: all 0.3s ease;
      backdrop-filter: blur(10px);
    }

    .kpi:hover { 
      transform: translateY(-5px);
      box-shadow: 0 15px 35px rgba(14, 165, 233, 0.25);
    }

    .kpi .kv {
      font-size: 28px;
      font-weight: 800;
      background: linear-gradient(135deg, #0ea5e9, #3b82f6);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      background-clip: text;
      text-shadow: none;
    }

    .kpi .kl {
      font-size: 12px;
      color: #94a3b8;
      text-transform: uppercase;
      letter-spacing: 1.2px;
      font-weight: 600;
    }

    /* =========================
       SECTION HEADINGS
    ========================= */
    .sec-head {
      font-size: 28px;
      font-weight: 800;
      background: linear-gradient(135deg, #0ea5e9, #3b82f6, #06b6d4);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      background-clip: text;
      margin-bottom: 8px;
    }

    .sec-sub {
      font-size: 14px;
      color: #94a3b8;
      margin-bottom: 20px;
      font-weight: 400;
    }

    /* =========================
       TABLE
    ========================= */
    .model-tbl th {
      background: linear-gradient(90deg, #1e293b, #334155) !important;
      color: #cbd5e1 !important;
      font-size: 12px;
      border-bottom: 1px solid rgba(71, 85, 105, 0.5);
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }

    .model-tbl td {
      color: #e2e8f0;
      border-bottom: 1px solid rgba(71, 85, 105, 0.3);
      font-weight: 500;
    }

    .model-tbl tr.winner td {
      background: linear-gradient(90deg, rgba(34, 197, 94, 0.2), rgba(6, 182, 212, 0.2)) !important;
      color: #22c55e !important;
      font-weight: 700;
      box-shadow: inset 0 0 10px rgba(34, 197, 94, 0.1);
    }
    
  /* =========================
   TABLE HOVER - Google Dark Blue (Overview tab)
  ========================= */
    /* FORCE override Bootstrap hover (fix flashbang) */
    .table-hover > tbody > tr:hover > td,
    .table-hover > tbody > tr:hover > th {
      background-color: rgba(14, 165, 233, 0.12) !important;
      color: #f1f5f9 !important;
      border-color: rgba(14, 165, 233, 0.3) !important;
      box-shadow: inset 0 0 10px rgba(14, 165, 233, 0.1) !important;
    }

    /* Optional: whole row glow */
    .table-hover > tbody > tr:hover {
      background-color: rgba(14, 165, 233, 0.08) !important;
    }

    /* =========================
       INSIGHT BOX
    ========================= */
    .insight-box {
      background: rgba(14, 165, 233, 0.1);
      border-left: 4px solid #0ea5e9;
      padding: 15px;
      font-size: 13px;
      color: #cbd5e1;
      border-radius: 10px;
      backdrop-filter: blur(10px);
      border: 1px solid rgba(14, 165, 233, 0.2);
    }

    /* =========================
       PREDICTION BOX
    ========================= */
    .pred-result {
      background: linear-gradient(135deg, #06b6d4 0%, #0ea5e9 50%, #3b82f6 100%);
      border-radius: 24px;
      padding: 35px;
      text-align: center;
      color: white;
      box-shadow: 0 20px 40px rgba(14, 165, 233, 0.4);
      backdrop-filter: blur(20px);
      border: 1px solid rgba(255,255,255,0.2);
    }

    .pred-result .pr-label {
      font-size: 16px;
      opacity: 0.9;
      margin-bottom: 10px;
      font-weight: 500;
    }

    .pred-result .pr-val {
      font-size: 48px;
      font-weight: 900;
      text-shadow: 0 4px 12px rgba(0,0,0,0.3);
      margin-bottom: 8px;
    }

    .pred-result .pr-sub {
      font-size: 14px;
      opacity: 0.9;
      font-weight: 500;
    }

    /* =========================
       INPUTS
    ========================= */
    .form-control, .selectize-input {
      background: rgba(15, 23, 42, 0.8) !important;
      border: 1px solid rgba(59, 130, 246, 0.3) !important;
      color: #f1f5f9 !important;
      border-radius: 12px !important;
      backdrop-filter: blur(10px);
      transition: all 0.3s ease;
    }
    
    .form-control:focus, .selectize-input:focus {
      border-color: #0ea5e9 !important;
      box-shadow: 0 0 0 3px rgba(14, 165, 233, 0.2) !important;
    }

    .form-label {
      color: #f1f5f9 !important;
      font-weight: 600 !important;
      font-size: 12px !important;
      margin-bottom: 6px !important;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }

    /* =========================
       BUTTON
    ========================= */
    .btn-predict {
      background: linear-gradient(135deg, #0ea5e9, #3b82f6) !important;
      color: white !important;
      border: none !important;
      border-radius: 12px !important;
      font-weight: 700 !important;
      font-size: 14px !important;
      padding: 12px 24px !important;
      transition: all 0.3s ease !important;
      box-shadow: 0 4px 15px rgba(14, 165, 233, 0.4) !important;
      text-transform: uppercase;
      letter-spacing: 0.5px;
    }

    .btn-predict:hover {
      background: linear-gradient(135deg, #0284c7, #1d4ed8) !important;
      transform: translateY(-3px) !important;
      box-shadow: 0 8px 25px rgba(14, 165, 233, 0.5) !important;
    }
    
    /* =========================
       SLIDER
    ========================= */
    .irs--shiny .irs-bar {
      background: linear-gradient(90deg, #0ea5e9, #3b82f6) !important;
      border-radius: 8px !important;
    }

    .irs--shiny .irs-handle {
      background: #0ea5e9 !important;
      border: 2px solid white !important;
      box-shadow: 0 4px 12px rgba(14, 165, 233, 0.4) !important;
    }
    
  /* =========================
   MODEL TABLE SPACING FIX
  ========================= */
    .model-tbl th,
    .model-tbl td {
      padding: 10px 18px !important;
      text-align: center;
    }

    /* Add space between columns */
    .model-tbl {
      border-collapse: separate !important;
      border-spacing: 8px 0px; /* horizontal gap */
    }
    
  "))),
    
    tabItems(
      
      # ============================================================
      # SECTION 1 — OVERVIEW
      # ============================================================
      tabItem(tabName = "s1",
              
              fluidRow(
                column(12,
                       tags$p(class="sec-head", "Overview"),
                       tags$p(class="sec-sub",
                              "Summary of the dataset")
                )
              ),
              
              # KPI cards
              fluidRow(
                column(3,
                       tags$div(class="kpi",
                                tags$div(class="kv", textOutput("kpi_avg")),
                                tags$div(class="kl", "Average Price"),
                       )
                ),
                column(3,
                       tags$div(class="kpi",
                                tags$div(class="kv", textOutput("kpi_max")),
                                tags$div(class="kl", "Maximum Price"),
                       )
                ),
                column(3,
                       tags$div(class="kpi",
                                tags$div(class="kv", textOutput("kpi_min")),
                                tags$div(class="kl", "Minimum Price"),
                       )
                ),
                column(3,
                       tags$div(class="kpi",
                                tags$div(class="kv", textOutput("kpi_total")),
                                tags$div(class="kl", "Total Houses"),
                       )
                )
              ),
              
              # Supporting summary stats
              fluidRow(
                box(title = "Price Statistics", status = "warning",
                    solidHeader = TRUE, width = 4,
                    tableOutput("price_stats")
                ),
                box(title = "Dataset at a Glance", status = "primary",
                    solidHeader = TRUE, width = 4,
                    tableOutput("glance_table")
                ),
                box(title = "Furnishing Pie Chart", status = "success",
                    solidHeader = TRUE, width = 4,
                    plotOutput("furnish_pie", height = "200px")
                )
              )
      ),
      
      # ============================================================
      # SECTION 2 — DATA INSIGHTS
      # ============================================================
      tabItem(tabName = "s2",
              
              fluidRow(
                column(12,
                       tags$p(class="sec-head", "Data Insights"),
                       tags$p(class="sec-sub", "Key relationships and patterns in the Housing data")
                )
              ),
              
              # Chart 1 — Area vs Price (full width, most important)
              fluidRow(
                box(title = "Area vs Price",
                    status = "warning", solidHeader = TRUE, width = 12,
                    tags$div(class="insight-box",
                             "Insight: Bigger area, higher price.
               Area is the strongest numeric predictor of house price."
                    ),
                    plotOutput("chart_area_price", height = "320px")
                )
              ),
              
              # Chart 2 + Chart 3 — side by side bar charts
              fluidRow(
                box(title = "Price by Furnishing Status",
                    status = "info", solidHeader = TRUE, width = 6,
                    tags$div(class="insight-box",
                             "Insight: Furnished houses command a higher average price."
                    ),
                    plotOutput("chart_furnish", height = "280px")
                ),
                box(title = "Price by Air Conditioning",
                    status = "danger", solidHeader = TRUE, width = 6,
                    tags$div(class="insight-box",
                             "Insight: Houses with AC are priced significantly higher."
                    ),
                    plotOutput("chart_ac", height = "280px")
                )
              ),
              
              # Chart 4 — Price distribution
              fluidRow(
                box(title = "Price Distribution",
                    status = "success", solidHeader = TRUE, width = 12,
                    tags$div(class="insight-box",
                             "Insight: Most houses fall in the mid-price range.
               The distribution is right-skewed with a few high-value outliers."
                    ),
                    plotOutput("chart_hist", height = "280px")
                )
              )
      ),
      
      # ============================================================
      # SECTION 3 — MODEL RESULTS
      # ============================================================
      tabItem(tabName = "s3",
              
              fluidRow(
                column(12,
                       tags$p(class="sec-head", "Model Results"),
                       tags$p(class="sec-sub",
                              "Comparision between Linear Regression and Random Forest")
                )
              ),
              
              # Simple model comparison table
              fluidRow(
                box(title = "Model Comparison Table", status = "primary",
                    solidHeader = TRUE, width = 7,
                    tags$table(class="model-tbl",
                               tags$thead(
                                 tags$tr(
                                   tags$th("Model"),
                                   tags$th("MAE"),
                                   tags$th("RMSE"),
                                   tags$th("R\u00b2")
                                 )
                               ),
                               tags$tbody(
                                 tags$tr(
                                   tags$td("Linear Regression"),
                                   tags$td(textOutput("tbl_lm_mae",  inline=TRUE)),
                                   tags$td(textOutput("tbl_lm_rmse", inline=TRUE)),
                                   tags$td(textOutput("tbl_lm_r2",   inline=TRUE))
                                 ),
                                 tags$tr(class="winner",
                                         tags$td(tags$span("Random Forest"),
                                                 tags$span(class="badge-best", "Best")),
                                         tags$td(textOutput("tbl_rf_mae",  inline=TRUE)),
                                         tags$td(textOutput("tbl_rf_rmse", inline=TRUE)),
                                         tags$td(textOutput("tbl_rf_r2",   inline=TRUE))
                                 )
                               )
                    ),
                    tags$br(),
                    tags$div(class="insight-box",
                             "Random Forest performs better: lower MAE & RMSE,
               higher R\u00b2 (captures non-linear patterns that
               Linear Regression misses.)"
                    )
                ),
                
                box(title = "R\u00b2 Comparison", status = "success",
                    solidHeader = TRUE, width = 5,
                    plotOutput("r2_bar", height = "180px"),
                    tags$br(),
                    tags$div(class="insight-box",
                             "R\u00b2 = proportion of price variance explained by the model.
               Closer to 100% is better."
                    )
                )
              ),
              
              # Actual vs Predicted
              fluidRow(
                box(title = "Actual vs Predicted \u2014 Linear Regression",
                    status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("lm_avp", height = "260px")
                ),
                box(title = "Actual vs Predicted \u2014 Random Forest",
                    status = "success", solidHeader = TRUE, width = 6,
                    plotOutput("rf_avp", height = "260px")
                )
              )
      ),
      
      # ============================================================
      # SECTION 4 — PREDICTION
      # ============================================================
      tabItem(tabName = "s4",
              
              fluidRow(
                column(12,
                       tags$p(class="sec-head", "Predict a House Price"),
                       tags$p(class="sec-sub",
                              "Enter house details (Random Forest model)")
                )
              ),
              
              fluidRow(
                
                # Input form
                box(title = "House Details", status = "warning",
                    solidHeader = TRUE, width = 5,
                    
                    tags$div(class="form-label", "Area (sq ft)"),
                    sliderInput("p_area", label=NULL,
                                min=500, max=16200, value=5000, step=100),
                    
                    fluidRow(
                      column(6,
                             tags$div(class="form-label", "Bedrooms"),
                             numericInput("p_bed", label=NULL, value=3, min=1, max=6)
                      ),
                      column(6,
                             tags$div(class="form-label", "Bathrooms"),
                             numericInput("p_bath", label=NULL, value=2, min=1, max=4)
                      )
                    ),
                    fluidRow(
                      column(6,
                             tags$div(class="form-label", "Stories"),
                             numericInput("p_stories", label=NULL, value=2, min=1, max=4)
                      ),
                      column(6,
                             tags$div(class="form-label", "Parking Spots"),
                             numericInput("p_parking", label=NULL, value=1, min=0, max=3)
                      )
                    ),
                    fluidRow(
                      column(6,
                             tags$div(class="form-label", "Air Conditioning"),
                             selectInput("p_ac", label=NULL,
                                         choices=c("yes","no"), selected="yes")
                      ),
                      column(6,
                             tags$div(class="form-label", "Preferred Area"),
                             selectInput("p_prefarea", label=NULL,
                                         choices=c("yes","no"), selected="yes")
                      )
                    ),
                    fluidRow(
                      column(6,
                             tags$div(class="form-label", "Main Road"),
                             selectInput("p_mainroad", label=NULL,
                                         choices=c("yes","no"), selected="yes")
                      ),
                      column(6,
                             tags$div(class="form-label", "Basement"),
                             selectInput("p_basement", label=NULL,
                                         choices=c("yes","no"), selected="no")
                      )
                    ),
                    fluidRow(
                      column(6,
                             tags$div(class="form-label", "Guest Room"),
                             selectInput("p_guest", label=NULL,
                                         choices=c("yes","no"), selected="no")
                      ),
                      column(6,
                             tags$div(class="form-label", "Hot Water Heating"),
                             selectInput("p_hotwater", label=NULL,
                                         choices=c("yes","no"), selected="no")
                      )
                    ),
                    tags$div(class="form-label", "Furnishing Status"),
                    selectInput("p_furnish", label=NULL,
                                choices=c("furnished","semi-furnished","unfurnished"),
                                selected="furnished"),
                    tags$br(),
                    actionButton("predict_btn", "Calculate Predicted Price",
                                 class="btn-predict",
                                 icon=icon("calculator"))
                ),
                
                # Result panel
                column(7,
                       
                       # Main prediction output
                       tags$div(class="pred-result",
                                tags$div(class="pr-label", "Random Forest Predicted Price"),
                                tags$div(class="pr-val",   textOutput("pred_out")),
                                tags$div(class="pr-sub",   paste0("Model R\u00b2: ",
                                                                  round(m_rf$R2*100, 1), "% accuracy"))
                       ),
                       
                       # Feature summary of the entered house
                       box(title = "Your House at a Glance", status = "primary",
                           solidHeader = FALSE, width = 12,
                           tableOutput("house_summary")
                       ),
                       
                       # How it compares to dataset average
                       box(title = "How It Compares to the Dataset Average",
                           status = "success", solidHeader = FALSE, width = 12,
                           plotOutput("pred_vs_avg", height = "180px")
                       )
                )
              )
      )
      
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage


# ============================================================
# SERVER
# ============================================================

# Dark ggplot2 theme matching the dashboard
theme_dash <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background    = element_rect(fill="#1a2a3a", color=NA),
      panel.background   = element_rect(fill="#1a2a3a", color=NA),
      panel.grid.major   = element_line(color="#243545", linewidth=0.4),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(color="#7a9bb5", size=11),
      axis.title         = element_text(color="#aab4be", size=12),
      plot.title         = element_text(color="#cdd9e5", size=13, face="bold"),
      legend.background  = element_rect(fill="#1a2a3a", color=NA),
      legend.text        = element_text(color="#aab4be"),
      strip.text         = element_text(color="#aab4be")
    )
}

server <- function(input, output, session) {
  
  # --------------------------------------------------------------
  # SECTION 1 — Overview KPIs
  # --------------------------------------------------------------
  
  fmt_price <- function(x) paste0("RM ", format(round(x), big.mark=","))
  
  output$kpi_avg   <- renderText(fmt_price(mean(data$price)))
  output$kpi_max   <- renderText(fmt_price(max(data$price)))
  output$kpi_min   <- renderText(fmt_price(min(data$price)))
  output$kpi_total <- renderText(format(nrow(data), big.mark=","))
  
  output$price_stats <- renderTable({
    x <- data$price
    data.frame(
      Statistic = c("Mean", "Median", "Std Dev", "Min", "Max"),
      Value     = c(fmt_price(mean(x)), fmt_price(median(x)),
                    fmt_price(sd(x)),   fmt_price(min(x)), fmt_price(max(x)))
    )
  }, striped=FALSE, bordered=FALSE, hover=TRUE, align="lr")
  
  output$glance_table <- renderTable({
    data.frame(
      Item  = c("Total Houses", "Numeric Variables", "Categorical Variables",
                "Missing Values", "Duplicated Rows"),
      Value = c(nrow(data), 6, 7, sum(is.na(data)), sum(duplicated(data)))
    )
  }, striped=FALSE, bordered=FALSE, hover=TRUE, align="lr")
  
  output$furnish_pie <- renderPlot({
    df <- as.data.frame(table(data$furnishingstatus))
    names(df) <- c("Status","Count")
    df$Pct <- round(df$Count / sum(df$Count) * 100, 1)
    df$Label <- paste0(df$Status, "\n", df$Pct, "%")
    ggplot(df, aes(x="", y=Count, fill=Status)) +
      geom_col(width=1, color="#1a2a3a", linewidth=0.5) +
      coord_polar("y") +
      geom_text(aes(label=Label), position=position_stack(vjust=0.5),
                color="white", size=3.2, fontface="bold") +
      scale_fill_manual(values=c("#f0c040","#4a9eda","#4ade80")) +
      theme_void() +
      theme(
        plot.background = element_rect(fill="#1a2a3a", color=NA),
        legend.position = "none"
      )
  })
  
  # --------------------------------------------------------------
  # SECTION 2 — Data Insights
  # --------------------------------------------------------------
  
  output$chart_area_price <- renderPlot({
    ggplot(data, aes(x=area, y=price)) +
      geom_point(color="#f0c040", alpha=0.5, size=2.2) +
      geom_smooth(method="lm", color="#4a9eda", se=TRUE,
                  fill="#4a9eda", alpha=0.15, linewidth=1.2) +
      scale_y_continuous(labels=function(x) paste0("RM ", format(x, big.mark=","))) +
      labs(x="Area (sq ft)", y="Price") +
      theme_dash()
  })
  
  output$chart_furnish <- renderPlot({
    df <- aggregate(price ~ furnishingstatus, data=data, FUN=mean)
    df$price <- round(df$price)
    df <- df[order(-df$price), ]
    ggplot(df, aes(x=reorder(furnishingstatus, price), y=price,
                   fill=furnishingstatus)) +
      geom_col(width=0.55, show.legend=FALSE, alpha=0.9) +
      geom_text(aes(label=paste0("RM ", format(price, big.mark=","))),
                hjust=-0.08, color="#cdd9e5", size=3.5) +
      scale_fill_manual(values=c("#f0c040","#4a9eda","#4ade80")) +
      scale_y_continuous(expand=expansion(mult=c(0, 0.2))) +
      coord_flip() +
      labs(x=NULL, y="Average Price") +
      theme_dash() +
      theme(panel.grid.major.y=element_blank())
  })
  
  output$chart_ac <- renderPlot({
    df <- aggregate(price ~ airconditioning, data=data, FUN=mean)
    df$price <- round(df$price)
    df$airconditioning <- ifelse(df$airconditioning=="yes",
                                 "With AC", "Without AC")
    ggplot(df, aes(x=airconditioning, y=price, fill=airconditioning)) +
      geom_col(width=0.45, show.legend=FALSE, alpha=0.9) +
      geom_text(aes(label=paste0("RM ", format(price, big.mark=","))),
                vjust=-0.5, color="#cdd9e5", size=3.8, fontface="bold") +
      scale_fill_manual(values=c("#4ade80","#e74c3c")) +
      scale_y_continuous(expand=expansion(mult=c(0, 0.18)),
                         labels=function(x) paste0("RM ", format(x, big.mark=","))) +
      labs(x=NULL, y="Average Price") +
      theme_dash() +
      theme(panel.grid.major.x=element_blank())
  })
  
  output$chart_hist <- renderPlot({
    avg_p <- mean(data$price)
    ggplot(data, aes(x=price)) +
      geom_histogram(fill="#4a9eda", color="#1a2a3a", bins=30, alpha=0.85) +
      geom_vline(xintercept=avg_p, color="#f0c040",
                 linewidth=1.2, linetype="dashed") +
      annotate("text", x=avg_p, y=Inf,
               label=paste0("Avg: RM ", format(round(avg_p), big.mark=",")),
               vjust=2, hjust=-0.1, color="#f0c040", size=3.8) +
      scale_x_continuous(labels=function(x) paste0("RM ", format(x, big.mark=","))) +
      labs(x="Price", y="Number of Houses") +
      theme_dash()
  })
  
  # --------------------------------------------------------------
  # SECTION 3 — Model Results
  # --------------------------------------------------------------
  
  output$tbl_lm_mae  <- renderText(format(round(m_lm$MAE),  big.mark=","))
  output$tbl_lm_rmse <- renderText(format(round(m_lm$RMSE), big.mark=","))
  output$tbl_lm_r2   <- renderText(paste0(round(m_lm$R2*100, 1), "%"))
  output$tbl_rf_mae  <- renderText(format(round(m_rf$MAE),  big.mark=","))
  output$tbl_rf_rmse <- renderText(format(round(m_rf$RMSE), big.mark=","))
  output$tbl_rf_r2   <- renderText(paste0(round(m_rf$R2*100, 1), "%"))
  
  output$r2_bar <- renderPlot({
    df <- data.frame(
      Model = c("Linear\nRegression", "Random\nForest"),
      R2    = c(round(m_lm$R2*100, 1), round(m_rf$R2*100, 1))
    )
    ggplot(df, aes(x=Model, y=R2, fill=Model)) +
      geom_col(width=0.4, show.legend=FALSE, alpha=0.9) +
      geom_text(aes(label=paste0(R2, "%")), vjust=-0.4,
                color="#cdd9e5", size=4.5, fontface="bold") +
      scale_fill_manual(values=c("#4a9eda","#4ade80")) +
      scale_y_continuous(limits=c(0, 105), expand=expansion(mult=c(0, 0))) +
      labs(x=NULL, y="R\u00b2 (%)") +
      theme_dash() +
      theme(panel.grid.major.x=element_blank())
  })
  
  output$lm_avp <- renderPlot({
    df <- data.frame(Actual=testData$price, Predicted=pred_lm)
    ggplot(df, aes(x=Actual, y=Predicted)) +
      geom_point(color="#4a9eda", alpha=0.45, size=1.8) +
      geom_abline(slope=1, intercept=0, color="#f0c040", linewidth=1) +
      scale_x_continuous(labels=function(x) paste0("RM ", format(x, big.mark=","))) +
      scale_y_continuous(labels=function(x) paste0("RM ", format(x, big.mark=","))) +
      labs(x="Actual Price", y="Predicted Price") +
      theme_dash()
  })
  
  output$rf_avp <- renderPlot({
    df <- data.frame(Actual=testData$price, Predicted=pred_rf)
    ggplot(df, aes(x=Actual, y=Predicted)) +
      geom_point(color="#4ade80", alpha=0.45, size=1.8) +
      geom_abline(slope=1, intercept=0, color="#f0c040", linewidth=1) +
      scale_x_continuous(labels=function(x) paste0("RM ", format(x, big.mark=","))) +
      scale_y_continuous(labels=function(x) paste0("RM ", format(x, big.mark=","))) +
      labs(x="Actual Price", y="Predicted Price") +
      theme_dash()
  })
  
  # --------------------------------------------------------------
  # SECTION 4 — Prediction
  # --------------------------------------------------------------
  
  prediction <- eventReactive(input$predict_btn, {
    new_house <- data.frame(
      area             = input$p_area,
      bedrooms         = input$p_bed,
      bathrooms        = input$p_bath,
      stories          = input$p_stories,
      mainroad         = factor(input$p_mainroad, levels=levels(data$mainroad)),
      guestroom        = factor(input$p_guest,    levels=levels(data$guestroom)),
      basement         = factor(input$p_basement, levels=levels(data$basement)),
      hotwaterheating  = factor(input$p_hotwater, levels=levels(data$hotwaterheating)),
      airconditioning  = factor(input$p_ac,       levels=levels(data$airconditioning)),
      parking          = input$p_parking,
      prefarea         = factor(input$p_prefarea, levels=levels(data$prefarea)),
      furnishingstatus = factor(input$p_furnish,  levels=levels(data$furnishingstatus))
    )
    round(predict(rf_model, new_house))
  })
  
  output$pred_out <- renderText({
    req(input$predict_btn)
    paste0("RM ", format(prediction(), big.mark=","))
  })
  
  output$house_summary <- renderTable({
    data.frame(
      Feature  = c("Area", "Bedrooms", "Bathrooms", "Stories",
                   "Parking", "Air Conditioning", "Furnishing", "Preferred Area"),
      Value    = c(paste0(input$p_area, " sq ft"),
                   input$p_bed, input$p_bath, input$p_stories, input$p_parking,
                   input$p_ac, input$p_furnish, input$p_prefarea)
    )
  }, striped=FALSE, bordered=FALSE, hover=TRUE, align="lr")
  
  output$pred_vs_avg <- renderPlot({
    req(input$predict_btn)
    df <- data.frame(
      Label = c("Dataset\nAverage", "Your\nHouse"),
      Price = c(round(mean(data$price)), prediction())
    )
    ggplot(df, aes(x=Label, y=Price, fill=Label)) +
      geom_col(width=0.4, show.legend=FALSE, alpha=0.9) +
      geom_text(aes(label=paste0("RM ", format(Price, big.mark=","))),
                vjust=-0.4, color="#cdd9e5", size=4, fontface="bold") +
      scale_fill_manual(values=c("#4a9eda","#f0c040")) +
      scale_y_continuous(expand=expansion(mult=c(0, 0.18)),
                         labels=function(x) paste0("RM ", format(x, big.mark=","))) +
      labs(x=NULL, y="Price") +
      theme_dash() +
      theme(panel.grid.major.x=element_blank())
  })
}

## Run Dashboard
shinyApp(ui = ui, server = server)