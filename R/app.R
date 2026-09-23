# ============================================================
# DIABETES RISK PREDICTION SYSTEM
# R SHINY APPLICATION
# ============================================================

library(shiny)


# ============================================================
# 1. LOAD DATA
# ============================================================

if (file.exists("../data/diabetes.csv")) {
  data <- read.csv("../data/diabetes.csv")
} else {
  data <- read.csv("data/diabetes.csv")
}


# Prepare categorical columns
data$gender <- as.factor(data$gender)
data$smoking_history <- as.factor(data$smoking_history)


# ============================================================
# 2. TRAIN AND TEST DATA
# ============================================================

set.seed(123)

train_index <- c(
  sample(
    which(data$diabetes == 0),
    size = 0.8 * sum(data$diabetes == 0)
  ),
  sample(
    which(data$diabetes == 1),
    size = 0.8 * sum(data$diabetes == 1)
  )
)

train_data <- data[train_index, ]
test_data <- data[-train_index, ]


# ============================================================
# 3. LOGISTIC REGRESSION MODEL
# ============================================================

logistic_model <- glm(
  diabetes ~ age + bmi + HbA1c_level + blood_glucose_level +
    hypertension + heart_disease + gender + smoking_history,
  data = train_data,
  family = binomial
)


# ============================================================
# 4. MODEL PERFORMANCE
# ============================================================

test_probability <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

test_prediction <- ifelse(
  test_probability >= 0.5,
  1,
  0
)

actual <- test_data$diabetes


# Confusion matrix values

TP <- sum(test_prediction == 1 & actual == 1)
TN <- sum(test_prediction == 0 & actual == 0)
FP <- sum(test_prediction == 1 & actual == 0)
FN <- sum(test_prediction == 0 & actual == 1)


# Performance measures

accuracy <- (TP + TN) / length(actual)

sensitivity <- TP / (TP + FN)

specificity <- TN / (TN + FP)

precision <- TP / (TP + FP)

f1_score <- 2 * precision * sensitivity /
  (precision + sensitivity)


# AUC calculation
# Using ranking method so no extra package is required

positive_scores <- test_probability[actual == 1]
negative_scores <- test_probability[actual == 0]

auc <- sum(
  outer(
    positive_scores,
    negative_scores,
    FUN = ">"
  )
) / (length(positive_scores) * length(negative_scores))


# ============================================================
# 5. GRAPH FILE LOCATION
# ============================================================

if (dir.exists("../graphs")) {
  graphs_path <- "../graphs"
} else {
  graphs_path <- "graphs"
}

addResourcePath(
  "graphs",
  normalizePath(graphs_path)
)


# ============================================================
# 6. USER INTERFACE
# ============================================================

ui <- fluidPage(
  
  # ----------------------------------------------------------
  # CSS
  # ----------------------------------------------------------
  
  tags$head(
    
    tags$style(HTML("

      body {
        margin: 0;
        background-color: #f4f8fc;
        font-family: Arial, sans-serif;
        color: #17365d;
      }

      /* HEADER */

      .top-header {
        height: 92px;
        background-color: #173f6d;
        color: white;
        display: flex;
        align-items: center;
        padding: 0 35px;
        margin: -15px -15px 0 -15px;
      }

      .header-icon {
        font-size: 42px;
        margin-right: 18px;
      }

      .header-title {
        font-size: 28px;
        font-weight: bold;
      }

      .header-subtitle {
        font-size: 16px;
        margin-top: 5px;
        color: #dbe8f7;
      }

      .header-right {
        margin-left: auto;
        font-size: 15px;
      }


      /* SIDEBAR */

      .sidebar-area {
        position: fixed;
        top: 92px;
        left: 0;
        bottom: 0;
        width: 255px;
        background-color: #173f6d;
        color: white;
        padding-top: 20px;
        z-index: 1000;
      }

      .nav-button {
        width: 100%;
        padding: 18px 25px;
        font-size: 16px;
        color: white !important;
        background-color: transparent !important;
        border: none !important;
        border-radius: 0 !important;
        text-align: left;
        box-shadow: none !important;
      }

      .nav-button:hover {
        background-color: #285b91 !important;
      }

      .nav-button.active {
        background-color: #3f86d8 !important;
        border-left: 5px solid #8fc7ff !important;
      }


      /* MAIN AREA */

      .main-area {
        margin-left: 255px;
        padding: 25px 28px;
      }


      /* INTRO */

      .intro-card {
        background-color: #e7f3ff;
        border: 1px solid #cfe4f8;
        border-radius: 10px;
        padding: 25px;
        margin-bottom: 22px;
      }

      .intro-title {
        font-size: 27px;
        font-weight: bold;
        color: #173f6d;
        margin-bottom: 10px;
      }

      .intro-text {
        font-size: 16px;
        color: #36516f;
        line-height: 1.5;
      }


      /* CARDS */

      .main-card,
      .page-card {
        background-color: white;
        border: 1px solid #e1e8ef;
        border-radius: 10px;
        padding: 25px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.04);
        margin-bottom: 20px;
      }

      .main-card {
        min-height: 530px;
      }

      .card-title,
      .page-card-title {
        font-size: 22px;
        font-weight: bold;
        color: #173f6d;
        margin-bottom: 20px;
      }

      .page-text {
        font-size: 16px;
        color: #52697d;
        line-height: 1.6;
      }


      /* FORM */

      .control-label {
        color: #173f6d !important;
        font-weight: bold;
      }

      .form-control {
        border-radius: 7px;
        border: 1px solid #d6e0ea;
        height: 43px;
      }

      .form-group {
        margin-bottom: 18px;
      }


      /* PREDICT BUTTON */

      .predict-button {
        width: 100%;
        background-color: #2878e3;
        color: white;
        border: none;
        border-radius: 7px;
        padding: 13px;
        font-size: 16px;
        font-weight: bold;
        margin-top: 10px;
      }

      .predict-button:hover {
        background-color: #1765c8;
      }


      /* RESULT */

      .result-box {
        background-color: #eefbf6;
        border: 1px solid #c9eddf;
        border-radius: 10px;
        padding: 30px;
        text-align: center;
        margin-bottom: 20px;
      }

      .result-icon {
        font-size: 50px;
        color: #20b77a;
      }

      .result-title {
        font-size: 28px;
        font-weight: bold;
        color: #18a36c;
      }

      .result-description {
        font-size: 15px;
        color: #52697d;
        margin-top: 12px;
      }

      .probability-box {
        background-color: #e8f8f3;
        border-radius: 9px;
        padding: 22px;
        text-align: center;
        margin-top: 20px;
      }

      .probability-label {
        color: #178c69;
        font-weight: bold;
        font-size: 16px;
      }

      .probability-value {
        color: #15966d;
        font-size: 34px;
        font-weight: bold;
      }

      .probability-class {
        color: #60758a;
        font-size: 13px;
      }


      /* BMI */

      .bmi-box {
        background-color: #edf6ff;
        border: 1px solid #d7e9fa;
        border-radius: 9px;
        padding: 18px;
        margin-bottom: 20px;
      }

      .bmi-label {
        color: #2878c7;
        font-weight: bold;
      }

      .bmi-value {
        color: #173f6d;
        font-size: 28px;
        font-weight: bold;
      }


      /* NOTE */

      .note-box {
        background-color: #edf6ff;
        border: 1px solid #cfe4f8;
        border-radius: 9px;
        padding: 18px;
        color: #52697d;
        font-size: 14px;
      }


      /* STATISTIC BOX */

      .stat-box {
        background-color: white;
        border: 1px solid #e1e8ef;
        border-radius: 10px;
        padding: 22px;
        text-align: center;
        margin-bottom: 20px;
      }

      .stat-title {
        color: #60758a;
        font-size: 15px;
      }

      .stat-value {
        color: #173f6d;
        font-size: 30px;
        font-weight: bold;
        margin-top: 8px;
      }


      /* GRAPH */

      .graph-box {
        background-color: white;
        border: 1px solid #e1e8ef;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 20px;
        text-align: center;
      }

      .graph-title {
        font-size: 18px;
        font-weight: bold;
        color: #173f6d;
        margin-bottom: 12px;
      }

      .graph-box img {
        max-width: 100%;
        height: auto;
      }


      /* PERFORMANCE */

      .performance-box {
        background-color: #edf6ff;
        border: 1px solid #d7e9fa;
        border-radius: 10px;
        padding: 22px;
        text-align: center;
        margin-bottom: 20px;
      }

      .performance-value {
        font-size: 30px;
        font-weight: bold;
        color: #173f6d;
      }

      .performance-label {
        font-size: 14px;
        color: #60758a;
        margin-top: 5px;
      }


      /* FOOTER */

      .footer {
        margin-top: 25px;
        padding: 18px 5px;
        border-top: 1px solid #dce5ed;
        color: #60758a;
        font-size: 13px;
      }

    "))
  ),
  
  
  # ==========================================================
  # HEADER
  # ==========================================================
  
  div(
    class = "top-header",
    
    div(
      class = "header-icon",
      "♥"
    ),
    
    div(
      
      div(
        class = "header-title",
        "Diabetes Risk Prediction System"
      ),
      
      div(
        class = "header-subtitle",
        "Statistical Modelling Based Prediction"
      )
    ),
    
    div(
      class = "header-right",
      "ⓘ  R Shiny Application"
    )
  ),
  
  
  # ==========================================================
  # SIDEBAR
  # ==========================================================
  
  uiOutput("sidebar"),
  
  
  # ==========================================================
  # MAIN CONTENT
  # ==========================================================
  
  div(
    class = "main-area",
    
    uiOutput("main_content"),
    
    div(
      class = "footer",
      
      "© 2026 Disease Risk Prediction Model",
      
      span(
        style = "margin-left:20px;",
        "| Built with R Shiny"
      ),
      
      span(
        style = "float:right;",
        "For Educational Purposes Only"
      )
    )
  )
)


# ============================================================
# 7. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # CURRENT PAGE
  # ==========================================================
  
  current_page <- reactiveVal("prediction")
  
  
  # ==========================================================
  # SIDEBAR
  # ==========================================================
  
  output$sidebar <- renderUI({
    
    page <- current_page()
    
    div(
      class = "sidebar-area",
      
      actionButton(
        "nav_prediction",
        "⌂   Prediction",
        class = paste(
          "nav-button",
          if (page == "prediction") "active" else ""
        )
      ),
      
      actionButton(
        "nav_analysis",
        "▣   Data Analysis",
        class = paste(
          "nav-button",
          if (page == "analysis") "active" else ""
        )
      ),
      
      actionButton(
        "nav_visualizations",
        "▥   Visualizations",
        class = paste(
          "nav-button",
          if (page == "visualizations") "active" else ""
        )
      ),
      
      actionButton(
        "nav_performance",
        "◔   Model Performance",
        class = paste(
          "nav-button",
          if (page == "performance") "active" else ""
        )
      ),
      
      actionButton(
        "nav_about",
        "ⓘ   About",
        class = paste(
          "nav-button",
          if (page == "about") "active" else ""
        )
      ),
      
      div(
        style = "
          position:absolute;
          bottom:30px;
          left:25px;
          color:#cbdced;
          font-size:13px;
          line-height:1.5;
        ",
        
        "♥",
        br(),
        "Better Insights",
        br(),
        "for a Healthier Tomorrow"
      )
    )
  })
  
  
  # ==========================================================
  # NAVIGATION
  # ==========================================================
  
  observeEvent(input$nav_prediction, {
    current_page("prediction")
  })
  
  observeEvent(input$nav_analysis, {
    current_page("analysis")
  })
  
  observeEvent(input$nav_visualizations, {
    current_page("visualizations")
  })
  
  observeEvent(input$nav_performance, {
    current_page("performance")
  })
  
  observeEvent(input$nav_about, {
    current_page("about")
  })
  
  
  # ==========================================================
  # BMI
  # ==========================================================
  
  bmi_value <- reactive({
    
    height_m <- input$height / 100
    
    bmi <- input$weight / (height_m ^ 2)
    
    round(bmi, 2)
  })
  
  
  # ==========================================================
  # PREDICTION
  # ==========================================================
  
  prediction_result <- eventReactive(
    input$predict,
    {
      
      new_patient <- data.frame(
        
        age = input$age,
        
        bmi = bmi_value(),
        
        HbA1c_level = input$hba1c,
        
        blood_glucose_level = input$glucose,
        
        hypertension = as.numeric(
          input$hypertension
        ),
        
        heart_disease = as.numeric(
          input$heart_disease
        ),
        
        gender = factor(
          input$gender,
          levels = levels(data$gender)
        ),
        
        smoking_history = factor(
          input$smoking,
          levels = levels(data$smoking_history)
        )
      )
      
      
      probability <- predict(
        logistic_model,
        newdata = new_patient,
        type = "response"
      )
      
      
      prediction <- ifelse(
        probability >= 0.5,
        1,
        0
      )
      
      
      list(
        probability = probability,
        prediction = prediction
      )
    },
    
    ignoreNULL = FALSE
  )
  
  
  # ==========================================================
  # MAIN CONTENT
  # ==========================================================
  
  output$main_content <- renderUI({
    
    page <- current_page()
    
    
    # ========================================================
    # PAGE 1: PREDICTION
    # ========================================================
    
    if (page == "prediction") {
      
      tagList(
        
        div(
          class = "intro-card",
          
          div(
            class = "intro-title",
            "Predict Diabetes Risk"
          ),
          
          div(
            class = "intro-text",
            
            "Enter the patient details below to get the predicted diabetes risk using our statistical modelling approach."
          )
        ),
        
        
        fluidRow(
          
          # PATIENT INFORMATION
          
          column(
            6,
            
            div(
              class = "main-card",
              
              div(
                class = "card-title",
                "♙  Patient Information"
              ),
              
              
              fluidRow(
                
                column(
                  6,
                  
                  selectInput(
                    "gender",
                    "Gender *",
                    choices = c(
                      "Female",
                      "Male"
                    )
                  )
                ),
                
                column(
                  6,
                  
                  numericInput(
                    "age",
                    "Age *",
                    value = 45,
                    min = 11,
                    max = 80
                  )
                )
              ),
              
              
              fluidRow(
                
                column(
                  6,
                  
                  numericInput(
                    "height",
                    "Height (cm) *",
                    value = 170,
                    min = 100,
                    max = 220
                  )
                ),
                
                column(
                  6,
                  
                  numericInput(
                    "weight",
                    "Weight (kg) *",
                    value = 65,
                    min = 20,
                    max = 150
                  )
                )
              ),
              
              
              fluidRow(
                
                column(
                  6,
                  
                  selectInput(
                    "hypertension",
                    "Hypertension *",
                    
                    choices = c(
                      "No" = 0,
                      "Yes" = 1
                    )
                  )
                ),
                
                column(
                  6,
                  
                  selectInput(
                    "heart_disease",
                    "Heart Disease *",
                    
                    choices = c(
                      "No" = 0,
                      "Yes" = 1
                    )
                  )
                )
              ),
              
              
              fluidRow(
                
                column(
                  6,
                  
                  numericInput(
                    "hba1c",
                    "HbA1c Level *",
                    value = 5.8,
                    min = 3.5,
                    max = 9.0,
                    step = 0.1
                  )
                ),
                
                column(
                  6,
                  
                  numericInput(
                    "glucose",
                    "Blood Glucose Level *",
                    value = 120,
                    min = 80,
                    max = 300
                  )
                )
              ),
              
              
              fluidRow(
                
                column(
                  6,
                  
                  selectInput(
                    "smoking",
                    "Smoking History *",
                    
                    choices = c(
                      "never",
                      "current",
                      "ever",
                      "former",
                      "No Info",
                      "not current"
                    )
                  )
                )
              ),
              
              
              actionButton(
                "predict",
                "Predict Diabetes",
                class = "predict-button"
              )
            )
          ),
          
          
          # RESULT
          
          column(
            6,
            
            div(
              class = "main-card",
              
              div(
                class = "card-title",
                "▣  Prediction Result"
              ),
              
              
              div(
                class = "result-box",
                
                div(
                  class = "result-icon",
                  "✓"
                ),
                
                div(
                  class = "result-title",
                  textOutput("prediction_result")
                ),
                
                div(
                  class = "result-description",
                  textOutput("prediction_description")
                ),
                
                
                div(
                  class = "probability-box",
                  
                  div(
                    class = "probability-label",
                    "Estimated Probability"
                  ),
                  
                  div(
                    class = "probability-value",
                    textOutput("probability_result")
                  ),
                  
                  div(
                    class = "probability-class",
                    textOutput("probability_class")
                  )
                )
              ),
              
              
              div(
                class = "bmi-box",
                
                div(
                  class = "bmi-label",
                  "Calculated BMI"
                ),
                
                div(
                  class = "bmi-value",
                  textOutput("bmi_result")
                )
              ),
              
              
              div(
                class = "note-box",
                
                strong("ⓘ  Note"),
                
                br(),
                br(),
                
                "This prediction is for educational and project purposes only.",
                
                br(),
                
                "It is not a medical diagnosis."
              )
            )
          )
        )
      )
    }
    
    
    # ========================================================
    # PAGE 2: DATA ANALYSIS
    # ========================================================
    
    else if (page == "analysis") {
      
      tagList(
        
        div(
          class = "intro-card",
          
          div(
            class = "intro-title",
            "Data Analysis"
          ),
          
          div(
            class = "intro-text",
            
            "Statistical overview of the diabetes dataset used in this project."
          )
        ),
        
        
        # DATASET SUMMARY
        
        fluidRow(
          
          column(
            3,
            
            div(
              class = "stat-box",
              
              div(
                class = "stat-title",
                "Total Records"
              ),
              
              div(
                class = "stat-value",
                nrow(data)
              )
            )
          ),
          
          column(
            3,
            
            div(
              class = "stat-box",
              
              div(
                class = "stat-title",
                "Total Variables"
              ),
              
              div(
                class = "stat-value",
                ncol(data)
              )
            )
          ),
          
          column(
            3,
            
            div(
              class = "stat-box",
              
              div(
                class = "stat-title",
                "Missing Values"
              ),
              
              div(
                class = "stat-value",
                sum(is.na(data))
              )
            )
          ),
          
          column(
            3,
            
            div(
              class = "stat-box",
              
              div(
                class = "stat-title",
                "Duplicate Records"
              ),
              
              div(
                class = "stat-value",
                
                sum(
                  duplicated(data)
                )
              )
            )
          )
        ),
        
        
        # DIABETES DISTRIBUTION
        
        div(
          class = "page-card",
          
          div(
            class = "page-card-title",
            "Diabetes Distribution"
          ),
          
          tableOutput(
            "diabetes_table"
          )
        ),
        
        
        # DESCRIPTIVE STATISTICS
        
        div(
          class = "page-card",
          
          div(
            class = "page-card-title",
            "Descriptive Statistics"
          ),
          
          tableOutput(
            "statistics_table"
          )
        ),
        
        
        # CATEGORICAL INFORMATION
        
        div(
          class = "page-card",
          
          div(
            class = "page-card-title",
            "Dataset Information"
          ),
          
          tableOutput(
            "category_table"
          )
        )
      )
    }
    
    
    # ========================================================
    # PAGE 3: VISUALIZATIONS
    # ========================================================
    
    else if (page == "visualizations") {
      
      tagList(
        
        div(
          class = "intro-card",
          
          div(
            class = "intro-title",
            "Data Visualizations"
          ),
          
          div(
            class = "intro-text",
            
            "The following graphs show distributions and relationships present in the diabetes dataset."
          )
        ),
        
        
        uiOutput(
          "graphs_content"
        )
      )
    }
    
    
    # ========================================================
    # PAGE 4: MODEL PERFORMANCE
    # ========================================================
    
    else if (page == "performance") {
      
      tagList(
        
        div(
          class = "intro-card",
          
          div(
            class = "intro-title",
            "Model Performance"
          ),
          
          div(
            class = "intro-text",
            
            "Performance of the Logistic Regression model on the test dataset."
          )
        ),
        
        
        # PERFORMANCE VALUES
        
        fluidRow(
          
          column(
            4,
            
            div(
              class = "performance-box",
              
              div(
                class = "performance-value",
                paste0(
                  round(accuracy * 100, 2),
                  "%"
                )
              ),
              
              div(
                class = "performance-label",
                "Accuracy"
              )
            )
          ),
          
          column(
            4,
            
            div(
              class = "performance-box",
              
              div(
                class = "performance-value",
                paste0(
                  round(sensitivity * 100, 2),
                  "%"
                )
              ),
              
              div(
                class = "performance-label",
                "Sensitivity"
              )
            )
          ),
          
          column(
            4,
            
            div(
              class = "performance-box",
              
              div(
                class = "performance-value",
                paste0(
                  round(specificity * 100, 2),
                  "%"
                )
              ),
              
              div(
                class = "performance-label",
                "Specificity"
              )
            )
          )
        ),
        
        
        fluidRow(
          
          column(
            4,
            
            div(
              class = "performance-box",
              
              div(
                class = "performance-value",
                paste0(
                  round(precision * 100, 2),
                  "%"
                )
              ),
              
              div(
                class = "performance-label",
                "Precision"
              )
            )
          ),
          
          column(
            4,
            
            div(
              class = "performance-box",
              
              div(
                class = "performance-value",
                paste0(
                  round(f1_score * 100, 2),
                  "%"
                )
              ),
              
              div(
                class = "performance-label",
                "F1-Score"
              )
            )
          ),
          
          column(
            4,
            
            div(
              class = "performance-box",
              
              div(
                class = "performance-value",
                round(auc, 3)
              ),
              
              div(
                class = "performance-label",
                "ROC-AUC"
              )
            )
          )
        ),
        
        
        # CONFUSION MATRIX
        
        div(
          class = "page-card",
          
          div(
            class = "page-card-title",
            "Confusion Matrix"
          ),
          
          tableOutput(
            "confusion_table"
          )
        ),
        
        
        # ROC CURVE
        
        if (
          file.exists(
            file.path(
              graphs_path,
              "roc_curve.png"
            )
          )
        ) {
          
          div(
            class = "graph-box",
            
            div(
              class = "graph-title",
              "ROC Curve"
            ),
            
            img(
              src = "graphs/roc_curve.png",
              style = "max-width: 800px; width: 100%;"
            )
          )
        }
      )
    }
    
    
    # ========================================================
    # PAGE 5: ABOUT
    # ========================================================
    
    else if (page == "about") {
      
      tagList(
        
        div(
          class = "intro-card",
          
          div(
            class = "intro-title",
            "About the Project"
          ),
          
          div(
            class = "intro-text",
            
            "Disease Risk Prediction Using Statistical Modelling"
          )
        ),
        
        
        div(
          class = "page-card",
          
          div(
            class = "page-card-title",
            "Project Objective"
          ),
          
          div(
            class = "page-text",
            
            "The objective of this project is to use statistical modelling techniques to estimate diabetes risk using selected patient characteristics.",
            
            br(),
            br(),
            
            "The project demonstrates data cleaning, descriptive statistics, correlation analysis, regression, data visualization and prediction using R."
          )
        ),
        
        
        div(
          class = "page-card",
          
          div(
            class = "page-card-title",
            "Technology Used"
          ),
          
          div(
            class = "page-text",
            
            "• R Programming",
            br(),
            "• RStudio",
            br(),
            "• R Shiny",
            br(),
            "• Logistic Regression",
            br(),
            "• ggplot2",
            br(),
            "• Plotly"
          )
        ),
        
        
        div(
          class = "page-card",
          
          div(
            class = "page-card-title",
            "Dataset"
          ),
          
          div(
            class = "page-text",
            
            "The project uses the Diabetes Prediction Dataset from Kaggle.",
            
            br(),
            br(),
            
            "The final dataset used in the project contains 5,500 records and 9 variables."
          )
        ),
        
        
        div(
          class = "note-box",
          
          strong(
            "ⓘ  Educational Purpose"
          ),
          
          br(),
          br(),
          
          "This application is developed for academic and educational purposes.",
          
          br(),
          
          "The prediction should not be considered a medical diagnosis."
        )
      )
    }
    
  })
  
  
  # ==========================================================
  # DATA ANALYSIS OUTPUTS
  # ==========================================================
  
  output$diabetes_table <- renderTable({
    
    total <- nrow(data)
    
    count_0 <- sum(data$diabetes == 0)
    
    count_1 <- sum(data$diabetes == 1)
    
    data.frame(
      
      Status = c(
        "No Diabetes",
        "Diabetes"
      ),
      
      Count = c(
        count_0,
        count_1
      ),
      
      Percentage = c(
        round(count_0 / total * 100, 2),
        round(count_1 / total * 100, 2)
      )
    )
  })
  
  
  output$statistics_table <- renderTable({
    
    data.frame(
      
      Variable = c(
        "Age",
        "BMI",
        "HbA1c Level",
        "Blood Glucose Level"
      ),
      
      Mean = c(
        round(mean(data$age), 2),
        round(mean(data$bmi), 2),
        round(mean(data$HbA1c_level), 2),
        round(mean(data$blood_glucose_level), 2)
      ),
      
      Median = c(
        median(data$age),
        median(data$bmi),
        median(data$HbA1c_level),
        median(data$blood_glucose_level)
      ),
      
      Minimum = c(
        min(data$age),
        min(data$bmi),
        min(data$HbA1c_level),
        min(data$blood_glucose_level)
      ),
      
      Maximum = c(
        max(data$age),
        max(data$bmi),
        max(data$HbA1c_level),
        max(data$blood_glucose_level)
      )
    )
  })
  
  
  output$category_table <- renderTable({
    
    data.frame(
      
      Variable = c(
        "Female",
        "Male",
        "Hypertension",
        "Heart Disease"
      ),
      
      Count = c(
        sum(data$gender == "Female"),
        sum(data$gender == "Male"),
        sum(data$hypertension == 1),
        sum(data$heart_disease == 1)
      )
    )
  })
  
  
  # ==========================================================
  # CONFUSION MATRIX
  # ==========================================================
  
  output$confusion_table <- renderTable({
    
    data.frame(
      
      Actual_No_Diabetes = c(
        TN,
        FP
      ),
      
      Actual_Diabetes = c(
        FN,
        TP
      ),
      
      row.names = c(
        "Predicted No Diabetes",
        "Predicted Diabetes"
      )
    )
  }, rownames = TRUE)
  
  
  # ==========================================================
  # VISUALIZATION PAGE
  # ==========================================================
  
  output$graphs_content <- renderUI({
    
    graph_files <- list(
      
      list(
        "Diabetes Distribution",
        "diabetes_distribution.png"
      ),
      
      list(
        "Gender Distribution",
        "gender_distribution.png"
      ),
      
      list(
        "Hypertension Distribution",
        "hypertension_distribution.png"
      ),
      
      list(
        "Heart Disease Distribution",
        "heart_disease_distribution.png"
      ),
      
      list(
        "Smoking History Distribution",
        "smoking_history_distribution.png"
      ),
      
      list(
        "Age Distribution",
        "age_distribution.png"
      ),
      
      list(
        "BMI Distribution",
        "bmi_distribution.png"
      ),
      
      list(
        "HbA1c Level Distribution",
        "hba1c_distribution.png"
      ),
      
      list(
        "Blood Glucose Level Distribution",
        "blood_glucose_distribution.png"
      ),
      
      list(
        "HbA1c Level vs Blood Glucose Level",
        "hba1c_vs_glucose.png"
      ),
      
      list(
        "BMI vs Blood Glucose Level",
        "bmi_vs_glucose.png"
      ),
      
      list(
        "Age vs Blood Glucose Level",
        "age_vs_glucose.png"
      ),
      
      list(
        "Age vs HbA1c Level",
        "age_vs_hba1c.png"
      )
    )
    
    
    rows <- list()
    
    for (i in seq(1, length(graph_files), by = 2)) {
      
      first_graph <- graph_files[[i]]
      
      first_box <- div(
        class = "graph-box",
        
        div(
          class = "graph-title",
          first_graph[[1]]
        ),
        
        img(
          src = paste0(
            "graphs/",
            first_graph[[2]]
          )
        )
      )
      
      
      if (i + 1 <= length(graph_files)) {
        
        second_graph <- graph_files[[i + 1]]
        
        second_box <- div(
          class = "graph-box",
          
          div(
            class = "graph-title",
            second_graph[[1]]
          ),
          
          img(
            src = paste0(
              "graphs/",
              second_graph[[2]]
            )
          )
        )
        
      } else {
        
        second_box <- div()
      }
      
      
      rows[[length(rows) + 1]] <- fluidRow(
        
        column(
          6,
          first_box
        ),
        
        column(
          6,
          second_box
        )
      )
    }
    
    
    # Add interactive 3D graph
    
    three_d_graph <- file.path(
      graphs_path,
      "age_hba1c_glucose_3d.html"
    )
    
    
    if (file.exists(three_d_graph)) {
      
      rows[[length(rows) + 1]] <- div(
        
        class = "graph-box",
        
        div(
          class = "graph-title",
          "Age vs HbA1c Level vs Blood Glucose Level"
        ),
        
        tags$iframe(
          src = "graphs/age_hba1c_glucose_3d.html",
          style = "width:100%; height:600px; border:none;"
        )
      )
    }
    
    
    tagList(rows)
  })
  
  
  # ==========================================================
  # BMI OUTPUT
  # ==========================================================
  
  output$bmi_result <- renderText({
    
    bmi_value()
    
  })
  
  
  # ==========================================================
  # PREDICTION OUTPUTS
  # ==========================================================
  
  output$prediction_result <- renderText({
    
    result <- prediction_result()
    
    if (result$prediction == 1) {
      
      "Diabetes Risk Detected"
      
    } else {
      
      "No Diabetes"
    }
  })
  
  
  output$prediction_description <- renderText({
    
    result <- prediction_result()
    
    if (result$prediction == 1) {
      
      "The model predicts an increased diabetes risk."
      
    } else {
      
      "The model predicts a low predicted risk of diabetes."
    }
  })
  
  
  output$probability_result <- renderText({
    
    result <- prediction_result()
    
    paste0(
      round(
        result$probability * 100,
        2
      ),
      "%"
    )
  })
  
  
  output$probability_class <- renderText({
    
    result <- prediction_result()
    
    if (result$prediction == 1) {
      
      "(Diabetes Risk)"
      
    } else {
      
      "(No Diabetes)"
    }
  })
  
}


# ============================================================
# 8. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)