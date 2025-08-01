library(logregNR)

# Step 1: Handle file input and check format
server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file1)
    
    # Membaca file CSV
    data <- tryCatch({
      read.csv(input$file1$datapath)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        "Terjadi kesalahan saat membaca file CSV. Pastikan format CSV sudah benar.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    })
    
    # Validasi kolom pertama adalah variabel target (death)
    if (ncol(data) < 2) {
      showModal(modalDialog(
        title = "Error",
        "File CSV harus memiliki minimal dua kolom. Kolom pertama adalah target (death).",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    
    # Menentukan target dan prediktor
    target <- names(data)[1]  # Kolom pertama adalah target
    predictors <- names(data)[-1]  # Sisanya adalah prediktor
    
    list(data = data, target = target, predictors = predictors)
  })
  
  # Menampilkan data preview
  output$data_preview <- renderTable({
    req(dataset())
    head(dataset()$data, 10)
  })
  
  # Menampilkan pilihan variabel target dan prediktor
  output$var_target <- renderUI({
    req(dataset())
    selectInput("target", "Pilih Variabel Target", names(dataset()$data)[1])  # Hanya kolom pertama (target)
  })
  
  output$var_predictors <- renderUI({
    req(dataset())
    selectInput("predictors", "Pilih Variabel Prediktor", names(dataset()$data)[-1], multiple = TRUE)  # Sisanya adalah prediktor
  })
  
  # Step 4: Train Model on Button Click
  model <- reactiveVal(NULL)  # Store model
  observeEvent(input$train_model, {
    req(dataset())
    
    # Ambil data, target dan prediktor
    data <- dataset()$data
    target <- input$target
    predictors <- input$predictors
    
    # Convert predictors to numeric
    data[,predictors] <- lapply(data[,predictors], as.numeric)
    
    # Buat formula dinamis berdasarkan kolom yang ada
    formula <- as.formula(paste(target, "~", paste(predictors, collapse = "+")))
    
    # Latih model
    model_fit <- nr_logistic(formula = formula, data = data)
    model(model_fit)
    
    # Tampilkan output nr_logistic menggunakan summary()
    output$nr_logistic_output <- renderPrint({
      print(summary(model_fit))  # Langsung menggunakan fungsi summary()
    })
    
    # 2. predict()
    output$predict_output <- renderPrint({
      predictions <- predict(model_fit, newdata = data, type = "response")
      cat("Predictions (first 5 rows):\n")
      print(head(predictions))
    })
    
    # 3. evaluate_model()
    output$evaluate_output <- renderPrint({
      pred_class <- predict(model_fit, newdata = data, type = "class")
      actual <- data[[target]]
      eval_results <- evaluate_model(actual, pred_class)
      print(eval_results)
    })
    
    # 4. plot_roc()
    output$roc_plot <- renderPlot({
      probs <- predict(model_fit, newdata = data, type = "response")
      actual <- data[[target]]
      plot_roc(actual, probs)
    })
  })
}

ui <- fluidPage(
  titlePanel("Aplikasi Log Regresi Newton Raphson"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Pilih File CSV",
                accept = c(".csv")),
      uiOutput("var_target"),
      uiOutput("var_predictors"),
      actionButton("train_model", "Latih Model", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Upload CSV",
                 tableOutput("data_preview")),
        
        tabPanel("Model Training", 
                 h4("Hasil Newton-Raphson Logistic Regression"),
                 verbatimTextOutput("nr_logistic_output")),
        
        tabPanel("Predictions", 
                 h4("Hasil Prediksi"),
                 verbatimTextOutput("predict_output")),
        
        tabPanel("Evaluation", 
                 h4("Hasil Evaluasi Model"),
                 verbatimTextOutput("evaluate_output")),
        
        tabPanel("ROC Curve", 
                 h4("ROC Curve"),
                 plotOutput("roc_plot"))
      )
    )
  )
)
