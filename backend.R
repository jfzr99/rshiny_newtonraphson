library(logregNR)
library(shiny)
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
  model <- reactiveVal(NULL)
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
    
    # Output untuk setiap tab
    # 1. Tab Model Training - nr_logistic output
    output$nr_logistic_output <- renderPrint({
      summary(model_fit)
    })
    
    # 2. Tab Predictions
    output$predict_output <- renderPrint({
      probs <- predict(model_fit, newdata = data, type = "response")
      pred_class <- ifelse(probs > 0.5, 1, 0)
      cat("Predictions (first 5 rows):\n\n")
      print(head(data.frame(
        Actual = data[[target]],
        Probability = round(probs, 4),
        Predicted_Class = pred_class
      ), 5))
    })
    
    # 3. Tab Evaluation
    output$evaluate_output <- renderPrint({
      probs <- predict(model_fit, newdata = data, type = "response")
      pred_class <- ifelse(probs > 0.5, 1, 0)
      actual <- data[[target]]
      
      # Pastikan kedua kelas 0 dan 1 ada dengan mendefinisikan faktor
      actual_f <- factor(actual, levels = c(0, 1))
      pred_class_f <- factor(pred_class, levels = c(0, 1))
      
      conf_mat <- table(Actual = actual_f, Predicted = pred_class_f)
      
      # Hitung accuracy
      accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
      
      # Hitung precision dan recall dengan handle kemungkinan pembagian dengan nol
      precision <- if (sum(conf_mat[, "1"]) == 0) NA else conf_mat["1", "1"] / sum(conf_mat[, "1"])
      recall <- if (sum(conf_mat["1", ]) == 0) NA else conf_mat["1", "1"] / sum(conf_mat["1", ])
      
      cat("Model Evaluation Metrics:\n\n")
      cat("Accuracy:", round(accuracy, 4), "\n")
      cat("Precision:", ifelse(is.na(precision), "NA", round(precision, 4)), "\n")
      cat("Recall:", ifelse(is.na(recall), "NA", round(recall, 4)), "\n\n")
      cat("Confusion Matrix:\n")
      print(conf_mat)
    })
    # 4. Tab ROC Curve
    output$roc_plot <- renderPlot({
      probs <- predict(model_fit, newdata = data, type = "response")
      actual <- data[[target]]
      plot_roc(actual, probs)
    })
  })
}
