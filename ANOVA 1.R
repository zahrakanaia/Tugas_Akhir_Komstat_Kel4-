
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(car)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Aplikasi ANOVA"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                br(), br(),
                div(style = "padding: 20px;",
                    radioButtons(
                      inputId = "dark_mode",
                      label = "Mode Tampilan",
                      choices = c("Light" = "light", "Dark" = "dark"),
                      selected = "light",
                      inline = TRUE
                    )
                ),
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Input Data", tabName = "input", icon = icon("upload")),
                menuItem("Uji Kenormalan", tabName = "normal", icon = icon("chart-line")),
                menuItem("Uji Varians", tabName = "varians", icon = icon("balance-scale")),
                menuItem("ANOVA", tabName = "anova", icon = icon("project-diagram")),
                menuItem("Tukey", tabName = "tukey", icon = icon("list"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".status-box { display: inline-block; padding: 5px 10px; border-radius: 5px; margin-right: 5px; color: white; } .green { background-color: green; } .red { background-color: red; }"))),
    tabItems(
      tabItem("home",
              tabItem("home", 
                      div(
                        style = "
      display: flex; 
      justify-content: center; 
      align-items: center; 
      height: 80vh; 
      text-align: center;",
                        h2("SELAMAT DATANG DI APLIKASI ANOVA!")
                      )
              )
      ),
      tabItem("input",
              fluidRow(
                box(title = "Upload dan Pilih Data", width = 4, fileInput("file1", "Upload CSV"),
                    radioButtons("sep", "Pemisah", choices = c(",", ";", "\t")),
                    sliderInput("alpha", "Taraf Signifikansi", 0.01, 0.1, 0.05, 0.01),
                    uiOutput("varSelect"),
                    uiOutput("groupSelect"),
                    actionButton("submitData", "OK")),
                box(title = "Preview Data", width = 8, tableOutput("dataPreview"))
              )
      ),
      tabItem("normal",
              uiOutput("statusBox"),
              fluidRow(
                box(title = "Hipotesis Uji Kenormalan", width = 6, img(src = "shapiro.png", width = "100%")),
                box(title = "Taraf Signifikansi", width = 6, textOutput("tarafNormal"))
              ),
              fluidRow(
                tabBox(title = "Detail Uji Kenormalan", side = "left", width = 12,
                       tabPanel("Hasil", verbatimTextOutput("hasilNormal")),
                       tabPanel("Keputusan", verbatimTextOutput("keputusanNormal")),
                       tabPanel("Kesimpulan", verbatimTextOutput("kesimpulanNormal"))
                )
              ),
              actionButton("toVarians", "Lanjut ke Uji Varians")
      ),
      tabItem("varians",
              uiOutput("statusBox"),
              fluidRow(
                box(title = "Hipotesis Uji Kesamaan Varians", width = 6, img(src = "levene.png", width = "100%")),
                box(title = "Taraf Signifikansi", width = 6, textOutput("tarafVarians"))
              ),
              fluidRow(
                tabBox(title = "Detail Uji Varians", side = "left", width = 12,
                       tabPanel("Hasil", verbatimTextOutput("variansResult")),
                       tabPanel("Keputusan", verbatimTextOutput("keputusanVarians")),
                       tabPanel("Kesimpulan", verbatimTextOutput("kesimpulanVarians"))
                )
              ),
              actionButton("toAnova", "Lanjut ke Uji ANOVA")
      ),
      tabItem("anova",
              uiOutput("statusBox"),
              fluidRow(
                box(title = "Hipotesis Uji ANOVA", width = 6, img(src = "anova.png", width = "100%")),
                box(title = "Taraf Signifikansi", width = 6, textOutput("tarafAnova"))
              ),
              fluidRow(
                tabBox(title = "Detail Uji ANOVA", side = "left", width = 12,
                       tabPanel("Hasil ANOVA", verbatimTextOutput("anovaResult")),
                       tabPanel("Boxplot", plotOutput("plotBox")),
                       tabPanel("Keputusan", verbatimTextOutput("keputusanAnova")),
                       tabPanel("Kesimpulan", verbatimTextOutput("kesimpulanAnova"))
                )
              ),
              conditionalPanel("output.anovaSig == true", actionButton("toTukey", "Lanjut ke Uji Tukey"))
      ),
      tabItem("tukey",
              uiOutput("statusBox"),
              fluidRow(
                box(title = "Hipotesis Uji Tukey", width = 6, img(src = "tukey.png", width = "100%")),
                box(title = "Taraf Signifikansi", width = 6, textOutput("tarafTukey"))
              ),
              fluidRow(
                tabBox(title = "Detail Uji Tukey", side = "left", width = 12,
                       tabPanel("Hasil", verbatimTextOutput("tukeyResult")),
                       tabPanel("Keputusan", verbatimTextOutput("keputusanTukey")),
                       tabPanel("Kesimpulan", verbatimTextOutput("kesimpulanTukey"))
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  dataInput <- reactiveVal()
  hasilNormal <- reactiveVal(NULL)
  hasilVarians <- reactiveVal(NULL)
  hasilAnova <- reactiveVal(NULL)
  hasilTukey <- reactiveVal(NULL)
  keputusanNormal <- reactiveVal()
  kesimpulanNormal <- reactiveVal()
  keputusanVarians <- reactiveVal()
  kesimpulanVarians <- reactiveVal()
  keputusanAnova <- reactiveVal()
  kesimpulanAnova <- reactiveVal()
  keputusanTukey <- reactiveVal()
  kesimpulanTukey <- reactiveVal()
  
  observeEvent(input$file1, {
    df <- read.csv(input$file1$datapath, sep = input$sep)
    dataInput(df)
  })
  
  output$dataPreview <- renderTable({ head(dataInput(), 10) })
  output$varSelect <- renderUI({ req(dataInput()); selectInput("numericVar", "Variabel Numerik", names(Filter(is.numeric, dataInput()))) })
  output$groupSelect <- renderUI({ req(dataInput()); selectInput("groupVar", "Variabel Grup", names(dataInput())) })

   observe({
    req(input$dark_mode)
    session$sendCustomMessage(
      type = "toggleDark",
      message = input$dark_mode == "dark"
    )
  })
  
  observeEvent(input$submitData, {
    updateTabItems(session, "tabs", "normal")
  })
  
  output$tarafNormal <- renderText({ paste("\u03B1 =", input$alpha) })
  output$tarafVarians <- renderText({ paste("\u03B1 =", input$alpha) })
  output$tarafAnova <- renderText({ paste("\u03B1 =", input$alpha) })
  output$tarafTukey <- renderText({ paste("\u03B1 =", input$alpha) })
  
  output$plotBox <- renderPlot({
    req(dataInput(), input$numericVar, input$groupVar)
    ggplot(dataInput(), aes_string(x = input$groupVar, y = input$numericVar, fill = input$groupVar)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Boxplot Berdasarkan Kelompok", x = input$groupVar, y = input$numericVar)
  })
  
  output$hasilNormal <- renderPrint({
    req(dataInput(), input$numericVar, input$groupVar)
    df <- dataInput()
    result <- lapply(split(df[[input$numericVar]], df[[input$groupVar]]), shapiro.test)
    hasilNormal(result)
    res <- hasilNormal()
    req(res)
    for (group in names(res)) {
      cat("Kelompok", group, 
          ": Statistik =", round(res[[group]]$statistic, 7), 
          ", p-value =", round(res[[group]]$p.value, 7), "\n---\n")
    }
  })
  
  output$keputusanNormal <- renderPrint({
    res <- hasilNormal()
    req(res)
    for (group in names(res)) {
      if (res[[group]]$p.value < input$alpha) {
        cat("Kelompok", group, ": Tolak H0 (data tidak normal)\n")
      } else {
        cat("Kelompok", group, ": Terima H0 (data normal)\n")
      }
    }
  })
  
  output$kesimpulanNormal <- renderPrint({
    res <- hasilNormal()
    req(res)
    if (all(sapply(res, function(x) x$p.value > input$alpha))) {
      cat("Kesimpulan: Semua kelompok berdistribusi normal.")
    } else {
      cat("Kesimpulan: Ada kelompok yang tidak berdistribusi normal.")
    }
  })
  
  output$variansResult <- renderPrint({
    req(dataInput(), input$numericVar, input$groupVar)
    df <- dataInput()
    result <- leveneTest(as.formula(paste(input$numericVar, "~", input$groupVar)), data = df)
    hasilVarians(result)
    print(result)
  })
  
  output$keputusanVarians <- renderPrint({
    res <- hasilVarians()
    req(res)
    pval <- res[["Pr(>F)"]][1]
    if (pval < input$alpha) {
      cat("Keputusan: Tolak H0 (varians tidak homogen)")
    } else {
      cat("Keputusan: Terima H0 (varians homogen)")
    }
  })
  
  output$kesimpulanVarians <- renderPrint({
    res <- hasilVarians()
    req(res)
    pval <- res[["Pr(>F)"]][1]
    if (pval < input$alpha) {
      cat("Kesimpulan: Varians antar kelompok tidak homogen.")
    } else {
      cat("Kesimpulan: Varians antar kelompok homogen.")
    }
  })
  
  output$anovaResult <- renderPrint({
    req(dataInput(), input$numericVar, input$groupVar)
    df <- dataInput()
    model <- aov(as.formula(paste(input$numericVar, "~", input$groupVar)), data = df)
    hasil <- summary(model)
    hasilAnova(hasil)
    print(hasil)
  })
  
  output$keputusanAnova <- renderPrint({
    res <- hasilAnova()
    req(res)
    pval <- res[[1]]$"Pr(>F)"[1]
    if (pval < input$alpha) {
      cat("Keputusan: Tolak H0 (terdapat perbedaan rata-rata)")
    } else {
      cat("Keputusan: Terima H0 (tidak terdapat perbedaan rata-rata)")
    }
  })
  
  output$kesimpulanAnova <- renderPrint({
    res <- hasilAnova()
    req(res)
    pval <- res[[1]]$"Pr(>F)"[1]
    if (pval < input$alpha) {
      cat("Kesimpulan: Rata-rata antar kelompok berbeda secara signifikan.")
    } else {
      cat("Kesimpulan: Rata-rata antar kelompok tidak berbeda secara signifikan.")
    }
  })
  
  output$tukeyResult <- renderPrint({
    req(dataInput(), input$numericVar, input$groupVar)
    df <- dataInput()
    model <- aov(as.formula(paste(input$numericVar, "~", input$groupVar)), data = df)
    hasil <- TukeyHSD(model)
    hasilTukey(hasil)
    print(hasil)
  })
  
  output$keputusanTukey <- renderPrint({
    res <- hasilTukey()
    req(res)
    # Ambil nama faktor (karena hasil TukeyHSD bisa punya banyak jika multi-way ANOVA)
    faktor <- names(res)[1]
    tukey_matrix <- res[[faktor]]  # matrix: rownames comparison, columns diff, lwr, upr, p ad
    tukey_p <- tukey_matrix[, "p adj"]
    signif <- tukey_p < input$alpha
    
    for (i in seq_along(signif)) {
      cat(rownames(tukey_matrix)[i], ": ",
          ifelse(signif[i], "Berbeda signifikan", "Tidak berbeda signifikan"), "\n")
    }
  })
  
  output$kesimpulanTukey <- renderPrint({
    res <- hasilTukey()
    req(res)
    
    faktor <- names(res)[1]
    tukey_matrix <- res[[faktor]]
    tukey_p <- tukey_matrix[, "p adj"]
    
    if (any(tukey_p < input$alpha)) {
      cat("Kesimpulan: Terdapat pasangan kelompok yang berbeda secara signifikan.")
    } else {
      cat("Kesimpulan: Tidak ada pasangan kelompok yang berbeda secara signifikan.")
    }
  })
  
  output$anovaSig <- reactive({
    res <- hasilAnova()
    if (!is.null(res)) {
      return(res[[1]]$"Pr(>F)"[1] < input$alpha)
    }
    FALSE
  })
  outputOptions(output, "anovaSig", suspendWhenHidden = FALSE)
  
  observeEvent(input$toVarians, {
    updateTabItems(session, "tabs", "varians")
  })
  
  observeEvent(input$toAnova, {
    updateTabItems(session, "tabs", "anova")
  })
  
  observeEvent(input$toTukey, {
    updateTabItems(session, "tabs", "tukey")
  })
  
  output$statusBox <- renderUI({
    if (is.null(hasilNormal())) return()
    normColor <- if (all(sapply(hasilNormal(), function(x) x$p.value > input$alpha))) "green" else "red"
    varColor <- if (!is.null(hasilVarians()) && hasilVarians() [["Pr(>F)"]][1] > input$alpha) "green" else "red"
    anovaColor <- if (!is.null(hasilAnova()) && hasilAnova()[[1]]$"Pr(>F)"[1] < input$alpha) "green" else "red"
    
    tagList(
      div(class = paste("status-box", normColor), ifelse(normColor == "green", "Data Berdistribusi Normal", "Data Tidak Berdistribusi Normal")),
      div(class = paste("status-box", varColor), ifelse(varColor == "green", "Data Homogen", "Data Tidak Homogen")),
      div(class = paste("status-box", anovaColor), ifelse(anovaColor == "green", "Rata-Rata Antar Kelompok Berbeda", "Rata-Rata Antar Kelompok Tidak Berbeda"))
    )
  })
}
shinyApp(ui, server)