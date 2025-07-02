# Aplikasi ANOVA Multi-Tahapan dengan Indikator dan Navigasi Otomatis
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
      tabItem("home", h2("Selamat Datang di Aplikasi ANOVA")),
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
                box(title = "Hipotesis Uji Kenormalan", width = 6, img(src="shapiro.png", width="100%")),
                box(title = "Taraf Signifikansi", width = 6, textOutput("tarafNormal"))
              ),
              fluidRow(
                box(title = "Hasil, Keputusan, Kesimpulan", width = 12, verbatimTextOutput("normalResult"))
              ),
              actionButton("toVarians", "Lanjut ke Uji Varians")
      ),
      tabItem("varians",
              uiOutput("statusBox"),
              fluidRow(
                box(title = "Hipotesis Uji Kesamaan Varians", width = 6, img(src="levene.png", width="100%")),
                box(title = "Taraf Signifikansi", width = 6, textOutput("tarafVarians"))
              ),
              fluidRow(
                box(title = "Hasil, Keputusan, Kesimpulan", width = 12, verbatimTextOutput("variansResult"))
              ),
              actionButton("toAnova", "Lanjut ke Uji ANOVA")
      ),
      tabItem("anova",
              uiOutput("statusBox"),
              fluidRow(
                box(title = "Hipotesis Uji ANOVA", width = 6, img(src="anova.png", width="100%")),
                box(title = "Taraf Signifikansi", width = 6, textOutput("tarafAnova"))
              ),
              fluidRow(
                box(title = "Hasil, Keputusan, Kesimpulan", width = 12, verbatimTextOutput("anovaResult"))
              ),
              conditionalPanel("output.anovaSig == true", actionButton("toTukey", "Lanjut ke Uji Tukey"))
      ),
      tabItem("tukey",
              uiOutput("statusBox"),
              fluidRow(
                box(title = "Hipotesis Uji Tukey", width = 6, img(src="tukey.png", width="100%")),
                box(title = "Taraf Signifikansi", width = 6, textOutput("tarafTukey"))
              ),
              fluidRow(
                box(title = "Hasil, Keputusan, Kesimpulan", width = 12, verbatimTextOutput("tukeyResult"))
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
  output$normalResult <- renderPrint({
    req(dataInput(), input$numericVar, input$groupVar)
    df <- dataInput()
    result <- lapply(split(df[[input$numericVar]], df[[input$groupVar]]), shapiro.test)
    hasilNormal(result)
    for (group in names(result)) {
      cat("Kelompok", group, ": Statistik =", result[[group]]$statistic, ", p-value =", result[[group]]$p.value, "\n")
      if (result[[group]]$p.value < input$alpha) {
        cat("Keputusan: Tolak H0 (data tidak normal)\n")
      } else {
        cat("Keputusan: Terima H0 (data normal)\n")
      }
      cat("---\n")
    }
    if (all(sapply(result, function(x) x$p.value > input$alpha))) {
      cat("Kesimpulan: Semua data berdistribusi normal berdasarkan uji Shapiro-Wilk.\n")
    } else {
      cat("Kesimpulan: Ada data yang tidak berdistribusi normal berdasarkan uji Shapiro-Wilk.\n")
    }
  })
  
  observeEvent(input$toVarians, {
    updateTabItems(session, "tabs", "varians")
  })
  
  output$tarafVarians <- renderText({ paste("\u03B1 =", input$alpha) })
  output$variansResult <- renderPrint({
    req(dataInput(), input$numericVar, input$groupVar)
    df <- dataInput()
    result <- leveneTest(as.formula(paste(input$numericVar, "~", input$groupVar)), data = df)
    hasilVarians(result)
    print(result)
    pval <- result[[1]]$"Pr(>F)"[1]
    if (pval < input$alpha) {
      cat("Keputusan: Tolak H0 (varians tidak homogen)\n")
      cat("Kesimpulan: Terdapat varians yang tidak homogen antara kelompok.\n")
    } else {
      cat("Keputusan: Terima H0 (varians homogen)\n")
      cat("Kesimpulan: Varians antar kelompok homogen.\n")
    }
  })
  
  observeEvent(input$toAnova, {
    updateTabItems(session, "tabs", "anova")
  })
  
  output$tarafAnova <- renderText({ paste("\u03B1 =", input$alpha) })
  output$anovaResult <- renderPrint({
    req(dataInput(), input$numericVar, input$groupVar)
    df <- dataInput()
    model <- aov(as.formula(paste(input$numericVar, "~", input$groupVar)), data = df)
    hasilAnova(summary(model))
    print(summary(model))
    pval <- summary(model)[[1]]$"Pr(>F)"[1]
    if (pval < input$alpha) {
      cat("Keputusan: Tolak H0 (terdapat perbedaan rata-rata)\n")
      cat("Kesimpulan: Rata-rata antar kelompok berbeda signifikan.\n")
    } else {
      cat("Keputusan: Terima H0 (tidak terdapat perbedaan rata-rata)\n")
      cat("Kesimpulan: Rata-rata antar kelompok tidak berbeda signifikan.\n")
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
  
  observeEvent(input$toTukey, {
    updateTabItems(session, "tabs", "tukey")
  })
  
  output$tarafTukey <- renderText({ paste("\u03B1 =", input$alpha) })
  output$tukeyResult <- renderPrint({
    req(dataInput(), input$numericVar, input$groupVar)
    df <- dataInput()
    model <- aov(as.formula(paste(input$numericVar, "~", input$groupVar)), data = df)
    print(TukeyHSD(model))
  })
  
  output$statusBox <- renderUI({
    if (is.null(hasilNormal())) return()
    normColor <- if (all(sapply(hasilNormal(), function(x) x$p.value > input$alpha))) "green" else "red"
    varColor <- if (!is.null(hasilVarians()) && hasilVarians()[[1]]$`Pr(>F)`[1] > input$alpha) "green" else "red"
    anovaColor <- if (!is.null(hasilAnova()) && hasilAnova()[[1]]$"Pr(>F)"[1] > input$alpha) "green" else "red"
    tagList(
      div(class=paste("status-box", normColor), ifelse(normColor == "green", "Normal", "Tidak Normal")),
      div(class=paste("status-box", varColor), ifelse(varColor == "green", "Homogen", "Tidak Homogen")),
      div(class=paste("status-box", anovaColor), ifelse(anovaColor == "green", "Tidak Berbeda", "Berbeda"))
    )
  })
}

shinyApp(ui, server)
