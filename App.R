library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Produksi Perkebunan Sumatera"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Tren Tahunan", tabName = "tren", icon = icon("chart-line")),
      menuItem("Analisis Hubungan", tabName = "analisis", icon = icon("bar-chart")),
      menuItem("Peta", tabName = "peta", icon = icon("globe-asia")),
      menuItem("Perbandingan Komoditas", tabName = "perbandingan", icon = icon("balance-scale")),
      menuItem("Tabel Data", tabName = "tabel", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  title = "📊 Visualisasi Produksi Perkebunan di Pulau Sumatera (2017–2023)",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("Dashboard ini menyajikan data produksi komoditas perkebunan utama di wilayah Sumatera, 
               serta informasi iklim yang berkaitan (suhu dan curah hujan). Beberapa fitur utama dashboard ini antara lain:"),
                  tags$ul(
                    tags$li(strong("Tren Tahunan:"), " Memantau perkembangan produksi setiap tahun."),
                    tags$li(strong("Analisis Hubungan:"), " Melihat pengaruh iklim terhadap produksi komoditas."),
                    tags$li(strong("Peta Interaktif:"), " Menjelajahi produksi antar provinsi di Sumatera."),
                    tags$li(strong("Perbandingan Komoditas:"), " Membandingkan produksi antar jenis atau wilayah."),
                    tags$li(strong("Tabel Data:"), " Akses langsung ke data produksi dan iklim.")
                  ),
                  p("Gunakan menu di sebelah kiri untuk menjelajahi fitur dashboard.")
                )
              ),
              fluidRow(
                valueBoxOutput("box_total_produksi"),
                valueBoxOutput("box_jumlah_provinsi"),
                valueBoxOutput("box_jumlah_komoditas")
              )
      ),
      tabItem(tabName = "tren",
              h2("Tren Tahunan"),
              fluidRow(
                box(
                  title = "Filter", status = "primary", solidHeader = TRUE, width = 12,
                  selectInput("tren_provinsi", "Pilih Provinsi:",
                              choices = c("Semua"), selected = "Semua"),
                  selectInput("tren_komoditas", "Pilih Komoditas:",
                              choices = c("Semua"), selected = "Semua")
                )
              ),
              fluidRow(
                box(
                  title = "Tren Produksi Tahunan", status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("plot_tren_tahunan", height = "400px")
                )
              ),
              fluidRow(
                box(
                  verbatimTextOutput("info_tren_tahunan")
                )
              )
      ),
      tabItem(tabName = "analisis",
              h2("Analisis Hubungan: Pengaruh Suhu dan Curah Hujan terhadap Produksi Perkebunan"),
              fluidRow(
                box(
                  title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE, width = 12,
                  
                  selectInput("analisis_provinsi", "Pilih Provinsi:",
                              choices = c("Semua")),
                  
                  selectInput("analisis_komoditas", "Pilih Komoditas:",
                              choices = c("Semua")),
                  
                  uiOutput("analisis_tahun_ui"),
                  
                  actionButton("jalankan_regresi", "Jalankan Analisis", icon = icon("play"))
                )
              ),
              
              fluidRow(
                box(
                  title = "Scatter Plot: Produksi vs Suhu", status = "info", solidHeader = TRUE, width = 6,
                  plotOutput("plot_suhu")
                ),
                
                box(
                  title = "Scatter Plot: Produksi vs Curah Hujan", status = "info", solidHeader = TRUE, width = 6,
                  plotOutput("plot_hujan")
                )
              ),
              
              fluidRow(
                box(
                  title = "Hasil Regresi Linear", status = "success", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("regresi_output")
                )
              )
      ),
      tabItem(tabName = "peta",
        h2("Peta Interaktif Produksi Perkebunan"),
        fluidRow(
          box(
            title = "Filter Peta", status = "primary", solidHeader = TRUE, width = 12,
            sliderInput("peta_tahun", "Pilih Tahun:",
                        min = 2017, max = 2023, value = 2020, step = 1, sep = ""),
            selectInput("peta_komoditas", "Pilih Komoditas:",
                        choices = c("Semua"), selected = "Semua")
          )
        ),
        fluidRow(
          box(
            title = "Peta Produksi Perkebunan di Sumatera", status = "info", solidHeader = TRUE, width = 12,
            leafletOutput("peta_produksi", height = "500px")
          )
        )
      ),
      tabItem(tabName = "perbandingan",
              h2("Perbandingan Komoditas"),
              fluidRow(
                box(
                  title = "Filter", status = "primary", solidHeader = TRUE, width = 12,
                  selectInput("perbandingan_mode", "Opsi Menu Tampilan:",
                              choices = c("Jenis", "Wilayah"), selected = "Jenis"),
                  sliderInput("perbandingan_tahun", "Pilih Tahun:",
                              min = 2017, max = 2023, value = 2020, step = 1, sep = ""),
                  conditionalPanel(
                    condition = "input.perbandingan_mode == 'Wilayah'",
                    selectInput("perbandingan_komoditas", "Pilih Komoditas:",
                                choices = c("Semua"), selected = "Semua")
                  ),
                )
              ),
              fluidRow(
                box(
                  title = "Perbandingan Produksi Komoditas", status = "info", solidHeader = TRUE, width = 12,
                  plotlyOutput("plot_perbandingan_komoditas", height = "400px"),
                  verbatimTextOutput("info_perbandingan_komoditas")
                )
              )
      ),
      tabItem(tabName = "tabel",
              h2("Tabel Data"),
              tabsetPanel(
                tabPanel("Produksi",
                         DTOutput("tabel_produksi")
                ),
                tabPanel("Iklim (Monthly)",
                         DTOutput("tabel_iklim")
                ),
                tabPanel("Gabungan (Yearly)",
                         DTOutput("tabel_gabungan")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  produksi_data <- reactive({
    read_excel("data/Perkebunan_Sumatra.xlsx")
  })
  iklim_data <- reactive({
    read_excel("data/Iklim_Sumatra.xlsx")
  })
  shape_sumatera <- reactive({
    st_read("asset/indonesia-prov.geojson")
  })
  iklim_tahunan <- reactive({
    iklim_data() %>%
      group_by(Provinsi, Tahun) %>%
      summarise(
        Suhu = mean(Suhu, na.rm = TRUE),
        CurahHujan = sum(CurahHujan, na.rm = TRUE),
        .groups = "drop"
      )
  })
  # Daftar provinsi di Sumatera
  provinsi_sumatera <- c(
    "Aceh", "Sumatera Utara", "Sumatera Barat", "Riau", "Kepulauan Riau",
    "Jambi", "Bengkulu", "Sumatera Selatan", "Kepulauan Bangka Belitung", "Lampung"
  )
  
  # Join Iklim dan produksi
  gabung_data <- reactive({
    produksi_data() %>%
      left_join(iklim_tahunan(), by = c("Provinsi", "Tahun")) %>%
      filter(Provinsi %in% provinsi_sumatera)
  })
  output$box_total_produksi <- renderValueBox({
    total <- sum(gabung_data()$Produksi, na.rm = TRUE)
    valueBox(
      value = paste0(round(total, 2), " Ribu Ton"),
      subtitle = "Total Produksi (2017–2023)",
      icon = icon("leaf"),
      color = "green"
    )
  })
  
  output$box_jumlah_provinsi <- renderValueBox({
    jumlah <- length(unique(gabung_data()$Provinsi))
    valueBox(
      value = jumlah,
      subtitle = "Jumlah Provinsi",
      icon = icon("map"),
      color = "blue"
    )
  })
  
  output$box_jumlah_komoditas <- renderValueBox({
    jumlah <- length(unique(gabung_data()$Komoditas))
    valueBox(
      value = jumlah,
      subtitle = "Jumlah Komoditas",
      icon = icon("seedling"),
      color = "olive"
    )
  })
  
  # Output Tabel
  output$tabel_produksi <- renderDT({
    datatable(produksi_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$tabel_iklim <- renderDT({
    datatable(iklim_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$tabel_gabungan <- renderDT({
    datatable(gabung_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  observe({
    df <- gabung_data()
    updateSelectInput(
      session,
      "analisis_provinsi",
      choices = c("Semua", sort(unique(df$Provinsi)))
    )
    updateSelectInput(
      session,
      "analisis_komoditas",
      choices = c("Semua", sort(unique(df$Komoditas)))
    )
  })
  
  data_analisis <- reactive({
    df <- gabung_data()
    if (input$analisis_provinsi != "Semua") {
      df <- df %>% filter(Provinsi == input$analisis_provinsi)
    }
    if (input$analisis_komoditas != "Semua") {
      df <- df %>% filter(Komoditas == input$analisis_komoditas)
    }
    req(input$analisis_tahun)
    df <- df %>% filter(Tahun >= input$analisis_tahun[1], Tahun <= input$analisis_tahun[2])
    validate(
      need(nrow(df) > 2, "Data tidak cukup untuk analisis. Pilih komoditas lain.")
    )
    df
  })
  
  # Event: Jalankan regresi
  model_regresi <- eventReactive(input$jalankan_regresi, {
    lm(Produksi ~ Suhu + CurahHujan, data = data_analisis())
  })
  
  # Output ringkasan regresi
  output$regresi_output <- renderPrint({
    req(model_regresi())
    summary(model_regresi())
  })
  output$plot_suhu <- renderPlot({
    df <- data_analisis() %>%
      filter(!is.na(Suhu), !is.na(Produksi))
    validate(
      need(nrow(df) > 2, "Data tidak cukup untuk membuat plot.")
    )
    ggplot(df, aes(x = Suhu, y = Produksi)) +
      geom_point(color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "Produksi vs Suhu",
           x = "Suhu (Celsius)",
           y = "Produksi (Ribu Ton)") +
      theme_minimal()
  })
  
  output$plot_hujan <- renderPlot({
    df <- data_analisis() %>%
      filter(!is.na(CurahHujan), !is.na(Produksi))
    validate(
      need(nrow(df) > 2, "Data tidak cukup untuk membuat plot.")
    )
    ggplot(df, aes(x = CurahHujan, y = Produksi)) +
      geom_point(color = "darkgreen") +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(title = "Produksi vs Curah Hujan",
           x = "Curah Hujan (mm)",
           y = "Produksi (Ribu Ton)") +
      theme_minimal()
  })
  
  output$analisis_tahun_ui <- renderUI({
    df <- gabung_data()
    tahun_min <- min(df$Tahun, na.rm = TRUE)
    tahun_max <- max(df$Tahun, na.rm = TRUE)
    
    sliderInput(
      "analisis_tahun",
      "Rentang Tahun:",
      min = tahun_min,
      max = tahun_max,
      value = c(tahun_min, tahun_max),
      sep = ""
    )
  })
  
  observe({
    df <- gabung_data()
    updateSelectInput(
      session,
      "tren_provinsi",
      choices = c("Semua", sort(unique(df$Provinsi)))
    )
    updateSelectInput(
      session,
      "tren_komoditas",
      choices = c("Semua", sort(unique(df$Komoditas)))
    )
    
    print(names(shape_sumatera()))
  })
  
  tren_data <- reactive({
    df <- gabung_data()

    if (input$tren_provinsi != "Semua") {
      df <- df %>% filter(Provinsi == input$tren_provinsi)
    }    
    if (input$tren_komoditas != "Semua") {
      df <- df %>% filter(Komoditas == input$tren_komoditas)
    }
    df %>%
      group_by(Tahun) %>%
      summarise(Produksi = sum(Produksi, na.rm = TRUE), .groups = "drop")
  })
  
  output$plot_tren_tahunan <- renderPlotly({
    df <- tren_data()
    
    validate(
      need(nrow(df) > 1, "Data tidak cukup untuk membuat grafik tren tahunan.")
    )
    
    p <- ggplot(df, aes(x = Tahun, y = Produksi, group = 1, text = paste("Tahun:", Tahun, "<br>Produksi:", Produksi))) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "blue", size = 2) +
      labs(title = "Tren Produksi Tahunan",
           x = "Tahun",
           y = "Produksi (Ribu Ton)") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$table_frekuensi_kumulatif <- renderDT({
    df <- tren_data() %>%
      arrange(Tahun) %>%
      mutate(Frekuensi = Produksi,
             Frekuensi_Kumulatif = cumsum(Produksi)) %>%
      select(Tahun, Frekuensi, Frekuensi_Kumulatif)
    
    datatable(df, options = list(pageLength = 10, searching = FALSE))
  })
  
  # Update filter choices for Perbandingan Komoditas tab
  observe({
    df <- gabung_data()
    
    updateSelectInput(
      session,
      "perbandingan_provinsi",
      choices = c("Semua", sort(unique(df$Provinsi)))
    )
    
    updateSelectInput(
      session,
      "perbandingan_komoditas",
      choices = c("Semua", sort(unique(df$Komoditas)))
    )
  })
  
  perbandingan_data <- reactive({
    df <- gabung_data()
    if (input$perbandingan_mode == "Jenis") {
      df <- df %>% filter(Tahun == input$perbandingan_tahun)
      df %>%
        group_by(Komoditas) %>%
        summarise(Produksi = sum(Produksi, na.rm = TRUE), .groups = "drop")
    } else {
      if (input$perbandingan_komoditas != "Semua") {
        df <- df %>% filter(Komoditas == input$perbandingan_komoditas)
      }
      df <- df %>% filter(Tahun == input$perbandingan_tahun)
      df %>%
        group_by(Provinsi, Komoditas) %>%
        summarise(Produksi = sum(Produksi, na.rm = TRUE), .groups = "drop")
    }
  })
  
  output$plot_perbandingan_komoditas <- renderPlotly({
    df <- perbandingan_data()
    validate(
      need(nrow(df) > 0, "Data tidak cukup untuk membuat grafik perbandingan komoditas.")
    )
    if (input$perbandingan_mode == "Jenis") {
      p <- ggplot(df, aes(x = reorder(Komoditas, Produksi), y = Produksi, fill = Komoditas,
                          text = paste("Komoditas:", Komoditas, "<br>Produksi:", Produksi))) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = paste("Perbandingan Produksi Komoditas Tahun", input$perbandingan_tahun),
             x = "Komoditas",
             y = "Produksi (Ribu Ton)") +
        theme_minimal() +
        theme(legend.position = "none")
    } else {
      p <- ggplot(df, aes(x = Provinsi, y = Produksi, fill = Komoditas,
                          text = paste("Wilayah:", Provinsi, "<br>Komoditas:", Komoditas, "<br>Produksi:", Produksi))) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = paste("Perbandingan Produksi Komoditas per Wilayah Tahun", input$perbandingan_tahun),
             x = "Wilayah",
             y = "Produksi (Ribu Ton)") +
        theme_minimal()
    }
    ggplotly(p, tooltip = "text")
  })
  
  observeEvent(event_data("plotly_click", source = "tren"), {
    click_data <- event_data("plotly_click", source = "tren")
    if (is.null(click_data)) {
      output$info_tren_tahunan <- renderText({ "" })
    } else {
      tahun_clicked <- click_data$x
      produksi_clicked <- click_data$y
      output$info_tren_tahunan <- renderText({
        paste("Tahun:", tahun_clicked, "\nProduksi:", produksi_clicked)
      })
    }
  })
  
  observeEvent(event_data("plotly_click", source = "perbandingan"), {
    click_data <- event_data("plotly_click", source = "perbandingan")
    if (is.null(click_data)) {
      output$info_perbandingan_komoditas <- renderText({ "" })
    } else {
      komoditas_clicked <- click_data$x
      produksi_clicked <- click_data$y
      tahun_selected <- input$perbandingan_tahun
      if (input$perbandingan_mode == "Jenis") {
        output$info_perbandingan_komoditas <- renderText({
          paste("Komoditas:", komoditas_clicked, "\nTahun:", tahun_selected, "\nProduksi:", produksi_clicked)
        })
      } else {
        wilayah_clicked <- click_data$pointNumber + 1
        df <- perbandingan_data()
        wilayah_name <- unique(df$Provinsi)[wilayah_clicked]
        output$info_perbandingan_komoditas <- renderText({
          paste("Komoditas:", komoditas_clicked, "\nWilayah:", wilayah_name, "\nTahun:", tahun_selected, "\nProduksi:", produksi_clicked)
        })
      }
    }
  })
  observe({
    df <- gabung_data()
    updateSelectInput(session, "peta_komoditas",
                      choices = c("Semua", sort(unique(df$Komoditas))))
  })
  output$peta_produksi <- renderLeaflet({
    df <- gabung_data()
    
    if (input$peta_komoditas != "Semua") {
      df <- df %>% filter(Komoditas == input$peta_komoditas)
    }
    df_filter <- df %>% filter(Tahun == input$peta_tahun)
    
    df_agg <- df_filter %>%
      group_by(Provinsi) %>%
      summarise(Produksi = sum(Produksi, na.rm = TRUE), .groups = "drop") %>%
      mutate(Provinsi_Join = case_when(
        Provinsi == "Aceh" ~ "DI. ACEH",
        Provinsi == "Sumatera Utara" ~ "SUMATERA UTARA",
        Provinsi == "Sumatera Barat" ~ "SUMATERA BARAT",
        Provinsi == "Riau" ~ "RIAU",
        Provinsi == "Jambi" ~ "JAMBI",
        Provinsi == "Sumatera Selatan" ~ "SUMATERA SELATAN",
        Provinsi == "Bengkulu" ~ "BENGKULU",
        Provinsi == "Lampung" ~ "LAMPUNG",
        Provinsi == "Kepulauan Riau" ~ "KEPULAUAN RIAU",
        Provinsi == "Kepulauan Bangka Belitung" ~ "BANGKA BELITUNG",
        TRUE ~ Provinsi
      ))
    
    
    
    shp <- shape_sumatera() %>%
      mutate(Propinsi = trimws(Propinsi),
             Propinsi = recode(Propinsi,
                               "DI. ACEH" = "Aceh",
                               "SUMATERA UTARA" = "Sumatera Utara",
                               "SUMATERA BARAT" = "Sumatera Barat",
                               "RIAU" = "Riau",
                               "JAMBI" = "Jambi",
                               "SUMATERA SELATAN" = "Sumatera Selatan",
                               "BENGKULU" = "Bengkulu",
                               "LAMPUNG" = "Lampung",
                               "KEPULAUAN RIAU" = "Kepulauan Riau",
                               "BANGKA BELITUNG" = "Kepulauan Bangka Belitung"
             )) %>%
      left_join(df_agg, by = c("Propinsi" = "Provinsi"))
    
    validate(need(!all(is.na(shp$Produksi)), "Tidak ada data produksi untuk ditampilkan di peta."))
    
    pal <- colorNumeric("YlGn", domain = shp$Produksi, na.color = "transparent")
    label_popup <- ifelse(
      is.na(shp$Produksi),
      paste0(shp$Propinsi, ": Tidak ada data"),
      paste0(shp$Propinsi, ": ", round(shp$Produksi, 2), " Ribu Ton")
    )
    
    leaflet(shp) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Produksi),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste0(Propinsi, ": ", round(Produksi, 2), " Ribu Ton")
      ) %>%
      
      addLegend(pal = pal, values = ~Produksi, opacity = 0.7, title = "Produksi",
                position = "bottomright") %>%
    fitBounds(lng1 = 95, lat1 = -6.5, lng2 = 106, lat2 = 6)
  })
  
}

shinyApp(ui, server)