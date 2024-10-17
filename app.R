library("shiny")
library("readxl")
library("DT")
library("plotly")

ui <- fluidPage(
  titlePanel("Visualize your RNA Seq data"),
  # sidebarPanel(),
  mainPanel(
    actionButton(inputId = "upload_excel_sheet",label = "Upload Excel Data"),
    br(),br(),
    dataTableOutput("data_table"),
    br(),br(),
    plotlyOutput("volcano_plot")
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Popup window
  observeEvent(input$upload_excel_sheet,{
    showModal(modalDialog(title = "Data Structure",
                          HTML("<p>Make sure the excel sheet has gene names as row names and a column with <b>log2FoldChange</b> and one with adjusted p-values (<b>padj</b>):</p>
                               <img src='excel_example.png'
                               <br>"),
                          size = "m",
                          footer = list(
                            fileInput('uploaded_dataset_id',
                                      label = NULL,
                                      multiple = FALSE,
                                      accept = c(".xlsx",".xls"),
                                      width = NULL,
                                      buttonLabel = "Browse...",
                                      placeholder = "No file selected"),
                            downloadButton("get_example_data",label = "Example"),
                            actionButton("dismiss_modal",label = "Cancel")),
    ))
  })
  
  # Allow download of example data
  output$get_example_data <- downloadHandler(
    filename <- function() {
      "RNA_Seq_example_data.xlsx"
    },
    content <- function(file) {
      file.copy("data/RNA_Seq_example_data.xlsx", file)
    }
  )
  
  # Close popup
  observeEvent(input$dismiss_modal,{
    removeModal()
  })
  
  # Reactive value to store the uploaded data
  uploaded_data <- reactiveVal(NULL)
  
  # Read excel sheet
  observeEvent(input$uploaded_dataset_id,{
    
    tmp_data <- read_excel(input$uploaded_dataset_id$datapath)
    
    # Check so excel sheet contains correct info
    validate(
      if(!all(c("Gene","padj","log2FoldChange")%in%colnames(tmp_data))){
        showModal(
          modalDialog(
            title = div("Oops, seems you're missing some data!", align = "center"),
            align = "center",
            "Please make sure 'Gene', 'padj' and 'log2FoldChange' are in your excel sheet and try again.",
            easyClose = TRUE,
            footer = NULL
          )
        )
      }
    )
    
    # Update reactive value
    uploaded_data(tmp_data)
    
    # Close popup
    removeModal()
    
  })
  
  output$data_table <- renderDataTable({
    req(uploaded_data())  # Only render if data is available
    datatable(uploaded_data(), options = list(pageLength = 10))
  })
  
  output$volcano_plot <- renderPlotly({
    req(uploaded_data())
    plot_ly(data = uploaded_data(), 
            x = ~log2FoldChange, 
            y = ~-log10(padj), 
            type = 'scatter', 
            mode = 'markers',
            text = ~paste(
              "Gene: ", Gene, "<br>",
              "padj: ", padj, "<br>",
              "log2FoldChange: ", round(log2FoldChange, 2)
            ),
            hoverinfo = 'text',
            marker = list(size = 10, color = '#3498db',opacity = 0.5)) %>%
      layout(
        title = "Volcano",
        xaxis = list(title = "log2FoldChange"),
        yaxis = list(title = "-log10(padj)")
      )
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
