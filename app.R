library(shiny)
library(shinyFiles)
library(shinydashboard)
library(xlsx)
library(survival)
library(survminer)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(dashboardthemes)
library(crosstalk)
library(data.table)
library(highcharter)
library(shinycustomloader)

source("D:/NIH_classifier/R_files/Copy_number/plot_copy_number.R")
source("D:/NIH_classifier/R_files/plot_supervised_SD.R")
source("D:/NIH_classifier/R_files/Copy_number/Import_copy_number.R")
source("D:/NIH_classifier/R_files/plot_dendrogram.R")
source("D:/NIH_classifier/R_files/fviz_dend_modified.R")
#source("D:/NIH_classifier/R_files/qc_plots.R")

source("D:/NIH_classifier/R_files/Shiny/customTheme.R")

jsCode <- "shinyjs.resetSel = function() { Plotly.restyle(plot, {selectedpoints: [null]});}"

options(max.print=1000000)

#ui------------------------
ui <- dashboardPage(
  dashboardHeader(
    # Set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 100px}"),
            tags$style(".main-header .logo {height: 100px;}"),
            tags$style(".sidebar-toggle {height: 100px; padding-top: 0px !important;}"),
            tags$style(".navbar {min-height:100px !important}"),
            
    ),
    title = span(img(src='nci-logo3.png', height="74")), titleWidth = 500),
  ## Sidebar content
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      # menuItem("DKFZ MNP Classifier",
      #          href = "https://www.molecularneuropathology.org/mnp"),
      # br(),
      # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
      #                   label = "Sample", icon = shiny::icon("search")),
      # br(),
      # shinySaveButton("save", "Save file", "Save file as ...", filetype=list(xlsx="xlsx")),
      tags$style(".topimg {
                            margin-left:10px;
                            position: absolute;
                            bottom:0;
                          }"),
      tags$style(".left-side, .main-sidebar {padding-top: 140px}")
      #div(class="topimg",img(src = "nih-logo-footer.png", height = "30%", width = "30%"), img(src = "931-9314301_university-of-michigan-usgs-logo-white.png", height = "40%", width = "40%"), style="text-align: center;")
    )
  ),
  ## Body content
  dashboardBody(
    tags$head(tags$style(HTML('
                        .main-header .logo {
                          font-family: "Arial", Helvetica, sans-serif;
                          #font-weight: bold;
                          #font-size: 30px;
                          #float: left!important;
                          #line-height: 200px !important;
                          padding: 10px 0px;}
                        .content-wrapper, .right-side {
                                background-color: white;
                        }
                        .form-group{
                        margin-bottom: 0px;
                        }
                          .main-sidebar { font-size: 22px; }
                          .radio label {
                            font-size: 22px;
                          }
                          
                      ')),
    ),
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("resetSel")),
    ### changing theme
    customTheme,
    # shinyDashboardThemes(
    #   theme = "grey_light"
    # ),
    setShadow(id = "tabset1"),
    fluidRow(
      column(12,
             fluidRow(
               column(width = 6, offset = 0,
                      div(
                        tags$style("label{color: black;}"),
                        div(style = "font-size: 15px;display: inline-block;",
                            selectInput("organ_system", "Organ System", choices = c("Central Nervous System",
                                                                                    "Bone and Soft Tissue",
                                                                                    "CNS/Sarcoma",
                                                                                    "Hematopoietic",
                                                                                    "Renal",
                                                                                    "Gastrointestinal",
                                                                                    "Gynecologic",
                                                                                    "Hepatobiliary and Pancreatic",
                                                                                    "Thoracic",
                                                                                    "Skin",
                                                                                    "Endocrine",
                                                                                    "Head and Neck",
                                                                                    "Genitourinary",
                                                                                    "Pan-Cancer"), selected = "Central Nervous System", width = "250px"),
                            tags$head(tags$style(HTML(".select-input {height: 20px;}"))),
                            #tags$head(tags$style(".shiny-notification {position: fixed; top: 60% ;left: 0%;}"))
                            align = "left"),
                        div(style="display: inline-block; font-size: 15px;",
                            selectInput("x", "Select embedding", 
                                        c("umap","densmap","semisupervised_umap","tsne"), width = "250px"), align = "left"),
                        div(style="display: inline-block; font-size: 15px;",
                            selectizeInput(
                              "select", 'Search', choices = "", label = "Search sample(s)",
                              options = list(create = TRUE, maxItems = 5, placeholder = 'Sample(s)'), width = "250px"), align = "left"),
                        div(style="display: inline-block; font-size: 15px;",
                            selectInput("color_labels", "Color by:", 
                                        c("DKFZ_v11","NCI_METRIC","Purity","CD4_T",
                                          "CD8_T","B_cells","Eosinophils","NK_cells",
                                          "Monocytes","Neutrophils","Endoth_cells","Glia", "Neurons",
                                          "MGMT_meth", "MLH1_meth", "Age", "Sex_prediction",
                                          "Location_general", "Location_specific"), selected = "DKFZ_v11", width = "250px"), align = "left"),
                        align = "center"),
                      conditionalPanel(
                        condition = "input.organ_system == 'Central Nervous System'",
                        div(plotlyOutput("plot_CNS"), align = "left")
                      ),
                      conditionalPanel(
                        condition = "input.organ_system == 'Bone and Soft Tissue'",
                        div(plotlyOutput("plot_SARC"), align = "left")
                      ),
                      conditionalPanel(
                        condition = "input.organ_system == 'CNS/Sarcoma'",
                        div(plotlyOutput("plot_CNS_SARC"), align = "left")
                      ),
                      conditionalPanel(
                        condition = "input.organ_system == 'Hematopoietic'",
                        div(plotlyOutput("plot_HEME"), align = "left")
                      ),
                      conditionalPanel(
                        condition = "input.organ_system == 'Renal'",
                        div(plotlyOutput("plot_RENAL"), align = "left")
                      )
               ),
               column(width = 6, offset = 0,
                      tabBox(
                        id = "tabset1", width = "auto",
                        tabPanel("Overview",
                                 h3("NCI DNA Methylation Data Hub and Repository", 
                                    style = c("color:#2a666e; font-size:24px; font-weight:normal;")),
                                 p("The NCI Laboratory of Pathology has implemented a
                                                             novel clinically-reportable assay that uses genome-wide 
                                                             DNA methylation profiling as a diagnostic tool for tumors of the central 
                                                             nervous system. The validated tool is based, in part, on data published 
                                                             in a recent Nature study (Capper et al., 2018) that showed tumor methylation
                                                             profiles can serve as an important adjunct and help refine morphology-based 
                                                             diagnostics in tumors of the brain and spinal cord. Notably, methylation 
                                                             results led to diagnostic revisions in a significant proportion (129/139 or 
                                                             12% of the entire cohort) of cases.This finding was confirmed in five external 
                                                             centers (50/401, 12%), with reclassification rates in 6%-25% of cases.", 
                                   style = c("color:black; font-size:17px")),
                                 br(),
                                 p("Since its adaptation as an investigative diagnostic tool in 2018, 
                                                             Infinium methylation arrays have led to the identification of new CNS tumor
                                                             types and subtypes, many of which harbor meaningful associations with clinical
                                                             course and outcome. Not infrequently, however, classification results may be 
                                                             non-contributory (suboptimal classifier score) or not congruent with the 
                                                             clinical, histopathologic, or molecular features of the case (misleading profile). 
                                                             Interpretation can often be improved with visual inspection of unsupervised UMAP 
                                                             or t-SNE embedding(s).", style = c("color:black; font-size:17px")),
                                 br(),
                                 p("The NCI Laboratory of Pathology is poised to become a diagnostic 
                                                             reference center to implement this tool for diagnostically challenging 
                                                             neuropathology cases. Going forward, it is likely that new methylation-based 
                                                             classifiers will emerge for additional tumor types. Areas of future growth 
                                                             include the implementation of clinical whole-exome sequencing, gene 
                                                             expression diagnostics (RNA-seq), and a dynamic liquid biopsy program.", 
                                   style = c("color:black; font-size:17px")),
                                 br(),
                                 # A static valueBox
                                 tags$head(tags$style(HTML(".small-box {height: 90px}"))),
                                 infoBoxOutput("info_box2"),
                                 infoBoxOutput("info_box3"),
                                 infoBoxOutput("info_box4"),
                                 br(),
                                 highchartOutput("bar_tumor_types", height = "600px")
                        ),
                        tabPanel("QC",
                                 h3("Sample quality"),
                                 tags$head(tags$style("#text1{color: grey;
                                                                 font-size: 14px;
                                                                 font-style: bold;
                                                                 }")),
                                 p("To view sample-level quality control metrics, click on a point in the embedding
                                                             (please allow approximately 4-5 seconds for the data to be displayed after
                                                             clicking a sample). The summary data and plots are generated during functional
                                                             normalization of the methylation signal intensities. It is important to assess both
                                                             sample and probe quality prior to interpretatin of a case.", 
                                   style = c("color:black; font-size:17px")),
                                 br(),
                                 fluidRow(
                                   column(6,
                                          div(plotOutput("plot_qc_sex")),
                                          h4("Predicted sex"),
                                          tags$head(tags$style("#text1{color: grey;
                                                             font-size: 12px;
                                                             font-style: bold;
                                                             }")),
                                          p("This is a plot of the difference between median chromosome Y and
                                                                  chromosome X probe intensities (XY diff). Cutoff for sex detection
                                                                  was XY diff = -2. Mismatched samples are shown in red. The dashed lines
                                                                  represent 3 SD from the mean xy difference. Samples that fall in this
                                                                  interval are denoted as outliers.", style = c("color:black; font-size:14px"))
                                   ),
                                   column(6,
                                          div(plotOutput("plot_qc_methunmeth")),
                                          h4("Unmethylated vs methylated intensity"),
                                          tags$head(tags$style("#text1{color: grey;
                                                                 font-size: 12px;
                                                                 font-style: bold;
                                                                 }")),
                                          p("To explore the quality of the samples, it is useful to plot the median
                                                                      methylation intensity against the median unmethylation intensity with
                                                                      the option to color outliers by group. There are 97 outliers from the meth
                                                                      vs unmeth comparison. Outliers are samples whose predicted median methylated
                                                                      signal is more than 3 standard deviations from the expected (regression line).",
                                            style = c("color:black; font-size:14px"))
                                   )
                                 )
                        ),
                        tabPanel("Copy number",
                                 h3("Genome-wide Copy Number"),
                                 tags$head(tags$style("#text1{color: grey;
                                                                 font-size: 14px;
                                                                 font-style: bold;
                                                                 }")),
                                 p("To view sample-level copy number data, click a point in the embedding
                                                             (please allow approximately 4-5 seconds for the data to be displayed after
                                                             clicking a sample).", style = c("color:black; font-size:17px")),
                                 br(),
                                 div(withLoader(plotlyOutput("plot_CNA"),type="html", loader="dnaspin")),
                                 br(),
                                 tabBox(
                                   id = "tabset2", width = "auto",
                                   tabPanel("Oncogenic",
                                            div(DT::dataTableOutput("brush_CNA")),
                                            tags$head(tags$style("#brush_CNA{font-size:14px;}"))
                                   ),
                                   tabPanel("Complete",
                                            div(DT::dataTableOutput("brush_CNA_complete")),
                                            tags$head(tags$style("#brush_CNA{font-size:14px;}"))
                                   ),
                                   tabPanel("Candidate fusions",
                                            div(DT::dataTableOutput("brush_breakpoint")),
                                            tags$head(tags$style("#brush_breakpoint{font-size:14px;}"))
                                   ),
                                   tabPanel("Breakpoint genes",
                                            div(DT::dataTableOutput("brush_breakpoint_genes")),
                                            tags$head(tags$style("#brush_breakpoint_genes{font-size:14px;}"))
                                   )
                                 )
                        ),
                        
                        tabPanel("MGMT/MLH1",
                                 h3("Promoter methylation status"),
                                 tags$head(tags$style("#text1{color: grey;
                                                                 font-size: 14px;
                                                                 font-style: bold;
                                                                 }")),
                                 p("The plots shown here illustrate the methylation status of the
                                                             MGMT and MLH1 gene promoters.", style = c("color:black; font-size:17px")),
                                 br(),
                                 fluidRow(
                                   column(6,
                                          div(plotOutput("plot_MGMT")),
                                          h4("MGMT promoter methylation"),
                                          tags$head(tags$style("#text1{color: grey;
                                                             font-size: 12px;
                                                             font-style: bold;
                                                             }")),
                                          paste0("Result:", textOutput("MGMT_result")),
                                          tags$head(tags$style("#MGMT_result{font-size:17px;color: black;}")),
                                          br(),
                                          p("The methylation status of the MGMT promoter regions was determined
                                                                      using the MGMT-STP27 logistic regression model (see Methodology).
                                                                      The model uses two CpG probes and was trained on 63 GBM samples
                                                                      for which MGMT promoter mehtylation status was known by 
                                                                      methylation-specific PCR (MSP).", 
                                            style = c("color:black; font-size:14px"))
                                   ),
                                   column(6,
                                          div(plotOutput("plot_MLH1")),
                                          h4("MLH1 promoter methylation"),
                                          tags$head(tags$style("#text1{color: grey;
                                                                 font-size: 12px;
                                                                 font-style: bold;
                                                                 }")),
                                          p("MLH1 promoter methylation status was assessed using previously established
                                                                      beta value cutoffs among four specific CpG probes. The criteria 
                                                                      require all four MLH1 CpG sites to be above their respective cutoffs (see Methodology).",
                                            style = c("color:black; font-size:14px"))
                                   )
                                 )
                        ),
                        
                        tabPanel("Cohort analysis"),
                        
                        tabPanel("Survival",
                                 radioGroupButtons("radio", h3("Select groups for comparison", style = "font-size:14px;color:black"),
                                                   choices = list("Group 1" = 1, "Group 2" = 2,
                                                                  "Group 3" = 3), selected = 1),
                                 actionButton("reset", "Reset all Groups"),
                                 uiOutput("currentSelections"),
                                 fluidRow(
                                   column(6, div(plotOutput("survival_OS"))),
                                   column(6, div(plotOutput("survival_PFS")))
                                 )),
                        tabPanel("Table", div(DT::dataTableOutput("brush_table")),
                                 tags$head(tags$style("#brush_table{font-size:12px;}")),
                        ),
                        
                        tabPanel("Subgroup analysis",
                                 h3("Iterative unsupervised clustering"),
                                 tags$head(tags$style("#text1{color: grey;
                                                                 font-size: 14px;
                                                                 font-style: bold;
                                                                 }")),
                                 p("This function allows a user to iteratively select groups of samples and perform unsupervised
                                                             dimensionality reduction. Feature selection and principal component analysis are performed on
                                                             the full probes set, rather than the reduced probeset used for the global plot. Finally,
                                                             depending on the number of smaples that are selected, this function may take a long time to process.
                                                             Once samples of interest are selected in the plot on the left, you must press the 'Render' button to
                                                             begin the anlaysis.", 
                                   style = c("color:black; font-size:17px")),
                                 br(),
                                 tabBox(
                                   id = "tabset3", width = "auto",
                                   tabPanel("Dimensionality reduction",
                                            actionButton("render_dimenred", "Render"),
                                            #verbatimTextOutput("plot_dimenred"),
                                            div(withLoader(plotlyOutput("plot_dimenred", width = "400px", height = "500px"),type="html", loader="dnaspin")),
                                            br(),
                                            div(DT::dataTableOutput("brush_supervised")),
                                            tags$head(tags$style("#brush_supervised{font-size:14px;}"))
                                   ),
                                   tabPanel("Hierarchical clustering",
                                            actionButton("render_hc", "Render"),
                                            div(withLoader(plotlyOutput("plot_hc", width = "800px", height = "400px"),type="html", loader="dnaspin")),
                                            br(),
                                            # div(DT::dataTableOutput("brush_hc_cluster")),
                                            # tags$head(tags$style("#brush_hc_cluster{font-size:14px;}"))
                                            textOutput("brush_hc_cluster")
                                   )
                                 )
                                 #uiOutput("currentSelections"),
                        ),
                        tabPanel("Methodology")
                      )
               )
             )
      )
    )
  )
)

#server------------------------
server <- shinyServer(function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  # suppress warnings
  storeWarn<- getOption("warn")
  options(warn = -1)
  
  output$info_box2 <- renderInfoBox({
    #df_output <- anno_base %>% group_by(Primary_category) %>% tally()
    df_output <- anno_base[!is.na(anno_base$Primary_category),]
    df_output <- df_output[!df_output$matched_cases=="Duplicate",]
    # infoBox("Total samples", paste(df_output$Type[df_output$n == max(df_output$n)],max(df_output$n)),
    #         color = "blue", icon = icon("pie-chart"))
    infoBox("Total samples", tags$p(paste(nrow(df_output)), style = "font-size: 24px;"),
            color = "blue", icon = icon("pie-chart"))
  })
  
  output$info_box3 <- renderInfoBox({
    df_output <- anno_base[!is.na(anno_base$Primary_category),]
    df_output <- df_output[!df_output$matched_cases=="Duplicate",]
    df_output <- anno_base[!is.na(anno_base$Primary_study),]
    infoBox("Number of studies", tags$p(paste(length(unique(df_output$Primary_study))), style = "font-size: 24px;"),
            color = "purple", icon = icon("cloud-download"))
  })
  
  output$info_box4 <- renderInfoBox({
    df_output <- anno_base[!is.na(anno_base$Primary_category),]
    df_output <- df_output[!df_output$matched_cases=="Duplicate",]
    df_output <- anno_base[!is.na(anno_base$Center_methy),]
    infoBox("Number of institutions", tags$p(paste(length(unique(df_output$Center_methy))), style = "font-size: 24px;"),
            color = "navy", icon = icon("institution"))
  })
  
  ## data.frame with an extra group column (initially set to NA)  
  rv <- reactiveValues(data_df = anno_base %>% mutate(group = NA))
  
  ## when a selection is made, assign group values to data_df based on selected radio button
  observeEvent(
    event_data("plotly_selected"), {
      d <- event_data("plotly_selected")
      ## reset values for this group
      rv$data_df$group <- ifelse(rv$data_df$group == input$radio, NA, rv$data_df$group)
      ## then re-assign values:
      rv$data_df[d$key,"group"] <- input$radio
    }
  )
  
  observeEvent(
    event_data("plotly_click"), {
      d <- event_data("plotly_click")
    }
  )
  
  ## when reset button is pressed, reset the selection rectangle 
  ## and also reset the group column of data_df to NA
  observeEvent(input$reset, {
    js$resetSel()
    rv$data_df$group <- NA
  })
  
  ## when radio button changes, reset the selection rectangle and reset plotly_selected
  ## (otherwise selecting the same set of points for two groups consecutively will 
  ## not register the selection the second time)
  observeEvent(input$radio, {
    js$resetSel()
    runjs("Shiny.setInputValue('plotly_selected-A', null);")
  })
  
  ## for each group, show the number of selected points
  ## (not required by the rest of the app but useful for debugging)
  output$currentSelections <- renderUI({
    number_by_class <- summary(factor(rv$data_df$group, levels = c("1","2","3")))
    tagList(
      h5("Current Selections:"),
      p(paste0("Group 1: ",number_by_class[1], " points selected")),
      p(paste0("Group 2: ",number_by_class[2], " points selected")),
      p(paste0("Group 3: ",number_by_class[3], " points selected"))
    )
  })
  
  # updateSelectizeInput(session, 'select', choices = anno_base$Sample, selected = NULL)
  # 
  # output$values <- renderPrint({
  #   list(select = input$select)
  # })
  
  output$bar_tumor_types = renderHighchart({
    # Get one row per theme, with the relevant columns.
    df_output <- anno_base[!is.na(anno_base$Primary_category),]
    # # Get log num parts, and the maximum over the dataset.
    # temp.heads.df = temp.heads.df %>%
    #   mutate(num.parts.col = log(total.heads) / max(log(total.heads)))
    # # Set the format for the tooltip.
    # point.format = paste(
    #   " ({point.num_pieces} pieces)</span><br/><span>",
    #   input$demographicsMeasurePicker,
    #   ":\u00A0{point.y}</span>",
    #   sep = ""
    # )
    # Make the plot.
    agg <- as.data.frame(table(df_output$Primary_category))
    agg <- agg[rev(order(agg$Freq)),]
    library(viridis)
    cols <- colorspace::sequential_hcl(length(agg$Freq))
    hc = highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = agg$Var1, labels = list(style = list(fontSize = "12px"))) %>%
      hc_yAxis(labels = list(style = list(fontSize = "12px"))) %>%
      hc_add_series(pointPadding = 0,
                    data = agg$Freq,
                    colorByPoint = T,
                    colors = cols,
                    borderColor = NULL) %>%
      # hc_plotOptions(
      #   column = list(
      #     color = list(
      #       #linearGradient = list(x1 = 0, x2 = 0, y1 = 0, y2 = 1),
      #       stops = list(
      #         c(0, '#979ec4'),
      #         c(1, '#ffffff')
      #       )
      #     )
      #   )
      # ) %>%
    # hc_tooltip(headerFormat = "<span><b>{point.key}</b>",
    #            pointFormat = point.format,
    #            valueDecimals = 2) %>%
    hc_legend(enabled = F)
    hc
  })
  
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  m <- list(
    l = 100,
    r = 200,
    b = 100,
    t = 50)
  
  
  #CNS
  output$plot_CNS <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    key <- rownames(anno_neuro)
    #sample_filter <- filter_select("filter", "Find sample", tx, ~Sample)
    x <- paste0(input$x,"_x")
    y <- paste0(input$x,"_y")
    # cnames <- aggregate(cbind(umap_x, umap_y) ~ DKFZ_v11, data=anno_neuro,
    #                    FUN=function(x)mean((x)))
    p <- ggplot(data=anno_neuro, aes(x=anno_neuro[[x]],y=anno_neuro[[y]],key=key)) +
      geom_point(aes(color=DKFZ_v11), size=3, alpha=1) +
      #geom_text(data=cnames, aes(x=umap_x, y=umap_y, label = DKFZ_v11), position = position_dodge(width=0.5),  size=2.5, inherit.aes = FALSE) +
      theme_classic() +
      theme(axis.text.y = element_text(size=9, color="black"),
            axis.text.x = element_text(size=9, color="black"),
            axis.ticks.y = element_line(color="black", size = 0.5),
            axis.ticks.x = element_line(color="black", size = 0.5),
            axis.ticks.length = unit(2,"mm"),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major = element_line(colour="grey", size=0.5),
            axis.line = element_blank(),
            legend.text=element_text(size=10),
            legend.title=element_text(size=11),
            legend.key.size = unit(0.5, 'lines'),
            axis.title.x = element_text(size=14, color="black"),
            axis.title.y = element_text(size=14, color="black")) +
      labs(x = paste0(input$x,"_1"), y = paste0(input$x,"_2")) +
      theme(legend.position="none") +
      scale_color_manual("Methylation class", values = mycols, guide = guide_legend(override.aes = list(shape = 15))) +
      #scale_color_gradient2(low="red", mid="white", high="blue", midpoint = mean(anno_neuro$Purity[which(!is.na(anno_neuro$Purity))])) +
      scale_x_continuous(breaks = seq(-100, 100, by=5)) +
      scale_y_continuous(breaks = seq(-100, 100, by=5)) +
      coord_fixed(ratio = 1, xlim = ranges2$x, ylim = ranges2$y, expand = TRUE, clip = "on")
    #guides(color = guide_legend(override.aes = list(shape = c(15))))
    #coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    ggplotly(p) %>% 
      add_annotations(x = subset(p$data, !is.na(NIH_labels))[[x]],
                      y = subset(p$data, !is.na(NIH_labels))[[y]],
                      text = subset(p$data, !is.na(NIH_labels))$NIH_labels,
                      showarrow = TRUE,
                      arrowcolor='red',
                      arrowhead = 6,
                      arrowsize = 1,
                      xref = "x",
                      yref = "y",
                      font = list(color = 'black',
                                  family = 'arial',
                                  size = 14)) %>%
      config(scrollZoom = TRUE) %>%
      layout(title = paste0("Central Nervous System (n=",nrow(p$data),")"),
             #height=1000, 
             dragmode = "pan", xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
             margin = m,
             showlegend = T,
             legend = list(orientation = "v",
                           yanchor = "center",
                           itemclick = "toggleothers",
                           tracegroupgap = 2,
                           itemwidth = 75,
                           itemsizing = "constant")) %>% 
      toWebGL()
    
  })
  
  #SARCOMA
  output$plot_SARC <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    key <- rownames(anno_sarcoma)
    #sample_filter <- filter_select("filter", "Find sample", tx, ~Sample)
    x <- paste0(input$x,"_x")
    y <- paste0(input$x,"_y")
    #cnames <- aggregate(cbind(umap_x, umap_y) ~ Combined_class_match_dkfz, data=anno_sarcoma,
    #                    FUN=function(x)mean((x)))
    p <- ggplot(data=anno_sarcoma, aes(x=anno_sarcoma[[x]],y=anno_sarcoma[[y]],key=key)) +
      geom_point(aes(color=DKFZ_v11),  size=2, alpha=1) +
      #geom_text(data=cnames, aes(x=umap_x, y=umap_y, label = Combined_class_match_dkfz), position = position_dodge(width=0.5),  size=2.5, inherit.aes = FALSE) +
      theme_classic() +
      theme(axis.text.y = element_text(size=9, color="black"),
            axis.text.x = element_text(size=9, color="black"),
            axis.ticks.y = element_line(color="black", size = 0.5),
            axis.ticks.x = element_line(color="black", size = 0.5),
            axis.ticks.length = unit(2,"mm"),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major = element_line(colour="grey", size=0.5),
            axis.line = element_blank(),
            legend.text=element_text(size=10),
            legend.title=element_text(size=11),
            legend.key.size = unit(0.5, 'lines'),
            axis.title.x = element_text(size=14, color="black"),
            axis.title.y = element_text(size=14, color="black")) +
      labs(x = paste0(input$x,"_1"), y = paste0(input$x,"_2")) +
      #theme(legend.position="right") +
      scale_color_manual("Methylation class", values = mycols, guide = guide_legend(override.aes = list(shape = 15))) +
      scale_x_continuous(breaks = seq(-100, 100, by=5)) +
      scale_y_continuous(breaks = seq(-100, 100, by=5)) +
      coord_fixed(ratio = 1, xlim = ranges2$x, ylim = ranges2$y, expand = TRUE, clip = "on")
    #guides(color = guide_legend(override.aes = list(shape = c(15))))
    #coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    ggplotly(p) %>%
      add_annotations(x = subset(p$data, !is.na(NIH_labels))[[x]],
                      y = subset(p$data, !is.na(NIH_labels))[[y]],
                      text = subset(p$data, !is.na(NIH_labels))$NIH_labels,
                      showarrow = TRUE,
                      arrowcolor='red',
                      arrowhead = 6,
                      arrowsize = 1,
                      xref = "x",
                      yref = "y",
                      font = list(color = 'black',
                                  family = 'arial',
                                  size = 14)) %>%
      config(scrollZoom = TRUE) %>%
      layout(title = paste0("Bone and Soft Tissue (n=",nrow(p$data),")"),
             #height=1000, 
             dragmode = "pan", xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
             margin = list(t = 50),
             showlegend = T,
             legend = list(orientation = "v",
                           yanchor = "top",
                           itemclick = "toggleothers",
                           tracegroupgap = 2,
                           itemwidth = 75,
                           itemsizing = "constant")) %>%
      toWebGL()
    
  })
  
  #CNS/SARCOMA
  output$plot_CNS_SARC <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    key <- rownames(anno_neuro_sarcoma)
    #sample_filter <- filter_select("filter", "Find sample", tx, ~Sample)
    x <- paste0(input$x,"_x")
    y <- paste0(input$x,"_y")
    #cnames <- aggregate(cbind(umap_x, umap_y) ~ Combined_class_match_dkfz, data=anno_neuro_sarcoma,
    #                    FUN=function(x)mean((x)))
    p <- ggplot(data=anno_neuro_sarcoma, aes(x=anno_neuro_sarcoma[[x]],y=anno_neuro_sarcoma[[y]],key=key)) +
      geom_point(aes(color=DKFZ_v11),  size=2, alpha=1) +
      #geom_text(data=cnames, aes(x=umap_x, y=umap_y, label = Combined_class_match_dkfz), position = position_dodge(width=0.5),  size=2.5, inherit.aes = FALSE) +
      theme_classic() +
      theme(axis.text.y = element_text(size=9, color="black"),
            axis.text.x = element_text(size=9, color="black"),
            axis.ticks.y = element_line(color="black", size = 0.5),
            axis.ticks.x = element_line(color="black", size = 0.5),
            axis.ticks.length = unit(2,"mm"),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major = element_line(colour="grey", size=0.5),
            axis.line = element_blank(),
            legend.text=element_text(size=10),
            legend.title=element_text(size=11),
            legend.key.size = unit(0.5, 'lines'),
            axis.title.x = element_text(size=14, color="black"),
            axis.title.y = element_text(size=14, color="black")) +
      labs(x = paste0(input$x,"_1"), y = paste0(input$x,"_2")) +
      #theme(legend.position="right") +
      scale_color_manual("Methylation class", values = mycols, guide = guide_legend(override.aes = list(shape = 15))) +
      scale_x_continuous(breaks = seq(-100, 100, by=5)) +
      scale_y_continuous(breaks = seq(-100, 100, by=5)) +
      coord_fixed(ratio = 1, xlim = ranges2$x, ylim = ranges2$y, expand = TRUE, clip = "on")
    #guides(color = guide_legend(override.aes = list(shape = c(15))))
    #coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    ggplotly(p) %>%
      add_annotations(x = subset(p$data, !is.na(NIH_labels))[[x]],
                      y = subset(p$data, !is.na(NIH_labels))[[y]],
                      text = subset(p$data, !is.na(NIH_labels))$NIH_labels,
                      showarrow = TRUE,
                      arrowcolor='red',
                      arrowhead = 6,
                      arrowsize = 1,
                      xref = "x",
                      yref = "y",
                      font = list(color = 'black',
                                  family = 'arial',
                                  size = 14)) %>%
      config(scrollZoom = TRUE) %>%
      layout(title = paste0("Combined CNS and Sarcoma (n=",nrow(p$data),")"),
             #height=1000, 
             dragmode = "pan", xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
             margin = m,
             showlegend = T,
             legend = list(orientation = "v",
                           yanchor = "top",
                           itemclick = "toggleothers",
                           tracegroupgap = 2,
                           itemwidth = 75,
                           itemsizing = "constant")) %>%
      toWebGL()
    
  })
  
  datInput_dimred <- eventReactive(input$render_dimenred, {
    # datInput only validated once the go button is clicked
    key <- rownames(anno_base)
    d <- event_data("plotly_selected")
    req(d)
    samples_supervised <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    samples_supervised
  })
  
  
  output$plot_dimenred <- renderPlotly({
    plot_dat <- datInput_dimred()
    plot_supervised(plot_dat)
    #plot_dat
  })
  
  datInput_hc <- eventReactive(input$render_hc, {
    # datInput only validated once the go button is clicked
    key <- rownames(anno_base)
    d <- event_data("plotly_selected")
    req(d)
    samples_supervised <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    samples_supervised
  })
  
  output$plot_hc <- renderPlotly({
    plot_dat <- datInput_hc()
    plot_dendrogram(plot_dat)
  })
  
  
  
  #HEME
  output$plot_HEME <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    key <- rownames(anno_hemepath)
    #sample_filter <- filter_select("filter", "Find sample", tx, ~Sample)
    x <- paste0(input$x,"_x")
    y <- paste0(input$x,"_y")
    #cnames <- aggregate(cbind(umap_x, umap_y) ~ Combined_class_match_dkfz, data=anno_hemepath,
    #                    FUN=function(x)mean((x)))
    p <- ggplot(data=anno_hemepath, aes(x=anno_hemepath[[x]],y=anno_hemepath[[y]],key=key)) +
      geom_point(aes(color=NCI_METRIC),  size=2, alpha=1) +
      #geom_text(data=cnames, aes(x=umap_x, y=umap_y, label = Combined_class_match_dkfz), position = position_dodge(width=0.5),  size=2.5, inherit.aes = FALSE) +
      theme_classic() +
      theme(axis.text.y = element_text(size=9, color="black"),
            axis.text.x = element_text(size=9, color="black"),
            axis.ticks.y = element_line(color="black", size = 0.5),
            axis.ticks.x = element_line(color="black", size = 0.5),
            axis.ticks.length = unit(2,"mm"),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major = element_line(colour="grey", size=0.5),
            axis.line = element_blank(),
            legend.text=element_text(size=10),
            legend.title=element_text(size=11),
            legend.key.size = unit(0.5, 'lines'),
            axis.title.x = element_text(size=14, color="black"),
            axis.title.y = element_text(size=14, color="black")) +
      labs(x = paste0(input$x,"_1"), y = paste0(input$x,"_2")) +
      #theme(legend.position="right") +
      scale_color_manual("Methylation class", values = mycols, guide = guide_legend(override.aes = list(shape = 15))) +
      scale_x_continuous(breaks = seq(-100, 100, by=5)) +
      scale_y_continuous(breaks = seq(-100, 100, by=5)) +
      coord_fixed(ratio = 1, xlim = ranges2$x, ylim = ranges2$y, expand = TRUE, clip = "on")
    #guides(color = guide_legend(override.aes = list(shape = c(15))))
    #coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    ggplotly(p) %>%
      # add_annotations(x = subset(p$data, !is.na(NIH_labels))[[x]],
      #                 y = subset(p$data, !is.na(NIH_labels))[[y]],
      #                 text = subset(p$data, !is.na(NIH_labels))$NIH_labels,
      #                 showarrow = TRUE,
      #                 arrowcolor='red',
      #                 arrowhead = 6,
      #                 arrowsize = 1,
      #                 xref = "x",
      #                 yref = "y",
      #                 font = list(color = 'black',
      #                             family = 'arial',
    #                             size = 14)) %>%
    config(scrollZoom = TRUE) %>%
      layout(title = paste0("Hematopoietic (n=",nrow(p$data),")"),
             #height=1000, 
             dragmode = "pan", xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
             margin = m,
             showlegend = T,
             legend = list(orientation = "v",
                           yanchor = "top",
                           itemclick = "toggleothers",
                           tracegroupgap = 2,
                           itemwidth = 75,
                           itemsizing = "constant")) %>%
      toWebGL()
    
  })
  
  #RENAL
  output$plot_RENAL <- renderPlotly({
    # use the key aesthetic/argument to help uniquely identify selected observations
    key <- rownames(anno_renal)
    #sample_filter <- filter_select("filter", "Find sample", tx, ~Sample)
    x <- paste0(input$x,"_x")
    y <- paste0(input$x,"_y")
    #cnames <- aggregate(cbind(umap_x, umap_y) ~ Combined_class_match_dkfz, data=anno_renal,
    #                    FUN=function(x)mean((x)))
    p <- ggplot(data=anno_renal, aes(x=anno_renal[[x]],y=anno_renal[[y]],key=key)) +
      geom_point(aes(color=NCI_METRIC),  size=2, alpha=1) +
      #geom_text(data=cnames, aes(x=umap_x, y=umap_y, label = Combined_class_match_dkfz), position = position_dodge(width=0.5),  size=2.5, inherit.aes = FALSE) +
      theme_classic() +
      theme(axis.text.y = element_text(size=9, color="black"),
            axis.text.x = element_text(size=9, color="black"),
            axis.ticks.y = element_line(color="black", size = 0.5),
            axis.ticks.x = element_line(color="black", size = 0.5),
            axis.ticks.length = unit(2,"mm"),
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major = element_line(colour="grey", size=0.5),
            axis.line = element_blank(),
            legend.text=element_text(size=10),
            legend.title=element_text(size=11),
            legend.key.size = unit(0.5, 'lines'),
            axis.title.x = element_text(size=14, color="black"),
            axis.title.y = element_text(size=14, color="black")) +
      labs(x = paste0(input$x,"_1"), y = paste0(input$x,"_2")) +
      #theme(legend.position="right") +
      scale_color_manual("Methylation class", values = mycols, guide = guide_legend(override.aes = list(shape = 15))) +
      scale_x_continuous(breaks = seq(-100, 100, by=5)) +
      scale_y_continuous(breaks = seq(-100, 100, by=5)) +
      coord_fixed(ratio = 1, xlim = ranges2$x, ylim = ranges2$y, expand = TRUE, clip = "on")
    #guides(color = guide_legend(override.aes = list(shape = c(15))))
    #coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
    ggplotly(p) %>%
      # add_annotations(x = subset(p$data, !is.na(NIH_labels))[[x]],
      #                 y = subset(p$data, !is.na(NIH_labels))[[y]],
      #                 text = subset(p$data, !is.na(NIH_labels))$NIH_labels,
      #                 showarrow = TRUE,
      #                 arrowcolor='red',
      #                 arrowhead = 6,
      #                 arrowsize = 1,
      #                 xref = "x",
      #                 yref = "y",
      #                 font = list(color = 'black',
      #                             family = 'arial',
    #                             size = 14)) %>%
    config(scrollZoom = TRUE) %>%
      layout(title = paste0("Renal (n=",nrow(p$data),")"),
             #height=800, 
             dragmode = "pan", xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE),
             margin = m,
             showlegend = T,
             legend = list(orientation = "v",
                           yanchor = "top",
                           itemclick = "toggleothers",
                           tracegroupgap = 2,
                           itemwidth = 75,
                           itemsizing = "constant")) %>%
      toWebGL()
    
  })
  
  output$brush_table <- DT::renderDataTable(server = FALSE, {
    d <- event_data("plotly_selected")
    req(d)
    DT::datatable(anno_base[unlist(d$key), 
                            c("Sample", "Age", "Sex_prediction", "Histology", "Subtype/pattern", "WHO_2007_grade", 
                              "Molecular", "Location_general", "Location_specific", "Sampling", "Sampling_treatment",
                              "idat_filename")],
                  extensions = 'Buttons',
                  class = "display",
                  options = list(lengthMenu = c(5, 30, 50), pageLength = 50, scrollY = '1000px',
                                 paging = TRUE,
                                 searching = TRUE,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 ordering = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel'))) %>%
      DT::formatStyle(columns = c(1, 2, 3, 4, 5, 6, 7, 8), fontSize = '120%')
  })
  
  
  output$brush_supervised <- DT::renderDataTable(server = FALSE, {
    d <- event_data("plotly_selected", source = "B")
    req(d)
    DT::datatable(anno_base[unlist(d$key),
                            c("Sample", "Age", "Sex_prediction", "Histology", "Subtype/pattern", "WHO_2007_grade", 
                              "Molecular", "Location_general", "Location_specific", "Sampling", "Sampling_treatment",
                              "idat_filename")],
                  extensions = 'Buttons',
                  class = "display",
                  options = list(lengthMenu = c(5, 30, 50), pageLength = 10, scrollY = '500px',
                                 paging = TRUE,
                                 searching = TRUE,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 ordering = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel'))) %>%
      DT::formatStyle(columns = c(1, 2, 3, 4, 5, 6, 7, 8), fontSize = '100%')
    
  })
  
  output$brush_hc_cluster <- renderText( {
    d <- event_data("plotly_click", source = "C")
    d
  })
  
  # output$brush_hc_cluster <- DT::renderDataTable(server = FALSE, {
  #   d <- event_data("plotly_click", source = "C")
  #   req(d)
  #   DT::datatable(anno_base[unlist(d$key),
  #                           c("Sample", "idat_filename", "Age", "Sex", "Sex_prediction", "Location_general",
  #                             "Sampling", "Sampling_treatment", "Neoplastic",
  #                             "Location_specific", "Primary_study", "Molecular", "Histology", "Likely_integrated_diagnosis")],
  #                 extensions = 'Buttons',
  #                 class = "display",
  #                 options = list(lengthMenu = c(5, 30, 50), pageLength = 10, scrollY = '500px',
  #                                paging = TRUE,
  #                                searching = TRUE,
  #                                fixedColumns = TRUE,
  #                                autoWidth = TRUE,
  #                                ordering = TRUE,
  #                                dom = 'Bfrtip',
  #                                buttons = c('copy', 'csv', 'excel'))) %>%
  #     DT::formatStyle(columns = c(1, 2, 3, 4, 5, 6, 7, 8), fontSize = '100%')
  # })
  
  
  output$plot_CNA <- renderPlotly({
    key <- rownames(anno_base)
    d <- event_data("plotly_click")
    req(d)
    case <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    CNA_plot(case)
    # add_annotations(x = subset(p$data, !is.na(NIH_labels))[[x]],
    #                 y = subset(p$data, !is.na(NIH_labels))[[y]],
    #                 text = subset(p$data, !is.na(NIH_labels))$NIH_labels,
    #                 showarrow = TRUE,
    #                 arrowcolor='red',
    #                 arrowhead = 6,
    #                 arrowsize = 1,
    #                 xref = "x",
    #                 yref = "y",
    #                 font = list(color = 'black',
    #                             family = 'arial',
    #                             size = 14))
  })
  
  
  output$brush_CNA <- DT::renderDataTable({
    key <- rownames(anno_base)
    d <- event_data("plotly_click")
    req(d)
    case <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    sample_deleterious <- CNA_deleterious[CNA_deleterious$sample %in% case,]
    sample_deleterious$alteration <- paste0("<b>", sample_deleterious$alteration, "</b>")
    DT::datatable(sample_deleterious[,
                                     c("chromosome", "mean log2ratio", "alteration", "CGI", "CIViC",
                                       "MMD", "MMatch", "OncoKB","PMKB","TFGD")],
                  #extensions = 'Buttons',
                  class = "display",
                  rownames = FALSE,
                  escape = FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; font-size: 17px; text-align: left;',
                    'CGI: ', htmltools::em('Cancer Genome Interpreter Cancer Biomarkers Database; '),
                    'CIViC: ', htmltools::em('Clinical Interpretation of Variants in Cancer; '),
                    'MMD: ', htmltools::em('Mitelman Database of Chromosome Aberrations and Gene Fusions in Cancer; '),
                    'MMatch: ', htmltools::em('MolecularMatch; '),
                    'PMKB: ', htmltools::em('Precision Medicine Knowledgebase; '),
                    'TFGD: ', htmltools::em('TCGA Fusion Gene Database')),
                  options = list(lengthMenu = c(5, 30, 50), pageLength = 10, scrollY = '200px',
                                 paging = TRUE,
                                 searching = TRUE,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 ordering = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel'))
    ) %>%
      DT::formatStyle(columns = c(1, 2, 3, 4, 5, 6, 7, 8), fontSize = '120%', color = "black")
    
  })
  
  output$brush_CNA_complete <- DT::renderDataTable({
    key <- rownames(anno_base)
    d <- event_data("plotly_click")
    req(d)
    case <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    sample <- CNA_all[CNA_all$sample %in% case,]
    #sample$alteration <- paste0("<b>", sample$alteration, "</b>")
    DT::datatable(sample[,
                         c("chromosome", "mean log2ratio", "alteration", "CGI", "CIViC",
                           "MMD", "MMatch", "OncoKB","PMKB","TFGD")],
                  #extensions = 'Buttons',
                  class = "display",
                  rownames = FALSE,
                  escape = FALSE,
                  caption = "Complete list of sample CNA (homozygous deletion, amplification)",
                  options = list(lengthMenu = c(5, 30, 50), pageLength = 10, scrollY = '200px',
                                 paging = TRUE,
                                 searching = TRUE,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 ordering = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel'))
    ) %>%
      DT::formatStyle(columns = c(1, 2, 3, 4, 5, 6, 7, 8), fontSize = '120%', color = "black")
    
  })
  
  output$brush_breakpoint <- DT::renderDataTable({
    key <- rownames(anno_base)
    d <- event_data("plotly_click")
    req(d)
    case <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    sample <- CNA_breakpoint_fusions[CNA_breakpoint_fusions$sample %in% case,]
    #sample$alteration <- paste0("<b>", sample$alteration, "</b>")
    DT::datatable(sample[,
                         c("matches")],
                  #extensions = 'Buttons',
                  class = "display",
                  rownames = FALSE,
                  escape = FALSE,
                  caption = "List of possible unbalanced translocations/fusion genes",
                  options = list(lengthMenu = c(5, 30, 50), pageLength = 10, scrollY = '200px',
                                 paging = TRUE,
                                 searching = TRUE,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 ordering = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel'))
    ) %>%
      DT::formatStyle(columns = c(1, 2, 3, 4, 5, 6, 7, 8), fontSize = '120%', color = "black")
    
  })
  
  output$brush_breakpoint_genes <- DT::renderDataTable({
    key <- rownames(anno_base)
    d <- event_data("plotly_click")
    req(d)
    case <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    sample <- CNA_breakpoint_genes[CNA_breakpoint_genes$sample %in% case,]
    #sample$alteration <- paste0("<b>", sample$alteration, "</b>")
    DT::datatable(sample[,
                         c("gene")],
                  #extensions = 'Buttons',
                  class = "display",
                  rownames = FALSE,
                  escape = FALSE,
                  caption = "List of possible unbalanced translocations/fusion genes",
                  options = list(lengthMenu = c(5, 30, 50), pageLength = 10, scrollY = '200px',
                                 paging = TRUE,
                                 searching = TRUE,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 ordering = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel'))
    ) %>%
      DT::formatStyle(columns = c(1, 2, 3, 4, 5, 6, 7, 8), fontSize = '120%', color = "black")
    
  })
  
  output$plot_qc_sex <- renderPlot({
    key <- rownames(anno_base)
    d <- event_data("plotly_click")
    req(d)
    case <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    dat <- fread("D:/NIH_classifier/meffil_files/sex_prediction.txt")
    ggplot(dat, aes(y=1, x=xy.diff)) +
      geom_jitter(aes(shape=predicted.sex, colour=sex.mismatch), size=1.5) +
      geom_point(data = dat[dat$sample.name==case,], color="green", size = 4) +
      scale_colour_manual(values=c("grey", "red", "blue")) + ## alphabetic order: FALSE=black, TRUE=red, Unspecified sex=blue
      labs(shape="Predicted sex", x="XY diff", y="", colour="Incorrect\nprediction") +
      theme_bw() +
      theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.line.y=element_blank(),
            legend.text=element_text(size=12), legend.title=element_text(size=12),
            axis.title.x = element_text(size=14, color="black")) +
      geom_vline(xintercept=unique(c(dat$upper, dat$lower)), linetype="dashed", colour="purple")
    
    
  })
  
  output$plot_qc_methunmeth <- renderPlot({
    key <- rownames(anno_base)
    d <- event_data("plotly_click")
    req(d)
    case <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    dat <-  fread("D:/NIH_classifier/meffil_files/meth_umeth.txt")
    p1 <- ggplot(dat, aes(y=methylated, x=unmethylated)) +
      geom_point(colour="grey")
    p1 <- p1 + geom_point(data=subset(dat, outliers), color="red", size=2) +
      geom_point(data=dat[dat$sample.name==case,], color="green", size=4) +
      theme(legend.position="none", axis.title.x = element_text(size=14, color="black"),
            axis.title.y = element_text(size=14, color="black"))
    p1 +
      labs(y = "Median methylated signal", x = "Median unmethylated signal") + 
      geom_smooth() +
      geom_line(aes(y=methylated.lm), col="red") +
      geom_line(aes(y=upper.lm), col="red", linetype="dashed") +
      geom_line(aes(y=lower.lm), col="red", linetype="dashed")
  })
  
  output$plot_MGMT <- renderImage({
    key <- rownames(anno_base)
    d <- event_data("plotly_click")
    req(d)
    case <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    files <- as.data.frame(list.files(path = "D:/NIH_classifier/mgmt_plots/", recursive = TRUE, full.names = TRUE))
    names(files) <- "filename"
    files$file <- list.files(path = "D:/NIH_classifier/mgmt_plots/", recursive = TRUE, full.names = FALSE)
    files$file <- gsub(".jpg", "", files$file)
    file <- files$filename[files$file %in% case]
    list(src = file, height = 400, width = 400)
  })
  
  output$MGMT_result <- renderText({
    key <- rownames(anno_base)
    d <- event_data("plotly_click")
    req(d)
    case <- anno_base[rownames(anno_base) %in% d$key,]
    case.res <- ifelse(grepl("M", case$MGMT),
                       "methylated", "unmethylated")
    paste("Result: ", case.res)})
  
  output$plot_MLH1 <- renderImage({
    key <- rownames(anno_base)
    d <- event_data("plotly_click")
    req(d)
    case <- anno_base$idat_filename[rownames(anno_base) %in% d$key]
    files <- as.data.frame(list.files(path = "D:/NIH_classifier/mlh1_plots/", recursive = TRUE, full.names = TRUE))
    names(files) <- "filename"
    files$file <- list.files(path = "D:/NIH_classifier/mlh1_plots/", recursive = TRUE, full.names = FALSE)
    files$file <- gsub(".png", "", files$file)
    file <- files$filename[files$file %in% case]
    list(src = file, height = 400, width = 400)
  })
  
  
  
  
  ## draw survival curves if a point has been selected
  ## if none have been selected then draw a blank plot with matching background color
  output$survival_OS <- renderPlot({
    if (any(c(1,2,3) %in% rv$data_df$group)) {
      fit <- survfit(Surv(OS_months, OS_status) ~ group,
                     data = rv$data_df)
      ggsurvplot(fit, data = rv$data_df,
                 censor.size=1, size = 1,
                 conf.int = FALSE,
                 conf.int.style = "ribbon",
                 risk.table = TRUE,
                 tables.height = 0.25,
                 pval = TRUE,
                 pval.size = 10,
                 #palette = "jco",
                 legend = "none",
                 legend.title = "",
                 xlab = "Overall survival (months)",
                 palette = c("red", "blue", "green"),
                 ggtheme = theme_bw(base_size = 20,
                                    base_family = "Arial"))
    } else {
      par(bg = "#ecf0f5")
      plot.new()
    }
  }, height = 500)
  
  output$survival_PFS <- renderPlot({
    if (any(c(1,2,3) %in% rv$data_df$group)) {
      fit <- survfit(Surv(PFS_months, PFS_status) ~ group,
                     data = rv$data_df)
      ggsurvplot(fit, data = rv$data_df,
                 censor.size=1, size = 1,
                 conf.int = FALSE,
                 conf.int.style = "ribbon",
                 risk.table = TRUE,
                 tables.height = 0.25,
                 pval = TRUE,
                 pval.size = 10,
                 #palette = "jco",
                 legend = "none",
                 legend.title = "",
                 xlab = "Progression-/event-free survival (months)",
                 palette = c("red", "blue", "green"),
                 ggtheme = theme_bw(base_size = 20,
                                    base_family = "Arial"))
    } else {
      par(bg = "#ecf0f5")
      plot.new()
    }
  }, height = 500)
  
}
)

shinyApp(ui = ui, server = server)
