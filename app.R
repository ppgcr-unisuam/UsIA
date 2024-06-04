# create dir
dir.name <- 'www'
if (!dir.exists(dir.name)){
  dir.create(dir.name, recursive = TRUE, showWarnings = FALSE)
}
shiny::addResourcePath(prefix = "www", directoryPath = "www")

# copy favicon folder to the www dir
R.utils::copyDirectory(from = "favicon_io", to = file.path(dir.name, "favicon_io"))

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
#   BiocManager::install("EBImage")
# remotes::install_github("r-spatial/sf")

# load libraries
library(dplyr)
library(htmltools)
library(BiocManager)
library(EBImage)

# source all scripts
source("f_border.R", local = TRUE)
source("f_meas.R", local = TRUE)
source("f_process_color.R", local = TRUE)
source("f_process_detrend.R", local = TRUE)
source("f_process_equalization.R", local = TRUE)
source("f_process_filter.R", local = TRUE)
source("f_process_morphological.R", local = TRUE)
source("f_process_threshold.R", local = TRUE)
source("f_process_track.R", local = TRUE)
source("f_track_all.R", local = TRUE)
source("f_track_first.R", local = TRUE)
source("us_track.R", local = TRUE)

# use this code to debug
# rsconnect::showLogs()

ui <- shiny::fluidPage(
  # add favicon
  shiny::tags$head(
    shiny::tags$link(rel = "shortcut icon", href = "www/favicon_io/favicon.ico"),
    shiny::tags$link(rel = "shortcut icon", href = "www/favicon_io/android-chrome-192x192.png"),
    shiny::tags$link(rel = "shortcut icon", href = "www/favicon_io/android-chrome-512x512.png"),
    shiny::tags$link(rel = "apple-touch-icon", href = "www/favicon_io/apple-touch-icon.png"),
    shiny::tags$link(rel = "icon", href = "www/favicon_io/favicon-16x16.png"),
    shiny::tags$link(rel = "icon", href = "www/favicon_io/favicon-32x32.png"),
    shiny::tags$link(rel = "shortcut icon", href = "www/favicon_io/favicon.ico"),
  ),

  # add font awesome
  tags$head(
    tags$link(rel = "stylesheet", href = "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css")
  ),
  
  # use shinythemes
  theme = shinythemes::shinytheme("flatly"),
  
  # use shinyjs
  shinyjs::useShinyjs(),
  
  # status bar
  shinybusy::add_busy_bar(color = "#FF0000"),
  
  # Application title
  shiny::titlePanel(
    shiny::fluidRow(
      shiny::column(
        8,
        list(
          fontawesome::fa("wifi"),
          shiny::HTML("<strong>UsIA</strong>"),
          shiny::br(),
          "Ultrasound Image Analysis"
        ),
        style = "text-align:left;"
      ),
      shiny::column(
        4,
        tags$a(
          id = "restart",
          class = "btn btn-primary",
          href = "javascript:history.go(0)",
          shiny::HTML('<i class="fa fa-refresh fa-1x"></i>'),
          title = "restart",
          style = "color:white; border-color:white; border-radius:100%"
        ),
        style = "text-align:right;"
      )
    ),
    windowTitle = "UsIA | Ultrasound Image Analysis"
  ),
  
  shiny::HTML(
    "<a href=\"https://doi.org/10.5281/zenodo.10439718\" style=\"vertical-align:middle;\"><img src=\"https://zenodo.org/badge/DOI/10.5281/zenodo.10439718.svg\" alt=\"DOI\"  style=\"vertical-align:top;\"></a>"
  ),
  shiny::br(),
  shiny::br(),
  
  # change color of fileInput button
  tags$head(tags$style(
    shiny::HTML(".btn-file {background-color: #2C3E50;}")
  )),
  
    # Main panel for displaying outputs ----
      shiny::tabsetPanel(
        id = "tabs",
        type = "tabs",
        shiny::tabPanel(
          title = list(fontawesome::fa("right-to-bracket"), "Input"),
          shiny::br(),
          shiny::fileInput(
            inputId = "InputFile",
            label = NULL,
            multiple = FALSE,
            buttonLabel = list(fontawesome::fa("file-video"), "Upload"),
            accept = c(".mp4"),
            width = "100%",
          ),
          shiny::tabsetPanel(
            id = "tabInput",
            type = "tabs",
            shiny::tabPanel(
              title = list(fontawesome::fa("video"), "Video"),
              shiny::br(),
              shiny::uiOutput(outputId = "videoraw"),
              align = "center"
            ),
            shiny::tabPanel(
              title = list(fontawesome::fa("edit"), "Edit"),
              shiny::br(),
              shiny::sliderInput(
                inputId = "framesEdit",
                label = NULL,
                min = 1,
                max = 100,
                value = c(1, 100),
                step = 1,
                ticks = FALSE,
                animate = TRUE,
                width = "100%"
              ),
              shiny::uiOutput(outputId = "videoedit"),
              align = "center"
            ),
          ),
        ),
        shiny::tabPanel(
          title = list(fontawesome::fa("ruler"), "Measure"),
          shiny::br(),
          shiny::tabsetPanel(
            id = "tabMeasure",
            type = "tabs",
            shiny::tabPanel(
              title = list(fontawesome::fa("image"), "Frame"),
              shiny::br(),
              # split two columns
              shiny::fluidRow(
                shiny::column(
                  4,
                  shiny::sliderInput(
                    inputId = "imgEdit",
                    label = "Frame",
                    min = 1,
                    max = 100,
                    value = 1,
                    step = 1,
                    ticks = FALSE,
                    animate = TRUE,
                    width = "100%"
                  ),
                  shiny::numericInput(
                    inputId = "imgScale",
                    label = "Scale (mm)",
                    value = 1,
                    min = 0.1,
                    max = 10,
                    step = 0.1,
                    width = "100%"
                  ),
                  shiny::br(),
                  shiny::actionButton(
                    inputId = "reset",
                    label = "Reset ROI",
                    width = "100%"),
                  shiny::br(),
                  htmltools::h5(htmltools::strong("Click"), " on plot to start. ", htmltools::strong("Move"), " the cursor to draw the ROI. ", htmltools::strong("Click"), " again to stop."),
                  shiny::br(),
                  align = "center"
                ),
                shiny::column(
                  8,
                  shiny::plotOutput(
                    outputId = "plotMeasure",
                    width = "auto",
                    click = "img_click",
                    hover = shiny::hoverOpts(
                      id = "hover",
                      delay = 100,
                      delayType = "throttle",
                      clip = TRUE,
                      nullOutside = TRUE
                    )
                  ),
                ),
              ),
              align = "center"
            ),
            shiny::tabPanel(
              title = list(fontawesome::fa("table"), "Tables"),
              shiny::br(),
              DT::dataTableOutput("tableMeasure", width = "100%"),
              align = "center"
            ),
          ),
        ),
        shiny::tabPanel(
          title = list(fontawesome::fa("arrow-trend-up"), "Track"),
          shiny::br(),
          shiny::tabsetPanel(
            id = "tabTrack",
            type = "tabs",
            shiny::tabPanel(
              title = list(fontawesome::fa("sliders"), "Setup"),
              shiny::br(),
              # split two columns
              shiny::fluidRow(
                shiny::column(
                  4,
                  shiny::sliderInput(
                    inputId = "KernelSizeTrack",
                    label = "Object (px)",
                    min = 1,
                    max = 201,
                    value = 50,
                    step = 2,
                    ticks = FALSE,
                    width = "100%",
                  ),
                  shiny::sliderInput(
                    inputId = "Overlap",
                    label = "ROI (%)",
                    min = 0,
                    max = 100,
                    value = 50,
                    step = 5,
                    ticks = FALSE,
                    width = "100%",
                  ),
                  shiny::radioButtons(
                    inputId = "FilterType",
                    label = "Filter",
                    choices = c("none", "mean", "median"),
                    selected = "none",
                    inline = TRUE,
                    width = "100%"
                  ),
                  shiny::sliderInput(
                    inputId = "FilterSize",
                    label = "Size (px)",
                    min = 1,
                    max = 11,
                    value = 1,
                    step = 2,
                    ticks = FALSE,
                    width = "100%",
                  ),
                  shiny::sliderInput(
                    inputId = "Jump",
                    label = "Jump (frames)",
                    min = 0,
                    max = 5,
                    value = 0,
                    ticks = FALSE,
                    width = "100%",
                  ),
                  shiny::br(),
                  shiny::actionButton(
                    inputId = "buttAnalyze",
                    label = "Analyze",
                    class = "btn-primary",
                    style = "width:100%; border-color:white; border-radius: 10px",
                    icon("play")
                  ),
                  shiny::br(),
                  align = "center"
                ),
                shiny::column(
                  8,
                  shiny::tabPanel(
                    title = list(fontawesome::fa("crop"), "ROI"),
                    shiny::br(),
                    shiny::plotOutput(
                      outputId = "plotROI",
                      width = "auto",
                      click = "roi_click"
                    ),
                    align = "center"
                  ),
                ),
              ),
            ),
            shiny::tabPanel(
              value = "track",
              title = list(fontawesome::fa("right-from-bracket"), "Output"),
              shiny::br(),
              shiny::downloadButton(
                outputId = "downloadMP4",
                label = "Video",
                class = "btn-primary",
                style = "width:100%; border-color:white; border-radius: 10px;",
              ),
              shiny::br(),
              shiny::br(),
              shiny::uiOutput(outputId = "videooutput"),
              align = "center"
            ),
            shiny::tabPanel(
              title = list(fontawesome::fa("chart-line"), "Plots"),
              shiny::br(),
              shiny::fluidPage(
                shiny::fluidRow(
                  shiny::column(
                    4,
                    shiny::downloadButton(
                      outputId = "downloadPATH",
                      label = "Trajectory data",
                      class = "btn-primary",
                      style = "width:100%; border-color:white; border-radius: 10px;",
                    ),
                  ),
                  shiny::column(
                    4,
                    shiny::downloadButton(
                      outputId = "downloadDISPL",
                      label = "Displacement data",
                      class = "btn-primary",
                      style = "width:100%; border-color:white; border-radius: 10px;",
                    ),
                  ),
                  shiny::column(
                    4,
                    shiny::downloadButton(
                      outputId = "downloadCC",
                      label = "Cross-correlation data",
                      class = "btn-primary",
                      style = "width:100%; border-color:white; border-radius: 10px;",
                    ),
                    align = "center"
                  ),
                ),
              ),
              shiny::br(),
              shiny::br(),
              shiny::plotOutput("plotTrack",  width = "100%"),
              align = "center"
            ),
            shiny::tabPanel(
              title = list(fontawesome::fa("table"), "Tables"),
              shiny::br(),
              DT::dataTableOutput("tableTrack", width = "100%"),
              align = "center"
            ),
          ),
        ),
        shiny::tabPanel(
          title = list(fontawesome::fa("circle-info")),
          shiny::br(),
          shiny::HTML(
            "<p>1. Upload a video file (.mp4) with the <b>Upload video</b>.</p>
            <p>2. If necessary, edit the video by subsetting the uploaded video using <b>Edit</b>.</p>
            <p>3. Click <b>Track</b> to configure the analysis.</p>
            <p>4. Set up the parameters for analysis:</p>
            <p>   - <i>Object size</i>: '1' to '201' (odd numbers only).</p>
            <p>   - <i>Overlap</i>: '0' to '100%'.</p>
            <p>   - <i>Filter type</i>: 'none', 'gaussian', 'median'.</p>
            <p>   - <i>Filter size</i>: '1' to '11' (odd numbers only).</p>
            <p>   - <i>Jump</i>: '1' to '5' frames.</p>
            <p>5. Set the region of interest (<b>ROI</b>) for analysis. Click on the screen to mark the center of the ROI.</p>
            <p>6. Click <b>Analyze</b>. Wait until the red progress bar on the top stops blinking.</p>
            <p>7. Check <b>Output</b> tab to visualize the processed video.</p>
            <p>8. Check <b>Plots</b> and <b>Table</b> tabs for the results.</p>
            <p>9. Click <b>restart</b> icon before running new analisys.",
          ),
        ),
        shiny::tabPanel(
          title = list(fontawesome::fa("people-group")),
          shiny::br(),
          shiny::HTML("<a href=\"mailto:arthurde@souunisuam.com.br\">Arthur Ferreira, DSc</a>"),
          shiny::HTML("<b> (Developer)</b>"),
          shiny::br(),
          shiny::HTML(
            "<a href=\"mailto:gustavo.telles@souunisuam.com.br\">Gustavo Telles, MSc</a>; <a href=\"mailto:jessica.rio@souunisuam.com.br\">Jessica Rio, MSc</a>; <a href=\"mailto:alicepagnez@souunisuam.com.br\"> Maria Alice Pagnez, DSc</a>; <a href=\"mailto:leandronogueira@souunisuam.com.br\">Leandro Nogueira, DSc</a>"
          ),
          shiny::HTML("<b> (Contributors)</b>"),
          shiny::br(),
          shiny::HTML(
            "<a href=\"https://www.unisuam.edu.br/programa-pos-graduacao-ciencias-da-reabilitacao\">PPGCR</a> | Programa de Pós-graduação em Ciências da Reabilitação, Centro Universitário Augusto Motta, Rio de Janeiro, RJ, Brazil"
          ),
          shiny::HTML("<b> (Affiliation)</b>"),
          shiny::br(),
          shiny::br(),
          shiny::HTML("<b>License</b>"),
          shiny::HTML(
            "This work is licensed under an <a rel=\"license\" data-spdx=\"Apache-2.0\" href=\"https://www.apache.org/licenses/LICENSE-2.0\">Apache License 2.0</a>."
          ),
          shiny::br(),
          shiny::HTML("<b>Cite as</b>"),
          shiny::HTML(
            "Ferreira, A.S. (2023). UsIA | Ultrasound Image Analysis (1.0.0). Zenodo. https://doi.org/10.5281/zenodo.10439718"
          ),
        ),
      ),
)

# Define server script
server <- function(input, output, session) {
  
  # store plot click coords
  roi_coords <-
    shiny::reactiveValues(xy = data.frame(x = c(1, 1),  y = c(1, 1)))
  
  # observe plot click event ---------------------------------------------------------
  shiny::observeEvent(input$roi_click, {
    roi_coords$xy[2, ] <- round(c(input$roi_click$x, input$roi_click$y), digits = 0)
  })
  
  # free-hand drawing
  vals = shiny::reactiveValues(x = NULL, y = NULL)
  draw = shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$img_click, handlerExpr = {
    temp <- draw()
    draw(!temp)
    if (!draw()) {
      vals$x <- c(vals$x, NA)
      vals$y <- c(vals$y, NA)
    }
  })
  shiny::observeEvent(input$hover, {
    if (draw()) {
      vals$x <- c(vals$x, input$hover$x)
      vals$y <- c(vals$y, input$hover$y)
      # Convert a linestring into a closed polygon when the points are not in order
    }
  })
  shiny::observeEvent(input$reset, handlerExpr = {
    vals$x <- NULL
    vals$y <- NULL
  })
  
  # change to tab under event ---------------------------------------------------------
  shiny::observeEvent(input[["buttAnalyze"]], {
    shiny::updateTabsetPanel(inputId = "tabTrack",
                             selected = "track")
  })
  
  # enable/disable filter
  shiny::observeEvent(input$FilterType, {
    shinyjs::toggleState(id = "FilterSize", condition = input$FilterType != "none")
  })
  
  # upload video ---------------------------------------------------------
  values <- shiny::reactiveValues(
    upload_state = NULL
  )
  
  shiny::observeEvent(input$InputFile, {
    values$upload_state <- 'uploaded'
  })
  
  shiny::observeEvent(input$restart, {
    values$upload_state <- 'restart'
  })
  
  Video <- shiny::reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input[["InputFile"]][["datapath"]])
    } else if (values$upload_state == 'restart') {
      return(NULL)
    }
  })
  
  # play uploaded video ---------------------------------------------------------
  output[["videoraw"]] <- shiny::renderUI({
    shiny::req(Video())
    # Get video info such as width, height, format, duration and framerate
    info <- av::av_media_info(Video())
    
    # center ROI for the first time
    roi_coords$xy[2, ] <- c(info$video$width / 2, info$video$height / 2)
    
    # copy and rename file
    file.copy(from = Video(),
              to = file.path(dir.name, "rawvideo.mp4"))
    # Splits a video file in a set of image files. Use format = "png" for losless images
    av::av_video_images(
      video = file.path(dir.name, "rawvideo.mp4"),
      destdir = file.path(dir.name, "0 raw"),
      format = "png",
      fps = NULL
    )
    
    # copy raw files to edit folder
    R.utils::copyDirectory(file.path(dir.name, "0 raw"), file.path(dir.name, "1 edited"))
    
    # copy and rename file
    file.copy(from = Video(),
              to = file.path(dir.name, "editedvideo.mp4"))
    
    # update max value of slider under event
    shiny::updateSliderInput(
      inputId = "framesEdit",
      min = 1,
      value = c(1, length(
        list.files(file.path(dir.name, "0 raw"), pattern = ".png")
      )),
      max = length(list.files(
        file.path(dir.name, "0 raw"), pattern = ".png"
      ))
    )
    
    # update max value of slider under event
    shiny::updateSliderInput(
      inputId = "imgEdit",
      min = 1,
      value = 1,
      max = length(list.files(
        file.path(dir.name, "0 raw"), pattern = ".png"
      ))
    )
    
    # show input video
    tags$video(
      width = "90%",
      height = "90%",
      controls = "",
      tags$source(src = "rawvideo.mp4", type = "video/mp4")
    )
  })
  
  # edit mp4 video using the sliderEdit input ---------------------------------------------------------
  output[["videoedit"]] <- shiny::renderUI({
    shiny::req(Video())
    shiny::req(input$framesEdit)
    # Get video info such as width, height, format, duration and framerate
    info <- av::av_media_info(Video())
    
    # generate mp4 video using the sliderEdit input
    av::av_encode_video(
      input = list.files(
        file.path(dir.name, "0 raw"),
        pattern = ".png",
        full.names = TRUE
      )[input$framesEdit[1]:input$framesEdit[2]],
      output = file.path(dir.name, "editedvideo.mp4"),
      framerate = info$video$framerate
    )
    
    # delete previous edit files
    unlink(
      list.files(
        path = file.path(dir.name, "1 edited"),
        recursive = TRUE,
        include.dirs = TRUE,
        full.names = TRUE,
        pattern = "png"
      )
    )
    
    # Splits a video file in a set of image files. Use format = "png" for losless images
    av::av_video_images(
      video = file.path(dir.name, "editedvideo.mp4"),
      destdir = file.path(dir.name, "1 edited"),
      format = "png",
      fps = NULL
    )
    
    # update max value of slider under event
    shiny::updateSliderInput(
      inputId = "imgEdit",
      min = 1,
      value = 1,
      max = length(list.files(
        file.path(dir.name, "1 edited"), pattern = ".png"
      ))
    )
    
    # show video
    tags$video(
      width = "90%",
      height = "90%",
      controls = "",
      tags$source(src = "editedvideo.mp4", type = "video/mp4")
    )
  })
  
  # plot single frame of video imgEdit ---------------------------------------------------------
  output[["plotMeasure"]] <- shiny::renderPlot({
    shiny::req(Video())
    shiny::req(input$framesEdit)
    shiny::req(input$imgEdit)
    # Get video info such as width, height, format, duration and framerate
    info <- av::av_media_info(Video())
    
    # show 1st frame
    img <-
      magick::image_read(
        list.files(
          path = file.path(dir.name, "1 edited"),
          full.names = TRUE,
          pattern = "png")[input$imgEdit]
      )

    # color palette (grayscale)
    pal <- grDevices::gray(seq(
      from = 0,
      to = 1,
      length.out = 256
    ), alpha = NULL)
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    plot(
      img,
      xlim = c(0, info$video$width),
      ylim = c(0, info$video$height),
      asp = 1,
      col = pal
    )
    
    # draw free-hand object
    lines(
      x = vals$x,
      y = vals$y,
      col = "red",
      lty = "solid",
      lwd = 2
    )
    
  }, height = function() {
    (session$clientData$output_plotMeasure_width) * (0.6584)
  })
  
  # measurements of single frame of video imgEdit ---------------------------------------------------------
  output[["tableMeasure"]] <- DT::renderDataTable({
    shiny::req(Video())
    shiny::req(input$framesEdit)
    shiny::req(input$imgEdit)
    shiny::req(input$imgScale)
    
    # Get video info such as width, height, format, duration and framerate
    info <- av::av_media_info(Video())
    
    # show 1st frame
    img <-
      magick::image_read(
        list.files(
          path = file.path(dir.name, "1 edited"),
          full.names = TRUE,
          pattern = "png")[input$imgEdit]
      )
    
    # get separate channels
    img_object <- as.integer(img[[1]])
    img_object_R <- img_object[, , 1]
    img_object_G <- img_object[, , 2]
    img_object_B <- img_object[, , 3]
    
    # flip image vertically
    img_object_R <- img_object_R[nrow(img_object_R):1, ]
    img_object_G <- img_object_G[nrow(img_object_G):1, ]
    img_object_B <- img_object_B[nrow(img_object_B):1, ]
    
    # create closed polygon
    poly <- round(concaveman::concaveman(cbind(vals$x, vals$y), concavity = 1, length_threshold = 0), 0)
    poly <- poly[complete.cases(poly), ]
    
    # subset the image using polygon coordinates
    img_object_R <- img_object_R[
      min(poly[, 1]):max(poly[, 1]),
      min(poly[, 2]):max(poly[, 2])
    ]
    img_object_G <- img_object_G[
      min(poly[, 1]):max(poly[, 1]),
      min(poly[, 2]):max(poly[, 2])
    ]
    img_object_B <- img_object_B[
      min(poly[, 1]):max(poly[, 1]),
      min(poly[, 2]):max(poly[, 2])
    ]

    # custom functions
    source("f_meas.R", local = TRUE)
    data <- f_measurement(R = img_object_R, G = img_object_G, B = img_object_B)
    
    # distance
    distancia <- 0
    
    # cross-sectional area
    area <- distancia * distancia * pi
    
    # show measurements
    df_meas <- data.frame(
      "File name" = input$InputFile[1],
      "Frames (n)" = info$video$frames,
      "Start - End (frames)" = paste0(input$framesEdit[1], " - ", input$framesEdit[2]),
      "Video size (px)" = paste0(info$video$width, " x ", info$video$height),
      "Current frame (n)" = input$imgEdit[1],
      "Object size (px)" = max(poly[, 1] - min(poly[, 1])) * max(poly[, 2] - min(poly[, 2])),
      "Distance (mm)" = round(distancia, digits = 2),
      "Cross-sectional area (mm²)" = round(area, digits = 2),
      "Threshold (Otsu)" = data$threshold,
      "Echogenicity, B&W (%)" = round(data$ecogenicidade_bw, digits = 2),
      "Echogenicity, gray (%)" = round(data$ecogenicidade_gray, digits = 2)
    )
    df_meas <- t(df_meas)
    
    labels <-
      c(
        "File name",
        "Frames (n)",
        "Start - End (frames)",
        "Video size (px)",
        "Current frame (n)",
        "Object size (px)",
        "Distance (mm)",
        "Cross-sectional area (mm²)",
        "Threshold (Otsu)",
        "Echogenicity, B&W (%)",
        "Echogenicity, gray (%)"
      )
    rownames(df_meas) <-
      labels
    
    # show DT table with buttons
    DT::datatable(
      df_meas,
      rownames = TRUE,
      colnames = rep("", ncol(df_meas)),
      extensions = c("Buttons", "FixedColumns"),
      options = list(
        dom = "B",
        ordering = F,
        buttons = list(
          list(extend = "copy",
               text = "Copy"),
          list(extend = "csv",
               text = "CSV"),
          list(extend = "excel",
               text = "Excel"),
          list(extend = "pdf",
               text = "PDF")
        ),
        fixedColumns = TRUE,
        pageLength = length(labels),
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  })
  
  # show PNG file of 1st frame ---------------------------------------------------------
  output[["plotROI"]] <- shiny::renderPlot({
    shiny::req(Video())
    shiny::req(input$framesEdit)
    
    # Get video info such as width, height, format, duration and framerate
    info <- av::av_media_info(Video())
    
    # show 1st frame
    img <-
      magick::image_read(
        list.files(
          path = file.path(dir.name, "1 edited"),
          full.names = TRUE,
          pattern = "png")[1]
        )
    
    # color palette (grayscale)
    pal <- grDevices::gray(seq(
      from = 0,
      to = 1,
      length.out = 256
    ), alpha = NULL)
    par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
    plot(
      img,
      xlim = c(0, info$video$width),
      ylim = c(0, info$video$height),
      asp = 1,
      col = pal
    )
    # custom functions
    round_2_odd <- function(x) {
      2 * floor(x / 2) + 1
    }
    
    # draw object rectangle
    rect(
      xleft = roi_coords$xy[2, 1] - floor(input$KernelSizeTrack / 2),
      ybottom = roi_coords$xy[2, 2] - floor(input$KernelSizeTrack / 2),
      xright = roi_coords$xy[2, 1] + floor(input$KernelSizeTrack / 2),
      ytop = roi_coords$xy[2, 2] + floor(input$KernelSizeTrack / 2),
      col = "transparent",
      border = "red",
      lty = "solid",
      lwd = 2
    )
    
    roi <-
      round_2_odd(input$KernelSizeTrack * (1 + as.numeric(input$Overlap) / 100)) # odd numbers only
    # draw ROI rectangle
    rect(
      xleft = roi_coords$xy[2, 1] - floor(roi / 2),
      ybottom = roi_coords$xy[2, 2] - floor(roi / 2),
      xright = roi_coords$xy[2, 1] + floor(roi / 2),
      ytop = roi_coords$xy[2, 2] + floor(roi / 2),
      col = "transparent",
      border = "yellow",
      lty = "solid",
      lwd = 2
    )
    # show coordinates of ROI
    text(
      x = roi_coords$xy[2, 1],
      y = roi_coords$xy[2, 2],
      paste0(
        "x=",
        round(roi_coords$xy[2, 1], 0),
        "\n",
        "y=",
        round(roi_coords$xy[2, 2], 0)
      ),
      col = "red"
    )
  }, height = function() {
    (session$clientData$output_plotROI_width) * (0.6584)
  })
  
  # process, play and export video ---------------------------------------------------------
  shiny::observeEvent(input[["buttAnalyze"]], {
    shiny::req(Video())
    # Get video info such as width, height, format, duration and framerate
    info <- av::av_media_info(Video())
    
    # Capture the result of us_track function call
    us_track(
      center.ini <-
        list(x = roi_coords$xy[2, 1], y = roi_coords$xy[2, 2]),
      inputfile = file.path(dir.name, "editedvideo.mp4"),
      filtertype = input$FilterType,
      filtersize = input$FilterSize,
      overlap = input$Overlap,
      jump = input$Jump,
      kernel = input$KernelSizeTrack
    )
    
    # draw trajectory on images
    path <-
      read.csv(file.path(dir.name, "CSV", "trajectory_measured.csv"))
    for (i in 1:length(list.files(file.path(dir.name, "8 output")))) {
      # read image from file
      img <- png::readPNG(file.path(dir.name, "8 output", paste0("image_", sprintf("%06d", i), ".png")))
      img <- grDevices::as.raster(img[, , 1:3])
      
      # create a ggplot object with the image in background and trajectory as data points
      par(mar = rep(0, 4), oma = rep(0, 4), omi = rep(0, 4), mai = rep(0, 4))
      ggplot2::ggplot() +
        ggplot2::annotation_raster(
          img,
          xmin = 0,
          xmax = info$video$width,
          ymin = 0,
          ymax = info$video$height,
          interpolate = TRUE
        ) +
        ggplot2::geom_point(
          data = path[1:i,],
          ggplot2::aes(x = X, y = Y),
          colour = "red",
          size = 100
        ) +
        ggplot2::scale_x_continuous(limits = c(0, info$video$width),
                                    expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(0, info$video$height),
                                    expand = c(0, 0)) +
        ggplot2::coord_fixed() +
        ggplot2::theme_void()
      
      # save ggplot as png
      ggplot2::ggsave(
        filename = file.path(dir.name, "8 output", paste0("image_", sprintf("%06d", i), ".png")),
        width = info$video$width,
        height = info$video$height,
        units = "px",
        dpi = 1,
        type = "cairo",
        limitsize = FALSE
      )
    }
    
    # build mp4 video using av video package from out.dir files
    av::av_encode_video(
      input = list.files(
        file.path(dir.name, "8 output"),
        pattern = ".png",
        full.names = TRUE
      ),
      output = file.path(dir.name, "outputvideo.mp4"),
      framerate = info$video$framerate
    )
  })
  
  # show MP4 video of output file
  output[["videooutput"]] <- shiny::renderUI({
    shiny::req(Video())
    # Get video info such as width, height, format, duration and framerate
    info <- av::av_media_info(Video())
    
    # show video
    tags$video(
      width = "90%",
      height = "90%",
      controls = "",
      tags$source(src = "outputvideo.mp4", type = "video/mp4")
    )
  })
  
  df <-
    shiny::reactive(if (file.exists(file.path(
      dir.name, "CSV", "max_cross_correlation.csv"
    )) &
    file.exists(file.path(dir.name, "CSV", "displacement.csv"))) {
      info <- av::av_media_info(Video())
      data.frame(
        "File name" = input$InputFile[1],
        "Frames (n)" = info$video$frames,
        "Start - End (frames)" = paste0(input$framesEdit[1], " - ", input$framesEdit[2]),
        "Video size (px)" = paste0(info$video$width, " x ", info$video$height),
        "X0 (px)" = round(roi_coords$xy[2, 1], 0),
        "Y0 (px)" = round(roi_coords$xy[2, 2], 0),
        "Object size (px)" = input$KernelSizeTrack,
        "Filter type" = input$FilterType,
        "Filter size (px)" = input$FilterSize,
        "Overlap (%)" = input$Overlap,
        "Jump (frames)" = input$Jump,
        "Displacement, total (px)" = round(sum(read.csv(
          file.path(dir.name, "CSV", "displacement.csv")
        )[[1]], na.rm = TRUE), 0),
        "Displacement, mean (px)" = round(mean(read.csv(
          file.path(dir.name, "CSV", "displacement.csv")
        )[[1]], na.rm = TRUE), 0),
        "Speed, mean (px/frame)" = round(mean(abs(
          diff(read.csv(
            file.path(dir.name, "CSV", "displacement.csv")
          )[[1]], differences = 1)
        ), na.rm = TRUE), 3),
        "Speed, max (px/frame)" = round(max(abs(
          diff(read.csv(
            file.path(dir.name, "CSV", "displacement.csv")
          )[[1]], differences = 1)
        ), na.rm = TRUE), 3),
        "Cross-correlation, max" = round(max(read.csv(
          file.path(dir.name, "CSV", "max_cross_correlation.csv")
        )[[1]], na.rm = TRUE), 3),
        "Cross-correlation, mean" = round(mean(read.csv(
          file.path(dir.name, "CSV", "max_cross_correlation.csv")
        )[[1]], na.rm = TRUE), 3),
        "Cross-correlation, min" = round(min(read.csv(
          file.path(dir.name, "CSV", "max_cross_correlation.csv")
        )[[1]], na.rm = TRUE), 3)
      )
    } else {
      info <- av::av_media_info(Video())
      data.frame(
        "File name" = input$InputFile[1],
        "Frames (n)" = info$video$frames,
        "Start - End (frames)" = paste0(input$framesEdit[1], " - ", input$framesEdit[2]),
        "Video size (px)" = paste0(info$video$width, " x ", info$video$height),
        "X0 (px)" = round(roi_coords$xy[2, 1], 0),
        "Y0 (px)" = round(roi_coords$xy[2, 2], 0),
        "Object size (px)" = input$KernelSizeTrack,
        "Filter type" = input$FilterType,
        "Filter size (px)" = input$FilterSize,
        "Overlap (%)" = input$Overlap,
        "Jump (frames)" = input$Jump,
        "Displacement, total (px)" = NA,
        "Displacement, mean (px)" = NA,
        "Speed, mean (px/frame)" = NA,
        "Speed, max (px/frame)" = NA,
        "Cross-correlation, max" = NA,
        "Cross-correlation, mean" = NA,
        "Cross-correlation, min" = NA
      )
    })
  
  # plot results of th CSV files ---------------------------------------------------------
  output[["plotTrack"]] <- shiny::renderImage({
    shiny::req(Video())
    # Get video info such as width, height, format, duration and framerate
    info <-
      av::av_media_info(file.path(dir.name, "outputvideo.mp4"))
    
    source("f_plot.R", local = TRUE)
    img <- htmltools::capturePlot({
      plot.trajectory(res.dir = file.path(dir.name, "CSV"),
                      info = info)
    }, height = 500, width = 500)
    list(
      src = img,
      height = "auto",
      width = "auto",
      contentType = "image/png"
    )
  }, deleteFile = TRUE)
  
  # show datatable of results ---------------------------------------------------------
  output[["tableTrack"]] <- DT::renderDataTable({
    shiny::req(Video())
    shiny::req(df())
    df <- t(df())
    labels <-
      c(
        "File name",
        "Frames (n)",
        "Start - End (frames)",
        "Video size (px)",
        "X0 (px)",
        "Y0 (px)",
        "Object size (px)",
        "Filter type",
        "Filter size (px)",
        "Overlap (%)",
        "Jump (frames)",
        "Displacement, total (px)",
        "Displacement, mean (px)",
        "Speed, mean (px/frame)",
        "Speed, max (px/frame)",
        "Cross-correlation, max",
        "Cross-correlation, mean",
        "Cross-correlation, min"
      )
    rownames(df) <-
      labels
    # show DT table with buttons
    DT::datatable(
      df,
      rownames = TRUE,
      colnames = rep("", ncol(df)),
      extensions = c("Buttons", "FixedColumns"),
      options = list(
        dom = "B",
        ordering = F,
        buttons = list(
          list(extend = "copy",
               text = "Copy"),
          list(extend = "csv",
               text = "CSV"),
          list(extend = "excel",
               text = "Excel"),
          list(extend = "pdf",
               text = "PDF")
        ),
        fixedColumns = TRUE,
        pageLength = length(labels),
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      )
    )
  })
  
  # download MP4 file generated by 8 output files ---------------------------------------------------------
  output[["downloadMP4"]] <-
    shiny::downloadHandler(
      filename = function() {
        paste0("outputvideo.mp4")
      },
      content = function(file) {
        file.copy(from = file.path(dir.name, "outputvideo.mp4"),
                  to = file)
      }
    )
  
  # download TRAJECTORY CSV files ---------------------------------------------------------
  output[["downloadPATH"]] <-
    shiny::downloadHandler(
      filename = function() {
        paste0("trajectory_measured.csv")
      },
      content = function(file) {
        file.copy(from = file.path(dir.name, "CSV", "trajectory_measured.csv"),
                  to = file)
      }
    )
  
  # download DISPLACEMENT CSV files ---------------------------------------------------------
  output[["downloadDISPL"]] <-
    shiny::downloadHandler(
      filename = function() {
        paste0("displacement.csv")
      },
      content = function(file) {
        file.copy(from = file.path(dir.name, "CSV", "displacement.csv"),
                  to = file)
      }
    )
  
  # download CC CSV files ---------------------------------------------------------
  output[["downloadCC"]] <-
    shiny::downloadHandler(
      filename = function() {
        paste0("max_cross_correlation.csv")
      },
      content = function(file) {
        file.copy(from = file.path(dir.name, "CSV", "max_cross_correlation.csv"),
                  to = file)
      }
    )
  
  # restart button ---------------------------------------------------------
  shinyjs::onclick("restart", {
    
    # clean InputFile
    shinyjs::reset("InputFile")
    
    # delete files
    unlink(
      list.files(
        path = dir.name,
        recursive = TRUE,
        include.dirs = TRUE,
        full.names = TRUE,
        pattern = "mp4"
      )
    )
    unlink(
      list.files(
        path = dir.name,
        recursive = TRUE,
        include.dirs = TRUE,
        full.names = TRUE,
        pattern = "png"
      )
    )
    unlink(
      list.files(
        path = dir.name,
        recursive = TRUE,
        include.dirs = TRUE,
        full.names = TRUE,
        pattern = "csv"
      )
    )
  })
  shiny::outputOptions(output, "downloadMP4", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "downloadPATH", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "downloadDISPL", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "downloadCC", suspendWhenHidden = FALSE)
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
