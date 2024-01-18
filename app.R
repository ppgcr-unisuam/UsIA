# create dir
dir.name <- 'www'
if (!dir.exists(dir.name)){
  dir.create(dir.name, recursive = TRUE, showWarnings = FALSE)
}
shiny::addResourcePath(prefix = "www", directoryPath = "www")

# load libraries
library(dplyr)
library(htmltools)
library(BiocManager)
library(EBImage)

# source all scripts
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
        9,
        list(
          fontawesome::fa("wifi"),
          shiny::HTML("<strong>UsIA</strong>"),
          "| Ultrasound Image Analysis"
        ),
        style = "text-align:left;"
      ),
      shiny::column(
        3,
        tags$a(
          id = "refresh",
          class = "btn btn-primary",
          href = "javascript:history.go(0)",
          shiny::HTML('<i class="fa fa-refresh fa-1x"></i>'),
          title = "Reset",
          style = "color:white; border-color:white; border-radius:100%"
        ),
        style = "text-align:right;"
      )
    ),
    windowTitle = "UsIA | Ultrasound Image Analysis"
  ),
  
  # change color of fileInput button
  tags$head(tags$style(
    shiny::HTML(".btn-file {background-color: #2C3E50;}")
  )),
  
  # control labels to the left
  tags$style(
    shiny::HTML(
      "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 90%;                /* Set width for container */
      max-width: 90%;
    }
    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 20%;          /* Target width for label */
    }
    .label-left .irs {
      flex-basis: 80%;          /* Target width for slider */
    }
    "
    )
  ),
  
  # Sidebar with controls
  ui_sidebar <- shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::tabsetPanel(
        id = "tab",
        type = "tabs",
        shiny::tabPanel(
          title = "Controls",
          shiny::br(),
          shiny::fileInput(
            inputId = "InputFile",
            label = NULL,
            multiple = FALSE,
            buttonLabel = list(fontawesome::fa("video"), "Upload"),
            accept = c(".mp4")
          ),
          shiny::fluidRow(shiny::column(
            6,
            shiny::actionButton(
              inputId = "buttEdit",
              label = "Edit",
              class = "btn-primary",
              style = "width:100%; border-color:white; border-radius: 10px;",
              shiny::icon("edit")
            ),
          ),
          shiny::column(
            6,
            shiny::actionButton(
              inputId = "buttROI",
              label = "ROI",
              class = "btn-primary",
              style = "width:100%; border-color:white; border-radius: 10px;",
              shiny::icon("crop")
            ),
          ),),
          shiny::br(),
          shiny::HTML("<center>"),
          div(
            class = "label-left",
            shiny::sliderInput(
              inputId = "KernelSize",
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
          ),
          shiny::HTML("</center>"),
          shiny::actionButton(
            inputId = "buttAnalyze",
            label = "Analyze",
            class = "btn-primary",
            style = "width:100%; border-color:white; border-radius: 10px; font-size:150%;",
            shiny::icon("play")
          ),
          shiny::br(),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::actionButton(
                inputId = "buttPlots",
                label = "Plots",
                class = "btn-primary",
                style = "width:100%; border-color:white; border-radius: 10px;",
                shiny::icon("chart-bar")
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                inputId = "buttTable",
                label = "Table",
                class = "btn-primary",
                style = "width:100%; border-color:white; border-radius: 10px;",
                shiny::icon("table")
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                inputId = "buttDowns",
                label = "Download",
                class = "btn-primary",
                style = "width:100%; border-color:white; border-radius: 10px;",
                shiny::icon("download")
              )
            )
          )
        ),
        shiny::tabPanel(
          title = "Instructions",
          shiny::br(),
          shiny::HTML(
            "<p>1. Upload a video file (.mp4) with the <b>Upload video</b>.</p>
            <p>2. If necessary, edit the video by subsetting the uploaded video using <b>Edit</b>.</p>
            <p>3. Click <b>Select ROI</b> to set the region of interest (ROI) for analysis. Click on the screen to mark the center of the ROI.</p>
            <p>4. Adjust the parameters for analysis:</p>
            <p>   - <i>Object size</i>: '1' to '201' (odd numbers only).</p>
            <p>   - <i>Overlap</i>: '0' to '100%'.</p>
            <p>   - <i>Filter type</i>: 'none', 'gaussian', 'median'.</p>
            <p>   - <i>Filter size</i>: '1' to '11' (odd numbers only).</p>
            <p>   - <i>Jump</i>: '1' to '5' frames.</p>
            <p>5. Click <b>Analyze</b>. Wait until the red progress bar on the top stops blinking.</p>
            <p>6. Check <b>Output</b> tab to visualize the processed video.</p>
            <p>7. Check <b>Plots</b> and <b>Table</b> tabs for the results.</p>
            <p>8. Click on <b>Downloads</b> to download time series for:</p>
            <p>   - <i>Output video</i> (.mp4)
            <p>   - <i>Object trajectory</i> (.csv)
            <p>   - <i>Object displacement</i> (.csv)
            <p>   - </i>Maximum cross-correlation</i> (.csv)
            <p>9. Click <b>Reset</b> icon for running new analisys.",
          ),
        ),
        shiny::tabPanel(
          title = "About",
          shiny::br(),
          shiny::HTML("<b>Author</b>"),
          shiny::br(),
          shiny::HTML(
            "<a href=\"mailto:arthurde@souunisuam.com.br\">Arthur Ferreira, DSc</a>"
          ),
          shiny::br(),
          shiny::br(),
          shiny::HTML("<b>Contributors</b>"),
          shiny::br(),
          shiny::HTML(
            "<a href=\"mailto:gustavo.telles@souunisuam.com.br\">Gustavo Telles, MSc</a>; <a href=\"mailto:jessica.rio@souunisuam.com.br\">Jessica Rio, MSc</a>; <a href=\"mailto:alicepagnez@souunisuam.com.br\"> Maria Alice Pagnez, DSc</a>; <a href=\"mailto:leandronogueira@souunisuam.com.br\">Leandro Nogueira, DSc</a>"
          ),
          shiny::br(),
          shiny::br(),
          shiny::HTML("<b>Affiliation</b>"),
          shiny::br(),
          shiny::HTML(
            "<a href=\"https://www.unisuam.edu.br/programa-pos-graduacao-ciencias-da-reabilitacao\">PPGCR</a> | Programa de Pós-graduação em Ciências da Reabilitação, Centro Universitário Augusto Motta, Rio de Janeiro, RJ, Brazil"
          ),
          shiny::br(),
          shiny::br(),
          shiny::HTML("<b>License</b>"),
          shiny::br(),
          shiny::HTML(
            "This work is licensed under an <a rel=\"license\" data-spdx=\"Apache-2.0\" href=\"https://www.apache.org/licenses/LICENSE-2.0\">Apache License 2.0</a>."
          ),
          shiny::br(),
          shiny::br(),
          shiny::HTML("<b>Cite as</b>"),
          shiny::br(),
          shiny::HTML(
            "Ferreira, A.S. (2023). UsIA | Ultrasound Image Analysis (1.0.0). Zenodo. https://doi.org/10.5281/zenodo.10439718"
          ),
          shiny::br(),
          shiny::br(),
          shiny::HTML(
            "<a href=\"https://doi.org/10.5281/zenodo.10439718\" style=\"vertical-align:middle;\"><img src=\"https://zenodo.org/badge/DOI/10.5281/zenodo.10439718.svg\" alt=\"DOI\"  style=\"vertical-align:top;\"></a>"
          ),
          shiny::br(),
        ),
      ),
    ),
    
    # Main panel for displaying outputs ----
    shiny::mainPanel(
      shiny::tabsetPanel(
        id = "tabResults",
        type = "tabs",
        shiny::tabPanel(
          title = "Input",
          shiny::br(),
          shiny::uiOutput(outputId = "videoraw"),
          align = "center"
        ),
        shiny::tabPanel(
          title = "Edit",
          shiny::br(),
          shiny::uiOutput(outputId = "videoedit"),
          shiny::br(),
          shiny::sliderInput(
            inputId = "framesEdit",
            label = "Frames",
            min = 1,
            max = 100,
            value = c(1, 100),
            step = 1,
            ticks = FALSE,
            animate = TRUE,
            width = "100%"
          ),
          align = "center"
        ),
        shiny::tabPanel(
          title = "ROI",
          shiny::br(),
          shiny::plotOutput(
            outputId = "plotROI",
            width = "auto",
            click = "plot_click"
          ),
          align = "center"
        ),
        shiny::tabPanel(
          title = "Output",
          shiny::br(),
          shiny::uiOutput(outputId = "videooutput"),
          align = "center"
        ),
        shiny::tabPanel(
          title = "Plots",
          shiny::br(),
          shiny::plotOutput("plotResults",  width = "100%"),
          align = "center"
        ),
        shiny::tabPanel(
          title = "Table",
          shiny::br(),
          DT::dataTableOutput("tableResults", width = "100%"),
          align = "center"
        ),
        shiny::tabPanel(
          title = "Download",
          shiny::br(),
          shiny::br(),
          shiny::downloadButton(
            outputId = "downloadMP4",
            label = "Video (.mp4)",
            class = "btn-primary",
            style = "width:100%; border-color:white; border-radius: 10px;",
          ),
          shiny::br(),
          shiny::br(),
          shiny::downloadButton(
            outputId = "downloadPATH",
            label = "Trajectory data (.csv)",
            class = "btn-primary",
            style = "width:100%; border-color:white; border-radius: 10px;",
          ),
          shiny::br(),
          shiny::br(),
          shiny::downloadButton(
            outputId = "downloadDISPL",
            label = "Displacement data (.csv)",
            class = "btn-primary",
            style = "width:100%; border-color:white; border-radius: 10px;",
          ),
          shiny::br(),
          shiny::br(),
          shiny::downloadButton(
            outputId = "downloadCC",
            label = "Cross-correlation data (.csv)",
            class = "btn-primary",
            style = "width:100%; border-color:white; border-radius: 10px;",
          ),
          align = "center"
        )
      )
    )
  )
)

# Define server script
server <- function(input, output, session) {
  
  # store plot click coords
  source_coords <-
    shiny::reactiveValues(xy = data.frame(x = c(1, 1),  y = c(1, 1)))
  
  # observe plot clic event ---------------------------------------------------------
  shiny::observeEvent(input$plot_click, {
    source_coords$xy[2, ] <- c(input$plot_click$x, input$plot_click$y)
  })
  
  # change to tab Edit under event ---------------------------------------------------------
  shiny::observeEvent(input[["buttEdit"]], {
    shiny::updateTabsetPanel(inputId = "tabResults",
                             selected = "Edit")
  })
  
  # change to tab Input under event ---------------------------------------------------------
  shiny::observeEvent(input[["InputFile"]], {
    shiny::updateTabsetPanel(inputId = "tabResults",
                             selected = "Input")
  })
  
  # change to tab ROI under event ---------------------------------------------------------
  shiny::observeEvent(input[["buttROI"]], {
    shiny::updateTabsetPanel(inputId = "tabResults",
                             selected = "ROI")
  })
  
  # change to tab Output under event ---------------------------------------------------------
  shiny::observeEvent(input[["buttAnalyze"]], {
    shiny::updateTabsetPanel(inputId = "tabResults",
                             selected = "Output")
  })
  
  # change to tab Plots under event ---------------------------------------------------------
  shiny::observeEvent(input[["buttPlots"]], {
    shiny::updateTabsetPanel(inputId = "tabResults",
                             selected = "Plots")
  })
  
  # change to tab Table under event ---------------------------------------------------------
  shiny::observeEvent(input[["buttTable"]], {
    shiny::updateTabsetPanel(inputId = "tabResults",
                             selected = "Table")
  })
  
  # change to tab Downloads under event ---------------------------------------------------------
  shiny::observeEvent(input[["buttDowns"]], {
    shiny::updateTabsetPanel(inputId = "tabResults",
                             selected = "Download")
  })
  
  # enable/disable filter
  shiny::observeEvent(input$FilterType, {
    shinyjs::toggleState(id = "FilterSize", condition = input$FilterType != "none")
  })
  
  # upload video ---------------------------------------------------------
  Video <- shiny::eventReactive(input[["InputFile"]], {
    input[["InputFile"]][["datapath"]]
  })
  videoname <- shiny::reactive ({
    paste(input$InputFile)
  })
  
  # play uploaded video ---------------------------------------------------------
  output[["videoraw"]] <- shiny::renderUI({
    shiny::req(Video())
    # Get video info such as width, height, format, duration and framerate
    info <- av::av_media_info(Video())
    
    # center ROI for the first time
    source_coords$xy[2, ] <- c(info$video$width / 2, info$video$height / 2)
    
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
    
    # show video
    tags$video(
      width = "90%",
      height = "90%",
      controls = "",
      tags$source(src = "editedvideo.mp4", type = "video/mp4")
    )
  })
  
  # show PNG file of 1st frame ---------------------------------------------------------
  output[["plotROI"]] <- shiny::renderPlot({
    shiny::req(Video())
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
      xleft = source_coords$xy[2, 1] - floor(input$KernelSize / 2),
      ybottom = source_coords$xy[2, 2] - floor(input$KernelSize / 2),
      xright = source_coords$xy[2, 1] + floor(input$KernelSize / 2),
      ytop = source_coords$xy[2, 2] + floor(input$KernelSize / 2),
      col = "transparent",
      border = "red",
      lty = "solid",
      lwd = 2
    )
    
    roi <-
      round_2_odd(input$KernelSize * (1 + as.numeric(input$Overlap) / 100)) # odd numbers only
    # draw ROI rectangle
    rect(
      xleft = source_coords$xy[2, 1] - floor(roi / 2),
      ybottom = source_coords$xy[2, 2] - floor(roi / 2),
      xright = source_coords$xy[2, 1] + floor(roi / 2),
      ytop = source_coords$xy[2, 2] + floor(roi / 2),
      col = "transparent",
      border = "yellow",
      lty = "solid",
      lwd = 2
    )
    # show coordinates of ROI
    text(
      x = source_coords$xy[2, 1],
      y = source_coords$xy[2, 2],
      paste0(
        "x=",
        round(source_coords$xy[2, 1], 0),
        "\n",
        "y=",
        round(source_coords$xy[2, 2], 0)
      ),
      col = "red"
    )
  }, height = function() {
    (session$clientData$output_plotROI_width) * (0.6585)
  })
  
  # process, play and export video ---------------------------------------------------------
  shiny::observeEvent(input[["buttAnalyze"]], {
    shiny::req(Video())
    # Get video info such as width, height, format, duration and framerate
    info <- av::av_media_info(Video())
    
    # Capture the result of us_track function call
    us_track(
      center.ini <-
        list(x = source_coords$xy[2, 1], y = source_coords$xy[2, 2]),
      inputfile = file.path(dir.name, "editedvideo.mp4"),
      filtertype = input$FilterType,
      filtersize = input$FilterSize,
      overlap = input$Overlap,
      jump = input$Jump,
      kernel = input$KernelSize
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
        "File name" = videoname()[1],
        "Frames (n)" = info$video$frames,
        "Start - End (frames)" = paste0(input$framesEdit[1], " - ", input$framesEdit[2]),
        "Video size (px)" = paste0(info$video$width, " x ", info$video$height),
        "X0 (px)" = round(source_coords$xy[2, 1], 0),
        "Y0 (px)" = round(source_coords$xy[2, 2], 0),
        "Object size (px)" = input$KernelSize,
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
        "File name" = videoname()[1],
        "Frames (n)" = info$video$frames,
        "Start - End (frames)" = paste0(input$framesEdit[1], " - ", input$framesEdit[2]),
        "Video size (px)" = paste0(info$video$width, " x ", info$video$height),
        "X0 (px)" = round(source_coords$xy[2, 1], 0),
        "Y0 (px)" = round(source_coords$xy[2, 2], 0),
        "Object size (px)" = input$KernelSize,
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
  
  # show datatable of results ---------------------------------------------------------
  output[["tableResults"]] <- DT::renderDataTable({
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
        autoWidth = TRUE
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
  
  # plot results of th CSV files ---------------------------------------------------------
  output[["plotResults"]] <- shiny::renderImage({
    shiny::req(file.path(dir.name, "outputvideo.mp4"))
    # Get video info such as width, height, format, duration and framerate
    info <-
      av::av_media_info(file.path(dir.name, "outputvideo.mp4"))
    
    source("f_plot.R", local = TRUE)
    img <- htmltools::capturePlot({
      plot.trajectory(res.dir = file.path(dir.name, "CSV"),
                      info = info)
    }, height = "500", width = "500")
    list(
      src = img,
      height = "auto",
      width = "auto",
      contentType = "image/png"
    )
  }, deleteFile = TRUE)
  
  # reset button ---------------------------------------------------------
  shinyjs::onclick("refresh", {
    # delete temp files
    unlink(
      list.files(
        path = dir.name,
        recursive = TRUE,
        include.dirs = TRUE,
        full.names = TRUE,
        pattern = "gif"
      )
    )
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
