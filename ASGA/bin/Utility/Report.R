################################################################################
# Module Report
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
# Reviewed: Robert Kiewisz 28/08/2020 (v0.31.1)
################################################################################

# Style of the setting for the plot --------------------------------------------
Data_Plot_Settings <- function(input, output, session) {
  tags$div(
    class = "splash-report-code",
    fluidRow(
      column(
        4,
        lapply(1:numfiles, function(i) {
          textInput(
            inputId = paste("Data_label", i, sep = "_"),
            label = paste("Data", i, sep = "_"),
            value = paste("Data", i, sep = "_")
          )
        })
      ),

      column(
        4,
        lapply(1:numfiles, function(i) {
          colourInput(
            inputId = paste("Data_color", i, sep = "_"),
            label = paste("Select Colour for Data_", i, sep = ""),
            value = paste("gray")
          )
        })
      ),

      column(
        4,
        lapply(1:numfiles, function(i) {
          selectInput(
            inputId = paste("Data_bin", i, sep = "_"),
            label = paste("Which data to plot Data_", i, sep = ""),
            choices = c(
              "All" = "all",
              "Pole_1" = "P1",
              "Pole_2" = "P2"
            ),
            selected = "All"
          )
        })
      )
    ),

    fluidRow(column(
      12,

      tags$div(
        class = "table-GS-Center",
        actionBttn(
          inputId = "Refresh",
          label = "Refresh",
          style = "material-flat",
          color = "primary",
          size = "lg"
        )
      )
    ))
  )
}

# Render of a plots ------------------------------------------------------------
Report_Plot_KMT_No <- function(input, output, session) {
  plotOutput("Home-plot_KMT_No")
}

Report_Plot_LD <- function(input, output, session) {
  plotOutput("Home-plot_LD")
}

Report_Plot_LD2 <- function(input, output, session) {
  plotOutput("Home-plot_LD2")
}

Report_Plot_IKD <- function(input, output, session) {
  plotOutput("Home-plot_IKD")
}

# Set uo of a plot to render ---------------------------------------------------
Report_Plot <- function(input, output, session) {

  # Plot for KMT no per kinetochore --------------------------------------------
  output$`plot_KMT_No` <- renderPlot({
    tryCatch(
      {
        Plot_Data <<- File_name[File_name$V2 == "KMT_No", ]

        for (i in 1:nrow(Plot_Data)) {
          if (get(paste("Data_bin", i, sep = "_")) == "all") {
            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
              data.frame(
                Data = get(paste("Data_", "label_", i, sep = "")),
                get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], sep = ""))["KMTs_per_kinetochore"]
              ),
              envir = .GlobalEnv
            )
          } else if (get(paste("Data_bin", i, sep = "_")) == "P1") {
            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P1", sep = ""),
              data.frame(
                Data = get(paste("Data_", "label_", i, sep = "")),
                get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P1", sep = ""))["No_of_KMTs_at_kinetochore_P1...1."]
              ),
              envir = .GlobalEnv
            )

            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
              get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P1", sep = "")),
              envir = .GlobalEnv
            )
          } else if (get(paste("Data_bin", i, sep = "_")) == "P2") {
            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P2", sep = ""),
              data.frame(
                Data = get(paste("Data_", "label_", i, sep = "")),
                get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P2", sep = ""))["No_of_KMTs_at_kinetochore_P2...1."]
              ),
              envir = .GlobalEnv
            )

            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
              get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P2", sep = "")),
              envir = .GlobalEnv
            )
          }

          if (get(paste("Data_bin", i, sep = "_")) == "P1") {
            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
              get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = "")) %>%
                rename(
                  KMTs_per_kinetochore = No_of_KMTs_at_kinetochore_P1...1.
                ),
              envir = .GlobalEnv
            )
          } else if (get(paste("Data_bin", i, sep = "_")) == "P2") {
            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
              get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = "")) %>%
                rename(
                  KMTs_per_kinetochore = No_of_KMTs_at_kinetochore_P2...1.
                ),
              envir = .GlobalEnv
            )
          }
        }

        P1 <<- ggplot(
          get(paste("Data_", Plot_Data[1, "V1"], "_", Plot_Data[1, "V2"], "_bin", sep = "")),
          aes_string("Data", "KMTs_per_kinetochore")
        ) +
          geom_boxplot(fill = get(paste("Data_", "color_", 1, sep = "")), color = "black") +
          theme_classic() +
          xlab("Data-set names") +
          ylab("Number of KMTs per kinetochore")

        tryCatch(
          {
            for (i in 2:nrow(Plot_Data)) {
              P1 <<- P1 + geom_boxplot(
                data = get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = "")), aes_string("Data", "KMTs_per_kinetochore"),
                fill = get(paste("Data_", "color_", i, sep = "")), color = "black"
              )
            }

            All_KMT_No <<- get(paste("Data_", Plot_Data[1, "V1"], "_", Plot_Data[1, "V2"], "_bin", sep = ""))

            for (i in 2:nrow(Plot_Data)) {
              assign("All_KMT_No",
                rbind(
                  All_KMT_No["KMTs_per_kinetochore"],
                  get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""))["KMTs_per_kinetochore"]
                ),
                envir = .GlobalEnv
              )
            }

            All_KMT_No <<- data.frame(
              Data = "Average",
              All_KMT_No["KMTs_per_kinetochore"]
            )

            P1 <<- P1 + geom_boxplot(data = All_KMT_No, aes("Avg.", KMTs_per_kinetochore), fill = "darkred", color = "black", outlier.alpha = 0) +
              geom_jitter(data = All_KMT_No, aes("Avg.", KMTs_per_kinetochore), alpha = 0.1, size = 1, width = 0.25)
          },
          error = function(e) {}
        )

        print(P1)
      },
      error = function(e) {}
    )
  })

  # Plot for KMT length distribution -------------------------------------------
  output$`plot_LD` <- renderPlot({
    tryCatch(
      {
        Plot_Data <<- File_name[File_name$V2 == "LD", ]

        for (i in 1:nrow(Plot_Data)) {
          if (get(paste("Data_bin", i, sep = "_")) == "all") {
            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
              data.frame(
                Data = get(paste("Data_", "label_", i, sep = "")),
                get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], sep = ""))["length"]
              ),
              envir = .GlobalEnv
            )
          } else if (get(paste("Data_bin", i, sep = "_")) == "P1") {
            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P1", sep = ""),
              data.frame(
                Data = get(paste("Data_", "label_", i, sep = "")),
                get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P1", sep = ""))["length"]
              ),
              envir = .GlobalEnv
            )

            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
              get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P1", sep = "")),
              envir = .GlobalEnv
            )
          } else if (get(paste("Data_bin", i, sep = "_")) == "P2") {
            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P2", sep = ""),
              data.frame(
                Data = get(paste("Data_", "label_", i, sep = "")),
                get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P2", sep = ""))["length"]
              ),
              envir = .GlobalEnv
            )

            assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
              get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_P2", sep = "")),
              envir = .GlobalEnv
            )
          }
        }

        P2 <<- ggplot(
          get(paste("Data_", Plot_Data[1, "V1"], "_", Plot_Data[1, "V2"], "_bin", sep = "")),
          aes_string("length")
        ) +
          geom_density(kernel = "gaussian", size = 1, color = get(paste("Data_", "color_", 1, sep = "")), linetype = "solid") +
          theme_classic() +
          xlab("KMT lengths") +
          ylab("KMT density [Gaussian Kernal density]")

        tryCatch(
          {
            for (i in 2:nrow(Plot_Data)) {
              P2 <<- P2 + geom_density(
                data = get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = "")), aes_string("length"),
                color = get(paste("Data_", "color_", i, sep = "")), kernel = "gaussian", size = 1
              )
            }

            All_LD <<- get(paste("Data_", Plot_Data[1, "V1"], "_", Plot_Data[1, "V2"], "_bin", sep = ""))

            for (i in 2:nrow(Plot_Data)) {
              assign("All_LD",
                rbind(
                  All_LD["length"],
                  get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""))["length"]
                ),
                envir = .GlobalEnv
              )
            }

            All_LD <<- data.frame(
              Data = "Average",
              All_LD["length"]
            )

            P2 <<- P2 + geom_density(data = All_LD, aes(length), kernel = "gaussian", size = 1, color = "darkred", linetype = "solid")
          },
          error = function(e) {}
        )

        print(P2)
      },
      error = function(e) {}
    )
  })

  # Plot for the LD showing a average length -----------------------------------
  output$`plot_LD2` <- renderPlot({
    tryCatch(
      {
        P2.1 <<- ggplot(
          get(paste("Data_", Plot_Data[1, "V1"], "_", Plot_Data[1, "V2"], "_bin", sep = "")),
          aes_string("Data", "length")
        ) +
          geom_violin(
            draw_quantiles = c(0.25, 0.5, 0.75),
            fill = get(paste("Data_", "color_", 1, sep = "")), color = "black"
          ) +
          theme_classic() +
          ylab("KMT lengths [um]") +
          ylim(0, 10)

        tryCatch(
          {
            for (i in 2:nrow(Plot_Data)) {
              P2.1 <<- P2.1 + geom_violin(
                data = get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = "")),
                aes_string("Data", "length"), draw_quantiles = c(0.25, 0.5, 0.75),
                fill = get(paste("Data_", "color_", i, sep = "")), color = "black"
              )
            }

            P2.1 <<- P2.1 + geom_violin(
              data = All_LD,
              aes_string("Data", "length"), draw_quantiles = c(0.25, 0.5, 0.75),
              fill = "darkred", color = "black"
            )
          },
          error = function(e) {}
        )

        print(P2.1)
      },
      error = function(e) {}
    )
  })

  # Plot for the inter-kinetochore distance ------------------------------------
  output$`plot_IKD` <- renderPlot({
    tryCatch(
      {
        Plot_Data <<- File_name[File_name$V2 == "IKD", ]

        for (i in 1:nrow(Plot_Data)) {
          assign(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = ""),
            data.frame(
              Data = get(paste("Data_", "label_", i, sep = "")),
              get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], sep = ""))["Inter-kinetochore distance"]
            ),
            envir = .GlobalEnv
          )
        }

        P3 <<- ggplot(
          get(paste("Data_", Plot_Data[1, "V1"], "_", Plot_Data[1, "V2"], "_bin", sep = "")),
          aes_string("Data", "Inter.kinetochore.distance")
        ) +
          geom_boxplot(fill = get(paste("Data_", "color_", 1, sep = "")), color = "black") +
          theme_classic() +
          xlab("Data-set names") +
          ylab("Inter-kinetochore distance [um]")

        tryCatch(
          {
            for (i in 2:nrow(Plot_Data)) {
              P3 <<- P3 + geom_boxplot(
                data = get(paste("Data_", Plot_Data[i, "V1"], "_", Plot_Data[i, "V2"], "_bin", sep = "")), aes_string("Data", "Inter.kinetochore.distance"),
                fill = get(paste("Data_", "color_", i, sep = "")), color = "black"
              )
            }
          },
          error = function(e) {}
        )

        print(P3)
      },
      error = function(e) {}
    )
  })
}
