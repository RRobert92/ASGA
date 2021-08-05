Error_Handler <- function(input, output, session) {
    if (DATA_TEST == 1) {
        sendSweetAlert(
                session = session,
                title = "The data structure looks great!",
                text = paste(NUM_FILES, datatype, text_dataset, "successfuly uploaded!", "Press Ok to analyze your awesome data!", sep = " "),
                type = "success",
                btn_labels = "OK",
                btn_colors = "#39B855",
                closeOnClickOutside = TRUE
        )
    } else if (DATA_TEST == 2) {
        sendSweetAlert(
                session = session,
                title = "Looks like you there is a problem with your data",
                text = "The labeling in the 'segments' file should start with Pole1_00, not Pole2_00.
                Additionally, the 'segments' file may not have any labeling, in that case, consider this massage as a warning.
                Spindle poles position will be estimated based on MT ends distribution.
                Please check it with the guidelines for further processing, limited action may be possible.",
                type = "warning",
                btn_labels = "OK",
                btn_colors = "#f8bb86",
                closeOnClickOutside = TRUE
        )
    } else if (DATA_TEST == 3) {
        sendSweetAlert(
                session = session,
                title = "Looks like you there is a problem with your data",
                text = "The 'segments' data structure looks strange! 'Segment ID', 'Point IDs', or 'length' are missing or are in the wrong order...
               Please check it with the guidelines and try again.",
                type = "error",
                btn_colors = "#C95050",
                btn_labels = "OK",
                closeOnClickOutside = TRUE
        )
    } else if (DATA_TEST == 4) {
        sendSweetAlert(
                session = session,
                title = "Looks like you there is a problem with your data",
                text = "The labeling in the 'Nodes' file missing information about Pole1...
                Please check it with the guidelines and try again.",
                type = "error",
                btn_colors = "#C95050",
                btn_labels = "OK",
                closeOnClickOutside = TRUE
        )
    } else if (DATA_TEST == 5) {
        sendSweetAlert(
                session = session,
                title = "Looks like you there is a problem with your data",
                text = "The labeling in the 'Nodes' file missing information about Pole2...
                Please check it with the guidelines and try again.",
                type = "error",
                btn_colors = "#C95050",
                btn_labels = "OK",
                closeOnClickOutside = TRUE
        )
    } else if (DATA_TEST == 6) {
        sendSweetAlert(
                session = session,
                title = "Looks like you there is a problem with your data",
                text = "Could not find any 'Poles' coordinates in the Nodes file! Please check it with the guidelines and try again.",
                type = "error",
                btn_colors = "#C95050",
                btn_labels = "OK",
                closeOnClickOutside = TRUE
        )
    } else if (DATA_TEST == 7) {
        sendSweetAlert(
                session = session,
                title = "Looks like you there is a problem with your data",
                text = "The data structure is not compatible at all. Did you try to load a wrong file?
        Please check it with the guidelines and try again.",
                type = "error",
                btn_colors = "#C95050",
                btn_labels = "OK",
                closeOnClickOutside = TRUE
        )
    } else if (DATA_TEST == 0) {
        sendSweetAlert(
                session = session,
                title = "Looks like you try to upload a wrong file",
                text = "Please check it with the guidelines and try again.",
                type = "error",
                btn_colors = "#C95050",
                btn_labels = "OK",
                closeOnClickOutside = TRUE
        )
    }

    if (exists("AnalysisTest")) {
        if (AnalysisTest == 0) {
            sendSweetAlert(
                    session = session,
                    title = "Looks like you try to upload a wrong file",
                    text = "Please check if the file name starts with Data_[:digit:]_
      Where [:digit:] is any digit value 0 1 2 3 4 etc.",
                    type = "error",
                    btn_colors = "#C95050",
                    btn_labels = "OK",
                    closeOnClickOutside = TRUE
            )
        }
    }
}
