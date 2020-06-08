################################################################################
# Module Load excel files
#
# (c) 2019 Kiewisz
# This code is licensed under GPL V3.0 license (see LICENSE.txt for details)
#
# Author: Robert Kiewisz
# Created: 2020-05-21
################################################################################

file.list <- list.files(pattern='*.xlsx', path = 'Data/ASGA_Data/')

for (i in 1:length(file.list)) {
  assign(str_remove(paste(file.list[i]), ".xlsx"),
         read_excel(path = paste("Data/ASGA_Data/", file.list[i], sep = "")))
}