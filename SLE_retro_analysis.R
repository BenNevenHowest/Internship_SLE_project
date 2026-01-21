#---------Importing packages-------------

library("xlsx")

#----------------------------------------

#------------------------Loading and preprocessing data ---------------------------

#Load the data from the Retrospective SLE study and remove potential ghost columns
retro_sle = read.xlsx("SLE_retro.xlsx", sheetName = "SLE")
retro_sle <- retro_sle[, !grepl("^NA", names(retro_sle))]
retro_dc = read.xlsx("SLE_retro.xlsx", sheetName = "DC")
retro_dc <- retro_dc[, !grepl("^NA", names(retro_dc))]
retro_hc = read.xlsx("SLE_retro.xlsx", sheetName = "HC")
retro_hc <- retro_hc[, !grepl("^NA", names(retro_hc))]

#----------------------------------------------------------------------------------

