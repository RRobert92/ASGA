###################################################
# The Package to count Inter-kinetochore idstance #
###################################################
## The output of this function is data,framed named 'Inter_Kinetochore_Distance' with a distance in [um] for each pair.

## THe tool is relying on a label provided by the user. Before executing the code, user has to confirm the labels are correct
res <- dlg_message("*Impotent* !! You want to analyze Inter-Kinetochore distance...

                      This tool is taking into account labels form the Segment sheet.
                      Please make sure your data are properly labeled.
                   
                        Example:
                        Fibers with a pair form the opposite side should be labeled:
                          - Pole1_01 and correspond fiber as  Pole2_01
                          
                          Fibers without pair form the opposite side should be labeled:
                          - Pole0_01... for the Pole1
                          - Pole3_01... for the Pole2
                   
                      IS THIS TRUE FOR YOUR DATA?",
                   "yesno")$res

if(res == "yes"){
  Inter_Kinetochore_Distance <- Inter_Kinetochore_Dist()
  
} else if(res == "no"){
  if(all == "yes"){
    dlg_message("I am sorry to hear that :( Please fixed that! We will move now to analyse some more of your awsame data!")
    
  } else if( all == "no"){
    dlg_message("I am sorry to hear that :( Please fixed that and try again")
  }
}
rm(res)
