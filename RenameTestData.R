# Data is from the Kaggle Epileptic Seizure Prediction Competion 2014. 
# A large number of EEG data files are labelled as test segments.These files were 
# to be used for testing and to derive a private score in the competition. For the
# purposes of this study, the test segments will be included with the training segments
# to increase the size of the data set. All test files are anonymised and labelled as
# "*_test_segment.mat". The data labels were provided by the competition host once the 
# competion had closed. The data labels are contained in SzPrediction_answer_key.csv 
# which is available from https://www.kaggle.com/c/seizure-prediction/discussion/10955

# Data files are nested according to patient. Preictal and interictal segments are filed 
# under their relevant folder. Test segments are filed in a sub-folder ./Test_Data
#
# ./Data/Dog_1/Test_Data
#    |     |        |--Dog_1_test_segment_0001.mat
#    |     |        |--etc.
#    |     |
#    |     |--Dog_1_interictal_segment_0001.mat
#    |     |--etc.
#    |
#    |-- /Dog_2/Test_Data
#    |-- /Dog_3/Test_Data
#    |-- /Dog_4/Test_Data
#    |-- /Dog_5/Test_Data
#    |-- /Patient_1/Test_Data
#    |-- /Patient_2/Test_Data
#
# The csv file with the labels is easily filtered for the preictal segments and a string
# is formed that is easily cut and paste to form a list of preictal files. See code that
# follows hereafter. Once completed the relabelled files can be included
# with the other patient files and the Test_Data folder removed.

# Used for validating the files
require (R.matlab)

# List of files that are preictal (from the csv with labels)
preictfiles <- c("Dog_1_test_segment_0021.mat",
                       "Dog_1_test_segment_0031.mat",
                       "Dog_1_test_segment_0044.mat",
                       "Dog_1_test_segment_0048.mat",
                       "Dog_1_test_segment_0054.mat",
                       "Dog_1_test_segment_0118.mat",
                       "Dog_1_test_segment_0127.mat",
                       "Dog_1_test_segment_0134.mat",
                       "Dog_1_test_segment_0154.mat",
                       "Dog_1_test_segment_0175.mat",
                       "Dog_1_test_segment_0186.mat",
                       "Dog_1_test_segment_0212.mat",
                       "Dog_1_test_segment_0247.mat",
                       "Dog_1_test_segment_0261.mat",
                       "Dog_1_test_segment_0289.mat",
                       "Dog_1_test_segment_0302.mat",
                       "Dog_1_test_segment_0332.mat",
                       "Dog_1_test_segment_0340.mat",
                       "Dog_1_test_segment_0343.mat",
                       "Dog_1_test_segment_0365.mat",
                       "Dog_1_test_segment_0367.mat",
                       "Dog_1_test_segment_0425.mat",
                       "Dog_1_test_segment_0480.mat",
                       "Dog_1_test_segment_0491.mat",
                       "Dog_2_test_segment_0005.mat",
                       "Dog_2_test_segment_0012.mat",
                       "Dog_2_test_segment_0015.mat",
                       "Dog_2_test_segment_0043.mat",
                       "Dog_2_test_segment_0050.mat",
                       "Dog_2_test_segment_0054.mat",
                       "Dog_2_test_segment_0060.mat",
                       "Dog_2_test_segment_0076.mat",
                       "Dog_2_test_segment_0078.mat",
                       "Dog_2_test_segment_0084.mat",
                       "Dog_2_test_segment_0096.mat",
                       "Dog_2_test_segment_0107.mat",
                       "Dog_2_test_segment_0111.mat",
                       "Dog_2_test_segment_0123.mat",
                       "Dog_2_test_segment_0130.mat",
                       "Dog_2_test_segment_0132.mat",
                       "Dog_2_test_segment_0136.mat",
                       "Dog_2_test_segment_0145.mat",
                       "Dog_2_test_segment_0146.mat",
                       "Dog_2_test_segment_0152.mat",
                       "Dog_2_test_segment_0162.mat",
                       "Dog_2_test_segment_0166.mat",
                       "Dog_2_test_segment_0182.mat",
                       "Dog_2_test_segment_0184.mat",
                       "Dog_2_test_segment_0229.mat",
                       "Dog_2_test_segment_0235.mat",
                       "Dog_2_test_segment_0240.mat",
                       "Dog_2_test_segment_0242.mat",
                       "Dog_2_test_segment_0260.mat",
                       "Dog_2_test_segment_0263.mat",
                       "Dog_2_test_segment_0264.mat",
                       "Dog_2_test_segment_0286.mat",
                       "Dog_2_test_segment_0311.mat",
                       "Dog_2_test_segment_0337.mat",
                       "Dog_2_test_segment_0338.mat",
                       "Dog_2_test_segment_0351.mat",
                       "Dog_2_test_segment_0352.mat",
                       "Dog_2_test_segment_0353.mat",
                       "Dog_2_test_segment_0369.mat",
                       "Dog_2_test_segment_0380.mat",
                       "Dog_2_test_segment_0390.mat",
                       "Dog_2_test_segment_0400.mat",
                       "Dog_2_test_segment_0402.mat",
                       "Dog_2_test_segment_0404.mat",
                       "Dog_2_test_segment_0417.mat",
                       "Dog_2_test_segment_0431.mat",
                       "Dog_2_test_segment_0443.mat",
                       "Dog_2_test_segment_0451.mat",
                       "Dog_2_test_segment_0469.mat",
                       "Dog_2_test_segment_0489.mat",
                       "Dog_2_test_segment_0491.mat",
                       "Dog_2_test_segment_0513.mat",
                       "Dog_2_test_segment_0529.mat",
                       "Dog_2_test_segment_0531.mat",
                       "Dog_2_test_segment_0538.mat",
                       "Dog_2_test_segment_0550.mat",
                       "Dog_2_test_segment_0568.mat",
                       "Dog_2_test_segment_0569.mat",
                       "Dog_2_test_segment_0575.mat",
                       "Dog_2_test_segment_0580.mat",
                       "Dog_2_test_segment_0585.mat",
                       "Dog_2_test_segment_0602.mat",
                       "Dog_2_test_segment_0622.mat",
                       "Dog_2_test_segment_0654.mat",
                       "Dog_2_test_segment_0689.mat",
                       "Dog_2_test_segment_0748.mat",
                       "Dog_2_test_segment_0754.mat",
                       "Dog_2_test_segment_0757.mat",
                       "Dog_2_test_segment_0775.mat",
                       "Dog_2_test_segment_0779.mat",
                       "Dog_2_test_segment_0780.mat",
                       "Dog_2_test_segment_0794.mat",
                       "Dog_2_test_segment_0800.mat",
                       "Dog_2_test_segment_0817.mat",
                       "Dog_2_test_segment_0821.mat",
                       "Dog_2_test_segment_0826.mat",
                       "Dog_2_test_segment_0831.mat",
                       "Dog_2_test_segment_0853.mat",
                       "Dog_2_test_segment_0869.mat",
                       "Dog_2_test_segment_0900.mat",
                       "Dog_2_test_segment_0903.mat",
                       "Dog_2_test_segment_0911.mat",
                       "Dog_2_test_segment_0913.mat",
                       "Dog_2_test_segment_0917.mat",
                       "Dog_2_test_segment_0934.mat",
                       "Dog_2_test_segment_0942.mat",
                       "Dog_2_test_segment_0945.mat",
                       "Dog_2_test_segment_0956.mat",
                       "Dog_2_test_segment_0962.mat",
                       "Dog_2_test_segment_0996.mat",
                       "Dog_3_test_segment_0018.mat",
                       "Dog_3_test_segment_0047.mat",
                       "Dog_3_test_segment_0065.mat",
                       "Dog_3_test_segment_0086.mat",
                       "Dog_3_test_segment_0097.mat",
                       "Dog_3_test_segment_0105.mat",
                       "Dog_3_test_segment_0178.mat",
                       "Dog_3_test_segment_0198.mat",
                       "Dog_3_test_segment_0210.mat",
                       "Dog_3_test_segment_0237.mat",
                       "Dog_3_test_segment_0258.mat",
                       "Dog_3_test_segment_0274.mat",
                       "Dog_3_test_segment_0278.mat",
                       "Dog_3_test_segment_0315.mat",
                       "Dog_3_test_segment_0324.mat",
                       "Dog_3_test_segment_0326.mat",
                       "Dog_3_test_segment_0380.mat",
                       "Dog_3_test_segment_0396.mat",
                       "Dog_3_test_segment_0400.mat",
                       "Dog_3_test_segment_0417.mat",
                       "Dog_3_test_segment_0426.mat",
                       "Dog_3_test_segment_0461.mat",
                       "Dog_3_test_segment_0473.mat",
                       "Dog_3_test_segment_0489.mat",
                       "Dog_3_test_segment_0498.mat",
                       "Dog_3_test_segment_0509.mat",
                       "Dog_3_test_segment_0557.mat",
                       "Dog_3_test_segment_0601.mat",
                       "Dog_3_test_segment_0627.mat",
                       "Dog_3_test_segment_0671.mat",
                       "Dog_3_test_segment_0673.mat",
                       "Dog_3_test_segment_0680.mat",
                       "Dog_3_test_segment_0685.mat",
                       "Dog_3_test_segment_0718.mat",
                       "Dog_3_test_segment_0722.mat",
                       "Dog_3_test_segment_0771.mat",
                       "Dog_3_test_segment_0776.mat",
                       "Dog_3_test_segment_0813.mat",
                       "Dog_3_test_segment_0827.mat",
                       "Dog_3_test_segment_0830.mat",
                       "Dog_3_test_segment_0833.mat",
                       "Dog_3_test_segment_0861.mat",
                       "Dog_4_test_segment_0006.mat",
                       "Dog_4_test_segment_0032.mat",
                       "Dog_4_test_segment_0055.mat",
                       "Dog_4_test_segment_0094.mat",
                       "Dog_4_test_segment_0109.mat",
                       "Dog_4_test_segment_0131.mat",
                       "Dog_4_test_segment_0168.mat",
                       "Dog_4_test_segment_0182.mat",
                       "Dog_4_test_segment_0239.mat",
                       "Dog_4_test_segment_0240.mat",
                       "Dog_4_test_segment_0249.mat",
                       "Dog_4_test_segment_0259.mat",
                       "Dog_4_test_segment_0267.mat",
                       "Dog_4_test_segment_0275.mat",
                       "Dog_4_test_segment_0288.mat",
                       "Dog_4_test_segment_0301.mat",
                       "Dog_4_test_segment_0305.mat",
                       "Dog_4_test_segment_0309.mat",
                       "Dog_4_test_segment_0329.mat",
                       "Dog_4_test_segment_0352.mat",
                       "Dog_4_test_segment_0353.mat",
                       "Dog_4_test_segment_0371.mat",
                       "Dog_4_test_segment_0392.mat",
                       "Dog_4_test_segment_0393.mat",
                       "Dog_4_test_segment_0398.mat",
                       "Dog_4_test_segment_0426.mat",
                       "Dog_4_test_segment_0433.mat",
                       "Dog_4_test_segment_0438.mat",
                       "Dog_4_test_segment_0446.mat",
                       "Dog_4_test_segment_0448.mat",
                       "Dog_4_test_segment_0475.mat",
                       "Dog_4_test_segment_0478.mat",
                       "Dog_4_test_segment_0485.mat",
                       "Dog_4_test_segment_0538.mat",
                       "Dog_4_test_segment_0559.mat",
                       "Dog_4_test_segment_0573.mat",
                       "Dog_4_test_segment_0591.mat",
                       "Dog_4_test_segment_0634.mat",
                       "Dog_4_test_segment_0638.mat",
                       "Dog_4_test_segment_0646.mat",
                       "Dog_4_test_segment_0694.mat",
                       "Dog_4_test_segment_0723.mat",
                       "Dog_4_test_segment_0741.mat",
                       "Dog_4_test_segment_0745.mat",
                       "Dog_4_test_segment_0746.mat",
                       "Dog_4_test_segment_0751.mat",
                       "Dog_4_test_segment_0752.mat",
                       "Dog_4_test_segment_0819.mat",
                       "Dog_4_test_segment_0852.mat",
                       "Dog_4_test_segment_0853.mat",
                       "Dog_4_test_segment_0875.mat",
                       "Dog_4_test_segment_0887.mat",
                       "Dog_4_test_segment_0906.mat",
                       "Dog_4_test_segment_0927.mat",
                       "Dog_4_test_segment_0943.mat",
                       "Dog_4_test_segment_0980.mat",
                       "Dog_4_test_segment_0983.mat",
                       "Dog_5_test_segment_0026.mat",
                       "Dog_5_test_segment_0028.mat",
                       "Dog_5_test_segment_0035.mat",
                       "Dog_5_test_segment_0050.mat",
                       "Dog_5_test_segment_0067.mat",
                       "Dog_5_test_segment_0077.mat",
                       "Dog_5_test_segment_0098.mat",
                       "Dog_5_test_segment_0105.mat",
                       "Dog_5_test_segment_0111.mat",
                       "Dog_5_test_segment_0119.mat",
                       "Dog_5_test_segment_0163.mat",
                       "Dog_5_test_segment_0166.mat",
                       "Patient_1_test_segment_0007.mat",
                       "Patient_1_test_segment_0019.mat",
                       "Patient_1_test_segment_0023.mat",
                       "Patient_1_test_segment_0031.mat",
                       "Patient_1_test_segment_0092.mat",
                       "Patient_1_test_segment_0093.mat",
                       "Patient_1_test_segment_0124.mat",
                       "Patient_1_test_segment_0148.mat",
                       "Patient_1_test_segment_0158.mat",
                       "Patient_1_test_segment_0171.mat",
                       "Patient_1_test_segment_0173.mat",
                       "Patient_1_test_segment_0177.mat",
                       "Patient_2_test_segment_0034.mat",
                       "Patient_2_test_segment_0037.mat",
                       "Patient_2_test_segment_0039.mat",
                       "Patient_2_test_segment_0042.mat",
                       "Patient_2_test_segment_0045.mat",
                       "Patient_2_test_segment_0046.mat",
                       "Patient_2_test_segment_0060.mat",
                       "Patient_2_test_segment_0062.mat",
                       "Patient_2_test_segment_0086.mat",
                       "Patient_2_test_segment_0087.mat",
                       "Patient_2_test_segment_0094.mat",
                       "Patient_2_test_segment_0110.mat",
                       "Patient_2_test_segment_0117.mat",
                       "Patient_2_test_segment_0124.mat")
# Make into list object
preictfiles <- as.list(preictfiles)

# Set options for navigating folder structure
types=c('Dog_1','Dog_2','Dog_3','Dog_4','Dog_5','Patient_1','Patient_2')

# Loop for all patient folder 
for (mytype in types){   
        
        # Set working directory here
        datadir=paste0("C:/Users/ian_wa.IMAGINE/Downloads/New Data/",mytype,"/Test_Data")
        setwd(datadir)
     
        # Prefix any files that are listed as preictal, based on the list provided 
        for (myfiles in preictfiles){
          file.rename(myfiles, paste0("Preict_",myfiles))
          }

        # Get files with Preict prefix
        myfilelist=list.files(datadir, pattern="[P][r]")
        # Files are labelled as preictal with an index of 2000 and up
        # Calculate the number of files
        numfile <- length(myfilelist) + 1999
        # Rename as preictal (to and from in rename must match)
        file.rename(myfilelist, paste0(mytype,"_preictal_segment_", 2000:numfile,".mat"))
        
        # Get all other files and label interictal
        myfilelist=list.files(datadir, pattern="[t][e][s]")
        # Files are labelled as interictal with an index of 2000 and up
        # Calculate the number of files
        numfile <- length(myfilelist) + 1999
        # Rename as interictal (to and from in rename must match)
        file.rename(myfilelist, paste0(mytype,"_interictal_segment_", 2000:numfile,".mat"))
}





#Error checking
for (patients in types){  


myfilelist=dir(datadir, "*.mat")

for (filename in listfiles){
  tryCatch({
    a <- readMat(filename)
  }, error=function(e){cat("ERROR :",conditionMessage(e), filename, "\n")})
}