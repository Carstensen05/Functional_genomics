#################################
###### FUNCTIONAL GENOMICS ######
# ANA PALOMA OROZCO CARSTENSEN ##
########## HOMEWORK 0 ###########
#################################


## Aminoacids sequence 
Sequence <- readRNAStringSet ("Libraries/first_sequence.fasta") # Read sequence
Sequence # Print

Translation <- Biostrings::translate (Sequence) # Translate
Translation # Print

# HAPPY NEW YEAR ! 


## Counting DNA Nucleotides
DNA_Sequence <- DNAString ("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") # Give the sequence
DNA_Sequence # Print

Frequency <- alphabetFrequency (DNA_Sequence) # Get the frequency of the object
Frequency # Print

## Computing GC Content
GC_Sequence <- as.vector (DNA_Sequence) # Convert into a vector
Sequence_Width <- length (GC_Sequence) # Calculate the width of the sequences
print (Sequence_Width) # Print


GC_P <- (Sequence_Width - Frequency [1] - Frequency [4]) * 100 / Sequence_Width # Calculate the CG percentage
print (GC_P) # Print

