#################################
###### FUNCTIONAL GENOMICS ######
# ANA PALOMA OROZCO CARSTENSEN ##
########## HOMEWORK 1 ###########
#################################

library (seqinr) # Load library 
library (Biostrings) # Load library 


### Aminoacids sequence 
Sequence <- readRNAStringSet ("Sequences/first_sequence.fasta") # Read sequence
Sequence # Print

Translation <- Biostrings::translate (Sequence) # Translate
Translation # Print

# HAPPY NEW YEAR ! 


### Counting DNA Nucleotides
## Biostrings
DNA_Sequence <- DNAString ("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") # Give the sequence
DNA_Sequence # Print

Frequency <- (alphabetFrequency (DNA_Sequence) [1:4]) # Get the frequency of the object
Frequency # Print

## R Base
DNA_Sequence_2 <- c ("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") # Give the sequence

A_F = 0 # Assign the initial values
C_F = 0 # Assign the initial values
G_F = 0 # Assign the initial values
T_F = 0 # Assign the initial values

Sequence_Length <- nchar (DNA_Sequence_2) # Obtain the length of the sequence
Sequence_Length # Print

DNA_Base <- strsplit (DNA_Sequence_2, split = NULL) # Split the sequence into individual factors
DNA_Base # Print

# Function
Nucleotides <- function (){ # Name the function
  for (i in 1 : Sequence_Length) { # For i in 1 to the total length of the sequence
    if (DNA_Base [[1]][i] == "A" ) {A_F = A_F + 1 # If the first letter is "A", then add 1 to the original value
    } else { # If not... 
      if (DNA_Base [[1]][i] == "C" ) {C_F = C_F + 1 # If the first letter is "C", then add 1 to the original value
      }
      if (DNA_Base [[1]][i] == "G" ) {G_F = G_F + 1 # If the first letter is "G", then add 1 to the original value
      }
      if (DNA_Base [[1]][i] == "T" ) {T_F = T_F + 1 # If the first letter is "T", then add 1 to the original value
      }
    }
  }
  return (print (paste ("A =", A_F, ", C =", C_F, ", G =", G_F, ", T =", T_F))) # Return the results
}

Nucleotides() # Prove the function

# Not a function 
for (i in 1 : Sequence_Length) { # For i in 1 to the total length of the sequence
    if (DNA_Base [[1]][i] == "A" ) {A_F = A_F + 1 # If the first letter is "A", then add 1 to the original value
    } else { # If not... 
      if (DNA_Base [[1]][i] == "C" ) {C_F = C_F + 1 # If the first letter is "C", then add 1 to the original value
      }
      if (DNA_Base [[1]][i] == "G" ) {G_F = G_F + 1 # If the first letter is "G", then add 1 to the original value
      }
      if (DNA_Base [[1]][i] == "T" ) {T_F = T_F + 1 # If the first letter is "T", then add 1 to the original value
      }
    }
  }

print (paste ("A =", A_F, ", C =", C_F, ", G =", G_F, ", T =", T_F)) # Return the results


### Computing GC Content
## Biostrings
GC_Content <- (letterFrequency (DNA_Sequence, letters = "GC", as.prob = TRUE))*100 # Use the function to obtain the percentage of "G" and "C"
GC_Content # Print

## R Base
GC_Sequence <- as.vector (DNA_Sequence) # Convert into a vector
Sequence_Width <- length (GC_Sequence) # Calculate the width of the sequences
Sequence_Width # Print

GC_P <- (Sequence_Width - Frequency [1] - Frequency [4]) * 100 / Sequence_Width # Calculate the CG percentage
GC_P # Print

