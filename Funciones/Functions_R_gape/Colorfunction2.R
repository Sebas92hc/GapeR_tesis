## Here are the color functions used in the program interact and stadisticprogram.



pinnacolor <- function(Y) { ## Choose the color of the pinna data and pinna legend
      if(Y == "Pinna.1") {"red"} else if(Y == "Pinna.5") {"azure4"} else if
      (Y == "Pinna.2") {"green"} else if(Y == "Pinna.6") {"gold"} else if
      (Y == "Pinna.3") {"darkgoldenrod1"} else if(Y == "Pinna.7") {"darkorange"} else if
      (Y == "Pinna.4") {"cyan"} else if(Y == "Pinna.8") {"deepskyblue"} else if
      (Y == "Julianday") {"black"} else {"black"}
}



stadisticcolorlegend <- function(Y){
      if(Y == "Lowf_1A") {"red4"} else if(Y == "Lowf_1BW") {"azure4"} else if
      (Y == "Lowf_2A") {"darkgreen"} else if(Y == "Lowf_2BW") {"gold4"} else if
      (Y == "Lowf_3A") {"darkgoldenrod4"} else if(Y == "Lowf_3BW") {"darkorange4"} else if
      (Y == "Lowf_4A") {"darkblue"} else if(Y == "Lowf_4BW") {"deepskyblue4"} else if
      (Y == "Lowf_1BS") {"azure4"} else if(Y == "Lowf_2BS") {"gold4"} else if
      (Y == "Lowf_3BS") {"darkorange4"} else if (Y == "Lowf_4BS") {"deepskyblue4"} else if
      (Y == "Highf_1A") {"red4"} else if(Y == "Highf_1BW") {"azure4"} else if
      (Y == "Highf_2A") {"darkgreen"} else if(Y == "Highf_2BW") {"gold4"} else if
      (Y == "Highf_3A") {"darkgoldenrod4"} else if(Y == "Highf_3BW") {"darkorange4"} else if
      (Y == "Highf_4A") {"darkblue"} else if(Y == "Highf_4BW") {"deepskyblue4"} else if
      (Y == "Highf_1BS") {"azure4"} else if(Y == "Highf_2BS") {"gold4"} else if
      (Y == "Highf_3BS") {"darkorange4"} else if (Y == "Highf_4BS") {"deepskyblue4"} else if
      (Y == "Julianday") {"black"} else {"black"}
}



stadisticolor <- function(Y) {
      if(Y == "LowF_Pinna1A") {"red4"} else if(Y == "LowF_Pinna1B") {"azure4"} else if
      (Y == "LowF_Pinna2A") {"darkgreen"} else if(Y == "LowF_Pinna2B") {"gold4"} else if
      (Y == "LowF_Pinna3A") {"darkgoldenrod4"} else if(Y == "LowF_Pinna3B") {"darkorange4"} else if
      (Y == "LowF_Pinna4A") {"darkblue"} else if(Y == "LowF_Pinna4B") {"deepskyblue4"} else if
      (Y == "Julianday") {"black"} else {"black"}
      #green, cyan, red, darkgoldenrod1 y 4
}


linecolor <- function(Y) { ## Choose the color of the sensors data
      if(Y == "Temp.A") {"red4"} else if(Y == "Temp.B") {"red4"} else if
      (Y == "pH.A") {"green4"} else if(Y == "pH.B") {"green4"} else if
      (Y == "Ox.A") {"deepskyblue"} else if(Y == "Ox.B") {"deepskyblue"} else if
      (Y == "Sal.A") {"bisque3"} else if(Y == "Sal.B") {"bisque3"} else if
      (Y == "Julianday") {"black"} else if(Y == "Temp.AF") {"red"} else if
      (Y == "Temp.BF") {"red"} else if (Y == "Light") {"Yellow3"} else if 
      (Y == "Food") {"chartreuse3"} else {"black"}
}


legendlty <- function(Y){ ## Choose the type of line in the sensors data
      if(Y == "Temp.A") {1} else if(Y == "Temp.B") {2} else if
      (Y == "pH.A") {1} else if(Y == "pH.B") {2} else if
      (Y == "Ox.A") {1} else if(Y == "Ox.B") {2} else if
      (Y == "Sal.A") {1} else if(Y == "Sal.B") {2} else if
      (Y == "Julianday") {1} else if(Y == "Temp.AF") {1} else if
      (Y == "Temp.BF") {2} else {1}
}


axiscolor <- function(ac) {
      "black"
}


legendcolor <- function(lc) { ## Choose the color for the sensors legend
      if (lc == "Temp.A") {"red4"} else if(lc == "red4") {
            "indianred2"} else if(lc == "pH.A") {"green4"} else if(lc == "pH.B") {
                  "green4"} else if(lc == "Ox.A") {"deepskyblue"} else if(lc == "Ox.B") {
                        "deepskyblue"} else if(lc == "Sal.A") {"bisque3"} else if(lc == "Sal.B") {
                              "bisque3"} else if(lc == "Julianday") {"black"} else if(lc == "Temp.AF") {"red"} 
      else if(lc == "Temp.BF") {"red"} else if (lc == "Light") {"Yellow3"} else {"black"}
}