* write the function down
    program define nlpakes
        if "`1'" == "?" {
            global S_1 "alfa1 alfa2 alfa"
            global alfa1=.63
            global alfa2=-.72
            global alfa=4.95
            exit
        }
        replace `1'=$alfa+$alfa1*lead_ldnpt+$alfa2*lead_ldrst+(pred_polyn-$alfa1*ldnpt-$alfa2*ldrst)+(pred_polyn-$alfa1*ldnpt-$alfa2*ldrst)^2+phat+phat^2+2*phat*(pred_polyn-$alfa1*ldnpt-$alfa2*ldrst)
    end
