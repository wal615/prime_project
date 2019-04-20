* Written by R;
*  write.foreign(df = thyroid_data_tmp, datafile = "./real_data/NHANES/hemoglobin/SAS_nhance_hemoglobin_PCB.csv",  ;

DATA  rdata ;
INFILE  "./real_data/NHANES/hemoglobin/SAS_nhance_hemoglobin_PCB.csv" 
     DSD 
     LRECL= 233 ;
INPUT
 LBXTSH
 LBXT4
 LBD199
 LBX028
 LBX044
 LBX049
 LBX052
 LBX066
 LBX074
 LBX081
 LBX087
 LBX099
 LBX101
 LBX105
 LBX110
 LBX118
 LBX126
 LBX128
 LBX138
 LBX146
 LBX149
 LBX151
 LBX153
 LBX156
 LBX157
 LBX167
 LBX169
 LBX170
 LBX172
 LBX177
 LBX178
 LBX180
 LBX183
 LBX187
 LBX189
 LBX194
 LBX195
 LBX196
 LBX206
 LBX209
;
RUN;
