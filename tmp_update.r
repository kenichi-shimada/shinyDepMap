# x
 [1] "cl.info"       "df"            "df.0"          "df.1"         
 [5] "eids.ess"      "mems"          "nm.msigdb.ess" "thres"        
 [9] "x"             "xl"            "yl"           

# cl.info # done
#    DepMap_ID              CCLE_Name             Aliases COSMIC_ID Sanger.ID
# 1 ACH-000007  LS513_LARGE_INTESTINE               LS513    907795       569
# 2 ACH-000009 C2BBE1_LARGE_INTESTINE              C2BBe1    910700      2104
# 3 ACH-000012            HCC827_LUNG              HCC827   1240146       354
# 4 ACH-000015          NCIH1581_LUNG NCI-H1581;NCI-H2077    908471      1237
# 5 ACH-000017           SKBR3_BREAST             SK-BR-3        NA        NA
# 6 ACH-000018      T24_URINARY_TRACT                 T24    724812      1455
#           Primary.Disease
# 1 Colon/Colorectal Cancer
# 2 Colon/Colorectal Cancer
# 3             Lung Cancer
# 4             Lung Cancer
# 5           Breast Cancer
# 6          Bladder Cancer
#                                            Subtype.Disease Gender Source ind
# 1                                          Colon Carcinoma   Male          7
# 2                                     Colon Adenocarcinoma   Male   ATCC   9
# 3       Non-Small Cell Lung Cancer (NSCLC), Adenocarcinoma Female   ATCC  12
# 4 Non-Small Cell Lung Cancer (NSCLC), Large Cell Carcinoma   Male   ATCC  15
# 5                                                Carcinoma Female  NIBRI  17
# 6                                                Carcinoma Female   ATCC  18

# head(df) 15137 x 8 #
    eid efficacy selectivity       d is.ess    sym       syms.all
1     1   -0.099       0.243 #3888C0  FALSE   A1BG       A1BG (1)
2 29974   -0.094       0.211 #DFECF7  FALSE   A1CF   A1CF (29974)
3     2   -0.154       0.204 #EAF2FA  FALSE    A2M        A2M (2)
4 53947   -0.113       0.233 #79B6D9  FALSE A4GALT A4GALT (53947)
5 51146   -0.050       0.178 #7FB9DA  FALSE  A4GNT  A4GNT (51146)
6  8086   -0.491       0.305 #083573   TRUE   AAAS    AAAS (8086)
                                   text
1       A1BG (1)<br />e:-0.099, s:0.243
2   A1CF (29974)<br />e:-0.094, s:0.211
3        A2M (2)<br />e:-0.154, s:0.204
4 A4GALT (53947)<br />e:-0.113, s:0.233
5   A4GNT (51146)<br />e:-0.05, s:0.178
6    AAAS (8086)<br />e:-0.491, s:0.305

# df.0 13296 x 8 (text is empty) df -> df.0 (non.ess)
    eid efficacy selectivity       d is.ess    sym       syms.all text
1     1   -0.099       0.243 #3888C0  FALSE   A1BG       A1BG (1)     
2 29974   -0.094       0.211 #DFECF7  FALSE   A1CF   A1CF (29974)     
3     2   -0.154       0.204 #EAF2FA  FALSE    A2M        A2M (2)     
4 53947   -0.113       0.233 #79B6D9  FALSE A4GALT A4GALT (53947)     
5 51146   -0.050       0.178 #7FB9DA  FALSE  A4GNT  A4GNT (51146)     
6 65985   -0.077       0.230 #4896C8  FALSE   AACS   AACS (65985)     

# df.1 1841 x 10 (mem, lik) df -> df.1 (ess)
    eid efficacy selectivity       d is.ess   sym      syms.all
1  8086   -0.491       0.305 #083573   TRUE  AAAS   AAAS (8086)
2    14   -0.805       0.471 #08326E   TRUE  AAMP     AAMP (14)
3    16   -1.112       0.359 #08326F   TRUE  AARS     AARS (16)
4 57505   -0.668       0.458 #08336F   TRUE AARS2 AARS2 (57505)
5 26574   -0.570       0.276 #08316D   TRUE  AATF  AATF (26574)
6  6059   -1.261       0.580 #08306B   TRUE ABCE1  ABCE1 (6059)
                                                         text mem  lik
1  AAAS (8086)<br />clust:43, lik:0.83<br />e:-0.491, s:0.305  43 0.83
2     AAMP (14)<br />clust:1, lik:0.23<br />e:-0.805, s:0.471   1 0.23
3     AARS (16)<br />clust:8, lik:0.54<br />e:-1.112, s:0.359   8 0.54
4 AARS2 (57505)<br />clust:1, lik:0.94<br />e:-0.668, s:0.458   1 0.94
5   AATF (26574)<br />clust:2, lik:0.19<br />e:-0.57, s:0.276   2 0.19
6  ABCE1 (6059)<br />clust:11, lik:0.68<br />e:-1.261, s:0.58  11 0.68

# eids.ess 1841  #done
  AAAS (8086)     AAMP (14)     AARS (16) AARS2 (57505)  AATF (26574) 
       "8086"          "14"          "16"       "57505"       "26574" 
 ABCE1 (6059) 
       "6059" 

# mems 1841 x 11
    eid   sym mem  lik       x       y      syms.all
1  8086  AAAS  43 0.83   5.194 -16.465   AAAS (8086)
2    14  AAMP   1 0.23  12.238  -8.069     AAMP (14)
3    16  AARS   8 0.54 -25.135  16.629     AARS (16)
4 57505 AARS2   1 0.94  37.099  -9.725 AARS2 (57505)
5 26574  AATF   2 0.19   1.979  -1.588  AATF (26574)
6  6059 ABCE1  11 0.68  -9.167 -10.613  ABCE1 (6059)
                                                         text efficacy
1  AAAS (8086)<br />clust:43, lik:0.83<br />e:-0.491, s:0.305   -0.491
2     AAMP (14)<br />clust:1, lik:0.23<br />e:-0.805, s:0.471   -0.805
3     AARS (16)<br />clust:8, lik:0.54<br />e:-1.112, s:0.359   -1.112
4 AARS2 (57505)<br />clust:1, lik:0.94<br />e:-0.668, s:0.458   -0.668
5   AATF (26574)<br />clust:2, lik:0.19<br />e:-0.57, s:0.276   -0.570
6  ABCE1 (6059)<br />clust:11, lik:0.68<br />e:-1.261, s:0.58   -1.261
  selectivity       d
1       0.305 #3585BF
2       0.471 #5BA3CF
3       0.359 #3F8FC4
4       0.458 #CEE0F1
5       0.276 #519CCB
6       0.580 #509BCB

# nm.msigdb.ess 1911
[1] "GO_POSITIVE_REGULATION_OF_VIRAL_TRANSCRIPTION"           
[2] "GO_CARDIAC_CHAMBER_DEVELOPMENT"                          
[3] "GO_DNA_DEPENDENT_DNA_REPLICATION_MAINTENANCE_OF_FIDELITY"
[4] "GO_CIRCADIAN_RHYTHM"                                     
[5] "GO_POSITIVE_REGULATION_OF_KINASE_ACTIVITY"               
[6] "GO_NEGATIVE_REGULATION_OF_EPITHELIAL_CELL_PROLIFERATION" 

# thres
- 0.47

# xl
-1.762  0.338

# yl
0.122 0.983

