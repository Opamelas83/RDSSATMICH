*EXP.DETAILS: CCQU7901CS 1979 WATER SUPPLY * TWO CULTIVARS

*GENERAL
@PEOPLE
CONNOR,D.J. COCK,J.H. PALTA,J.A. PARRA,G.E.
@ADDRESS
CIAT,A.A.6713,CALI,COLOMBIA
@SITE
SANTANDER DE QUILICHAO,COLOMBIA  3.10;-76.51;990;CW
@NOTES
Rain withheld for 72 days in one treatment Published in Field Crops Research, 4:181-200; 4:285-296; 4:297-311

*TREATMENTS                        -------------FACTOR LEVELS------------
@N R O C TNAME.................... CU FL SA IC MP MI MF MR MC MT ME MH SM
 1 1 0 0 C1*E1 MCol-22;Control      1  1  0  0  1  0  1  0  1  0  0  1  1
 2 1 0 0 C1*E2 MCol-22;Reduced rai  1  1  0  0  1  0  1  0  1  0  2  1  1
 3 1 0 0 C2*E1 MMex-59;Control      2  1  0  0  1  0  1  0  1  0  0  1  1
 4 1 0 0 C2*E2 MMex-59;Reduced rai  2  1  0  0  1  0  1  0  1  0  2  1  1

*CULTIVARS
@C CR INGENO CNAME
 1 CS UC0002 MCOL-22
 2 CS UC0004 MMEX-59

*FIELDS
@L ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX  SLDP  ID_SOIL    FLNAME
 1 CCQU0001 CCQU         0     0     1     0     0    0F CLLO   260  CCQU000033 -99
@L ...........XCRD ...........YCRD .....ELEV .............AREA .SLEN .FLWR .SLAS FLHST FHDUR
 1               0               0         0                 0     0     0     0   -99   -99

*INITIAL CONDITIONS
@C   PCR ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID ICNAME
 1    BN 79115   -99     0     1     1   -99     0     0     0   100    15 -99
@C  ICBL  SH2O  SNH4  SNO3
 1     5   .47   -99   -99
 1    15  .473   -99   -99
 1    28  .491   -99   -99
 1    44   .51   -99   -99
 1    65  .527   -99   -99
 1    96  .536   -99   -99
 1   122  .549   -99   -99
 1   150  .553   -99   -99
 1   178  .541   -99   -99
 1   196  .488   -99   -99

*PLANTING DETAILS
@P PDATE EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  PLDP  PLWT  PAGE  PENV  PLPH  SPRL                        PLNAME
 1 79115   -99     1     1     V     R   100    90    20   450   -99   -99   -99    25                        -99

*FERTILIZERS (INORGANIC)
@F FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD FERNAME
 1 79115 FE001 AP001    20   100   260   500     0     0   -99 -99

*CHEMICAL APPLICATIONS
@C CDATE CHCOD CHAMT  CHME CHDEP   CHT..CHNAME
 1 79115 CH051     0 AP006    10 WEEDS  -99

*ENVIRONMENT MODIFICATIONS
@E ODATE EDAY  ERAD  EMAX  EMIN  ERAIN ECO2  EDEW  EWIND ENVNAME  
 2 79224 A   0 A   0 A   0 A   0 M0.00 A   0 A   0 A   0 
 2 79296 A   0 A   0 A   0 A   0 M1.00 A   0 A   0 A   0 

*HARVEST DETAILS
@H HDATE  HSTG  HCOM HSIZE   HPC  HBPC HNAME
 1 80056     6     H     A   100   -99 

*SIMULATION CONTROLS
@N GENERAL     NYERS NREPS START SDATE RSEED SNAME.................... SMODEL
 1 GE              1     1     P 79115  2150 WATER SUPPLY  2C*2E  (CO
@N OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2
 1 OP              Y     N     N     N     N     N     N     Y     M
@N METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT MESOM MESEV MESOL
 1 ME              M     M     E     R     S     R     R     1     G     S     2
@N MANAGEMENT  PLANT IRRIG FERTI RESID HARVS
 1 MA              R     N     R     R     R
@N OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT VBOSE CHOUT OPOUT
 1 OU              N     Y     Y     1     Y     N     Y     Y     N     N     Y     N     N

@  AUTOMATIC MANAGEMENT
@N PLANTING    PFRST PLAST PH2OL PH2OU PH2OD PSTMX PSTMN
 1 PL          79108 79122    40   100    30    40    10
@N IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF
 1 IR             30    50   100 IB001 IBI01    10     1
@N NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF
 1 NI             30    50    25 IB001 IB001
@N RESIDUES    RIPCN RTIME RIDEP
 1 RE            100     1    20
@N HARVEST     HFRST HLAST HPCNP HPCNR
 1 HA              0 80115   100     0



