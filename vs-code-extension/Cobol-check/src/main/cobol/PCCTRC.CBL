       IDENTIFICATION DIVISION.
       PROGRAM-ID.  PCCTRC.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLEANED-TRANSACTIONS
                  ASSIGN       TO TRANC
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS TRANC-STATUS.
           SELECT CONVERTED-TRANSACTIONS
                  ASSIGN       TO TRANU
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS TRANU-STATUS.
           SELECT REPT
                  ASSIGN       TO REPT
                  ORGANIZATION IS SEQUENTIAL
                  ACCESS MODE  IS SEQUENTIAL
                  FILE STATUS  IS REPORT-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       
       FD  CLEANED-TRANSACTIONS
           RECORD CONTAINS 57 CHARACTERS
           BLOCK CONTAINS 0 CHARACTERS
           RECORDING MODE IS F.
       01  TRANC-RECORD           PIC X(57).
      *
       FD  CONVERTED-TRANSACTIONS
           RECORD CONTAINS 57 CHARACTERS
           BLOCK CONTAINS 0 CHARACTERS
           RECORDING MODE IS F.
       01  TRANU-RECORD           PIC X(57).
      *
       FD  REPT
           RECORD CONTAINS 70 CHARACTERS
           BLOCK CONTAINS 0 CHARACTERS
           RECORDING MODE IS F.
       01  REPORT-RECORD          PIC X(70).
      *
       WORKING-STORAGE SECTION.
       01  WS-AREA.
           05  TRANC-ENDFILE      PIC X.
               88 TRANC-AT-ENDFILE      VALUE 'Y'.
               88 TRANC-NOT-AT-ENDFILE  VALUE 'N'.
       01  FILE-STATUS.
           05  TRANC-STATUS.
               10  TRANC-STATUS-1      PIC X.
                   88  TRANC-OK   VALUE '0'.
                   88  TRANC-EOF  VALUE '1'.
               10  TRANC-STATUS-2       PIC X.
           05  TRANU-STATUS.
               10  TRANU-STATUS-1      PIC X.
                   88  TRANU-OK   VALUE '0'.
                   88  TRANU-EOF  VALUE '1'.
               10  TRANU-STATUS-2       PIC X.
           05  REPORT-STATUS.
               10  REPORT-STATUS-1     PIC X.
                   88  REPORT-OK  VALUE '0'.
                   88  REPORT-EOF VALUE '1'.
               10  REPORT-STATUS-2      PIC X.
           05  ABEND-INFO.
               10  WS-ILBOABN0          PIC X(8) VALUE 'ILBOABN0'.
               10  WS-ABEND-CODE-1000   PIC S9(4) COMP VALUE 1000.
               10  WS-ABEND-CODE-1001   PIC S9(4) COMP VALUE 1001.
               10  WS-ABEND-CODE-1002   PIC S9(4) COMP VALUE 1002.
               10  WS-ABEND-CODE-1003   PIC S9(4) COMP VALUE 1003.
               10  WS-ABEND-CODE-1004   PIC S9(4) COMP VALUE 1004.
               10  WS-RETURN-CODE       PIC S9(4) COMP.
               10  WS-RETURN-CODE-D     PIC S9(4).
               10  BANNER               PIC X(60)
                   VALUE '************************************'.
      *
      * PCC (Phase Change Credit) Daily Transaction File - 57 bytes
      *
       01  DAILY-TRANSACTION-RECORD.
           05  DTRAN-ACC-NBR               PIC 9(16).
           05  DTRAN-DT-VAL                PIC 9(8).
           05  DTRAN-TM-VAL                PIC 9(6).
           05  DTRAN-CHG-AMT               PIC 9(8)V99.
           05  DTRAN-TYPE                  PIC X.
               88 DTRAN-DEBIT              VALUE 'D'.
               88 DTRAN-CREDIT             VALUE 'C'.
           05  DTRAN-CUR-CD                PIC XXX.
           05  DTRAN-RET-CD                PIC 9(6).
           05  DTRAN-ATH-CD                PIC 9(6).
           05  DTRAN-TYP-CD                PIC X.
               88 DTRAN-PAPER-ENTRY        VALUE 'P'.
               88 DTRAN-MANUAL-ENTER       VALUE 'M'.
               88 DTRAN-CARD-SWIPED        VALUE 'S'.
               88 DTRAN-CHIP-READ          VALUE 'C'.
       01  CURRENCY-CONVERSION.
           05  SYMBOL-AFN     PIC 9(4)V9(6). *> Afghanistan Afghani
           05  SYMBOL-ALL     PIC 9(4)V9(6). *> Albania Lek
           05  SYMBOL-DZD     PIC 9(4)V9(6). *> Algeria Dinar
           05  SYMBOL-AOA     PIC 9(4)V9(6). *> Angola Kwanza
           05  SYMBOL-ARS     PIC 9(4)V9(6). *> Argentina Peso
           05  SYMBOL-AMD     PIC 9(4)V9(6). *> Armenia Dram
           05  SYMBOL-AWG     PIC 9(4)V9(6). *> Aruba Florin
           05  SYMBOL-AUD     PIC 9(4)V9(6). *> Australia Dollar
           05  SYMBOL-ATS     PIC 9(4)V9(6). *> Austria Schilling (EURO)
           05  SYMBOL-BEF     PIC 9(4)V9(6). *> Belgium Franc (EURO)
           05  SYMBOL-AZN     PIC 9(4)V9(6). *> Azerbaijan New Manat
           05  SYMBOL-BSD     PIC 9(4)V9(6). *> Bahamas Dollar
           05  SYMBOL-BHD     PIC 9(4)V9(6). *> Bahrain Dinar
           05  SYMBOL-BDT     PIC 9(4)V9(6). *> Bangladesh Taka
           05  SYMBOL-BBD     PIC 9(4)V9(6). *> Barbados Dollar
           05  SYMBOL-BYR     PIC 9(4)V9(6). *> Belarus Ruble
           05  SYMBOL-BZD     PIC 9(4)V9(6). *> Belize Dollar
           05  SYMBOL-BMD     PIC 9(4)V9(6). *> Bermuda Dollar
           05  SYMBOL-BTN     PIC 9(4)V9(6). *> Bhutan Ngultrum
           05  SYMBOL-BOB     PIC 9(4)V9(6). *> Bolivia Boliviano
           05  SYMBOL-BAM     PIC 9(4)V9(6). *> Bosnia Mark
           05  SYMBOL-BWP     PIC 9(4)V9(6). *> Botswana Pula
           05  SYMBOL-BRL     PIC 9(4)V9(6). *> Brazil Real
           05  SYMBOL-GBP     PIC 9(4)V9(6). *> Great Britain Pound
           05  SYMBOL-BND     PIC 9(4)V9(6). *> Brunei Dollar
           05  SYMBOL-BGN     PIC 9(4)V9(6). *> Bulgaria Lev
           05  SYMBOL-BIF     PIC 9(4)V9(6). *> Burundi Franc
           05  SYMBOL-XOF     PIC 9(4)V9(6). *> CFA Franc BCEAO
           05  SYMBOL-XAF     PIC 9(4)V9(6). *> CFA Franc BEAC
           05  SYMBOL-XPF     PIC 9(4)V9(6). *> CFP Franc
           05  SYMBOL-KHR     PIC 9(4)V9(6). *> Cambodia Riel
           05  SYMBOL-CAD     PIC 9(4)V9(6). *> Canada Dollar
           05  SYMBOL-CVE     PIC 9(4)V9(6). *> Cape Verde Escudo
           05  SYMBOL-KYD     PIC 9(4)V9(6). *> Cayman Islands Dollar
           05  SYMBOL-CLP     PIC 9(4)V9(6). *> Chili Peso
           05  SYMBOL-CNY     PIC 9(4)V9(6). *> China Yuan/Renminbi
           05  SYMBOL-COP     PIC 9(4)V9(6). *> Colombia Peso
           05  SYMBOL-KMF     PIC 9(4)V9(6). *> Comoros Franc
           05  SYMBOL-CDF     PIC 9(4)V9(6). *> Congo Franc
           05  SYMBOL-CRC     PIC 9(4)V9(6). *> Costa Rica Colon
           05  SYMBOL-HRK     PIC 9(4)V9(6). *> Croatia Kuna
           05  SYMBOL-CUC     PIC 9(4)V9(6). *> Cuba Convertible Peso
           05  SYMBOL-CUP     PIC 9(4)V9(6). *> Cuba Peso
           05  SYMBOL-CYP     PIC 9(4)V9(6). *> Cyprus Pound (EURO)
           05  SYMBOL-CZK     PIC 9(4)V9(6). *> Czech Koruna
           05  SYMBOL-DKK     PIC 9(4)V9(6). *> Denmark Krone
           05  SYMBOL-DJF     PIC 9(4)V9(6). *> Djibouti Franc
           05  SYMBOL-DOP     PIC 9(4)V9(6). *> Dominican Republich Peso
           05  SYMBOL-XCD     PIC 9(4)V9(6). *> East Caribbean Dollar
           05  SYMBOL-EGP     PIC 9(4)V9(6). *> Egypt Pound
           05  SYMBOL-SVC     PIC 9(4)V9(6). *> El Salvador Colon
           05  SYMBOL-EEK     PIC 9(4)V9(6). *> Estonia Kroon (EURO)
           05  SYMBOL-ETB     PIC 9(4)V9(6). *> Ethiopia Birr
           05  SYMBOL-EUR     PIC 9(4)V9(6). *> Euro
           05  SYMBOL-FKP     PIC 9(4)V9(6). *> Falkland Islands Pound
           05  SYMBOL-FIM     PIC 9(4)V9(6). *> Finland Markka  (EURO)
           05  SYMBOL-FJD     PIC 9(4)V9(6). *> Fiji Dollar
           05  SYMBOL-GMD     PIC 9(4)V9(6). *> Gambia Dalasi
           05  SYMBOL-GEL     PIC 9(4)V9(6). *> Georgia Lari
           05  SYMBOL-DMK     PIC 9(4)V9(6). *> Germany Mark (EURO)
           05  SYMBOL-GHS     PIC 9(4)V9(6). *> Ghana New Cedi
           05  SYMBOL-GIP     PIC 9(4)V9(6). *> Gibraltar Pound
           05  SYMBOL-GRD     PIC 9(4)V9(6). *> Greece Drachma (EURO)
           05  SYMBOL-GTQ     PIC 9(4)V9(6). *> Guatemala Quetzal
           05  SYMBOL-GNF     PIC 9(4)V9(6). *> Guinea Franc
           05  SYMBOL-GYD     PIC 9(4)V9(6). *> Guyana Dollar
           05  SYMBOL-HTG     PIC 9(4)V9(6). *> Haiti Gourde
           05  SYMBOL-HNL     PIC 9(4)V9(6). *> Honduras Lempira
           05  SYMBOL-HKD     PIC 9(4)V9(6). *> Hong Kong Dollar
           05  SYMBOL-HUF     PIC 9(4)V9(6). *> Hungary Forint
           05  SYMBOL-ISK     PIC 9(4)V9(6). *> Iceland Krona
           05  SYMBOL-INR     PIC 9(4)V9(6). *> India Rupee
           05  SYMBOL-IDR     PIC 9(4)V9(6). *> Indonesia Rupiah
           05  SYMBOL-IRR     PIC 9(4)V9(6). *> Iran Rial
           05  SYMBOL-IQD     PIC 9(4)V9(6). *> Iraq Dinar
           05  SYMBOL-IED     PIC 9(4)V9(6). *> Ireland Pound (EURO)
           05  SYMBOL-ILS     PIC 9(4)V9(6). *> Israel New Shekel
           05  SYMBOL-ITL     PIC 9(4)V9(6). *> Italy Lira  (EURO)
           05  SYMBOL-JMD     PIC 9(4)V9(6). *> Jamaica Dollar
           05  SYMBOL-JPY     PIC 9(4)V9(6). *> Japan Yen
           05  SYMBOL-JOD     PIC 9(4)V9(6). *> Jordan Dinar
           05  SYMBOL-KZT     PIC 9(4)V9(6). *> Kazakhstan Tenge
           05  SYMBOL-KES     PIC 9(4)V9(6). *> Kenya Shilling
           05  SYMBOL-KWD     PIC 9(4)V9(6). *> Kuwait Dinar
           05  SYMBOL-KGS     PIC 9(4)V9(6). *> Kyrgyzstan Som
           05  SYMBOL-LAK     PIC 9(4)V9(6). *> Laos Kip
           05  SYMBOL-LVL     PIC 9(4)V9(6). *> Latvia Lats   (EURO)
           05  SYMBOL-LBP     PIC 9(4)V9(6). *> Lebanon Pound
           05  SYMBOL-LSL     PIC 9(4)V9(6). *> Lesotho Loti
           05  SYMBOL-LRD     PIC 9(4)V9(6). *> Liberia Dollar
           05  SYMBOL-LYD     PIC 9(4)V9(6). *> Libya Dinar
           05  SYMBOL-LTL     PIC 9(4)V9(6). *> Lithuania Litas  (EURO)
           05  SYMBOL-LUF     PIC 9(4)V9(6). *> Luxembourg Franc (EURO)
           05  SYMBOL-MOP     PIC 9(4)V9(6). *> Macau Pataca
           05  SYMBOL-MKD     PIC 9(4)V9(6). *> Macedonia Denar
           05  SYMBOL-MGA     PIC 9(4)V9(6). *> Malagasy Ariary
           05  SYMBOL-MWK     PIC 9(4)V9(6). *> Malawi Kwacha
           05  SYMBOL-MYR     PIC 9(4)V9(6). *> Malaysia Ringgit
           05  SYMBOL-MVR     PIC 9(4)V9(6). *> Maldives Rufiyaa
           05  SYMBOL-MTL     PIC 9(4)V9(6). *> Malta Lira  (EURO)
           05  SYMBOL-MRO     PIC 9(4)V9(6). *> Mauritania Ouguiya
           05  SYMBOL-MUR     PIC 9(4)V9(6). *> Mauritius Rupee
           05  SYMBOL-MXN     PIC 9(4)V9(6). *> Mexico Peso
           05  SYMBOL-MDL     PIC 9(4)V9(6). *> Moldova Leu
           05  SYMBOL-MNT     PIC 9(4)V9(6). *> Mongolia Tugrik
           05  SYMBOL-MAD     PIC 9(4)V9(6). *> Morocco Dirham
           05  SYMBOL-MZN     PIC 9(4)V9(6). *> Mozambique New Metical
           05  SYMBOL-MMK     PIC 9(4)V9(6). *> Myanmar Kyat
           05  SYMBOL-ANG     PIC 9(4)V9(6). *> NL Antilles Guilder
           05  SYMBOL-NAD     PIC 9(4)V9(6). *> Namibia Dollar
           05  SYMBOL-NPR     PIC 9(4)V9(6). *> Nepal Rupee
           05  SYMBOL-NLG     PIC 9(4)V9(6). *> Netherlands Guilder (EURO)
           05  SYMBOL-NZD     PIC 9(4)V9(6). *> New Zealand Dollar
           05  SYMBOL-NIO     PIC 9(4)V9(6). *> Nicaragua Cordoba Oro
           05  SYMBOL-NGN     PIC 9(4)V9(6). *> Nigeria Naira
           05  SYMBOL-KPW     PIC 9(4)V9(6). *> North Korea Won
           05  SYMBOL-NOK     PIC 9(4)V9(6). *> Norway Kroner
           05  SYMBOL-OMR     PIC 9(4)V9(6). *> Oman Rial
           05  SYMBOL-PKR     PIC 9(4)V9(6). *> Pakistan Rupee
           05  SYMBOL-PAB     PIC 9(4)V9(6). *> Panama Balboa
           05  SYMBOL-PGK     PIC 9(4)V9(6). *> Papua New Guinea Kina
           05  SYMBOL-PYG     PIC 9(4)V9(6). *> Paraguay Guarani
           05  SYMBOL-PEN     PIC 9(4)V9(6). *> Peru Nuevo Sol
           05  SYMBOL-PHP     PIC 9(4)V9(6). *> Philippines Peso
           05  SYMBOL-PLN     PIC 9(4)V9(6). *> Poland Zloty
           05  SYMBOL-PTE     PIC 9(4)V9(6). *> Portugal Escudo (EURO)
           05  SYMBOL-QAR     PIC 9(4)V9(6). *> Qatar Rial
           05  SYMBOL-RON     PIC 9(4)V9(6). *> Romania New Lei
           05  SYMBOL-RUB     PIC 9(4)V9(6). *> Russia Rouble
           05  SYMBOL-RWF     PIC 9(4)V9(6). *> Rwanda Franc
           05  SYMBOL-WST     PIC 9(4)V9(6). *> Samoa Tala
           05  SYMBOL-STD     PIC 9(4)V9(6). *> Sao Tome/Principe Dobra
           05  SYMBOL-SAR     PIC 9(4)V9(6). *> Saudi Arabia Riyal
           05  SYMBOL-RSD     PIC 9(4)V9(6). *> Serbia Dinar
           05  SYMBOL-SCR     PIC 9(4)V9(6). *> Seychelles Rupee
           05  SYMBOL-SLL     PIC 9(4)V9(6). *> Sierra Leone Leone
           05  SYMBOL-SGD     PIC 9(4)V9(6). *> Singapore Dollar
           05  SYMBOL-SKK     PIC 9(4)V9(6). *> Slovakia Koruna (EURO)
           05  SYMBOL-SIT     PIC 9(4)V9(6). *> Slovenia Tolar (EURO)
           05  SYMBOL-SBD     PIC 9(4)V9(6). *> Solomon Islands Dollar
           05  SYMBOL-SOS     PIC 9(4)V9(6). *> Somali Shilling
           05  SYMBOL-ZAR     PIC 9(4)V9(6). *> South Africa Rand
           05  SYMBOL-KRW     PIC 9(4)V9(6). *> South Korea Won
           05  SYMBOL-ESP     PIC 9(4)V9(6). *> Spain Peseta (EURO)
           05  SYMBOL-LKR     PIC 9(4)V9(6). *> Sri Lanka Rupee
           05  SYMBOL-SHP     PIC 9(4)V9(6). *> St Helena Pound
           05  SYMBOL-SDG     PIC 9(4)V9(6). *> Sudan Pound
           05  SYMBOL-SRD     PIC 9(4)V9(6). *> Suriname Dollar
           05  SYMBOL-SZL     PIC 9(4)V9(6). *> Swaziland Lilangeni
           05  SYMBOL-SEK     PIC 9(4)V9(6). *> Sweden Krona
           05  SYMBOL-CHF     PIC 9(4)V9(6). *> Switzerland Franc
           05  SYMBOL-SYP     PIC 9(4)V9(6). *> Syria Pound
           05  SYMBOL-TWD     PIC 9(4)V9(6). *> Taiwan Dollar
           05  SYMBOL-TZS     PIC 9(4)V9(6). *> Tanzania Shilling
           05  SYMBOL-THB     PIC 9(4)V9(6). *> Thailand Baht
           05  SYMBOL-TOP     PIC 9(4)V9(6). *> Tonga Pa'anga
           05  SYMBOL-TTD     PIC 9(4)V9(6). *> Trinidad/Tobago Dollar
           05  SYMBOL-TND     PIC 9(4)V9(6). *> Tunisia Dinar
           05  SYMBOL-TRY     PIC 9(4)V9(6). *> Turkish New Lira
           05  SYMBOL-TMM     PIC 9(4)V9(6). *> Turkmenistan Manat
           05  SYMBOL-USD     PIC 9(4)V9(6). *> USA Dollar
           05  SYMBOL-UGX     PIC 9(4)V9(6). *> Uganda Shilling
           05  SYMBOL-UAH     PIC 9(4)V9(6). *> Ukraine Hryvnia
           05  SYMBOL-UYU     PIC 9(4)V9(6). *> Uruguay Peso
           05  SYMBOL-AED     PIC 9(4)V9(6). *> United Arab Emirates Dirham
           05  SYMBOL-VUV     PIC 9(4)V9(6). *> Vanuatu Vatu
           05  SYMBOL-VEB     PIC 9(4)V9(6). *> Venezuela Bolivar
           05  SYMBOL-VND     PIC 9(4)V9(6). *> Vietnam Dong
           05  SYMBOL-YER     PIC 9(4)V9(6). *> Yemen Rial
           05  SYMBOL-ZMK     PIC 9(4)V9(6). *> Zambia Kwacha
           05  SYMBOL-ZWD     PIC 9(4)V9(6). *> Zimbabwe Dollar
       PROCEDURE DIVISION.
           PERFORM OPEN-FILES.
           PERFORM FILL-CONVERSION-VALUES.
           IF WS-RETURN-CODE NOT = 0
               CALL WS-ILBOABN0 USING WS-ABEND-CODE-1004
           SET TRANC-NOT-AT-ENDFILE TO TRUE.
           PERFORM PROCESS-TRANSACTIONS UNTIL TRANC-AT-ENDFILE.
       PROCESS-TRANSACTIONS.
           READ CLEANED-TRANSACTIONS INTO DAILY-TRANSACTION-RECORD
               AT END SET TRANC-AT-ENDFILE TO TRUE
               NOT AT END PERFORM CONVERT-TRANSACTION THRU CONVX
           END-READ.
       CONVERT-TRANSACTION.
           IF DTRAN-CUR-CD = 'USD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-USD     *> USA Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'GBP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-GBP     *> Great Britain Pound
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'EUR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-EUR     *> Euro
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CAD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CAD     *> Canada Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'AFN'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-AFN     *> Afghanistan Afghani
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ALL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ALL     *> Albania Lek
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'DZD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-DZD     *> Algeria Dinar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'AOA'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-AOA     *> Angola Kwanza
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ARS'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ARS     *> Argentina Peso
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'AMD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-AMD     *> Armenia Dram
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'AWG'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-AWG     *> Aruba Florin
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'AUD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-AUD     *> Australia Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ATS'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ATS     *> Austria Schilling (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BEF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BEF     *> Belgium Franc (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'AZN'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-AZN     *> Azerbaijan New Manat
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BSD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BSD     *> Bahamas Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BHD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BHD     *> Bahrain Dinar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BDT'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BDT      *> Bangladesh Taka
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BBD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BBD     *> Barbados Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BYR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BYR     *> Belarus Ruble
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BZD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BZD     *> Belize Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BMD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BMD     *> Bermuda Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BTN'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BTN     *> Bhutan Ngultrum
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BOB'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BOB     *> Bolivia Boliviano
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BAM'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BAM     *> Bosnia Mark
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BWP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BWP     *> Botswana Pula
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BRL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BRL     *> Brazil Real
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BND'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BND     *> Brunei Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BGN'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BGN     *> Bulgaria Lev
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'BIF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-BIF     *> Burundi Franc
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'XOF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-XOF     *> CFA Franc BCEAO
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'XAF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-XAF     *> CFA Franc BEAC
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'XPF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-XPF     *> CFP Franc
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'KHR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-KHR     *> Cambodia Riel
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CVE'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CVE     *> Cape Verde Escudo
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'KYD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-KYD     *> Cayman Islands Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CLP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CLP     *> Chili Peso
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CNY'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CNY     *> China Yuan/Renminbi
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'COP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-COP     *> Colombia Peso
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'KMF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-KMF     *> Comoros Franc
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CDF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CDF     *> Congo Franc
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CRC'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CRC     *> Costa Rica Colon
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'HRK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-HRK     *> Croatia Kuna
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CUC'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CUC     *> Cuba Convertible Peso
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CUP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CUP     *> Cuba Peso
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CYP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CYP     *> Cyprus Pound (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CZK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CZK     *> Czech Koruna
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'DKK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-DKK     *> Denmark Krone
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'DJF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-DJF     *> Djibouti Franc
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'DOP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-DOP     *> Dominican Republich Peso
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'XCD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-XCD     *> East Caribbean Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'EGP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-EGP     *> Egypt Pound
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SVC'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SVC     *> El Salvador Colon
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'EEK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-EEK     *> Estonia Kroon (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ETB'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ETB     *> Ethiopia Birr
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'FKP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-FKP     *> Falkland Islands Pound
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'FIM'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-FIM     *> Finland Markka  (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'FJD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-FJD     *> Fiji Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'GMD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-GMD     *> Gambia Dalasi
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'GEL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-GEL     *> Georgia Lari
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'DMK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-DMK     *> Germany Mark (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'GHS'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-GHS     *> Ghana New Cedi
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'GIP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-GIP     *> Gibraltar Pound
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'GRD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-GRD     *> Greece Drachma (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'GTQ'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-GTQ     *> Guatemala Quetzal
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'GNF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-GNF     *> Guinea Franc
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'GYD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-GYD     *> Guyana Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'HTG'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-HTG     *> Haiti Gourde
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'HNL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-HNL     *> Honduras Lempira
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'HKD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-HKD     *> Hong Kong Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'HUF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-HUF     *> Hungary Forint
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ISK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ISK     *> Iceland Krona
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'INR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-INR     *> India Rupee
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'IDR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-IDR     *> Indonesia Rupiah
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'IRR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-IRR     *> Iran Rial
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'IQD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-IQD     *> Iraq Dinar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'IED'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-IED     *> Ireland Pound (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ILS'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ILS     *> Israel New Shekel
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ITL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ITL     *> Italy Lira  (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'JMD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-JMD     *> Jamaica Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'JPY'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-JPY     *> Japan Yen
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'JOD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-JOD     *> Jordan Dinar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'KZT'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-KZT     *> Kazakhstan Tenge
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'KES'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-KES     *> Kenya Shilling
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'KWD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-KWD     *> Kuwait Dinar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'KGS'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-KGS     *> Kyrgyzstan Som
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'LAK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-LAK     *> Laos Kip
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'LVL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-LVL     *> Latvia Lats   (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'LBP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-LBP     *> Lebanon Pound
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'LSL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-LSL     *> Lesotho Loti
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'LRD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-LRD     *> Liberia Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'LYD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-LYD     *> Libya Dinar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'LTL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-LTL     *> Lithuania Litas  (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'LUF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-LUF     *> Luxembourg Franc (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MOP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MOP     *> Macau Pataca
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MKD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MKD     *> Macedonia Denar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MGA'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MGA     *> Malagasy Ariary
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MWK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MWK     *> Malawi Kwacha
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MYR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MYR     *> Malaysia Ringgit
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MVR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MVR     *> Maldives Rufiyaa
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MTL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MTL     *> Malta Lira  (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MRO'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MRO     *> Mauritania Ouguiya
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MUR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MUR     *> Mauritius Rupee
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MXN'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MXN     *> Mexico Peso
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MDL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MDL     *> Moldova Leu
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MNT'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MNT     *> Mongolia Tugrik
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MAD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MAD     *> Morocco Dirham
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MZN'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MZN     *> Mozambique New Metical
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'MMK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-MMK     *> Myanmar Kyat
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ANG'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ANG     *> NL Antilles Guilder
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'NAD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-NAD     *> Namibia Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'NPR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-NPR     *> Nepal Rupee
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'NLG'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-NLG     *> Netherlands Guilder (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'NZD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-NZD     *> New Zealand Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'NIO'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-NIO     *> Nicaragua Cordoba Oro
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'NGN'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-NGN     *> Nigeria Naira
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'KPW'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-KPW     *> North Korea Won
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'NOK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-NOK     *> Norway Kroner
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'OMR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-OMR     *> Oman Rial
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'PKR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-PKR     *> Pakistan Rupee
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'PAB'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-PAB     *> Panama Balboa
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'PGK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-PGK     *> Papua New Guinea Kina
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'PYG'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-PYG     *> Paraguay Guarani
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'PEN'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-PEN     *> Peru Nuevo Sol
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'PHP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-PHP     *> Philippines Peso
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'PLN'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-PLN     *> Poland Zloty
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'PTE'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-PTE     *> Portugal Escudo (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'QAR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-QAR     *> Qatar Rial
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'RON'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-RON     *> Romania New Lei
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'RUB'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-RUB     *> Russia Rouble
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'RWF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-RWF     *> Rwanda Franc
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'WST'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-WST     *> Samoa Tala
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'STD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-STD     *> Sao Tome/Principe Dobra
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SAR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SAR     *> Saudi Arabia Riyal
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'RSD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-RSD     *> Serbia Dinar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SCR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SCR     *> Seychelles Rupee
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SLL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SLL     *> Sierra Leone Leone
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SGD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SGD     *> Singapore Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SKK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SKK     *> Slovakia Koruna (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SIT'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SIT     *> Slovenia Tolar (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SBD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SBD     *> Solomon Islands Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SOS'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SOS     *> Somali Shilling
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ZAR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ZAR     *> South Africa Rand
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'KRW'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-KRW     *> South Korea Won
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ESP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ESP     *> Spain Peseta (EURO)
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'LKR'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-LKR     *> Sri Lanka Rupee
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SHP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SHP     *> St Helena Pound
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SDG'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SDG     *> Sudan Pound
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SRD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SRD     *> Suriname Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SZL'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SZL     *> Swaziland Lilangeni
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SEK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SEK     *> Sweden Krona
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'CHF'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-CHF     *> Switzerland Franc
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'SYP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-SYP     *> Syria Pound
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'TWD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-TWD     *> Taiwan Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'TZS'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-TZS     *> Tanzania Shilling
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'THB'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-THB     *> Thailand Baht
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'TOP'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-TOP     *> Tonga Pa'anga
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'TTD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-TTD     *> Trinidad/Tobago Dollar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'TND'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-TND     *> Tunisia Dinar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'TRY'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-TRY     *> Turkish New Lira
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'TMM'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-TMM     *> Turkmenistan Manat
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'UGX'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-UGX     *> Uganda Shilling
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'UAH'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-UAH     *> Ukraine Hryvnia
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'UYU'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-UYU     *> Uruguay Peso
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'AED'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-AED     *> United Arab Emirates Dirham
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'VUV'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-VUV     *> Vanuatu Vatu
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'VEB'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-VEB     *> Venezuela Bolivar
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'VND'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-VND     *> Vietnam Dong
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'YER'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-YER     *> Yemen Rial
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ZMK'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ZMK     *> Zambia Kwacha
               GO TO CONVX-SUCCESS.
           IF DTRAN-CUR-CD = 'ZWD'
               COMPUTE DTRAN-CHG-AMT ROUNDED = DTRAN-CHG-AMT *
                       SYMBOL-ZWD     *> Zimbabwe Dollar
               GO TO CONVX-SUCCESS.
           CALL WS-ILBOABN0 USING WS-ABEND-CODE-1003.
       CONVX-SUCCESS.
           WRITE TRANU-RECORD FROM DAILY-TRANSACTION-RECORD.
       CONVX.
           EXIT.

       OPEN-FILES.
           OPEN OUTPUT REPT.
           IF NOT REPORT-OK
               CALL WS-ILBOABN0 USING WS-ABEND-CODE-1002.
           OPEN INPUT CLEANED-TRANSACTIONS.
           IF NOT TRANC-OK
               CALL WS-ILBOABN0 USING WS-ABEND-CODE-1000.
           OPEN OUTPUT CONVERTED-TRANSACTIONS.
           IF NOT TRANU-OK
               CALL WS-ILBOABN0 USING WS-ABEND-CODE-1001.
       FILL-CONVERSION-VALUES.
           MOVE 0 TO WS-RETURN-CODE.
           MOVE 1.0     TO SYMBOL-USD.  *> US Dollar
           MOVE 1.29514 TO SYMBOL-CAD.  *> Canada Dollar
           MOVE 0.85760 TO SYMBOL-EUR.  *> Euro
           MOVE 0.74929 TO SYMBOL-GBP.  *> Great Britain Pound