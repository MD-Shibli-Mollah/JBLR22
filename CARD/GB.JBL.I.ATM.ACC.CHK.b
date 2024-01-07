* @ValidationCode : MjotMTc3NjIwMDQxNjpDcDEyNTI6MTcwNDM0NTg2MDExNzpuYXppaGFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 04 Jan 2024 11:24:20
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : nazihar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

*-----------------------------------------------------------------------------
***Develop By: Robiul Islam **********
**** Date: 02 FEB 2017 *********
* <Rating>1396</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE GB.JBL.I.ATM.ACC.CHK
*-----------------------------------------------------------------------------
* Modification History : RETROFIT from TAFC to TAFJ
* 1)
* Date :30/12/2023
* Modification Description :
* Modified By : MD Shibli Mollah - NITSL
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
* Subroutine Description: This routine is used for ATM CARD MANAGEMENT SYSTEM
* Subroutine Type: INPUT
* Attached To    : EB.JBL.ATM.CARD.MGT
* Attached As    : INPUT ROUTINE
* TAFC Routine Name :ATM.ACC.CHK - R09
*-----------------------------------------------------------------------------
    !PROGRAM ATM.ACC.CHK

    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT I_F.ACCOUNT
    $USING AC.AccountOpening
* $INSERT I_F.ATM.CARD.MGT
    $INSERT I_F.EB.JBL.ATM.CARD.MGT
* $INSERT I_F.CUSTOMER
    $USING ST.Customer
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.Foundation
    $USING EB.ErrorProcessing

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
RETURN

INIT:
    FN.AC = "F.ACCOUNT"
    F.AC = ""
    FN.AC.HIS = "F.ACCOUNT$HIS"
    F.AC.HIS = ""
    FN.CUS="F.CUSTOMER"
    F.CUS=""
    
    Y.ID.COMPANY = EB.SystemTables.getIdCompany()
    
    Y.ACCOUNT= EB.SystemTables.getRNew(EB.ATM19.ACCT.NO)
    Y.CATEGORY.ALLOW = 1001:@FM:6001:@FM:6004:@FM:6019

    Y.CELLPHONE=""
    Y.CLIENTPROPF=""
    Y.CLIENTPROPM=""
    Y.BIRTHDAY=""
    Y.ADDRESS=""
    Y.CORADDRESS=""
    
    FN.ATM = "F.EB.JBL.ATM.CARD.MGT"
    F.ATM = ""
    
    FN.ATM.NAU = "F.EB.JBL.ATM.CARD.MGT$NAU"
    F.ATM.NAU = ""
    
    Y.CARD.TYPE = EB.SystemTables.getRNew(EB.ATM19.CARD.TYPE)
    Y.CARD.NAME = EB.SystemTables.getRNew(EB.ATM19.CARD.NAME)
    Y.ID.NEW = EB.SystemTables.getIdNew()
    R.AC.REC = ""
    Y.FLAG = 0
    Y.MODE.FLAG = 0
    
OPENFILES:
*EB.DataAccess.Opf(YnameIn, YnameOut)
    EB.DataAccess.Opf(FN.AC,F.AC)
    EB.DataAccess.Opf(FN.AC.HIS,F.AC.HIS)
    EB.DataAccess.Opf(FN.CUS,F.CUS)
    EB.DataAccess.Opf(FN.ATM,F.ATM)
    EB.DataAccess.Opf(FN.ATM.NAU,F.ATM.NAU)
RETURN

PROCESS:

    IF Y.ACCOUNT NE "" THEN
* EB.DataAccess.FRead(Fileid, VKey, Rec, FFileid, Er)
        EB.DataAccess.FRead(FN.AC,Y.ACCOUNT,R.AC.REC,F.AC,Y.ERR)
*        Y.CATEGORY.ACC = R.AC.REC<AC.CATEGORY>
        Y.CATEGORY.ACC = R.AC.REC<AC.AccountOpening.Account.Category>
*        Y.JOINT.HOLDER=R.AC.REC<AC.JOINT.HOLDER>
        Y.JOINT.HOLDER = R.AC.REC<AC.AccountOpening.Account.JointHolder>
*        Y.POSTING.RESTRICT=R.AC.REC<AC.POSTING.RESTRICT>
        Y.POSTING.RESTRICT = R.AC.REC<AC.AccountOpening.Account.PostingRestrict>
*        Y.INACTIVE.MARKER=R.AC.REC<AC.INACTIV.MARKER>
        Y.INACTIVE.MARKER = R.AC.REC<AC.AccountOpening.Account.InactivMarker>
*        Y.CUSTOMER = R.AC.REC<AC.CUSTOMER>
        Y.CUSTOMER = R.AC.REC<AC.AccountOpening.Account.Customer>
*-----------------------ACCOUNT LT need to check--------------------------------------------*
        EB.Foundation.MapLocalFields('ACCOUNT', 'LT.MODE.OF.OPER', Y.MODE.OF.OPER.POS)
        EB.Foundation.MapLocalFields('ACCOUNT', 'LT.ACCOUNT.NATURE', Y.ACCOUNT.NATURE.POS)
 
*        CALL GET.LOC.REF('ACCOUNT','MODE.OF.OPER',Y.MODE.OF.OPER)
*        CALL GET.LOC.REF('ACCOUNT','ACCOUNT.NATURE',Y.ACCOUNT.NATURE)
        
*        Y.AC.MODE = UPCASE(R.AC.REC<AC.LOCAL.REF,Y.MODE.OF.OPER>)
        Y.AC.MODE = UPCASE(R.AC.REC<AC.AccountOpening.Account.LocalRef, Y.MODE.OF.OPER.POS>)
*        Y.AC.NATURE = UPCASE(R.AC.REC<AC.LOCAL.REF,Y.ACCOUNT.NATURE>)
        Y.AC.NATURE = UPCASE(R.AC.REC<AC.AccountOpening.Account.LocalRef, Y.ACCOUNT.NATURE.POS>)
        
        IF Y.AC.MODE EQ "SELF" OR Y.AC.MODE EQ "SINGLE" THEN
            Y.MODE.FLAG = 1
        END
        
        EB.DataAccess.FRead(FN.CUS,Y.CUSTOMER,R.CUS.REC,F.CUS,Y.ERR)

*        Y.SEX=R.CUS.REC<EB.CUS.GENDER>
        Y.SEX = R.CUS.REC<ST.Customer.Customer.EbCusGender>
*        Y.TITLE = R.CUS.REC<EB.CUS.TITLE>
        Y.TITLE = R.CUS.REC<ST.Customer.Customer.EbCusTitle>

*        CALL GET.LOC.REF('CUSTOMER','CUS.COMU.ADD',Y.COMU.ADD)
*        CALL GET.LOC.REF('CUSTOMER','CUS.COMU.VILL',Y.COMU.VILL)
*        CALL GET.LOC.REF('CUSTOMER','CUS.COMU.PO',Y.COMU.PO)
*        CALL GET.LOC.REF('CUSTOMER','CUS.COMU.UPZ',Y.COMU.UPZ)
*        CALL GET.LOC.REF('CUSTOMER','CUS.COMU.DIST',Y.COMU.DIST)
*        CALL GET.LOC.REF('CUSTOMER','FATHER.NAME',Y.FATHER.NAME)
*        CALL GET.LOC.REF('CUSTOMER','MOTHER.NAME',Y.MOTHER.NAME)
*        CALL GET.LOC.REF('CUSTOMER','SMS.ALERT',Y.SMS.ALT)

        FLD.POS = ""
        LOCAL.FIELDS = ""
        LOCAL.FIELDS = "LT.CUS.COMU.ADD":@VM:"LT.CUS.COMU.VILL":@VM:"CUS.COMU.PO":@VM:"LT.CUS.COMU.UPZ":@VM:"CUS.COMU.DIST":@VM:"LT.FATHER.NAME":@VM:"LT.MOTHER.NAME":@VM:"LT.SMS.ALERT"
        EB.Foundation.MapLocalFields("CUSTOMER", LOCAL.FIELDS, FLD.POS)
        Y.LT.CUS.COMU.ADD.POS = FLD.POS<1,1>
        Y.LT.CUS.COMU.VILL.POS = FLD.POS<1,2>
        Y.LT.CUS.COMU.PO.POS = FLD.POS<1,3>
        Y.LT.CUS.COMU.UPZ.POS = FLD.POS<1,4>
        Y.LT.CUS.COMU.DIST.POS = FLD.POS<1,5>
        Y.LT.FATHER.NAME.POS = FLD.POS<1,6>
        Y.LT.MOTHER.NAME.POS = FLD.POS<1,7>
        Y.LT.SMS.ALERT.POS = FLD.POS<1,8>
        
        Y.TOTAL.LT = R.CUS.REC<ST.Customer.Customer.EbCusLocalRef>
    
        Y.LT.CUS.COMU.ADD = Y.TOTAL.LT<1, Y.LT.CUS.COMU.ADD.POS>
        Y.LT.CUS.COMU.VILL = Y.TOTAL.LT<1, Y.LT.CUS.COMU.VILL.POS>
        Y.LT.CUS.COMU.PO = Y.TOTAL.LT<1, Y.LT.CUS.COMU.PO.POS>
        Y.LT.CUS.COMU.UPZ = Y.TOTAL.LT<1, Y.LT.CUS.COMU.UPZ.POS>
        Y.LT.CUS.COMU.DIST = Y.TOTAL.LT<1, Y.LT.CUS.COMU.DIST.POS>
        Y.LT.FATHER.NAME = Y.TOTAL.LT<1, Y.LT.FATHER.NAME.POS>
        Y.LT.MOTHER.NAME = Y.TOTAL.LT<1, Y.LT.MOTHER.NAME.POS>
        Y.LT.SMS.ALERT = Y.TOTAL.LT<1, Y.LT.SMS.ALERT.POS>
        
*        Y.CELLPHONE=R.CUS.REC<EB.CUS.SMS.1>
        Y.CELLPHONE = R.CUS.REC<ST.Customer.Customer.EbCusSmsOne>

*        Y.CLIENTPROPF=R.CUS.REC<EB.CUS.LOCAL.REF,Y.FATHER.NAME>
        Y.CLIENTPROPF = Y.LT.FATHER.NAME
*        Y.CLIENTPROPM=R.CUS.REC<EB.CUS.LOCAL.REF,Y.MOTHER.NAME>
        Y.CLIENTPROPM = Y.LT.MOTHER.NAME
*        Y.SMS.ALERT=R.CUS.REC<EB.CUS.LOCAL.REF,Y.SMS.ALT>
        Y.SMS.ALERT = Y.LT.SMS.ALERT

*        Y.BIRTHDAY=R.CUS.REC<EB.CUS.DATE.OF.BIRTH>
        Y.BIRTHDAY = R.CUS.REC<ST.Customer.Customer.EbCusDateOfBirth>
*        Y.ADDRESS=R.CUS.REC<EB.CUS.STREET>
        Y.ADDRESS = R.CUS.REC<ST.Customer.Customer.EbCusStreet>
*        Y.TOWN.COUNTRY=R.CUS.REC<EB.CUS.TOWN.COUNTRY>
        Y.TOWN.COUNTRY = R.CUS.REC<ST.Customer.Customer.EbCusTownCountry>

*        Y.CORADDRESS=R.CUS.REC<EB.CUS.LOCAL.REF,Y.COMU.ADD>
        Y.CORADDRESS = Y.LT.CUS.COMU.ADD
*        Y.COM.VILL=R.CUS.REC<EB.CUS.LOCAL.REF,Y.COMU.VILL>
        Y.COM.VILL = Y.LT.CUS.COMU.VILL
*        Y.COM.PO=R.CUS.REC<EB.CUS.LOCAL.REF,Y.COMU.PO>
        Y.COM.PO = Y.LT.CUS.COMU.PO
*        Y.COM.UPZ=R.CUS.REC<EB.CUS.LOCAL.REF,Y.COMU.UPZ>
        Y.COM.UPZ = Y.LT.CUS.COMU.UPZ
*        Y.COM.DIST=R.CUS.REC<EB.CUS.LOCAL.REF,Y.COMU.DIST>
        Y.COM.DIST = Y.LT.CUS.COMU.DIST

        Y.CATEGORY.COUNT = DCOUNT(Y.CATEGORY.ALLOW, @FM)
        Y.CATE.CHK = 0
        
        FOR I=1 TO Y.CATEGORY.COUNT
            Y.CATE = FIELD(Y.CATEGORY.ALLOW, @FM, I)
            
            IF Y.CATE EQ Y.CATEGORY.ACC THEN
                Y.CATE.CHK = 1
                BREAK
            END
        NEXT I

        Y.MSG = "PLEASE GIVE INPUT-"
        
        IF R.AC.REC NE "" AND Y.SEX EQ "" THEN
            Y.MSG = Y.MSG :" Gender,"
            Y.FLAG = 1
        END
        
        IF R.AC.REC NE "" AND Y.TITLE EQ "" THEN
            Y.MSG = Y.MSG :" Title ,"
            Y.FLAG=1
        END
        
        IF R.AC.REC NE "" AND Y.CELLPHONE EQ "" THEN
            Y.MSG = Y.MSG :" Mobile Phone Numbers,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.SMS.ALERT NE "Y" THEN
            Y.MSG = Y.MSG :" SMS ALERT,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.BIRTHDAY EQ "" THEN
            Y.MSG = Y.MSG :" Birthday,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.CLIENTPROPF EQ "" THEN
            Y.MSG = Y.MSG :" Father Name,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.CLIENTPROPM EQ "" THEN
            Y.MSG = Y.MSG :" Mother Name,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.ADDRESS EQ "" THEN
            Y.MSG = Y.MSG :"Present Address,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.TOWN.COUNTRY EQ "" THEN
            Y.MSG = Y.MSG :"Present Address-TOWN,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.CORADDRESS EQ "" THEN
            Y.MSG = Y.MSG :" Communication Address,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.COM.VILL EQ "" THEN
            Y.MSG = Y.MSG :" Communication Address-VILLAGE,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.COM.PO EQ "" THEN
            Y.MSG = Y.MSG :" Communication Address-POLISH STATION,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.COM.UPZ EQ "" THEN
            Y.MSG = Y.MSG :" Communication Address-UPAZALA,"
            Y.FLAG=1
        END
        IF R.AC.REC NE "" AND Y.COM.DIST EQ "" THEN
            Y.MSG = Y.MSG :" Communication Address-DISTRICT,"
            Y.FLAG=1
        END
    
*--------------------- Generate Error ------------------------------------*
        IF R.AC.REC NE "" AND Y.INACTIVE.MARKER NE "" THEN
*            ETEXT =Y.ACCOUNT: " INACTIVE ACCOUNT"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext(Y.ACCOUNT: " INACTIVE ACCOUNT")
            EB.ErrorProcessing.StoreEndError()
        END

        ELSE IF Y.CATEGORY.ACC NE "" AND Y.CATE.CHK EQ 0 THEN
*            ETEXT ="INVALID CATEGORY"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("INVALID CATEGORY")
            EB.ErrorProcessing.StoreEndError()
        END

        ELSE IF LEN(Y.CARD.NAME) LT 3 THEN
*            ETEXT ="CARD NAME MINIMUM 3 CHARACTERS"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("CARD NAME MUST BE MINIMUM OF 3 CHARACTERS")
            EB.ErrorProcessing.StoreEndError()
        END

* ELSE IF R.AC.REC NE "" AND Y.ID.COMPANY NE R.AC.REC<AC.CO.CODE>  THEN
    
        ELSE IF R.AC.REC NE "" AND Y.ID.COMPANY NE R.AC.REC<AC.AccountOpening.Account.CoCode> THEN
*            ETEXT =Y.ACCOUNT: " INVALID ACCOUNT"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext(Y.ACCOUNT: " INVALID ACCOUNT")
            EB.ErrorProcessing.StoreEndError()

        END
        ELSE IF R.AC.REC NE "" AND Y.JOINT.HOLDER NE "" THEN
*            ETEXT ="JOINT ACCOUNT NOT ALLOW"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("JOINT ACCOUNT IS NOT ALLOWED")
            EB.ErrorProcessing.StoreEndError()
        END
        ELSE IF Y.CUSTOMER EQ "" THEN
*            ETEXT ="CUSTOMER MISSING"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("CUSTOMER IS MISSING")
            EB.ErrorProcessing.StoreEndError()
        END
        ELSE IF NOT(ISDIGIT(Y.ACCOUNT)) THEN
*            ETEXT ="INVALID ACCOUNT"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("INVALID ACCOUNT")
            EB.ErrorProcessing.StoreEndError()
        END
        ELSE IF Y.AC.MODE NE "" AND Y.MODE.FLAG EQ 0 THEN
*            ETEXT ="INDIVIDUAL ACCOUNT ALLOW MODE OF OPERATION MUST BE SELF"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("INDIVIDUAL ACCOUNT ALLOW MODE OF OPERATION MUST BE SELF")
            EB.ErrorProcessing.StoreEndError()
        END
        ELSE IF  Y.AC.NATURE NE "" AND Y.AC.NATURE NE "INDIVIDUAL ACCOUNT" THEN
*            ETEXT ="INDIVIDUAL ACCOUNT ALLOW ACCOUNT NATURE MUST BE INDIVIDUAL ACCOUNT"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext("INDIVIDUAL ACCOUNT ALLOW ACCOUNT NATURE MUST BE INDIVIDUAL ACCOUNT")
            EB.ErrorProcessing.StoreEndError()
        END


        ELSE IF Y.POSTING.RESTRICT NE "" AND R.AC.REC NE "" THEN
*            ETEXT = Y.ACCOUNT: " IS POSTING RESTRICTED"
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext(Y.ACCOUNT: " IS POSTING RESTRICTED")
            EB.ErrorProcessing.StoreEndError()
        END
        ELSE IF Y.FLAG EQ 1 THEN
            Y.MSG = Y.MSG :" Field This Customer(":Y.CUSTOMER:") Module"
*            ETEXT = Y.MSG
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext(Y.MSG)
            EB.ErrorProcessing.StoreEndError()
        END
        ELSE IF LEN(Y.CELLPHONE<1,1>) LE 10 THEN
            Y.MSG = "Invalid Mobile Number This Customer(":Y.CUSTOMER:") Module"
*            ETEXT = Y.MSG
*            CALL STORE.END.ERROR
            EB.SystemTables.setEtext(Y.MSG)
            EB.ErrorProcessing.StoreEndError()
        END
        ELSE IF R.AC.REC NE "" THEN
            SEL.CMD = "SELECT ":FN.ATM:" WITH ACCT.NO EQ " : Y.ACCOUNT
            CALL EB.READLIST(SEL.CMD,SEL.LIST,"",NO.OF.RECORD,RET.CODE)
            LOOP
                REMOVE Y.ATM.ID FROM SEL.LIST SETTING Y.POS
            WHILE Y.ATM.ID:Y.POS
                EB.DataAccess.FRead(FN.ATM,Y.ATM.ID,R.ATM.REC,F.ATM,Y.ERR)
                IF R.ATM.REC<EB.ATM19.CARD.TYPE> EQ Y.CARD.TYPE AND Y.ATM.ID NE Y.ID.NEW THEN
*                    ETEXT = Y.ACCOUNT: " IS ALREADY ASSIGN THIS TYPE CARD"
*                    CALL STORE.END.ERROR
                    EB.SystemTables.setEtext(Y.ACCOUNT: " IS ALREADY ASSIGN THIS TYPE CARD")
                    EB.ErrorProcessing.StoreEndError()
                    BREAK
                END
            REPEAT

            SEL.CMD = "SELECT ":FN.ATM.NAU:" WITH ACCT.NO EQ " : Y.ACCOUNT
            CALL EB.READLIST(SEL.CMD,SEL.LIST,"",NO.OF.RECORD,RET.CODE)
            LOOP
                REMOVE Y.ATM.ID FROM SEL.LIST SETTING Y.POS
            WHILE Y.ATM.ID:Y.POS
                EB.DataAccess.FRead(FN.ATM.NAU,Y.ATM.ID,R.ATM.REC,F.ATM.NAU,Y.ERR)
                IF R.ATM.REC<EB.ATM19.CARD.TYPE> EQ Y.CARD.TYPE AND Y.ATM.ID NE Y.ID.NEW THEN
*                    ETEXT = Y.ACCOUNT: " IS ALREADY ASSIGN THIS TYPE CARD"
*                    CALL STORE.END.ERROR
                    EB.SystemTables.setEtext(Y.ACCOUNT: " IS ALREADY ASSIGN THIS TYPE CARD")
                    EB.ErrorProcessing.StoreEndError()
                    BREAK
                END
            REPEAT

        END

        Y.COUNT = DCOUNT(Y.CARD.NAME," ")
        FOR I=1 TO Y.COUNT
            IF NOT(ALPHA(FIELD(Y.CARD.NAME," ",I))) THEN
*                ETEXT ="PLEASE REMOVE SPECIAL CHARACTER FROM CARD NAME"
*                CALL STORE.END.ERROR
                EB.SystemTables.setEtext("PLEASE REMOVE SPECIAL CHARACTER FROM CARD NAME")
                EB.ErrorProcessing.StoreEndError()
            END
        NEXT I
    
**********************------------BANK COMMENT-----------------------------******************
        !ELSE IF Y.CARD.NAME NE "" THEN
        !  ETEXT = Y.ACCOUNT: " CATEGORY INVALID"
        ! CALL STORE.END.ERROR

        !END
        !   ELSE IF R.AC.REC NE "" THEN
        !      CALL GET.LOC.REF('ACCOUNT','AC.ATM.CARD.NUM',Y.AC.ATM.CARD.NUM)
        !     Y.NO.OF.CARD=DCOUNT(R.AC.REC<AC.LOCAL.REF,Y.AC.ATM.CARD.NUM>,@SM)

        !    FOR I = 1 TO Y.NO.OF.CARD
        !       Y.ATM.ID =R.AC.REC<AC.LOCAL.REF,Y.AC.ATM.CARD.NUM,I>
        !      EB.DataAccess.FRead(FN.ATM,Y.ATM.ID,R.ATM.REC,F.ATM,Y.ERR)
        !     IF R.ATM.REC<EB.ATM19.CARD.TYPE> EQ Y.CARD.TYPE THEN
        !        ETEXT = Y.ACCOUNT: " IS ALREADY ASSIGN THIS TYPE CARD"
        !       CALL STORE.END.ERROR
        !      BREAK
        ! END
        !EB.DataAccess.FRead(FN.ATM.NAU,Y.ATM.ID,R.ATM.REC,F.ATM.NAU,Y.ERR)
        !IF R.ATM.REC<EB.ATM19.CARD.TYPE> EQ Y.CARD.TYPE THEN
        !   ETEXT = Y.ACCOUNT: " IS ALREADY ASSIGN THIS TYPE CARD"
        !  CALL STORE.END.ERROR
        ! BREAK
        !END
        ! NEXT I
        !END
*******************------------BANK COMMENT--------------END---------------******************

    END
RETURN

END
