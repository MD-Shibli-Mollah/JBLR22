* @ValidationCode : MjoxNDAyNDM1OTc6Q3AxMjUyOjE3MDQyNjU1NzIzMTU6bmF6aWhhcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 03 Jan 2024 13:06:12
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
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* THIS ROUTINE USE FOR UPDATE DATA OF ALL VERSION
* Developed By: Md. Robiul Islam
*Deploy Date: 12 JAN 2017
*-----------------------------------------------------------------------------
SUBROUTINE GB.JBL.I.ATM.ISSUE.UPDATE
*-----------------------------------------------------------------------------
* Modification History :
* 1)
* Date :31/12/2023
* Modification Description : RETROFIT from TAFC to TAFJ
* Modified By : MD Shibli Mollah - NITSL
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
* Subroutine Description: This routine is used for ATM CARD MANAGEMENT SYSTEM
* Subroutine Type: INPUT
* Attached To    : EB.JBL.ATM.CARD.MGT
* Attached As    : INPUT ROUTINE
* TAFC Routine Name :ATM.ACC.CHK - R09
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT GLOBUS.BP I_F.ACCOUNT
    $USING AC.AccountOpening
    $INSERT I_F.EB.JBL.ATM.CARD.MGT
* $INSERT GLOBUS.BP I_F.FUNDS.TRANSFER
    $USING FT.Contract
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.Foundation
    $USING EB.ErrorProcessing
    $USING EB.TransactionControl
    $USING EB.API

    FN.AC = "F.ACCOUNT"
    F.AC = ""
    FN.ATM = "F.EB.JBL.ATM.CARD.MGT"
    F.ATM = ""
    FN.ATM.NAU = "F.EB.JBL.ATM.CARD.MGT$NAU"
    F.ATM.NAU = ""
    FN.ATM.HIS = "F.EB.JBL.ATM.CARD.MGT$HIS"
    F.ATM.HIS = ""

    FN.FT.NAU = "F.FUNDS.TRANSFER$NAU"
    F.FT.NAU = ""
    FN.FT = "F.FUNDS.TRANSFER"
    F.FT = ""
    FN.FT.HIS = "F.FUNDS.TRANSFER$HIS"
    F.FT.HIS = ""
    
* EB.DataAccess.Opf(YnameIn, YnameOut)
    EB.DataAccess.Opf(FN.ATM,F.ATM)
    EB.DataAccess.Opf(FN.ATM.NAU,F.ATM.NAU)
    EB.DataAccess.Opf(FN.ATM.HIS,F.ATM.HIS)
    EB.DataAccess.Opf(FN.FT.NAU,F.FT.NAU)
    EB.DataAccess.Opf(FN.FT,F.FT)
    EB.DataAccess.Opf(FN.FT.HIS,F.FT.HIS)

* Y.ACCOUNT = R.NEW(EB.ATM19.ACCT.NO)
    Y.ACCOUNT = EB.SystemTables.getRNew(EB.ATM19.ACCT.NO)
    
    EB.DataAccess.Opf(FN.AC,F.AC)
* EB.DataAccess.FRead(Fileid, VKey, Rec, FFileid, Er)
    EB.DataAccess.FRead(FN.AC,Y.ACCOUNT,R.AC.REC,F.AC,Y.ERR)
*    Y.CATEGORY = R.AC.REC<AC.CATEGORY>
    Y.CATEGORY = R.AC.REC<AC.AccountOpening.Account.Category>
*    Y.AC.COMPANY=R.AC.REC<AC.CO.CODE>
    Y.AC.COMPANY = R.AC.REC<AC.AccountOpening.Account.CoCode>
    Y.TIME.STAMP = TIMEDATE()
    
*---------------Assigning to new Var------------------------------------------------*
    Y.CARD.STATUS = EB.SystemTables.getRNew(EB.ATM19.CARD.STATUS)
    Y.ATM.REQ.TYPE = EB.SystemTables.getRNew(EB.ATM19.REQUEST.TYPE)
    Y.ATM.ISS.TIME = EB.SystemTables.getRNew(EB.ATM19.ISSUE.TIME)
    Y.CARD.CLOSE.DATE = EB.SystemTables.getRNew(EB.ATM19.CARD.CLOSE.DATE)
    Y.ATM.ATTRIBUTE4 = EB.SystemTables.getRNew(EB.ATM19.ATTRIBUTE4)
    Y.VFUNCTION = EB.SystemTables.getVFunction()
    Y.TODAY = EB.SystemTables.getToday()
    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()
    Y.IDCOMPANY = EB.SystemTables.getIdCompany()
*---------------Assigning to new Var---------------END---------------------------------*
    
*    IF R.NEW(EB.ATM19.CARD.STATUS)="PENDING" AND R.NEW(EB.ATM19.REQUEST.TYPE)="ISSUE" AND V$FUNCTION EQ 'I' THEN
*        R.NEW(EB.ATM19.ISSUE.TIME)=Y.TIME.STAMP[1,2]:Y.TIME.STAMP[4,2]
*    END
*    IF R.NEW(EB.ATM19.CARD.STATUS)="APPROVED" AND R.NEW(EB.ATM19.REQUEST.TYPE)="CLOSE" AND V$FUNCTION EQ 'I' THEN
*
*        R.NEW(EB.ATM19.APPROVED.DATE)=TODAY
*    END
    IF Y.CARD.STATUS EQ "PENDING" AND Y.ATM.REQ.TYPE EQ "ISSUE" AND Y.VFUNCTION EQ 'I' THEN
        EB.SystemTables.setRNew(EB.ATM19.ISSUE.TIME, Y.TIME.STAMP[1,2]:Y.TIME.STAMP[4,2])
    END
    IF Y.CARD.STATUS EQ "APPROVED" AND Y.ATM.REQ.TYPE EQ "CLOSE" AND Y.VFUNCTION EQ 'I' THEN
        EB.SystemTables.setRNew(EB.ATM19.APPROVED.DATE, Y.TODAY)
    END
    
* R.NEW(EB.ATM19.ACCT.CATEGORY)=Y.CATEGORY
    EB.SystemTables.setRNew(EB.ATM19.ACCT.CATEGORY, Y.CATEGORY)
    
* Y.REQUEST="EB.JBL.ATM.CARD.MGT":PGM.VERSION
    Y.REQUEST = "EB.JBL.ATM.CARD.MGT":Y.PGM.VERSION

    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,REISSUE" AND Y.VFUNCTION EQ 'I' THEN
*        R.NEW(EB.ATM19.REQUEST.TYPE)="REISSUE"
        EB.SystemTables.setRNew(EB.ATM19.REQUEST.TYPE, "REISSUE")
*        R.NEW(EB.ATM19.CARD.STATUS)="PENDING"
        EB.SystemTables.setRNew(EB.ATM19.CARD.STATUS, "PENDING")
*        R.NEW(EB.ATM19.RE.ISSUE.TIME)=Y.TIME.STAMP[1,2]:Y.TIME.STAMP[4,2]
        EB.SystemTables.setRNew(EB.ATM19.RE.ISSUE.TIME, Y.TIME.STAMP[1,2]:Y.TIME.STAMP[4,2])
*        R.NEW(EB.ATM19.RE.ISSUE.DATE)=TODAY
        EB.SystemTables.setRNew(EB.ATM19.RE.ISSUE.DATE, Y.TODAY)

*---------------Assigning to new Var-------------------------------------*
        Y.REISSUE.REASON = EB.SystemTables.getRNew(EB.ATM19.REISSUE.REASON)
        Y.ISSUE.WAIVE.CHARGE = EB.SystemTables.getRNew(EB.ATM19.ISSUE.WAIVE.CHARGE)
        Y.NARR = EB.SystemTables.getRNew(EB.ATM19.NARRATIVE)
        Y.TO.DATE = EB.SystemTables.getRNew(EB.ATM19.TO.DATE)
        
*        IF R.NEW(EB.ATM19.REISSUE.REASON) NE 5 AND R.NEW(EB.ATM19.ISSUE.WAIVE.CHARGE) EQ "YES" AND R.NEW(EB.ATM19.NARRATIVE) EQ "" THEN
*            ETEXT ="Please Provide Reissue Waive Reason"
*            CALL STORE.END.ERROR
*        END
        IF Y.REISSUE.REASON NE 5 AND Y.ISSUE.WAIVE.CHARGE EQ "YES" AND Y.NARR EQ "" THEN
            EB.SystemTables.setEtext("Please Provide Reissue Waive Reason")
            EB.ErrorProcessing.StoreEndError()
        END
    END
    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,PINREQ" AND Y.VFUNCTION EQ 'I' THEN
*        R.NEW(EB.ATM19.ATTRIBUTE3)=TODAY
        EB.SystemTables.setRNew(EB.ATM19.ATTRIBUTE3, Y.TODAY)
*        R.NEW(EB.ATM19.REQUEST.TYPE)="PINREISSUE"
        EB.SystemTables.setRNew(EB.ATM19.REQUEST.TYPE, "PINREISSUE")
*        R.NEW(EB.ATM19.CARD.STATUS)="PENDING"
        EB.SystemTables.setRNew(EB.ATM19.CARD.STATUS, "PENDING")

*        IF R.NEW(EB.ATM19.ISSUE.WAIVE.CHARGE) EQ "YES" AND R.NEW(EB.ATM19.NARRATIVE) EQ "" AND R.NEW(EB.ATM19.REISSUE.REASON) NE 7 THEN
*            ETEXT ="Please Provide Pin Reissue Waive Reason"
*            CALL STORE.END.ERROR
*        END
        IF Y.ISSUE.WAIVE.CHARGE EQ "YES" AND Y.NARR EQ "" AND Y.REISSUE.REASON NE 7 THEN
            EB.SystemTables.setEtext("Please Provide Pin Reissue Waive Reason")
            EB.ErrorProcessing.StoreEndError()
        END
    END
    
    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,CLOSE" AND Y.VFUNCTION EQ 'I' THEN
*        R.NEW(EB.ATM19.CARD.CLOSE.RE.DATE)=TODAY
        EB.SystemTables.setRNew(EB.ATM19.CARD.CLOSE.RE.DATE, Y.TODAY)
*        R.NEW(EB.ATM19.REQUEST.TYPE)="CLOSE"
        EB.SystemTables.setRNew(EB.ATM19.REQUEST.TYPE, "CLOSE")
*        R.NEW(EB.ATM19.CARD.STATUS)="PENDING"
        EB.SystemTables.setRNew(EB.ATM19.CARD.STATUS, "PENDING")

*        IF R.NEW(EB.ATM19.ISSUE.WAIVE.CHARGE) EQ "YES" AND R.NEW(EB.ATM19.NARRATIVE) EQ "" THEN
*            ETEXT ="Please Provide Card Close Waive Reason"
*            CALL STORE.END.ERROR
*        END

        IF Y.ISSUE.WAIVE.CHARGE EQ "YES" AND Y.NARR EQ "" THEN
            EB.SystemTables.setEtext("Please Provide Card Close Waive Reason")
            EB.ErrorProcessing.StoreEndError()
        END
    END
    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,WAIVE" AND Y.VFUNCTION EQ 'I' THEN
* R.NEW(EB.ATM19.REQUEST.TYPE)="WAIVE"
        EB.SystemTables.setRNew(EB.ATM19.REQUEST.TYPE, "WAIVE")
    END

    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,WAIVE" AND Y.VFUNCTION EQ 'A' THEN
* R.NEW(EB.ATM19.CARD.STATUS)="DONE"
        EB.SystemTables.setRNew(EB.ATM19.CARD.STATUS, "DONE")
    END

    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,ISSUE" AND Y.VFUNCTION EQ 'I' THEN
* R.NEW(EB.ATM19.CARD.STATUS)="PENDING"
        EB.SystemTables.setRNew(EB.ATM19.CARD.STATUS, "DONE")
    END

* Y.ID= ID.NEW
    Y.ID = EB.SystemTables.getIdNew()
    EB.DataAccess.FRead(FN.ATM,Y.ID,R.ATM.REC,F.ATM,Y.ERR)
    
    IF R.ATM.REC NE "" AND Y.VFUNCTION EQ 'I' THEN
* IF R.ATM.REC<EB.ATM19.CARD.STATUS> EQ "DENIED" THEN
        IF Y.CARD.STATUS EQ "DENIED" THEN
            R.ATM.REC<EB.ATM19.CARD.STATUS>=""
        END
        ELSE
            R.ATM.REC<EB.ATM19.CARD.STATUS>="PROCESSING"
        END
* CALL F.WRITE(FN.ATM,Y.ID,R.ATM.REC)
        EB.DataAccess.FWrite(FN.ATM, Y.ID, R.ATM.REC)
        EB.TransactionControl.JournalUpdate(Y.ID)
    END
    IF R.ATM.REC NE "" AND Y.VFUNCTION EQ 'D' AND ( Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,UPDATE" OR Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,DENIED" OR Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,CLOSEHO" OR  Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,PINHO") THEN
        R.ATM.REC<EB.ATM19.CARD.STATUS>="PENDING"
* CALL F.WRITE(FN.ATM,Y.ID,R.ATM.REC)
        EB.DataAccess.FWrite(FN.ATM, Y.ID, R.ATM.REC)
        EB.TransactionControl.JournalUpdate(Y.ID)
    END
    
    IF R.ATM.REC NE "" AND Y.VFUNCTION EQ 'D' AND ( Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,REISSUE" OR Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,CLOSE" OR Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,ISSUE" OR Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,WAIVE" OR Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,PINREQ") THEN
*        IF R.ATM.REC<EB.ATM19.CARD.STATUS> EQ "" THEN
        IF Y.CARD.STATUS EQ "" THEN
            R.ATM.REC<EB.ATM19.CARD.STATUS>="DENIED"
        END
        ELSE
            R.ATM.REC<EB.ATM19.CARD.STATUS>="DONE"
        END
* CALL F.WRITE(FN.ATM,Y.ID,R.ATM.REC)
        EB.DataAccess.FWrite(FN.ATM, Y.ID, R.ATM.REC)
        EB.TransactionControl.JournalUpdate(Y.ID)
    END
    IF R.ATM.REC NE "" AND Y.VFUNCTION EQ 'D' AND ( Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,DELIVERY" OR Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,CLOSEBR" ) THEN
        R.ATM.REC<EB.ATM19.CARD.STATUS>="APPROVED"
* CALL F.WRITE(FN.ATM,Y.ID,R.ATM.REC)
        EB.DataAccess.FWrite(FN.ATM, Y.ID, R.ATM.REC)
        EB.TransactionControl.JournalUpdate(Y.ID)
    END

    IF Y.VFUNCTION EQ 'I' THEN
* R.NEW(EB.ATM19.INPUTTER.CO.CODE)=ID.COMPANY
        EB.SystemTables.setRNew(EB.ATM19.INPUTTER.CO.CODE, Y.IDCOMPANY)
    END

    IF Y.VFUNCTION EQ 'R' THEN
*        ETEXT ="REVERSE NOT ALLOW"
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext("REVERSE IS NOT ALLOWED")
        EB.ErrorProcessing.StoreEndError()
    END
*------------------------------BANK COMMENT -----------------------------------------------------------------*
    !IF ID.COMPANY NE "BD0012001" AND (Y.REQUEST  EQ "EB.JBL.ATM.CARD.MGT,UPDATE" OR Y.REQUEST EQ  "EB.JBL.ATM.CARD.MGT,CLOSEHO" OR Y.REQUEST EQ  "EB.JBL.ATM.CARD.MGT,DENIED" OR Y.REQUEST EQ  "EB.JBL.ATM.CARD.MGT,PINHO") THEN
    !ETEXT ="CARD PROCESSING ALLOW ONLY HEAD OFFICE"
    !CALL STORE.END.ERROR
    !END
    !IF ID.COMPANY NE Y.AC.COMPANY THEN
    !IF Y.REQUEST  EQ "EB.JBL.ATM.CARD.MGT,UPDATE" OR Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,CLOSEHO" OR Y.REQUEST EQ  "EB.JBL.ATM.CARD.MGT,DENIED" OR Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,PINHO" THEN

    !END
    !ELSE
    !ETEXT ="CARD INPUT GIVEN ONLY OWN BRANCH"
    !CALL STORE.END.ERROR
    !END
    !END
*-----------------------------------BANK COMMENT ----END-----------------------------------------------------*
    
*    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,UPDATE"  AND V$FUNCTION EQ 'I' AND  R.NEW(EB.ATM19.REQUEST.TYPE) EQ "REISSUE"  AND R.NEW(EB.ATM19.CARD.CLOSE.DATE) EQ "" AND R.NEW(EB.ATM19.ATTRIBUTE4) EQ "" THEN
*        ETEXT ="Please Provide Reissue Date "
*        CALL STORE.END.ERROR
*    END
    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,UPDATE"  AND Y.VFUNCTION EQ 'I' AND Y.ATM.REQ.TYPE EQ "REISSUE"  AND Y.CARD.CLOSE.DATE EQ "" AND Y.ATM.ATTRIBUTE4 EQ "" THEN
        EB.SystemTables.setEtext("Please Provide Reissue Date")
        EB.ErrorProcessing.StoreEndError()
    END
    
*    IF R.NEW(EB.ATM19.NARRATIVE) NE "" AND   LEN(R.NEW(EB.ATM19.NARRATIVE)) GT 80 THEN
*        ETEXT ="narrative within 80 characters "
*        CALL STORE.END.ERROR
*    END
    IF Y.NARR NE "" AND   LEN(Y.NARR) GT 80 THEN
        EB.SystemTables.setEtext("Narrative must be within 80 characters")
        EB.ErrorProcessing.StoreEndError()
    END
    
*    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,UPDATE"  AND V$FUNCTION EQ 'I' AND  R.NEW(EB.ATM19.REQUEST.TYPE) NE "REISSUE" THEN
*        T(EB.ATM19.ATTRIBUTE4)<3> = 'NOINPUT'
*    END
    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,UPDATE" AND Y.VFUNCTION EQ 'I' AND Y.ATM.REQ.TYPE NE "REISSUE" THEN
        EB.SystemTables.setT(EB.ATM19.ATTRIBUTE4, 'NOINPUT')
    END
    
* IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,UPDATE"  AND V$FUNCTION EQ 'I' AND  R.NEW(EB.ATM19.REQUEST.TYPE) EQ "REISSUE" THEN
    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,UPDATE"  AND Y.VFUNCTION EQ 'I' AND Y.ATM.REQ.TYPE EQ "REISSUE" THEN
*        IF R.NEW(EB.ATM19.CARD.CLOSE.DATE) NE "" THEN
*            T(EB.ATM19.ATTRIBUTE4)<3> = 'NOINPUT'
*        END
        IF Y.CARD.CLOSE.DATE NE "" THEN
            EB.SystemTables.setT(EB.ATM19.ATTRIBUTE4, 'NOINPUT')
        END
*        ELSE IF R.NEW(EB.ATM19.CARD.CLOSE.DATE) EQ "" THEN
*            T(EB.ATM19.FROM.DATE)<3> = 'NOINPUT'
*        END
        ELSE IF Y.CARD.CLOSE.DATE EQ "" THEN
            EB.SystemTables.setT(EB.ATM19.FROM.DATE, 'NOINPUT')
        END
    END
    
    Y.ATTR5 = EB.SystemTables.getRNew(EB.ATM19.ATTRIBUTE5)
    
* IF V$FUNCTION EQ 'D' AND  R.NEW(EB.ATM19.CARD.STATUS) EQ "PENDING" AND R.NEW(EB.ATM19.ATTRIBUTE5) NE "" THEN
    IF Y.VFUNCTION EQ 'D' AND  Y.CARD.STATUS EQ "PENDING" AND Y.ATTR5 NE "" THEN
* Y.ATM.CR.ID=R.NEW(EB.ATM19.ATTRIBUTE5)
        Y.ATM.CR.ID = Y.ATTR5
    
        EB.DataAccess.FRead(FN.FT.NAU,Y.ATM.CR.ID,REC.FT.CHK,F.FT.NAU,ERR.FT)
        Y.FT.CHK = REC.FT.CHK<FT.Contract.FundsTransfer.RecordStatus>
        
        IF REC.FT.CHK EQ "" THEN
            EB.DataAccess.FRead(FN.FT,Y.ATM.CR.ID,REC.FT.CHK,F.FT,ERR.FT.LIVE)
        END
        IF REC.FT.CHK EQ "" THEN
* CALL EB.READ.HISTORY.REC(F.FT.HIS, Y.ATM.CR.ID,REC.FT.CHK,Y.ERR.FT.HIS)
            EB.DataAccess.ReadHistoryRec(F.FT.HIS, Y.ATM.CR.ID,REC.FT.CHK,Y.ERR.FT.HIS)
        END

* IF REC.FT.CHK NE "" AND REC.FT.CHK<FT.RECORD.STATUS> EQ "REVE" THEN
        IF REC.FT.CHK NE "" AND Y.FT.CHK EQ "REVE" THEN

        END
    
        ELSE  IF REC.FT.CHK NE ""  THEN
*            ETEXT ="FUNDS TRANSFER UNAUTHRISED STAGE OR ALREADY DEDUCT THIS REQUEST"
            EB.SystemTables.setEtext("FUNDS TRANSFER UNAUTHRISED STAGE OR ALREADY DEDUCT THIS REQUEST")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

    END

* IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,REISSUE"  AND V$FUNCTION EQ 'I' AND  R.NEW(EB.ATM19.CARD.STATUS) EQ "PENDING" AND R.NEW(EB.ATM19.REISSUE.REASON) EQ 5 THEN
    IF Y.REQUEST EQ "EB.JBL.ATM.CARD.MGT,REISSUE"  AND Y.VFUNCTION EQ 'I' AND  Y.CARD.STATUS EQ "PENDING" AND Y.REISSUE.REASON EQ 5 THEN
        Y.DAYS = "C"
* CALL CDD("",TODAY,R.NEW(EB.ATM19.TO.DATE),Y.DAYS)
        EB.API.Cdd("", Y.TODAY, Y.TO.DATE, Y.DAYS)
        
        IF(Y.DAYS GE 30) THEN
*            ETEXT ="CARD VALIDATION " : Y.DAYS :" DAYS REMAINING"
            EB.SystemTables.setEtext("CARD VALIDATION " : Y.DAYS :" DAYS REMAINING")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

    END

RETURN

END

