* @ValidationCode : MjoxOTI1NjM1MjM5OkNwMTI1MjoxNzA0NzA0ODU5NzQxOm5hemloYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jan 2024 15:07:39
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : nazihar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.JBL.BA.ATM.CARD.WRT.BAT

*-----------------------------------------------------------------------------
* Modification History :
* 1)
* Date :08/01/2024
* Modification Description :  RETROFIT from TAFC to TAFJ
* Modified By : MD Shibli Mollah - NITSL
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
* Subroutine Description: THIS ROUTINE IS USED FOR CRMS
* Subroutine Type: BEFORE AUTH
* Attached To    : EB.JBL.CARD.BATCH.CRE,INPUT
* Attached As    : BEFORE AUTH ROUTINE
* TAFC Routine Name :ATM.ACC.CHK - R09
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.JBL.CARD.BATCH.CRE
    $INSERT I_F.EB.JBL.ATM.CARD.MGT
    
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.TransactionControl

    FN.ATM = "F.EB.JBL.ATM.CARD.MGT"
    F.ATM = ""
    FN.BAT.NAU = "F.EB.JBL.CARD.BATCH.CRE$NAU"
    F.BAT.NAU = ""

    EB.DataAccess.Opf(FN.ATM,F.ATM)
    EB.DataAccess.Opf(FN.BAT.NAU,F.BAT.NAU)

* Y.BATCH.ID=R.NEW(EB.ATM19.ATTRIBUTE1)
    Y.BATCH.ID = EB.SystemTables.getRNew(EB.ATM19.ATTRIBUTE1)

* Y.REQ.ID=ID.NEW
    Y.REQ.ID = EB.SystemTables.getIdNew()

    EB.DataAccess.FRead(FN.BAT.NAU, Y.BATCH.ID, REC.BATCH, F.BAT.NAU, ERR.BATCH)
    Y.BAT.REQUEST.ID = REC.BATCH<EB.CARD.BAT.REQUEST.ID>
    IF DCOUNT(Y.BAT.REQUEST.ID, @VM) EQ 0 THEN
        REC.BATCH<EB.CARD.BAT.REQUEST.ID>= Y.REQ.ID
    END
    ELSE
        REC.BATCH<EB.CARD.BAT.REQUEST.ID>= REC.BATCH<EB.CARD.BAT.REQUEST.ID>:@VM:Y.REQ.ID
    END
    !WRITE REC.BATCH TO F.CHQ.BATCH,Y.BATCH.ID
* CALL F.WRITE(FN.BAT.NAU,Y.BATCH.ID,REC.BATCH)
    EB.DataAccess.FWrite(FN.BAT.NAU, Y.BATCH.ID, REC.BATCH)
    EB.TransactionControl.JournalUpdate(Y.BATCH.ID)

RETURN
END
