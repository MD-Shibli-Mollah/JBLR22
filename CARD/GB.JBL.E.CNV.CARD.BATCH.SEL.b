* @ValidationCode : MjoyMDA3Njk0MjY0OkNwMTI1MjoxNzA0NzAzODE5NDkwOm5hemloYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jan 2024 14:50:19
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
* Modification History : RETROFIT from TAFC to TAFJ
* 1)
* Date :08/01/2024
* Modification Description :
* Modified By : MD Shibli Mollah - NITSL
*-----------------------------------------------------------------------------
* Subroutine Description: This routine is used for ATM CARD MANAGEMENT SYSTEM
* Subroutine Type: Conversion
* Attached To    : JBL.ENQ.CARD.REISSUE.PEN.LST
* Attached As    : CONVERSION ROUTINE
* TAFC Routine Name : JBL.CARD.BATCH.SEL
*-----------------------------------------------------------------------------
SUBROUTINE GB.JBL.E.CNV.CARD.BATCH.SEL
    
    $INSERT I_EQUATE
    $INSERT I_COMMON
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.EB.JBL.CARD.BATCH.CRE
    $INSERT I_F.EB.JBL.ATM.CARD.MGT
    
    $USING EB.DataAccess
    $USING EB.Reports
    $USING EB.SystemTables
    
    !DEBUG
    FN.ATM = "F.EB.JBL.ATM.CARD.MGT"
    F.ATM = ""
    FN.BAT.NAU = "F.EB.JBL.CARD.BATCH.CRE$NAU"
    F.BAT.NAU = ""

    EB.DataAccess.Opf(FN.ATM, F.ATM)
    EB.DataAccess.Opf(FN.BAT.NAU, F.BAT.NAU)

    !LOCATE "ATTRIBUTE1" IN ENQ.DATA<2,1> SETTING Y.POS THEN
* Y.BATCH.ID = O.DATA
    Y.BATCH.ID = EB.Reports.getOData()
    !END

    EB.DataAccess.FRead(FN.BAT.NAU, Y.BATCH.ID, REC.BAT, F.BAT.NAU, BAT.ERR)
    Y.REQUEST.TYPE = REC.BAT<EB.CARD.BAT.REQUEST.TYPE>
    Y.TOT.CARD = REC.BAT<EB.CARD.BAT.TOT.CARD>
    Y.REQ = REC.BAT<EB.CARD.BAT.REQUEST.ID>
    Y.REC.COUNT = DCOUNT(Y.REQ, @VM)
    Y.TOT.CARD = Y.TOT.CARD - Y.REC.COUNT

    IF Y.TOT.CARD GT 0 THEN
        Y.TOT.CARD = Y.TOT.CARD - 1
        !ALL.IDS = DCOUNT(ENQ.KEYS,@FM)
    END

    W.ENQ.KEYS = ""
    Y.ENQ.KEYS = EB.Reports.getEnqKeys()

    FOR I = 1 TO Y.TOT.CARD
        IF Y.ENQ.KEYS<I> NE  "" THEN
            W.ENQ.KEYS<-1> = Y.ENQ.KEYS<I>
        END
    NEXT I

    Y.ENQ.KEYS = ""
    Y.ENQ.KEYS = W.ENQ.KEYS

RETURN
END
