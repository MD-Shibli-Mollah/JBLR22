* @ValidationCode : MjotODI5MDIwNjA3OkNwMTI1MjoxNzA0NzExNzUzMTMyOm5hemloYXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jan 2024 17:02:33
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
*******Develop By: Robiul (JBL)********
*****Date: 31 JAN 2017 ***************
*-----------------------------------------------------------------------------
SUBROUTINE GB.JBL.E.CNV.ATM.REISSUE.MASK
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
* TAFC Routine Name :ATM.ACC.CHK - R09
*-----------------------------------------------------------------------------
        
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*    $INSERT GLOBUS.BP I_F.ACCOUNT
    $USING AC.AccountOpening
    $INSERT I_F.EB.JBL.ATM.CARD.MGT
    $USING EB.Reports
    $USING EB.DataAccess
    $USING EB.LocalReferences
    $USING EB.SystemTables


    FN.ATM = "F.EB.ATM.CARD.MGT"
    F.ATM = ""
*    Y.REC.ID = O.DATA
    Y.REC.ID = EB.Reports.getOData()
*    CALL OPF(FN.ATM,F.ATM)
    EB.DataAccess.Opf(FN.ATM, F.ATM)

    EB.DataAccess.FRead(FN.ATM, Y.REC.ID, R.ATM.REC, F.ATM, Y.ERR)
    Y.CARD.NODB = R.ATM.REC<EB.ATM19.CARD.NO>
    Y.MASK = R.ATM.REC<EB.ATM19.CARD.MASK>
    Y.CARD.NAME = R.ATM.REC<EB.ATM19.ACCT.NO>
    CALL ATM.MS.MASK(Y.MASK,"DEP",Y.RESULT.DATA,Y.CARD.NAME)
    Y.CARD.NO = LEFT(Y.CARD.NODB,6):Y.RESULT.DATA:RIGHT(Y.CARD.NODB,4)
* O.DATA=Y.CARD.NO
    EB.Reports.setOData(Y.CARD.NO)
RETURN
END
