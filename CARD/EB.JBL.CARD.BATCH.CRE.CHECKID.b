* @ValidationCode : MjotMTEzNDg4ODA4MzpDcDEyNTI6MTcwNDc3NTI3NjI0MzpuYXppaGFyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 09 Jan 2024 10:41:16
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : nazihar
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE EB.JBL.CARD.BATCH.CRE.CHECKID

*-----------------------------------------------------------------------------
* Modification History : RETROFIT from TAFC to TAFJ
* 1)
* Date :01/01/2024
* Modification Description :
* Modified By : MD Shibli Mollah - NITSL
*-----------------------------------------------------------------------------

*-----------------------------------------------------------------------------
* Subroutine Description: This routine is used for EB.JBL.CARD.BATCH.CRE
* Subroutine Type: CHECKID
* Attached To    : EB.JBL.CARD.BATCH.CRE
* Attached As    : CHECKID
* TAFC Routine Name : JBL.CARD.OFF.INFO.ID - R09
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
* $INSERT GLOBUS.BP I_Table
    $INSERT I_F.EB.JBL.CARD.BATCH.CRE
    $USING EB.SystemTables
    
    Y.PGM.VERSION = EB.SystemTables.getPgmVersion()
    Y.VFUNCTION = EB.SystemTables.getVFunction()
    Y.TIMEDATE = EB.SystemTables.getTimeStamp() ;*TIMEDATE()
*    Y.COMI = EB.SystemTables.getComi()
*    Y.ID.NEW = EB.SystemTables.getIdNew()

    Y.REQUEST = "EB.JBL.CARD.BATCH.CRE":Y.PGM.VERSION

    IF Y.VFUNCTION EQ "I" AND Y.REQUEST NE "EB.JBL.CARD.BATCH.CRE,BATREM" THEN
        DATE.STAMP = OCONV(DATE(), 'D4-')
* Y.TIMEDATE = TIMEDATE()

        Y.DATE.TIME = "BA":DATE.STAMP[7,4]:DATE.STAMP[1,2]:DATE.STAMP[4,2]: Y.TIMEDATE[1,2]:Y.TIMEDATE[4,2]::Y.TIMEDATE[7,2]
        Y.COMI = Y.DATE.TIME
* ID.NEW=Y.DATE.TIME
        EB.SystemTables.setComi(Y.DATE.TIME)
        EB.SystemTables.setIdNew(Y.DATE.TIME)
    END
RETURN
END

