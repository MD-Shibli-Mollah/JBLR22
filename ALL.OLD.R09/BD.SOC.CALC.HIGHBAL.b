$PACKAGE BD.Soc
*
* Implementation of BD.SOC.SocGetHighBal
*
* Y.ACCOUNTNO(IN) :
* Y.YEAR(IN) :
* Y.BALANCE(OUT) :
*
SUBROUTINE BD.SOC.CALC.HIGHBAL(Y.ACCOUNTNO, Y.YEAR, Y.BALANCE)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.BD.SOC.BALINFO
*
    $USING EB.DataAccess
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS
RETURN
INIT:
    FN.BALINFO='F.BD.SOC.BALINFO'
    F.BALINFO=''
    FN.BALINFO='F.BD.SOC.BALINFO'
    F.BALINFO=''
RETURN
OPENFILE:
    EB.DataAccess.Opf(FN.BALINFO,F.BALINFO)
RETURN
PROCESS:
    BALINFO.ID=Y.ACCOUNTNO:Y.YEAR
    EB.DataAccess.FRead(FN.BALINFO, BALINFO.ID, R.BALINFO, F.BALINFO, ERR.BALINFO)
    Y.BALANCE=R.BALINFO<SOC.BAL.MAXIMUM.BALANCE>
    IF R.BALINFO EQ '' THEN
        BD.Soc.GetWBal(Y.ACCOUNTNO, Y.BALANCE)
    END
RETURN