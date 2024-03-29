*-----------------------------------------------------------------------------
* <Rating>635</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE MT.STMT.ACCT.CR(Y.ACCT.ID)

    $INSERT GLOBUS.BP I_COMMON
    $INSERT GLOBUS.BP I_EQUATE
    $INSERT GLOBUS.BP I_ENQUIRY.COMMON
    $INSERT GLOBUS.BP I_BATCH.FILES
    $INSERT GLOBUS.BP I_F.ACCOUNT
    $INSERT GLOBUS.BP I_GTS.COMMON
    $INSERT GLOBUS.BP I_F.COMPANY
    $INSERT GLOBUS.BP I_F.STMT.ACCT.CR
    $INSERT GLOBUS.BP I_F.STMT.ACCT.DR

    $INSERT JBL.BP/REPGEN MT.STMT.ACCT.CR.VARS


    CALL F.READ(FN.ACCT,Y.ACCT.ID,R.AC.REC,F.ACCT,Y.ERR)
    Y.CATEG = R.AC.REC<AC.CATEGORY>
    Y.CO.CODE = R.AC.REC<AC.CO.CODE>

    LOCATE Y.CO.CODE IN COMPANY.LIST SETTING COMP.POS THEN
        LOCATE Y.CATEG IN Y.CATEGORY.CR.INT SETTING POS THEN

            CAPDATE = FIELD(R.AC.REC<AC.CAP.DATE.CR.INT>,VM,1)
            LOCATE CAPDATE IN Y.DATE.LIST SETTING DT.POS THEN
                Y.STMT.ACCT.ID = Y.ACCT.ID : "-" : CAPDATE
                CALL F.READ(FN.STMT.ACCT.CR,Y.STMT.ACCT.ID,R.STMT.AC.CR.REC,F.STMT.ACCT.CR,Y.ERR)
                IF R.STMT.AC.CR.REC NE "" THEN
                    Y.INT.POST.DATE = R.STMT.AC.CR.REC<IC.STMCR.INT.POST.DATE>
                    Y.INTEREST.AMOUNT = R.STMT.AC.CR.REC<IC.STMCR.TOTAL.INTEREST>
                    Y.CR.INT.TAX.AMT = R.STMT.AC.CR.REC<IC.STMCR.TAX.FOR.CUSTOMER>
                    Y.CR.INT.TAX.RATE = R.STMT.AC.CR.REC<IC.STMCR.CR.INT.TAX.RATE>
                    Y.CR.INT.TAX.CODE = R.STMT.AC.CR.REC<IC.STMCR.CR.INT.TAX.CODE>
                    Y.CR.INT.AMT.ARRAY = R.STMT.AC.CR.REC<IC.STMCR.CR.INT.AMT>
                    Y.CR.INT.RATE.ARRAY = R.STMT.AC.CR.REC<IC.STMCR.CR.INT.RATE>
                    Y.CR.VAL.BALANCE.ARRAY = R.STMT.AC.CR.REC<IC.STMCR.CR.VAL.BALANCE>
                    Y.CR.NO.OF.DAYS.ARRAY = R.STMT.AC.CR.REC<IC.STMCR.CR.NO.OF.DAYS>
                    Y.CR.INT.DATE.ARRAY = R.STMT.AC.CR.REC<IC.STMCR.CR.INT.DATE>
                    Y.MANUAL.ADJ.AMT = R.STMT.AC.CR.REC<IC.STMCR.MANUAL.ADJ.AMT>

                    Y.RETURN = Y.STMT.ACCT.ID:'|':Y.INT.POST.DATE:'|':Y.INTEREST.AMOUNT:'|':Y.CR.INT.TAX.AMT:'|':Y.CR.INT.TAX.RATE:'|':Y.CR.INT.TAX.CODE:'|':Y.CR.INT.AMT.ARRAY:'|':Y.CR.INT.RATE.ARRAY:'|':Y.CR.VAL.BALANCE.ARRAY:'|':Y.CR.NO.OF.DAYS.ARRAY:'|':Y.CR.INT.DATE.ARRAY:'|':Y.MANUAL.ADJ.AMT:'|'

                    WRITESEQ Y.RETURN APPEND TO F.FILE.CR ELSE NULL
                    CRT 'ACCT CR-------CR'
                END
            END
        END

        LOCATE Y.CATEG IN Y.CATEGORY.DR.INT SETTING POS THEN

            CAPDATE = FIELD(R.AC.REC<AC.CAP.DATE.DR.INT>,VM,1)
            LOCATE CAPDATE IN Y.DATE.LIST SETTING DT.POS THEN
                Y.STMT.ACCT.ID = Y.ACCT.ID : "-" : CAPDATE
                CALL F.READ(FN.STMT.ACCT.DR,Y.STMT.ACCT.ID,R.STMT.AC.DR.REC,F.STMT.ACCT.DR,Y.ERR)
                IF R.STMT.AC.DR.REC NE "" THEN
                    Y.INT.POST.DATE = R.STMT.AC.DR.REC<IC.STMDR.INT.POST.DATE>
                    Y.INTEREST.AMOUNT = R.STMT.AC.DR.REC<IC.STMDR.TOTAL.INTEREST>
                    Y.DR.INT.TAX.AMT = R.STMT.AC.DR.REC<IC.STMDR.DR.INT.TAX.AMT>
                    Y.DR.INT.TAX.RATE = R.STMT.AC.DR.REC<IC.STMDR.DR.INT.TAX.RATE>
                    Y.DR.INT.TAX.CODE = R.STMT.AC.DR.REC<IC.STMDR.DR.INT.TAX.CODE>
                    Y.DR.INT.AMT.ARRAY = R.STMT.AC.DR.REC<IC.STMDR.DR.INT.AMT>
                    Y.DR.INT.RATE.ARRAY = R.STMT.AC.DR.REC<IC.STMDR.DR.INT.RATE>
                    Y.DR.VAL.BALANCE.ARRAY = R.STMT.AC.DR.REC<IC.STMDR.DR.VAL.BALANCE>
                    Y.DR.NO.OF.DAYS.ARRAY = R.STMT.AC.DR.REC<IC.STMDR.DR.NO.OF.DAYS>
                    Y.DR.INT.DATE.ARRAY = R.STMT.AC.DR.REC<IC.STMDR.DR.INT.DATE>
                    Y.MANUAL.ADJ.AMT = R.STMT.AC.DR.REC<IC.STMDR.MANUAL.ADJ.AMT>

                    Y.RETURN = Y.STMT.ACCT.ID:'|':Y.INT.POST.DATE:'|':Y.INTEREST.AMOUNT:'|':Y.DR.INT.TAX.AMT:'|':Y.DR.INT.TAX.RATE:'|':Y.DR.INT.TAX.CODE:'|':Y.DR.INT.AMT.ARRAY:'|':Y.DR.INT.RATE.ARRAY:'|':Y.DR.VAL.BALANCE.ARRAY:'|':Y.DR.NO.OF.DAYS.ARRAY:'|':Y.DR.INT.DATE.ARRAY:'|':Y.MANUAL.ADJ.AMT:'|'

                    WRITESEQ Y.RETURN APPEND TO F.FILE.DR ELSE NULL
                    CRT 'ACCT DR*******DR'
                END
            END
        END

    END

    RETURN
END
