*-----------------------------------------------------------------------------
* <Rating>-7</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE JBL.EDF.REGISTER.FIELDS
*-----------------------------------------------------------------------------
*<doc>
* Template for field definitions routine JBL.EDF.REGISTER.FIELDS
*
* @author tcoleman@temenos.com
* @stereotype fields template
* @uses Table
* @public Table Creation
* @package infra.eb
* </doc>
*-----------------------------------------------------------------------------
* Modification History :
*
* 19/10/07 - EN_10003543
*            New Template changes
*
* 14/11/07 - BG_100015736
*            Exclude routines that are not released
*-----------------------------------------------------------------------------
*** <region name= Header>
*** <desc>Inserts and control logic</desc>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_DataTypes
*** </region>
*-----------------------------------------------------------------------------
*    CALL Table.defineId("TABLE.NAME.ID", T24_String) ;* Define Table id
    ID.F = "EDF.NO" ; ID.N = "20" ; ID.T = "A"
*-----------------------------------------------------------------------------
    CALL Table.addFieldDefinition("APPLICANT.NAME","65","A","")
    CALL Table.addFieldDefinition("LC.NUMBER","30","A","")
    CALL Table.addFieldDefinition("DATE.OF.LC","8","D","")
    CALL Table.addFieldDefinition("LC.AMOUNT","19","AMT","")
    CALL Table.addFieldDefinition("LC.EXPIRY.DATE","8","D","")
    CALL Table.addFieldDefinition("XX.BENEFICIARY","35","A","")
    CALL Table.addFieldDefinition("BB.FUNDING.DATE","8","D","")
    CALL Table.addFieldDefinition("BB.FUND.AMT","19","AMT","")
    CALL Table.addFieldDefinition("BB.REFUND.DATE","8","D","")
    CALL Table.addFieldDefinition("BB.REFUND.AMT","19","AMT","")
    CALL Table.addFieldDefinition("BB.CREDIT.DATE","8","D","")
    CALL Table.addReservedField('RESERVED.05')
    CALL Table.addReservedField('RESERVED.04')
    CALL Table.addReservedField('RESERVED.03')
    CALL Table.addReservedField('RESERVED.02')
    CALL Table.addReservedField('RESERVED.01')
    CALL Table.addField('XX.LOCAL.REF', T24_String, Field_NoInput,'')
    CALL Table.addOverrideField
*-----------------------------------------------------------------------------
    CALL Table.setAuditPosition ;* Poputale audit information
*-----------------------------------------------------------------------------
RETURN
*-----------------------------------------------------------------------------
END
