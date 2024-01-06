*-----------------------------------------------------------------------------
* <Rating></Rating>
*-----------------------------------------------------------------------------
SUBROUTINE BD.SOC.PARAM
*-----------------------------------------------------------------------------
*<doc>
*Developer Info:
*    Date         : 13/02/2022
*    Description  : Main routine for the BD.SOC.PARAM template
*    Developed By : Md. Nazibul Islam
*    Designation  : Software Engineer
*    Email        : nazibul.ntl@nazihargroup.com
*
*
* </doc>
*-----------------------------------------------------------------------------
* TODO - You MUST write a .FIELDS routine for the field definitions
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------
*
* ----------------------------------------------------------------------------
* <region name= Inserts>
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_Table
* </region>
*-----------------------------------------------------------------------------
    Table.name = 'BD.SOC.PARAM'        ;* Full application name including product prefix
    Table.title = 'BD.SOC PARAM'       ;* Screen title
    Table.stereotype = 'H'    ;* H, U, L, W or T
    Table.product = 'EB'      ;* Must be on EB.PRODUCT
    Table.subProduct = ''     ;* Must be on EB.SUB.PRODUCT
    Table.classification = 'CUS'        ;* As per FILE.CONTROL
    Table.systemClearFile = 'Y'         ;* As per FILE.CONTROL
    Table.relatedFiles = ''   ;* As per FILE.CONTROL
    Table.isPostClosingFile = ''        ;* As per FILE.CONTROL
    Table.equatePrefix = 'SOC.PARAM'        ;* Use to create I_F.EB.LOG.PARAMETER
*-----------------------------------------------------------------------------
    Table.idPrefix = ''       ;* Used by EB.FORMAT.ID if set
    Table.blockedFunctions = ''         ;* Space delimeted list of blocked functions
    Table.trigger = ''        ;* Trigger field used for OPERATION style fields
*-----------------------------------------------------------------------------

RETURN
END
