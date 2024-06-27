*&---------------------------------------------------------------------*
*& Report Z_HCM_TEST7
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_hcm_test7.

TYPE-POOLS: slis. "SLIS is used for ALV (ABAP List Viewer) structures and constants"

NODES: person, "basic information about persons such as names, personnel numbers, dates of birth, etc."
       group,  "include information about departments, teams, or other groupings of employees."
       peras.  "include details about the role an employee holds, the duration of the assignment, etc."

TABLES: pernr,  "Standard Selections for HR Master Data Reporting"
        t512h,  "Assignment of Text Elements to Forms"
        tfkbt,  "Name of the functional areas"
        pa0022, "HR Master Record: Infotype 0022 (Education)"
        pa0016, "HR Master Record: Infotype 0016 (Contract Elements)"
        bkpf.   "Accounting Document Header"

INFOTYPES: 0000, "Actions"
           0001, "Organization Assignments"
           0022, "Education"
           0041, "Date Specification"
           0002, "Personal Data"
           0016. "Contract Elements"

DATA: BEGIN OF t_output OCCURS 0,
  pernr TYPE pernr_d, "Personnel Number"
  ename TYPE emnam,   "Employee Name"
  btrtl TYPE btrtl,   "Personnel Subarea"
  btext TYPE btext,   "Personnel Subarea Text"
  persk TYPE persk,   "Grade"
  ptext TYPE pktxt,   "Grade Text"
END OF t_output,

t_fcat TYPE slis_t_fieldcat_alv. "Global Types for Generic List Modules"

DATA: w_fieldcat  TYPE slis_t_fieldcat_alv, "Global Types for Generic List Modules"
      wa_fieldcat TYPE slis_fieldcat_alv.   "Global Types for Generic List Modules"

GET peras.

  PERFORM read_data. "Select the data to be sent to ALV"

END-OF-SELECTION.

PERFORM f_addcat.  "Adding the fields to be used in the screen display."
PERFORM f_display. "Screen Display."

"-------------------------------------------------------------------------------------------------------"

FORM read_data.

  rp_provide_from_last p0001 space pn-begda pn-endda. "Find the most recent data between a date range."

  IF pnp-sw-found = 1. "If at least one date is found..."

    "Local Structure receives the result of Macro"
    t_output-pernr = p0001-pernr.
    t_output-ename = p0001-ename.
    t_output-btrtl = p0001-btrtl.
    t_output-persk = p0001-persk.

    "Retrieves the text of the Sub Area"
    SELECT SINGLE btext             "Select a Single Data Personnel Subarea Text...
      FROM t001p                    "From Personnel Area/Subarea...
      INTO t_output-btext           "Into Local Structure Field...
      WHERE btrtl = t_output-btrtl. "Where Personnel Subarea = Local Structure Field"

    "Retrieves the Grade Text"
    SELECT SINGLE ptext            "Select a Single Data Name of Employee Subgroup...
      FROM t503t                   "Employee Subgroup Names"
      INTO t_output-ptext          "Into Local Structure Field...
      WHERE persk = t_output-persk "Where Employee Subgroup = Local Structure Field"
      AND sprsl = 'EN'.            "And Language Key = English"

    APPEND t_output.
    CLEAR t_output.

  ENDIF.

ENDFORM.

FORM f_addcat.

  PERFORM f_fieldcat USING 'T_OUTPUT' 'PERNR' 'Personnel Nro' 8 ''.
  PERFORM f_fieldcat USING 'T_OUTPUT' 'ENAME' 'Name' 15 ''.
  PERFORM f_fieldcat USING 'T_OUTPUT' 'PTEXT' 'Grade' 10 ''.

ENDFORM.

FORM f_fieldcat USING VALUE(p_tname) "Entry Parameters"
                      VALUE(p_fname)
                      VALUE(p_desc)
                      VALUE(p_leng)
                      VALUE(p_out).

"System-Structure receives the Parameters"
MOVE: p_tname TO wa_fieldcat-tabname,
      p_fname TO wa_fieldcat-fieldname,
      p_desc  TO wa_fieldcat-seltext_l,
      p_leng  TO wa_fieldcat-outputlen,
      p_out   TO wa_fieldcat-no_out.

APPEND wa_fieldcat TO t_fcat.
CLEAR wa_fieldcat.

ENDFORM.

FORM f_display.

  DATA: lv_repid LIKE sy-repid,         "Report Name"
        ls_layout TYPE slis_layout_alv. "Global Types for Generic List Modules"

  lv_repid = sy-repid.
  ls_layout-zebra = 'X'.

  CALL FUNCTION 'reuse_alv_grid_display'
    EXPORTING
      i_callback_program = lv_repid
      is_layout          = ls_layout
      it_fieldcat        = t_fcat
      i_save             = 'A'
      TABLES
        t_outtab         = t_output
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
