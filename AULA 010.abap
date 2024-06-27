*&---------------------------------------------------------------------*
*& Report Z_HCM_TEST6
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_hcm_test6.

  TYPE-POOLS slis. "SLIS is used for ALV (ABAP List Viewer) structures and constants"
  TABLES pa0001.   "From this table we want some fields."

  "work areas."
  "-------------------------------------------------------------------------"
  TYPES: BEGIN OF ty_pa0001, "Structure."
    pernr TYPE pa0001-pernr, "Employee ID."
    begda TYPE pa0001-begda, "Start Date from Register."
    orgeh TYPE pa0001-orgeh, "Hierarchical Structure of the Organization."
    ename TYPE pa0001-ename, "Employee Name."
  END OF ty_pa0001.

  TYPES: BEGIN OF ty_output, "Structure."
    sno   TYPE i,            "Number."
    orgeh TYPE pa0001-orgeh, "Hierarchical Structure of the Organization."
    ortx  TYPE t527x-orgtx,  "Description of the Organization."
    pernr TYPE pa0001-pernr, "Employee ID."
    ename TYPE pa0001-ename, "Employee Name."
    lgart TYPE pa0008-lga01, "Payment Type."
    betrg TYPE netwr,        "Monetary Value."
  END OF ty_output.
  "-------------------------------------------------------------------------"
  "Tables."
  "--------------------------------------------"
  DATA: "Structure Tables."
        i_pa0001 TYPE TABLE OF ty_pa0001, "Local Structure."
        i_output TYPE TABLE OF ty_output. "Local Structure."
  DATA: "System-Structures."
        it_rgdir TYPE TABLE OF pc261, "Table of System-Structure."
        i_rgdir  TYPE TABLE OF pc261. "Table of System-Structure."
  "--------------------------------------------"
  DATA: fs_pa0001 TYPE ty_pa0001, "Self-structure."
        fs_rgdir TYPE pc261.      "System-structure: Cluster Directory (For Export and Import of Payroll Results)."
  "---------------------------------------------------------------------------------------------------------------"
  DATA: "variables."
        v_begda TYPE begda,       "Start Date from Register."
        v_endda TYPE endda,       "End Date from Register."
        v_atext TYPE t549t-atext, "Description Text."
        v_error TYPE c.           "Text Type."

  DATA: v_mname(15),  "String Type Length 15."
        v_mname2(20). "String Type Length 20."
  "----------------------------------------------------------"
  "Constants"
  CONSTANTS: c_x TYPE c VALUE 'X'.
  "-------------------------------"
  "screen design
  "-------------------------------"
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
    PARAMETERS: p_abkrs TYPE pa0001-abkrs OBLIGATORY.
    SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 01(20) TEXT-t02 FOR FIELD p_month1.
    SELECTION-SCREEN POSITION 33.
    PARAMETERS: p_month1(2) TYPE n OBLIGATORY,
                p_fyear1(4) TYPE n OBLIGATORY.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK b1.
  "------------------------------------------------------------"
  "field validation
  "----------------"
  AT SELECTION-SCREEN ON p_month1.
    IF p_month1 GT '12'.
      MESSAGE e531(Ou) WITH 'Insert a Valid Period'.
    ENDIF.

  AT SELECTION-SCREEN ON p_abkrs.
    PERFORM validate_payroll_area.

  "-------------------------------------------------"
  "Main Programa Execution."
  "-------------------------------------------------"
  START-OF-SELECTION.

    PERFORM get_period.
    PERFORM get_pernrs.

  END-OF-SELECTION.
  "-------------------------------------------------"
  "Subroutines"
  "--------------------------------------------------------------------------------------------------------------------"
  FORM get_pernrs.
    "--------"
    "Pointers"
    "--------"
    FIELD-SYMBOLS: <payresult>    TYPE any,                  "Type Any."
                   <lv_payresult> TYPE h99_clst_s_payresult, "System-Structure: RPCLST: Structure for Payroll Result."
                   <lv_versc>     TYPE pc202,                "System-Structure: Payroll Status Information."
                   <li_rt>        TYPE hrpay99_rt,           "Results Table."
                   <li_ddntk>     TYPE hrpay99_ddntk.        "Deductions not taken."
    "---------"
    "Variables"
    "---------"
    DATA: lfs_rt    TYPE pc207, "System-Structure: Payroll Results: Results Table."
          lfs_ddtnk TYPE pc23e. "System-Structure: Payroll Results: Deductions Not Effected."

    DATA: lv_relid TYPE t500l-relid. "Personnel Country Grouping."

    DATA: lv_type     TYPE t52relid-typename, "Cluster Table: HR: Description of cluster in table PCLx."
          lv_typename TYPE hrpclx_type.       "Type name in dictionary for HR cluster tables PCLx."

    DATA: ref_payresult TYPE REF TO data. "Instance Data can receive any instance."

    DATA: lv_molga  TYPE molga,           "Country Grouping."
          lv_type_1 TYPE tadir-obj_name,  "Table-Structure: Directory of Repository Objects."
          lv_tadir  TYPE tadir-obj_name,  "Table-Structure: Directory of Repository Objects."
          lv_unpaid TYPE ktsol,           "Targeted work measured in calendar days."
          lv_paid   TYPE ktsol.           "Targeted work measured in calendar days."

    "----------"
    "Table Fill"
    "----------"
    SELECT pernr,             "Employee ID."
           begda,             "Start Date from Register."
           orgeh,             "Hierarchical Structure of the Organization."
           ename              "Employee Name."
      FROM pa0001             "from System-Table: HR Master Record: Infotype 0001 (Org. Assignment)."
      INTO TABLE @i_pa0001    "Into Local Table Structure"
      WHERE endda GE @v_begda "Where End Date from Register is Greater or Equal to Start Date from Register."
      AND begda LE @v_endda   "Where Start Date from Register is Less or Equal to End Date from Register."
      AND abkrs EQ @p_abkrs.  "Where Settlement to Cost Center information os Equal to Parameter of the same Type.

    IF sy-subrc NE 0. "If the query dont found any data..."

      MESSAGE s531(Ou) WITH 'No active employee found in the period.'.
      v_error = c_x. "Variable receives constant. [X]"
      STOP.

    ENDIF.

    "Else..."
    SORT i_pa0001 BY pernr begda DESCENDING. "Sort internal table by Employee ID and Start Date from Register descending."
    DELETE ADJACENT DUPLICATES FROM i_pa0001 COMPARING pernr. "Delete duplicated fields with the same Employee ID."
    SORT i_pa0001 BY orgeh pernr. "Sort internal table by Employee ID and Hierarchical Structure of the Organization."

    LOOP AT i_pa0001 INTO fs_pa0001. "Iterating on internal table using a structure like line of the table.

      "Clear the tables.
      REFRESH: it_rgdir,
               i_rgdir.

      "Clear Structure.
      CLEAR: fs_rgdir.

      "Cluster Table Reading."
      CALL FUNCTION 'cu_read_rgdir'
        EXPORTING
          pernrs          = fs_pa0001-pernr "Send the Employee ID from the internal table."
        TABLES
          in_rgdir        = it_rgdir        "Internal table is filled with the data."
        EXCEPTIONS
          no_record_found = 1
          OTHERS          = 2.

      IF sy-subrc = 0. "If the function works..."

        "Cluster Table Reading."
        CALL FUNCTION 'pyxx_get_relid_from_pernr'
          EXPORTING
            employee                    = fs_pa0001-pernr "Send the Employee ID from the internal table."
          IMPORTING
            relid                       = lv_relid       "Receives the Personnel Country Grouping."
            molga                       = lv_molga       "Country Grouping."
          EXCEPTIONS
            error_reading_infotype_0001 = 1
            error_reading_molga         = 2
            error_reading_relid         = 3
            OTHERS = 4.

      ENDIF.

      SELECT SINGLE typename    "Select Table Typename"
        FROM t52relid           "From Cluster Table - HR: Description of cluster in table PCLx."
        INTO lv_type            "Into the Cluster Table."
        WHERE relid EQ lv_relid "Where the Table Relid is Equal to Table exported by function."
        AND tabname = 'pcl2'.   "And the table has the name."

      IF sy-subrc NE 0. "If the query dont found any data..."
        lv_relid = 'IN'.
        lv_type = 'PAYIN_RESULT'.
      ENDIF.

      "else..."
      lv_typename = lv_type.

      CREATE DATA ref_payresult TYPE (lv_typename). "Creates a data object of the type from the table in cluster table."
      ASSIGN ref_payresult->* TO <payresult>.       "Reference to object and assign to field symbol above."
      DELETE it_rgdir WHERE srtza NE 'A'.           "Delete from internal table where Indicator: Status of record Not Equal 'A'.

      LOOP AT it_rgdir        "Iterating on internal table."
        INTO fs_rgdir         "Into Structure."
        WHERE payty = ''      "Where paytype is empty...
        AND fpbeg GE v_begda  "And Start Date from Register is greater or equal than variable Start Date from Register."
        AND fpend LE v_endda. "And End Date from Register is less or equal variable End Date from Register."

        APPEND fs_rgdir TO i_rgdir. "Fills the internal table."

      ENDLOOP.

      SORT i_rgdir BY seqnr DESCENDING.
      CLEAR: lv_unpaid, lv_paid.

      LOOP AT i_rgdir INTO fs_rgdir.

        CALL FUNCTION 'pyxx_read_payroll_result'
          EXPORTING
            clusterid = lv_relid
            employeenumber               = fs_pa0001-pernr
            sequencenumber               = fs_rgdir-seqnr
          CHANGING
            payroll_result               = <payresult>
          EXCEPTIONS
            illegal_isocode_or_clusterid = 1
            error_generating_import      = 2
            import_mismatch_import       = 3
            subpool_dir_full             = 4
            no_read_authority            = 5
            no_record_found              = 6
            versions_do_not_match        = 7
            error_reading_archive        = 8
            error_reading_relid          = 9
            OTHERS                       = 10.

        IF sy-subrc = 0.

          ASSIGN COMPONENT 'INTER-RT' OF STRUCTURE <payresult> TO <li_rt>.
          WRITE: / fs_pa0001-pernr.
          SKIP 2.

          LOOP AT <li_rt> INTO lfs_rt.

            WRITE: / lfs_rt-lgart,30 lfs_rt-betrg.

          ENDLOOP.

        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDFORM.

  FORM get_period.

   SELECT SINGLE begda,
                 endda
     INTO (@v_begda, @v_endda)
     FROM t549q
     WHERE permo = '01'
     AND pabrj = @p_fyear1
     AND pabrp = @p_month1.

     IF sy-subrc NE 0.

       MESSAGE s531(Ou) WITH 'Error in the calculated Period'.
       v_error = 'X'.
       STOP.

     ENDIF.

  ENDFORM.

  FORM validate_payroll_area.

    DATA: lv_abkrs TYPE t549a-abkrs.

    SELECT SINGLE abkrs
      FROM t549a
      INTO lv_abkrs
      WHERE abkrs EQ p_abkrs.

    IF sy-subrc NE 0.

      MESSAGE s531(Ou) WITH 'Enter with a valid payroll area'.
       v_error = 'X'.
       STOP.

    ENDIF.

  ENDFORM.
