*&---------------------------------------------------------------------*
*& Report Z_HCM_TEST7_PP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_hcm_test7_pp.

DATA: wa_data      TYPE zzcsk_emp,
      it_data      TYPE TABLE OF zzcsk_emp,
      it_data_temp TYPE TABLE OF zzcsk_emp.

DATA: lv_appsvr    TYPE rzllitab-classname VALUE 'parallel_generators'.

DATA: lv_total     TYPE i, "Total Work Processes"
      lv_available TYPE i, "Total Work Processes Availables"
      lv_occupied  TYPE i, "Total Work Processes Occupied"
      lv_diff      TYPE i, "% Difference from Availables Work Processes"
      lv_split     TYPE i. "Useds Work Processes"

DATA: lv_lines     TYPE i, "Number of Lines in from Table"
      lv_lines_tab TYPE i, "Number of Lines per Table"
      lv_start     TYPE i, "Start process point"
      lv_end       TYPE i. "End process point"

DATA: lv_task      TYPE string, "Task name"
      lv_index     TYPE string, "Index Variable"
      lv_sent      TYPE i,      "Sent Package Number"
      lv_comp      TYPE i.      "Completed Package Number"

DATA: lv_result_string TYPE string.
DATA: lv_result TYPE flag.

START-OF-SELECTION.

"Pass1: To List the Work Processes available to use for parallel process"

"Function to find how many Work Processes are available"
CALL FUNCTION 'spbt_initialize'
  EXPORTING
    group_name = lv_appsvr
  IMPORTING
    max_pbt_wps = lv_total
    free_pbt_wps = lv_available
  EXCEPTIONS
    invalid_group_name             = 1
    internal_error                 = 2
    pbt_env_already_initialized    = 3
    currently_no_resources_avail   = 4
    no_pbt_resources_found         = 5
    cant_init_different_pbt_groups = 6
    OTHERS                         = 7.

"Pass2: According to available WP, we divide our data to be processed

IF sy-subrc = 0. "If work processes are available...

  lv_occupied = lv_total - lv_available.
  lv_diff = ( lv_available * 100 ) / lv_total.

  IF lv_diff <= 25.
    lv_split = lv_available DIV 2.
  ELSE.
    IF lv_diff BETWEEN 25 AND 50.
      lv_split = lv_available * 2 DIV 3.
    ELSE.
      IF lv_diff > 50.
        lv_split = lv_available * 3 DIV 4.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.

"Pass3: We Prepare the data"

DO 1000 TIMES.

  wa_data-mandt = sy-mandt.
  wa_data-field1 = sy-index.

  APPEND wa_data TO it_data. "Append to internal table instead of loop

  CLEAR wa_data.

ENDDO.

lv_lines = lines( it_data ).
lv_lines_tab = lv_lines / lv_split.

DO lv_split TIMES.

  lv_index = sy-index.

  CONCATENATE 'task' lv_index INTO lv_task.

  lv_start = lv_start + lv_lines_tab.

  lv_end = lv_lines_tab + 1.

  IF lv_index = 1.
    lv_start = 0.
  ENDIF.

  IF lv_split = lv_index.
    lv_end = 0.
  ENDIF.

  it_data_temp[] = it_data[]. "Passing the data table to another table"

  IF lv_start IS NOT INITIAL.
    DELETE it_data_temp TO lv_start.
  ENDIF.

  IF lv_end IS NOT INITIAL.
    DELETE it_data_temp FROM lv_end.
  ENDIF.

  "Pass4: Execution of the Main Function in parallel"

  CALL FUNCTION 'zcsk_parallel_table_update'
  STARTING NEW TASK lv_task
  DESTINATION IN GROUP lv_appsvr
  PERFORMING update_status
  ON END OF TASK
  CHANGING data = wa_data.

  IF sy-subrc = 0.
    lv_sent = lv_sent + 1.
  ENDIF.

ENDDO.

WAIT UNTIL lv_comp >= lv_sent.

WRITE: / 'Number of Packets Sent: ', lv_sent,
         'Number of Complete Packets: ', lv_comp.

FORM update_status USING lv_task.

  lv_comp = lv_comp + 1.

  RECEIVE RESULTS FROM FUNCTION 'zcsk_parallel_table_update'
    IMPORTING
      lv_result = lv_result.

  IF lv_result IS INITIAL.
    lv_result_string = 'Sucess!'.
  ELSE.
    lv_result_string = 'Failed!'.
  ENDIF.

  CONCATENATE 'The data passed via task' lv_task 'updated in' lv_result_string INTO lv_result_string SEPARATED BY space.

  WRITE: / lv_result_string.

ENDFORM.
