REPORT z_hcm_test2.

TYPES: BEGIN OF data_employee,
         ls_emp_num TYPE persno,
         ls_emp_begda TYPE begda,
         ls_emp_endda TYPE endda,
       END OF data_employee.

DATA: emp_tab TYPE TABLE OF data_employee,
      ls_data_employee TYPE data_employee.

DATA: mindata1 TYPE pa0001-begda,
      mindata2 TYPE pa0001-begda.

SELECT pernr, begda, endda FROM pa0001 INTO TABLE @emp_tab WHERE pernr < 27.
SELECT MIN( begda ) FROM pa0001 INTO @mindata1.

LOOP AT emp_tab INTO ls_data_employee.
  IF ls_data_employee-ls_emp_begda > mindata2.
    mindata2 = ls_data_employee-ls_emp_begda.
  ENDIF.
ENDLOOP.

LOOP AT emp_tab INTO ls_data_employee.
  IF ls_data_employee-ls_emp_begda = mindata1.
    WRITE: / 'First Employee: '.
    WRITE: / 'Número: ', ls_data_employee-ls_emp_num.
    WRITE: / 'Hiring Date: ', ls_data_employee-ls_emp_begda.
    WRITE: / '------------------------------------'.
  ENDIF.
ENDLOOP.

LOOP AT emp_tab INTO ls_data_employee.
  IF ls_data_employee-ls_emp_begda = mindata2.
    WRITE: / 'Last Employee: '.
    WRITE: / 'Número: ', ls_data_employee-ls_emp_num.
    WRITE: / 'Hiring Date: ', ls_data_employee-ls_emp_begda.
    WRITE: / '------------------------------------'.
  ENDIF.
ENDLOOP.

DATA years type p.

CALL FUNCTION 'HR_GET_TIME_BETWEEN_DATES'
  EXPORTING
    beg_date             = mindata1
    end_date             = mindata2
 IMPORTING
    YEARS                = years.

WRITE: / 'INTERVAL BETWEEN HIRES', years.
