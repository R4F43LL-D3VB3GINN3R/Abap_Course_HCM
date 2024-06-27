*&---------------------------------------------------------------------*
*& Report Z_HCM_TEST4
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_HCM_TEST4.

call function 'hr_infotype_get_list'
  exporting 
    begda = pn-begda
    endda = pn-endda
  importing
    subrc = v_subrc
  tables
    pernr_tab =          rt_pernr_tab
    infty_tab =          rt_infty_tab
    infty_logg_key_tab = rt_infty_logg_key_tab
    datum_tab =          rt_datum_tab
    uname_tab =          rt_uname_tab.

import header to t_header from database pc14(la) id s_lo_key.

if sy-subrc = 0.
  
  call function 'hr_infotype_log_get_detail'
    exporting 
      logged_infotype = <s_doc_key>
    importing 
      subrc           = v_subrc
    tables
      infty_tab_before = t_before
      infty_tab_after  = t_after.
  
endif.
