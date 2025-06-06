CLASS lcl_birthday DEFINITION public.
  PUBLIC SECTION.
    METHODS: constructor
               IMPORTING i_pbegda TYPE sy-datum
                         i_pendda TYPE sy-datum,
             process_pernr
               IMPORTING is_p0002 TYPE p0002,
             display_alv,
             send_email.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_pernr,
             pernr TYPE p0002-pernr,
             vorna TYPE p0002-vorna,
             nachn TYPE p0002-nachn,
             gbdat TYPE p0002-gbdat,
             plans TYPE p0001-plans,
             orgtx TYPE hrp1000-stext,
           END OF ty_pernr.
    DATA: mt_pernrs TYPE STANDARD TABLE OF ty_pernr,
          ms_pernr TYPE ty_pernr,
          mv_pbegda TYPE sy-datum,
          mv_pendda TYPE sy-datum.
ENDCLASS.

CLASS lcl_birthday IMPLEMENTATION.

  METHOD constructor.
    mv_pbegda = i_pbegda.
    mv_pendda = i_pendda.
  ENDMETHOD.

  METHOD process_pernr.
    DATA: wa_hrp1000 TYPE hrp1000,
          wa_hrp1001 TYPE hrp1001,
          lt_hrp1001 TYPE TABLE OF hrp1001,
          lt_hrp1000 TYPE TABLE OF hrp1000.

    CHECK is_p0002-gbdat GE mv_pbegda AND
          is_p0002-gbdat LE mv_pendda.

    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
        pernr               = is_p0002-pernr
        infty               = '1001'
        subty               = 'A003'
        begda               = sy-datum
        endda               = sy-datum
      TABLES
        infty_tab           = lt_hrp1001
      EXCEPTIONS
        nothing_found       = 1
        wrong_condition     = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    READ TABLE lt_hrp1001 INDEX 1 INTO wa_hrp1001.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
        plvar               = wa_hrp1001-plvar
        otype               = 'O'
        objid               = wa_hrp1001-objid
        infty               = '1000'
        begda               = sy-datum
        endda               = sy-datum
      TABLES
        infty_tab           = lt_hrp1000
      EXCEPTIONS
        nothing_found       = 1
        wrong_condition     = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    READ TABLE lt_hrp1000 INDEX 1 INTO wa_hrp1000.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    CLEAR ms_pernr.
    ms_pernr-pernr = is_p0002-pernr.
    ms_pernr-vorna = is_p0002-vorna.
    ms_pernr-nachn = is_p0002-nachn.
    ms_pernr-gbdat = is_p0002-gbdat.
    ms_pernr-plans = wa_hrp1001-objid.
    ms_pernr-orgtx = wa_hrp1000-sgtext.
    APPEND ms_pernr TO mt_pernrs.
  ENDMETHOD.

  METHOD display_alv.
    DATA: lt_fieldcat TYPE lvc_t_fcat,
          ls_fieldcat TYPE lvc_s_fcat.

    CLEAR lt_fieldcat.
    ls_fieldcat-fieldname = 'PERNR'.
    ls_fieldcat-coltext   = 'Pernr'.
    APPEND ls_fieldcat TO lt_fieldcat.

    ls_fieldcat-fieldname = 'VORNA'.
    ls_fieldcat-coltext   = 'Vorname'.
    APPEND ls_fieldcat TO lt_fieldcat.

    ls_fieldcat-fieldname = 'NACHN'.
    ls_fieldcat-coltext   = 'Nachname'.
    APPEND ls_fieldcat TO lt_fieldcat.

    ls_fieldcat-fieldname = 'GBDAT'.
    ls_fieldcat-coltext   = 'Geburtsdatum'.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.

    ls_fieldcat-fieldname = 'ORGTX'.
    ls_fieldcat-coltext   = 'Organisation'.
    APPEND ls_fieldcat TO lt_fieldcat.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        it_fieldcat = lt_fieldcat
      TABLES
        t_outtab    = mt_pernrs.
  ENDMETHOD.

  METHOD send_email.
    DATA: lt_receivers TYPE TABLE OF adr6-smtp_addr,
          lv_receiv    TYPE adr6-smtp_addr,
          lv_text      TYPE soli,
          lt_text      TYPE TABLE OF soli,
          lv_objpack   LIKE sopcklsti1,
          lt_objpack   LIKE TABLE OF sopcklsti1,
          lt_objhead   LIKE solisti1 OCCURS 1 WITH HEADER LINE,
          lt_recepient LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
          lv_doc_chng  LIKE sodocchgi1.

    " Dummy read: Here you would fetch email addresses based on org assignment
    LOOP AT mt_pernrs INTO ms_pernr.
      lv_receiv = 'someone@example.com'. " placeholder
      APPEND lv_receiv TO lt_receivers.
    ENDLOOP.

    CLEAR lv_text.
    lv_text = 'Geburtstagsliste im Anhang'.
    APPEND lv_text TO lt_text.

    lv_doc_chng-obj_name = 'BIRTHDAY_LIST'.
    lv_doc_chng-obj_descr = 'Geburtstagsliste'.

    CLEAR lv_objpack.
    lv_objpack-head_start = 1.
    lv_objpack-head_num   = 0.
    lv_objpack-body_start = 1.
    lv_objpack-body_num   = LINES( lt_text ).
    lv_objpack-doc_type   = 'RAW'.
    APPEND lv_objpack TO lt_objpack.

    LOOP AT lt_receivers INTO lv_receiv.
      CLEAR lt_recepient.
      lt_recepient-receiver = lv_receiv.
      lt_recepient-rec_type = 'U'.
      APPEND lt_recepient.
    ENDLOOP.

    CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = lv_doc_chng
      TABLES
        packing_list               = lt_objpack
        contents_txt               = lt_text
        receivers                  = lt_recepient
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        operation_no_authorization = 3
        OTHERS                     = 4.
  ENDMETHOD.

ENDCLASS.
