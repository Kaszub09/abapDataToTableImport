CLASS zcl_dtti_mapper DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF t_mapping_result,
        ok_fields    TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY,
        error_fields TYPE STANDARD TABLE OF fieldname WITH EMPTY KEY,
        comment      TYPE string,
      END OF t_mapping_result,
      tt_mapping_result TYPE STANDARD TABLE OF t_mapping_result WITH EMPTY KEY.

    CLASS-METHODS:
        map IMPORTING mapping TYPE zcl_dtti_mapping_alv=>tt_mapping source_tab TYPE REF TO data target_tab TYPE REF TO data
                      convert_currrency_to_internal TYPE abap_bool
            RETURNING VALUE(mapping_result) TYPE tt_mapping_result,
        try_to_map IMPORTING type_kind TYPE abap_typecategory source_field TYPE any conversion_exit_input TYPE funcnam
                   CHANGING target_field TYPE any
                   RETURNING VALUE(error) TYPE string,
        try_converting_currency IMPORTING source_field TYPE any currency TYPE any
                                CHANGING target_field TYPE any
                                RETURNING VALUE(error) TYPE string.
ENDCLASS.



CLASS zcl_dtti_mapper IMPLEMENTATION.
  METHOD map.
    ref_to_tab_fs source_tab <source_tab>.
    ref_to_tab_fs target_tab <target_table>.
    CLEAR <target_table>.

    DATA target_row TYPE REF TO data.
    CREATE DATA target_row LIKE LINE OF <target_table>.
    ASSIGN target_row->* TO FIELD-SYMBOL(<target_row>).

    LOOP AT <source_tab> ASSIGNING FIELD-SYMBOL(<source_row>).
      DATA(map_result) = VALUE t_mapping_result( ).
      CLEAR <target_row>.

      LOOP AT mapping REFERENCE INTO DATA(map) WHERE source_field IS NOT INITIAL.
        ASSIGN COMPONENT map->source_field OF STRUCTURE <source_row> TO FIELD-SYMBOL(<source_field>).
        ASSIGN COMPONENT map->field OF STRUCTURE <target_row> TO FIELD-SYMBOL(<target_field>).

        DATA(error) = try_to_map( EXPORTING type_kind = map->type->type_kind source_field = <source_field> conversion_exit_input = map->conversion_exit_input
                                  CHANGING target_field = <target_field> ).

        IF convert_currrency_to_internal = abap_true AND map->currency_field IS NOT INITIAL AND strlen( error ) = 0.
          DATA(currency) = VALUE waers_curc( ).
          DATA(currency_source_field) = VALUE #( mapping[ KEY field field = map->currency_field ]-source_field OPTIONAL ).
          ASSIGN COMPONENT currency_source_field OF STRUCTURE <source_row> TO FIELD-SYMBOL(<currency>).
          IF sy-subrc = 0.
            currency = <currency>.
          ENDIF.
          "target now stores converted number, but possibly at wrong decimal places
          try_converting_currency( EXPORTING source_field = <target_field> currency = currency CHANGING target_field = <target_field> ).
        ENDIF.


        IF strlen( error ) = 0.
          APPEND map->source_field TO map_result-ok_fields.

        ELSE.
          APPEND map->source_field TO map_result-error_fields.
          map_result-comment = |{ map_result-comment }{ map->source_field_button }:{ error }; |.

        ENDIF.
      ENDLOOP.

      IF strlen( map_result-comment ) = 0.
        APPEND <target_row> TO <target_table>.
      ENDIF.

      APPEND map_result TO mapping_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD try_to_map.
    "Consider space as initial value for any type
    IF source_field = space.
      RETURN.
    ENDIF.

    TRY.
        IF conversion_exit_input IS NOT INITIAL.
          CALL FUNCTION conversion_exit_input EXPORTING input = source_field IMPORTING output = target_field EXCEPTIONS OTHERS = 1.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE zcx_dtti_exception EXPORTING custom_message = replace( val = TEXT-006 sub = '&1' with = conversion_exit_input occ = 0 ).
          ENDIF.
          RETURN.
        ENDIF.

        CASE type_kind.
          WHEN cl_abap_datadescr=>typekind_date.
            DATA(clean_date) = condense( source_field ).
            IF clean_date CP '++++-++-++'.
              "^Check for YYYY-MM-DD, which is a format imported from Excel
              SPLIT clean_date AT '-' INTO TABLE DATA(splitted_date_excel).
              clean_date = |{ splitted_date_excel[ 1 ] }{ splitted_date_excel[ 2 ] WIDTH = 2 PAD = '0' ALIGN = RIGHT }{ splitted_date_excel[ 3 ] WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.

            ELSEIF clean_date CP '*.*.*' OR clean_date CP '*/*/*' OR clean_date CP '*-*-*'.
              "^Assume that it's either YYYYMMSS or DD.MM.YYYY or DD/MM/YYYY or DD-MM-YYYY
              SPLIT replace( val = replace( val = clean_date sub = '-' with = '.' occ = 0 ) sub = '/' with = '.' occ = 0 ) AT '.' INTO TABLE DATA(splitted_date).
              clean_date = |{ splitted_date[ 3 ] }{ splitted_date[ 2 ] WIDTH = 2 PAD = '0' ALIGN = RIGHT }{ splitted_date[ 1 ] WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
            ENDIF.

            DATA(date_to_check) = CONV syst_datum( clean_date ).
            CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
              EXPORTING
                date                      = date_to_check
              EXCEPTIONS
                plausibility_check_failed = 1
                OTHERS                    = 2.
            IF sy-subrc <> 0 OR strlen( clean_date ) <> 8.
              RAISE EXCEPTION TYPE zcx_dtti_exception EXPORTING custom_message = replace( val = TEXT-002 sub = '&1' with = clean_date occ = 0 ).
            ENDIF.

            target_field = CONV d( clean_date ).

            "--------------------------------------------------
          WHEN cl_abap_datadescr=>typekind_time.
            "Assume that it's either HHMMSS or H:M:S
            DATA(clean_time) = condense( source_field ).
            IF clean_time CP '*:*:*'.
              SPLIT clean_time AT ':' INTO TABLE DATA(splitted_time).
              clean_time = |{ splitted_time[ 1 ] WIDTH = 2 PAD = '0' ALIGN = RIGHT }{ splitted_time[ 2 ] WIDTH = 2 PAD = '0' ALIGN = RIGHT }{ splitted_time[ 3 ] WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
            ENDIF.

            DATA(time_to_check) = CONV syst_uzeit( clean_time ).
            CALL FUNCTION 'TIME_CHECK_PLAUSIBILITY'
              EXPORTING
                time                      = time_to_check
              EXCEPTIONS
                plausibility_check_failed = 1
                OTHERS                    = 2.
            IF sy-subrc <> 0 OR strlen( clean_time ) <> 6.
              RAISE EXCEPTION TYPE zcx_dtti_exception EXPORTING custom_message = replace( val = TEXT-001 sub = '&1' with = clean_time occ = 0 ).
            ENDIF.

            target_field = CONV t( clean_time ).

            "--------------------------------------------------
          WHEN cl_abap_datadescr=>typekind_decfloat OR cl_abap_datadescr=>typekind_decfloat16 OR cl_abap_datadescr=>typekind_decfloat34
              OR cl_abap_datadescr=>typekind_float OR cl_abap_datadescr=>typekind_packed.
            "Assume comma as other possible separator, clean whitespaces.
            target_field = condense( replace( val = replace( val = source_field sub = ',' with = '.' occ = 0 ) sub = ` ` with = `` occ = 0 ) ).

            "--------------------------------------------------
          WHEN cl_abap_datadescr=>typekind_int OR cl_abap_datadescr=>typekind_int1 OR cl_abap_datadescr=>typekind_int2 OR cl_abap_datadescr=>typekind_int8.
            IF CONV string( source_field ) CA '.'.
              RAISE EXCEPTION TYPE zcx_dtti_exception EXPORTING custom_message = replace( val = TEXT-003 sub = '&1' with = source_field occ = 0 ).
            ENDIF.

            "Clean whitespaces.
            target_field = condense( replace( val = source_field sub = ` ` with = `` occ = 0 ) ).

            "--------------------------------------------------
          WHEN OTHERS.
            target_field = source_field.
        ENDCASE.

      CATCH cx_root INTO DATA(cx).
        error = cx->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD try_converting_currency.
    DATA bapireturn  TYPE bapireturn.
    DATA(amount_external) = CONV bapicurr_d( source_field ).
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
      EXPORTING
        currency             = currency
        amount_external      = amount_external                 " External Currency Amount
        max_number_of_digits = 23                 " Maximum Field Length of Internal Domains
      IMPORTING
        amount_internal      = target_field                 " Converted Internal Currency Amount
        return               = bapireturn.                  " Return Messages

    error = bapireturn-message.
  ENDMETHOD.

ENDCLASS.
