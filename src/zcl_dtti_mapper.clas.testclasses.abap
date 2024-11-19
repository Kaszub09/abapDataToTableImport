
CLASS ltcl_map_fields DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_testdata,
        type_kind  TYPE abap_typecategory,
        source     TYPE string,
        expected   TYPE string,
        any_errors TYPE abap_bool,
      END OF t_testdata,
      tt_testdata TYPE STANDARD TABLE OF t_testdata WITH EMPTY KEY.

    METHODS:
      map_dates FOR TESTING,
      map_times FOR TESTING,
      map_numbers FOR TESTING,
      map_string FOR TESTING,
      check_test_data IMPORTING test_data TYPE tt_testdata CHANGING target_field TYPE any.
ENDCLASS.

CLASS ltcl_map_fields IMPLEMENTATION.
  METHOD map_dates.
    DATA target_field TYPE d.

    check_test_data( EXPORTING test_data = VALUE #( type_kind = cl_abap_datadescr=>typekind_date
                                          ( source = '20241231' expected = '20241231' any_errors = abap_false )
                                          ( source = '31.12.2024' expected = '20241231' any_errors = abap_false )
                                          ( source = '2024-12-31' expected = '20241231' any_errors = abap_false )
                                          ( source = '31/12/2024' expected = '20241231' any_errors = abap_false )
                                          ( source = '31-12-2024' expected = '20241231' any_errors = abap_false )
                                          ( source = '1.1.2024' expected = '20240101' any_errors = abap_false )
                                          ( source = '31.12.24' any_errors = abap_true )
                                          ( source = '2024ASD' any_errors = abap_true )
                                          ( source = '243131' any_errors = abap_true ) )
                     CHANGING target_field = target_field ).
  ENDMETHOD.

  METHOD map_numbers.
    DATA target_field TYPE i.

    check_test_data( EXPORTING test_data = VALUE #( type_kind = cl_abap_datadescr=>typekind_int
                                          ( source = '123' expected = '123' any_errors = abap_false )
                                          ( source = '123 456' expected = '123456' any_errors = abap_false )
                                          ( source = '123.123' any_errors = abap_true )
                                          ( source = 'X' any_errors = abap_true ) )
                     CHANGING target_field = target_field ).

    DATA target_field_2 TYPE p LENGTH 6 DECIMALS 2.
    check_test_data( EXPORTING test_data = VALUE #( type_kind = cl_abap_datadescr=>typekind_packed
                                          ( source = '123' expected = '123.00' any_errors = abap_false )
                                          ( source = '123 456.089' expected = '123456.09' any_errors = abap_false )
                                          ( source = '123 456,089' expected = '123456.09' any_errors = abap_false )
                                          ( source = 'X' any_errors = abap_true ) )
                     CHANGING target_field = target_field_2 ).
  ENDMETHOD.

  METHOD map_string.
    DATA target_field TYPE string.

    check_test_data( EXPORTING test_data = VALUE #( type_kind = cl_abap_datadescr=>typekind_string
                                          ( source = 'Aa' expected = 'Aa' any_errors = abap_false ) )
                     CHANGING target_field = target_field ).
  ENDMETHOD.

  METHOD map_times.
    DATA target_field TYPE t.

    check_test_data( EXPORTING test_data = VALUE #( type_kind = cl_abap_datadescr=>typekind_time
                                          ( source = '202122' expected = '202122' any_errors = abap_false )
                                          ( source = '20:21:22' expected = '202122' any_errors = abap_false )
                                          ( source = '2:5:02' expected = '020502' any_errors = abap_false )
                                          ( source = '1123' any_errors = abap_true )
                                          ( source = '20.21.22' any_errors = abap_true ) )
                     CHANGING target_field = target_field ).
  ENDMETHOD.

  METHOD check_test_data.
    LOOP AT test_data REFERENCE INTO DATA(test).
      CLEAR target_field.
      DATA(msg) = |T={ test->type_kind };S={ test->source };E={ test->expected };ERR={ test->any_errors }|.

      DATA(error) = zcl_dtti_mapper=>try_to_map( EXPORTING type_kind = test->type_kind source_field = test->source
          CHANGING target_field = target_field ).

      IF test->any_errors = abap_true.
        cl_abap_unit_assert=>assert_not_initial( act = error msg = |Unexpected error. { msg }| quit = if_aunit_constants=>no ).
        cl_abap_unit_assert=>assert_initial( act = target_field msg = |Expected initial target. { msg }| ).
      ELSE.
        cl_abap_unit_assert=>assert_initial( act = error msg = |Expected error. { msg }| quit = if_aunit_constants=>no ).
        cl_abap_unit_assert=>assert_equals( act = |{ target_field }| exp = test->expected  msg = |Unexpected result. { msg }| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
