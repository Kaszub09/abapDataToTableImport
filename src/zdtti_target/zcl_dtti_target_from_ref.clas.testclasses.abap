CLASS ltcl_dtti_target_from_ref DEFINITION DEFERRED.
CLASS zcl_dtti_target_from_ref DEFINITION LOCAL FRIENDS ltcl_dtti_target_from_ref.
CLASS ltcl_dtti_target_from_ref DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      fields_recognized FOR TESTING,
      key_fields_for_ddic_table FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_dtti_target_from_ref.
ENDCLASS.


CLASS ltcl_dtti_target_from_ref IMPLEMENTATION.
  METHOD fields_recognized.
    TYPES:
      BEGIN OF t_test_table,
        field   TYPE fieldname,
        counter TYPE i,
      END OF t_test_table,
      tt_test_table TYPE STANDARD TABLE OF t_test_table WITH EMPTY KEY.
    DATA test_table TYPE tt_test_table.
    cut = NEW #( REF #( test_table ) ).

    DATA(table_info) = cut->zif_dtti_target~get_target_table_info( ).
    IF NOT line_exists( table_info[ field = 'FIELD' ] ).
      cl_abap_unit_assert=>fail( |Column FIELD not found in table info| ).
    ENDIF.
    IF NOT line_exists( table_info[ field = 'COUNTER' ] ).
      cl_abap_unit_assert=>fail( |Column COUNTER not found in table info| ).
    ENDIF.
  ENDMETHOD.

  METHOD key_fields_for_ddic_table.
    DATA tadir TYPE STANDARD TABLE OF tadir WITH EMPTY KEY.
    cut = NEW #( REF #( tadir ) ).

    DATA(table_info) = cut->zif_dtti_target~get_target_table_info( ).

    cl_abap_unit_assert=>assert_equals( act = table_info[ field = 'PGMID' ]-is_key exp = abap_true msg = |Expected PGMID to be key field| ).
    cl_abap_unit_assert=>assert_equals( act = table_info[ field = 'OBJECT' ]-is_key exp = abap_true msg = |Expected OBJECT to be key field| ).
    cl_abap_unit_assert=>assert_equals( act = table_info[ field = 'KORRNUM' ]-is_key exp = abap_false msg = |Expected KORRNUM to be non-key field| ).
  ENDMETHOD.


ENDCLASS.
