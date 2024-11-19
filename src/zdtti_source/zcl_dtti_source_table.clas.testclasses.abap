
CLASS ltcl_dtti_source_table DEFINITION DEFERRED.
CLASS zcl_dtti_source_table DEFINITION LOCAL FRIENDS ltcl_dtti_source_table.
CLASS ltcl_dtti_source_table DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS:
      return_supplied_data FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_dtti_source_table.
ENDCLASS.

CLASS ltcl_dtti_source_table IMPLEMENTATION.
  METHOD return_supplied_data.
    DATA test_table TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    test_table = VALUE #( ( 2 ) ( 1 ) ).

    cut = NEW #( REF #( test_table ) ).

    DATA(returned_data_ref) = cut->zif_dtti_source~get_source_table( ).
    ASSIGN returned_data_ref->* TO FIELD-SYMBOL(<returned_data>).
    cl_abap_unit_assert=>assert_equals( act = <returned_data> exp = test_table ).
  ENDMETHOD.
ENDCLASS.