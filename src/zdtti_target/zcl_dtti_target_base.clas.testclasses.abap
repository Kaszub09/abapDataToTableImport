CLASS ltcl_dtti_target_base DEFINITION DEFERRED.
CLASS zcl_dtti_target_base DEFINITION LOCAL FRIENDS ltcl_dtti_target_base.
CLASS ltcl_dtti_target_base DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      return_correct_structure FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_dtti_target_base.
ENDCLASS.


CLASS ltcl_dtti_target_base IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD return_correct_structure.
    cut->zif_dtti_target~set_target_table_info( VALUE #(
      ( field = 'FIELD_1' type = CAST cl_abap_datadescr( cl_abap_datadescr=>describe_by_data( 1 ) ) )
      ( field = 'FIELD_2' type = CAST cl_abap_datadescr( cl_abap_datadescr=>describe_by_name( 'FIELDNAME' ) ) ) ) ).
    cut->zif_dtti_target~set_field_info(
        VALUE #(  field = 'FIELD_3' type = CAST cl_abap_datadescr( cl_abap_datadescr=>describe_by_data( sy-datum ) ) ) ).

    cut->zif_dtti_target~set_field_info(
        VALUE #(  field = 'STRUCT1-FIELD1' type = CAST cl_abap_datadescr( cl_abap_datadescr=>describe_by_data( 1 ) ) ) ).

    cut->zif_dtti_target~set_field_info(
        VALUE #(  field = 'STRUCT1-SUBSTRUCT1-FIELD1' type = CAST cl_abap_datadescr( cl_abap_datadescr=>describe_by_data( 1 ) ) ) ).
    cut->zif_dtti_target~set_field_info(
        VALUE #(  field = 'STRUCT1-SUBSTRUCT1-FIELD2' type = CAST cl_abap_datadescr( cl_abap_datadescr=>describe_by_data( 1 ) ) ) ).



    "Get table with empty line
    DATA(tab_ref) = cut->zif_dtti_target~get_target_table( ).
    FIELD-SYMBOLS <table> TYPE table.
    ASSIGN tab_ref->* TO <table>.
    INSERT INITIAL LINE INTO TABLE <table> ASSIGNING FIELD-SYMBOL(<line>).

    "Check if fields created
    FIELD-SYMBOLS <field_1> TYPE i.
    ASSIGN COMPONENT 1 OF STRUCTURE <line> TO <field_1>.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( msg = 'Component 1 FIELD_1 not created' ).
    ENDIF.

    ASSIGN COMPONENT 2 OF STRUCTURE <line> TO FIELD-SYMBOL(<field_2>).
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( msg = 'Component 2 FIELD_2 not created' ).
    ENDIF.

    FIELD-SYMBOLS <field_3> TYPE d.
    ASSIGN COMPONENT 3 OF STRUCTURE <line> TO <field_3>.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( msg = 'Component 3 FIELD_3 not created' ).
    ENDIF.

    ASSIGN COMPONENT 'STRUCT1-FIELD1' OF STRUCTURE <line> TO FIELD-SYMBOL(<struct1_field1>).
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( msg = 'Component STRUCT1-FIELD1 not created' ).
    ENDIF.

    ASSIGN COMPONENT 'STRUCT1-SUBSTRUCT1-FIELD1' OF STRUCTURE <line> TO FIELD-SYMBOL(<struct1_substruct1_field1>).
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( msg = 'Component STRUCT1-SUBSTRUCT1-FIELD1 not created' ).
    ENDIF.

    ASSIGN COMPONENT 'STRUCT1-SUBSTRUCT1-FIELD2' OF STRUCTURE <line> TO FIELD-SYMBOL(<struct1_substruct1_field2>).
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( msg = 'Component STRUCT1-SUBSTRUCT1-FIELD2 not created' ).
    ENDIF.

    ASSIGN COMPONENT 5 OF STRUCTURE <line> TO FIELD-SYMBOL(<field_5>).
    IF sy-subrc = 0.
      cl_abap_unit_assert=>fail( msg = 'Component 5 FIELD_5 should not be created' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
