CLASS zcl_dtti_target_factory DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      "! @parameter target_table | <p class="shorttext synchronized" lang="en">If it's DDIC table, key fields are automatically detected and market as required</p>
      create_from_ref IMPORTING target_table TYPE REF TO data RETURNING VALUE(target) TYPE REF TO zif_dtti_target,
      create_from_info IMPORTING info TYPE zif_dtti_target=>tt_target OPTIONAL RETURNING VALUE(target) TYPE REF TO zif_dtti_target.
ENDCLASS.

CLASS zcl_dtti_target_factory IMPLEMENTATION.
  METHOD create_from_ref.
    target = NEW zcl_dtti_target_from_ref( target_table ).
  ENDMETHOD.

  METHOD create_from_info.
    target = NEW zcl_dtti_target_base( ).
    IF info IS SUPPLIED AND lines( info ) > 0.
      DATA(info_copy) = info.
      LOOP AT info_copy REFERENCE INTO DATA(info_row).
        DATA(new_description) = VALUE string( ).
        IF info_row->type->is_ddic_type( ) .
          info_row->is_ddic = abap_true.
          IF info_row->type IS INSTANCE OF cl_abap_elemdescr.
            DATA(de_descr) = CAST cl_abap_elemdescr( info_row->type ).
            new_description = de_descr->get_ddic_field( )-scrtext_l.
          ENDIF.
        ELSE.
          new_description = info_row->field.
        ENDIF.

        IF info_row->field_description IS INITIAL.
          info_row->field_description = new_description.
        ENDIF.
      ENDLOOP.
      target->set_target_table_info( info_copy ).
      target->refresh_table_structure( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
