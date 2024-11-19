CLASS zcl_dtti_new_mapping_info DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING source_col TYPE fieldname DEFAULT space target_col TYPE fieldname DEFAULT space.

    DATA:
      source_col TYPE fieldname,
      target_col TYPE fieldname.
ENDCLASS.

CLASS zcl_dtti_new_mapping_info IMPLEMENTATION.
  METHOD constructor.
    me->source_col = source_col.
    me->target_col = target_col.
  ENDMETHOD.
ENDCLASS.
