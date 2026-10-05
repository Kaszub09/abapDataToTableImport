"! <p class="shorttext synchronized">Target info - use ZCL_DTTI_TARGET_FACTORY to create</p>
INTERFACE zif_dtti_target PUBLIC.
  TYPES:
    BEGIN OF t_target,
      field                 TYPE fieldname,
      "! <p class="shorttext synchronized">Taken from DDIC if possible, otherwise from field name.</p>
      field_description     TYPE string,
      type                  TYPE REF TO cl_abap_datadescr,
      is_ddic               TYPE abap_bool,
      is_key                TYPE abap_bool,
      is_required           TYPE abap_bool,
      is_hidden             TYPE abap_bool,
      conversion_exit_input TYPE funcnam,
      use_conv_exit         TYPE abap_bool,
      source_field          TYPE fieldname,
      "! Fill manually if needed
      currency_field        TYPE fieldname,
    END OF t_target,
    tt_target TYPE STANDARD TABLE OF t_target WITH EMPTY KEY
    WITH UNIQUE SORTED KEY field COMPONENTS field.

  METHODS:
    get_target_table RETURNING VALUE(target_table) TYPE REF TO data,
    get_target_table_info RETURNING VALUE(info) TYPE tt_target,
    "! @parameter refresh_needed | <p class="shorttext synchronized" lang="en">Set to false only if table reference doens't need refresh,
    "! so e.g. you want to mass change source and stuff like that</p>
    set_target_table_info IMPORTING info TYPE tt_target refresh_needed TYPE abap_bool DEFAULT abap_true,
    set_field_info IMPORTING field_info TYPE t_target,
    set_field_is_key IMPORTING field TYPE fieldname is_key TYPE abap_bool DEFAULT abap_true,
    set_field_is_required IMPORTING field TYPE fieldname is_required TYPE abap_bool DEFAULT abap_true,
    set_field_is_hidden IMPORTING field TYPE fieldname is_hidden TYPE abap_bool DEFAULT abap_true,
    "! <p class="shorttext synchronized" lang="en">By default description is taken from DDIC if possible, otherwise same as field name.</p>
    set_field_description IMPORTING field TYPE fieldname description TYPE csequence,
    set_currency_field IMPORTING field TYPE fieldname currency_field TYPE fieldname,
    set_field_conv_exit_input IMPORTING field TYPE fieldname conv_exit_input TYPE funcnam,
    set_field_use_conv_exit IMPORTING field TYPE fieldname use_conv_exit TYPE abap_bool DEFAULT abap_true,
    set_field_source IMPORTING field TYPE fieldname source_field TYPE fieldname,
    remove_field IMPORTING field TYPE fieldname.
ENDINTERFACE.
