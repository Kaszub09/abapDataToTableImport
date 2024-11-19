"! <p class="shorttext synchronized">Source data - use ZCL_DTTI_SOURCE_FACTORY to create</p>
INTERFACE zif_dtti_source PUBLIC.
  TYPES:
    BEGIN OF t_source_field_info,
      field       TYPE fieldname,
      description TYPE string,
      is_ddic     TYPE abap_bool,
    END OF t_source_field_info,
    tt_source_field_info TYPE STANDARD TABLE OF t_source_field_info WITH EMPTY KEY
        WITH UNIQUE SORTED KEY field COMPONENTS field.
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Get reference to table with data to map</p>
    get_source_table RETURNING VALUE(source_table) TYPE REF TO data.
  DATA:
      source_field_info TYPE tt_source_field_info READ-ONLY.
ENDINTERFACE.
