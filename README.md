
### Installation
Via https://github.com/abapGit/abapGit. Requires https://github.com/Kaszub09/abapEasyALV. Written in ABAP 7.50.

### Description
Getting data from user made easy! 

Whenever you need to user to supply data, be it table or file - usually you have some target structure, which the file must match. But unless there is some documentation or example file (usually there isn't), you have to read code to see what kind of structure should file have. Instead, you could just use this framework to declare desired structure, and let it take care of parsing and mapping whatever data user (or you via internal table) supplies. Now documentation is optional since you know explicitly what columns are expected (and code doesn't lie).

For example, when you run this report:
```abap
"Sample target structure

TYPES:
  BEGIN OF t_target,
    date_field     TYPE d,
    some_name      TYPE fieldname,
    int_field      TYPE i,
    packed_field   TYPE p LENGTH 10 DECIMALS 2,
    another_column TYPE string,
  END OF t_target,
  tt_target TYPE STANDARD TABLE OF t_target WITH EMPTY KEY.
DATA:
    target_tab TYPE tt_target.

DATA(target) = zcl_dtti_target_factory=>create_from_ref( REF #( target_tab ) ).
"Set in code or create (or use existing) data elements if needed
target->set_field_description( field = 'ANOTHER_COLUMN' description = |Another column| ).
target->set_field_description( field = 'INT_FIELD' description = |Some int column| ).

"Data source from file
DATA(source) = zcl_dtti_source_factory=>create_from_file( ).
IF source IS NOT BOUND.
  MESSAGE TEXT-001 TYPE 'E'.
ENDIF.

"Get mapping
DATA(dtti) = NEW zcl_data_to_table_import( ).
DATA(result) = dtti->run_mapping( source = source target = target ).

"Display mapping result
DATA(alv) = NEW zcl_ea_salv_table(  ).
alv->set_data( target->get_target_table( ) ).
alv->display_data( ).
```
It will ask user for file, parse it, and allow user to map columns from file to the ones needed by program.
![obraz](https://github.com/user-attachments/assets/339c9267-4a47-4f21-a1c0-ecff5e543d24)

https://github.com/user-attachments/assets/ea282c18-54ae-404e-93ae-7c7c1ffdbeac


### Features
- Import data from clipboard, Excel file or text file (or supplied internal table)
- Allow user to map columns between source and target structure
- Supports Drag&Drop
- Support for editing source data in place
![obraz](https://github.com/user-attachments/assets/5d4dfe93-5dbc-4da4-9436-2bd9cbd2194a)
- Information if columns can't be mapped
![obraz](https://github.com/user-attachments/assets/4a8ace6f-3a8a-4f21-ac18-fc57df5f6f9e)
- Support for multiple common formats for date, time and numbers. In case of text files, most propable separator is detected. Currently doesn't support escaped characters inside quotes.

