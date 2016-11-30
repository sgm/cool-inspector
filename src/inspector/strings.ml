type id =
[ `_CODE
| `_WINDOWS_CODE
| `_ENGLISH_NAME
| `_LOCALE_NAME
| `_MESSAGE of message
| `_ERROR of error
| `ALL_FILES
| `APP_TITLE
| `APP_TITLE_WITH_FILE of string
| `AST
| `BIG_STEP_DERIV
| `CLOSE
| `COPY
| `COOL_SOURCE
| `CURRENT_ZOOM
| `CUT
| `DELETE
| `DETAILS
| `EDIT
| `ENV_STORE_ASSOC
| `ENVIRONMENT
| `ERROR
| `EXPORT_TO_PDF
| `FILE
| `IDENTIFIER
| `INPUT_METHODS
| `INSERT_UNICODE_CONTROL_CHARACTER
| `KIND_OF_PROGRAM_STATE_TO_ANALYZE
| `LOCATION
| `NEW
| `NEW_DOCUMENT_TITLE
| `METHOD_TO_ANALYZE
| `OPEN
| `OUTPUT
| `PASTE
| `PDF_FILE
| `PROGRAM_STATE_TO_ANALYZE
| `QUIT
| `REDO
| `RUN
| `SAVE
| `SAVE_AS
| `SELECT_ALL
| `STORE
| `STORE_AND_ENVS
| `TYPE_DERIV
| `UNDO
| `VALUE
| `VIEW
| `ZOOM
| `ZOOM_IN
| `ZOOM_OUT ]
and message =
[ `_CODE
| `HELLO_WORLD_EXAMPLE
| `SAVE_ON_CLOSE of string ]
and error =
[ `_CODE
| `COULD_NOT_READ_FILE of string
| `COULD_NOT_WRITE_FILE of string
| `FILE_DOES_NOT_EXIST of string
| `GENERIC_ERROR of string ]

let get language l = List.find (fun f -> f `_CODE = language) l

let hello_world_example message =
  "class Main() extends IO() {\n" ^
  "  {\n" ^
  "    out_string(\"" ^ message ^ "\\n\")\n" ^
  "  };\n" ^
  "}\n"
(*******************************************************************************
                                 Simple Strings
*******************************************************************************)
let rec list = [
  (* English *)
  (function
  | `_CODE                            -> "en_US"
  | `_WINDOWS_CODE                    -> "English_United States"
  | `_ENGLISH_NAME                    -> "English"
  | `_LOCALE_NAME                     -> "English"
  | `_MESSAGE message                 -> get "en_US" messages message
  | `_ERROR error                     -> get "en_US" errors error
  | `ALL_FILES                        -> "All files"
  | `APP_TITLE                        -> "Cool Inspector"
  | `APP_TITLE_WITH_FILE filename     -> filename ^ " - Cool Inspector"
  | `AST                              -> "Abstract syntax tree"
  | `BIG_STEP_DERIV                   -> "Big-step derivation tree"
  | `CLOSE                            -> "Close"
  | `COPY                             -> "Copy"
  | `COOL_SOURCE                      -> "Cool source file (*.cool)"
  | `CURRENT_ZOOM                     -> "Current zoom"
  | `CUT                              -> "Cut"
  | `DELETE                           -> "Delete"
  | `DETAILS                          -> "Detalhes"
  | `EDIT                             -> "Edit"
  | `ENV_STORE_ASSOC                  -> "Environment <-> Store"
  | `ENVIRONMENT                      -> "Environment"
  | `ERROR                            -> "Error"
  | `EXPORT_TO_PDF                    -> "Export to PDF"
  | `FILE                             -> "File"
  | `IDENTIFIER                       -> "Identifier"
  | `INPUT_METHODS                    -> "Input methods"
  | `INSERT_UNICODE_CONTROL_CHARACTER -> "Insert Unicode control character"
  | `KIND_OF_PROGRAM_STATE_TO_ANALYZE -> "Kind of program state to analyze"
  | `LOCATION                         -> "Location"
  | `NEW                              -> "New"
  | `NEW_DOCUMENT_TITLE               -> "(new)"
  | `METHOD_TO_ANALYZE                -> "Method to analyze"
  | `OPEN                             -> "Open"
  | `OUTPUT                           -> "Output"
  | `PASTE                            -> "Paste"
  | `PDF_FILE                         -> "PDF file (*.pdf)"
  | `PROGRAM_STATE_TO_ANALYZE         -> "Program state to analyze"
  | `QUIT                             -> "Quit"
  | `REDO                             -> "Redo"
  | `RUN                              -> "Run"
  | `SAVE                             -> "Save"
  | `SAVE_AS                          -> "Save as..."
  | `SELECT_ALL                       -> "Select all"
  | `STORE                            -> "Store"
  | `STORE_AND_ENVS                   -> "Stores and environments"
  | `TYPE_DERIV                       -> "Type derivation tree"
  | `UNDO                             -> "Undo"
  | `VALUE                            -> "Value"
  | `VIEW                             -> "View"
  | `ZOOM_IN                          -> "Zoom in"
  | `ZOOM_OUT                         -> "Zoom out");

  (* Brazilian Portuguese *)
  (function
  | `_CODE                            -> "pt_BR"
  | `_WINDOWS_CODE                    -> "Portuguese_Brazil"
  | `_ENGLISH_NAME                    -> "Brazilian portuguese"
  | `_LOCALE_NAME                     -> "Portugu\195\170s brasileiro"
  | `_MESSAGE message                 -> get "pt_BR" messages message
  | `_ERROR error                     -> get "pt_BR" errors error
  | `ALL_FILES                        -> "Todos os arquivos"
  | `APP_TITLE                        -> "Cool Inspector"
  | `APP_TITLE_WITH_FILE filename     -> filename ^ " - Cool Inspector"
  | `AST                              -> "\195\129rvore de sintaxe abstrata"
  | `BIG_STEP_DERIV       -> "\195\129rvore de deriva\195\167\195\163o big-step"
  | `CLOSE                            -> "Fechar"
  | `COPY                             -> "Copiar"
  | `COOL_SOURCE                     -> "Arquivo de c\195\179digo Cool (*.cool)"
  | `CURRENT_ZOOM                     -> "Zoom atual"
  | `CUT                              -> "Recortar"
  | `DELETE                           -> "Apagar"
  | `DETAILS                          -> "Detalhes"
  | `EDIT                             -> "Editar"
  | `ENV_STORE_ASSOC                  -> "Ambiente <-> Mem\195\179ria"
  | `ENVIRONMENT                      -> "Ambiente"
  | `ERROR                            -> "Erro"
  | `EXPORT_TO_PDF                    -> "Exportar para PDF"
  | `FILE                             -> "Arquivo"
  | `IDENTIFIER                       -> "Identificador"
  | `INPUT_METHODS                    -> "M\195\169todos de entrada"
  | `INSERT_UNICODE_CONTROL_CHARACTER -> "Inserir caractere de controle Unicode"
  | `KIND_OF_PROGRAM_STATE_TO_ANALYZE ->
      "Tipo de estado de programa a ser analisado"
  | `LOCATION                         -> "Localiza\195\167\195\163o"
  | `NEW                              -> "Novo"
  | `NEW_DOCUMENT_TITLE               -> "(novo)"
  | `METHOD_TO_ANALYZE                -> "M\195\169todo a ser analisado"
  | `OPEN                             -> "Abrir"
  | `OUTPUT                           -> "Sa\195\173da"
  | `PASTE                            -> "Colar"
  | `PDF_FILE                         -> "Arquivo PDF (*.pdf)"
  | `PROGRAM_STATE_TO_ANALYZE         -> "Estado do programa a ser analisado"
  | `QUIT                             -> "Sair"
  | `REDO                             -> "Refazer"
  | `RUN                              -> "Executar"
  | `SAVE                             -> "Salvar"
  | `SAVE_AS                          -> "Salvar como..."
  | `SELECT_ALL                       -> "Selecionar tudo"
  | `STORE                            -> "Mem\195\179ria"
  | `STORE_AND_ENVS                   -> "Mem\195\179rias e ambientes"
  | `TYPE_DERIV           -> "\195\129rvore de deriva\195\167\195\163o de tipos"
  | `UNDO                             -> "Desfazer"
  | `VALUE                            -> "Valor"
  | `VIEW                             -> "Visualizar"
  | `ZOOM_IN                          -> "Aumentar zoom"
  | `ZOOM_OUT                         -> "Diminuir zoom");
]

(*******************************************************************************
                                    Messages
*******************************************************************************)
and messages = [
  (* English *)
  (function
  | `_CODE -> "en_US"
  | `HELLO_WORLD_EXAMPLE ->
      hello_world_example "Hello, world!"
  | `SAVE_ON_CLOSE file ->
      "Save file " ^ file ^ "?");

  (* Brazilian Portuguese *)
  (function
  | `_CODE -> "pt_BR"
  | `HELLO_WORLD_EXAMPLE ->
      hello_world_example "Ol\195\161, mundo!"
  | `SAVE_ON_CLOSE file ->
      "Salvar arquivo " ^ file ^ "?");
]

(*******************************************************************************
                                 Error Messages
*******************************************************************************)
and errors = [
  (* English *)
  (function
  | `_CODE -> "en_US"
  | `COULD_NOT_READ_FILE file ->
      "Could not read to file:\n" ^ file ^
        "\n\nVerify that the filename is valid."
  | `COULD_NOT_WRITE_FILE file ->
      "Could not write to file:\n" ^ file ^
        "\n\nVerify that the filename is valid or " ^
        "if the file is opened in another program."
  | `FILE_DOES_NOT_EXIST file ->
      "Specified file does not exist:\n" ^ file ^
        "\n\nCheck the spelling of the filename, " ^
        "and verify that the file location is correct."
  | `GENERIC_ERROR s -> s);

  (* Brazilian Portuguese *)
  (function
  | `_CODE -> "pt_BR"
  | `COULD_NOT_READ_FILE file ->
      "N\195\163o foi poss\195\173vel ler o arquivo:\n" ^ file ^
        "\n\nVerifique se o nome do arquivo \195\169 v\195\161lido."
  | `COULD_NOT_WRITE_FILE file ->
      "N\195\163o foi poss\195\173vel salvar o arquivo:\n" ^ file ^
        "\n\nVerifique se o nome do arquivo \195\169 v\195\161lido ou " ^
        "se o arquivo est\195\161 aberto em outro programa."
  | `FILE_DOES_NOT_EXIST file ->
      "O arquivo especificado n\195\163o existe:\n" ^ file ^
        "\n\nVerifique a ortografia do nome do arquivo " ^
        "e que a localiza\195\167\195\163o do arquivo est\195\161 correta."
  | `GENERIC_ERROR s -> s);
]
