(keyword) @keyword
(operator) @operator

(Token_Integer) @number
(Token_Float) @number
(Token_Imag) @number
(Token_Rune) @number

(ERROR) @error

(type_identifier)    @type
(package_identifier) @namespace
(label_identifier)   @label

(Token_String) @string
; (escape_sequence) @string.escape TODO?

(Token_package) @keyword

(Token_Comment) @comment

((decl_identifier) @function
 (operator) (operator) [(proc_literal) (proc_group)])

(parapoly) @type

((decl_identifier) @type
 (operator) (operator) (keyword)) ; TODO review this?

((decl_identifier) @type
 (operator) (operator) (struct_type))

((decl_identifier) @type
 (operator) (operator) (union_type))

((decl_identifier) @type
 (operator) (operator) (enum_type))

((decl_identifier) @constant
 (operator) (operator) const_value: (_))

;;(decl_identifier) @constant
(declaration name: (decl_identifier) @constant)

(var_identifier) @variable

(compiler_directive) @attribute
(pragma_identifier) @attribute
(calling_convention) @attribute

((Token_Ident) @function
 (#is-not? local))

;;((Token_Ident) @constant
;; (.match? @constant "^[A-Z][A-Z_\\d]+"))

;;((Token_Ident) @variable
;; (#is-not? local))

;; (Token_Ident) @variable ; TODO default text vs Ident is that a thing?
;; TODO punctuation 
