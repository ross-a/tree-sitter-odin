
const
  op_prec = {
    control_flow: 11,
    r_unary: 10,
    l_unary: 9,
    multiplicative: 8,
    membership: 7,
    additive: 6,
    comparison: 5,
    range: 4,
    and: 3,
    or: 2,
    ternary: 1,
  }

module.exports = grammar({
  name: 'odin',

	extras: $ => [
		$.Token_Comment,
		$.Token_Semicolon,
    $.compiler_directive,
    /\s/
  ],
	
  inline: $ => [
    $._type,
    $._type_identifier,
    $._field_identifier,
		$._var_identifier,
    $._label_identifier,
    $._package_identifier,
    $._top_level_declaration,
    $._string_literal,
  ],

  conflicts: $ => [
    [$.declaration, $._range_clause],
		[$.declaration, $.assignment_statement],		
		[$.declaration, $.assignment_statement, $._range_clause],
    [$.single_assignment, $.assignment_statement],
    [$._for_init_and_update_clause, $._for_init_clause],
		[$._expression], // for parapoly
		[$.initializer_list, $.compound_literal_list],
  ],

	// note: the order here must match scanner.cc's TokenType enum
  externals: $ => [
		
		$.Token_Invalid,
		$.Token_EOF,
		$.Token_Comment,
		$.Token__LiteralBegin,
		$.Token_Ident,
		$.Token_Integer,
		$.Token_Float,
		$.Token_Imag,
		$.Token_Rune,
		$.Token_String,
		$.Token__LiteralEnd,
		$.Token__OperatorBegin,
		$.Token_Eq,
		$.Token_Not,
		$.Token_Hash,
		$.Token_At,
		$.Token_Dollar,
		$.Token_Pointer,
		$.Token_Question,
		$.Token_Add,
		$.Token_Sub,
		$.Token_Mul,
		$.Token_Quo,  
		$.Token_Mod,
		$.Token_ModMod,
		$.Token_And,
		$.Token_Or,
		$.Token_Xor,
		$.Token_AndNot,
		$.Token_Shl,                 
		$.Token_Shr,
		$.Token_CmpAnd,
		$.Token_CmpOr,
		$.Token__AssignOpBegin,
		$.Token_AddEq,
		$.Token_SubEq,   
		$.Token_MulEq,
		$.Token_QuoEq,
		$.Token_ModEq,
		$.Token_ModModEq,
		$.Token_AndEq,
		$.Token_OrEq,
		$.Token_XorEq,
		$.Token_AndNotEq,
		$.Token_ShlEq,
		$.Token_ShrEq,
		$.Token_CmpAndEq,
		$.Token_CmpOrEq,
		$.Token__AssignOpEnd,
		$.Token_Increment,
		$.Token_Decrement,
		$.Token_ArrowRight,
		$.Token_Undef,
		$.Token__ComparisonBegin,
		$.Token_CmpEq,
		$.Token_NotEq,
		$.Token_Lt,
		$.Token_Gt,
		$.Token_LtEq,
		$.Token_GtEq,
		$.Token__ComparisonEnd,
		$.Token_OpenParen,
		$.Token_CloseParen,
		$.Token_OpenBracket,
		$.Token_CloseBracket,
		$.Token_OpenBrace,
		$.Token_CloseBrace,
		$.Token_Colon,
		$.Token_Semicolon,
		$.Token_Period,
		$.Token_Comma,
		$.Token_Ellipsis,
		$.Token_RangeFull,
		$.Token_RangeHalf,
		$.Token_BackSlash,
		$.Token__OperatorEnd,
		$.Token__KeywordBegin,
		$.Token_import,
		$.Token_foreign,
		$.Token_package,
		$.Token_typeid,
		$.Token_when,
		$.Token_where,
		$.Token_if,
		$.Token_else,
		$.Token_for,
		$.Token_switch,
		$.Token_in,
		$.Token_not_in,
		$.Token_do,
		$.Token_case,
		$.Token_break,
		$.Token_continue,
		$.Token_fallthrough,
		$.Token_defer,
		$.Token_return,
		$.Token_proc,
		$.Token_struct,
		$.Token_union,
		$.Token_enum,
		$.Token_bit_set,
		$.Token_map,
		$.Token_dynamic,
		$.Token_auto_cast,
		$.Token_cast,
		$.Token_transmute,
		$.Token_distinct,
		$.Token_using,
		$.Token_context,
		$.Token_or_else,
		$.Token_or_return,
		$.Token_asm,
		$.Token_matrix,
		$.Token__KeywordEnd,
		$.Token_Count
  ],

  rules: {
		source_file: $ => seq(
      optional($.package_clause),
      repeat($.top_level),
			repeat(choice($.Token_Invalid, $.Token_EOF))
    ),

		top_level: $ => seq(
			optional(seq($.Token_when, $._expression, $.Token_OpenBrace)),
			choice(
				$.import_declaration,
				//$.foreign_block,
				$._declaration),
			optional($.Token_CloseBrace),
		),

    package_clause: $ => seq($.Token_package, $._package_identifier),

    import_declaration: $ => seq(
			optional($.Token_foreign),
      alias($.Token_import, $.keyword),
      optional(field('name', 
        $._package_identifier
      )),
      field('path', $.Token_String)
    ),

    foreign_block: $ => seq(
      repeat($.pragma),
      alias($.Token_foreign, $.keyword),
      optional($._package_identifier),
      $.Token_OpenBrace,
      list($.Token_Semicolon, optional($._declaration)),
      $.Token_CloseBrace,
    ),

    _simple_statement: $ => choice(
      $._declaration,
      $.assignment_statement,
      $.proc_call,
      $.return_statement,
      $.continue_statement,
      $.break_statement,
    ),

    _statement: $ => choice(
      $._simple_statement,
      $.if_statement,
      $.switch_statement,
      $.for_statement,
      $.block_statement,
      $.defer_statement,
      $.using_statement,

			$._parenthesized_expression, // for #assert(1 == 1)
			$.foreign_block,
			$.right_unary_expression,
			$.Token_fallthrough,
    ),

    defer_statement: $ => seq(
      alias($.Token_defer, $.keyword),
      choice(
        $._simple_statement,
        $.if_statement,
        $.block_statement,
      ),
    ),

    using_statement: $ => seq(
      alias($.Token_using, $.keyword),
			choice($.selector_expression, $.Token_Ident)
    ),

    return_statement: $ => prec.right(seq(
      alias($.Token_return, $.keyword),
      field('value', list($.Token_Comma, choice($._expression, $.compound_literal))),
    )),

    block_statement: $ => prec(1, seq(
      optional(field('label', seq($._label_identifier, $.Token_Colon))),
      $.block,
    )),

    block: $ => prec(1, seq(
      $.Token_OpenBrace,
      list($.Token_Semicolon, optional($._statement)),
      $.Token_CloseBrace,
    )),

    pragma: $ => seq(
      $.Token_At,
      choice(
        seq(
          $.Token_OpenParen,
          list1(
            $.Token_Comma,
            choice($._pragma_identifier, seq($._pragma_identifier, $.Token_Eq, $._expression))
          ),
          $.Token_CloseParen
        ),
        $._pragma_identifier,
      ),
    ),

    _declaration: $ => seq(
      repeat($.pragma),
			$.declaration,
    ),

    declaration: $ => prec.right(1, seq(
      field('name', list1($.Token_Comma, alias($.Token_Ident, $.decl_identifier))),
      alias($.Token_Colon, $.operator),
			choice(
				field('type', $._type),
				choice(seq(optional(field('type', $._type)),
									 alias($.Token_Colon, $.operator),
									 optional(alias($.Token_distinct, $.keyword)),
									 field('const_value', list1($.Token_Comma, choice($._expression, $.compound_literal))),
									),
							 seq(optional(field('type', $._type)),
									 alias($.Token_Eq, $.operator),
									 field('variable_value', list1($.Token_Comma, choice($._expression, $.compound_literal))),
									)),
			)
    )),

    _type: $ => seq(
      choice(
        alias($._proc_type, $.proc_type),
        $._simple_type,
				alias($.proc_call, $.parapoly)
      ),
    ),

    _simple_type: $ => choice(
      alias($.Token_Ident, $.type_identifier),
      $.selector_expression,
      seq(optional(alias($.Token_Dollar, $.operator)),
					$._type_identifier,
				 ),
      $.map_type,
      $.pointer_type,
      $.bit_set_type,
      $.matrix_type,
      $.array_type,
      $.struct_type,
      $.enum_type,
      $.union_type,
			$.Token_typeid,
    ),

    pointer_type: $ => prec(op_prec.l_unary, seq(
			optional(seq($.Token_OpenParen, alias($.Token_Ident, $.pragma_identifier), $.Token_CloseParen)), // for #relative(i16)			
      alias($.Token_Pointer, $.operator), field('type', $._type),
    )),
		
    bit_set_type: $ => prec(op_prec.l_unary, seq(
      alias($.Token_bit_set, $.keyword),
			$.Token_OpenBracket,
      choice(
        field('type', $._simple_type),
        field('range', $._expression),
	    ),
      optional(seq($.Token_Semicolon, field('backing', $._type))),
      $.Token_CloseBracket
    )),

    matrix_type: $ => prec(op_prec.l_unary, seq(
      alias($.Token_matrix, $.keyword), $.Token_OpenBracket,$._expression, $.Token_Comma, $._expression, $.Token_CloseBracket, field('type', $._type),
    )),

		array_type_prefix: $ => prec.right(repeat1(seq(
			$.Token_OpenBracket,
			optional(field(
				'size',
				choice(
					$._expression,
					alias($.Token_Pointer, $.operator),
					alias($.Token_Question, $.operator),
					alias($.Token_dynamic, $.keyword),
				)
			)),
			$.Token_CloseBracket,
		))),
		
    array_type: $ => prec.right(op_prec.l_unary, seq(
			optional(seq($.Token_OpenParen, alias($.Token_Ident, $.pragma_identifier), $.Token_CloseParen)), // for #relative(i16)			
			$.array_type_prefix,
			prec.dynamic(1, field('type', choice($._type, $.compound_literal))),
    )),

    union_type: $ => seq(
      alias($.Token_union, $.keyword),
			optional($.parameter_list),
      $.Token_OpenBrace, list($.Token_Comma, $._type), $.Token_CloseBrace,
    ),

    enum_type: $ => prec(1, seq(
      alias($.Token_enum, $.keyword), optional(field('backing', $._simple_type)),
      $.Token_OpenBrace,
      optional(field('variants', $.initializer_list)),
      $.Token_CloseBrace,
    )),

		struct_member: $ => seq(
			list1($.Token_Comma,
						seq(optional(alias($.Token_using, $.keyword)), $.Token_Ident)),
			$.Token_Colon,
			$._type,
			optional($.Token_String), // x:int 'tag_str'
		),

    struct_type: $ => seq(
      alias($.Token_struct, $.keyword),
      optional(field('parameters', $.parameter_list)),
			optional(field('align_size', $.Token_Integer)),  // eat up number after '#align'
      optional(seq(
        alias($.Token_where, $.keyword),
        list1($.Token_Comma, $._expression)
      )),
      $.Token_OpenBrace,
      list($.Token_Comma, $.struct_member),
      optional($.Token_Comma),
      $.Token_CloseBrace,
    ),

    map_type: $ => prec(op_prec.l_unary, seq(
      alias($.Token_map, $.keyword),
      $.Token_OpenBracket, field('key', $._type), $.Token_CloseBracket,
      field('value', $._type),
    )),

    _expression: $ => prec.right(1, choice(
			$.Token_Integer,
			choice($.Token_Float, seq($.Token_Integer, $.Token_Float), seq($.Token_Integer, $.Token_Period)),
			$.Token_Imag,
			$.Token_Rune,
			$.Token_String,
			$.Token_context,
			
			//$.compound_literal,
			$.Token_Ident,
      $.left_unary_expression,
      $.right_unary_expression,
      $.binary_expression,
      $.ternary_expression,
      $.type_conversion,
      $._parenthesized_expression,
      $.index_expression,
      $.selector_expression,
      $.proc_literal,
      $.proc_group,
      $.proc_call,
      $._type,
    )),

    selector_expression: $ => prec.left(op_prec.r_unary, seq(
      optional(field('parent', $._expression)), $.Token_Period, field('field', choice($.Token_Ident, $.Token_Question)),
    )),

    type_conversion: $ => prec(op_prec.l_unary, seq(
      choice(
        seq(
          alias(choice($.Token_cast, $.Token_transmute), $.keyword),
          $.Token_OpenParen, $._type, $.Token_CloseParen,
        ),
        alias($.Token_auto_cast, $.operator),
      ),
      $._expression,
    )),

    ternary_expression: $ => prec.right(op_prec.ternary, choice(
      seq(
        field('condition', $._expression),
        alias($.Token_Question, $.operator),
        field('if_true', $._expression),
        alias($.Token_Colon, $.operator),
        field('if_false', $._expression),
      ),
      seq(
        field('if_true', $._expression),
        alias($.Token_if, $.keyword),
        field('condition', $._expression),
        alias($.Token_else, $.keyword),
        field('if_false', $._expression),
      ),
    )),

    index_expression: $ => prec(op_prec.r_unary, seq(
      field('operand', $._expression),
      $.Token_OpenBracket,
      choice(
        field('index', list1($.Token_Comma, $._expression)),
        seq(
          optional(field('start', $._expression)),
          alias($.Token_Colon, $.operator),
          optional(field('end', $._expression))
        ),
      ),
      $.Token_CloseBracket,
    )),

    _parenthesized_expression: $ => seq(
      $.Token_OpenParen, $._expression, $.Token_CloseParen,
    ),

    proc_literal: $ => prec.right(20, seq(
      $._proc_type,
			optional($.Token_Semicolon),			
      optional(seq(
        alias($.Token_where, $.keyword),
        list1($.Token_Comma, $._expression)
      )),
			optional($.Token_Semicolon),
      choice(
        $.block,
        $.Token_Undef //'---'
      ),
    )),

    proc_group: $ => seq(
      alias($.Token_proc, $.keyword),
      $.Token_OpenBrace,
      list1($.Token_Comma, $.Token_Ident),
      $.Token_CloseBrace,
    ),

    _proc_type: $ => prec.right(10, seq(
      alias($.Token_proc, $.keyword),
      optional($._calling_convention),
      field('parameters', $.parameter_list),
      optional(seq(
        alias($.Token_ArrowRight, $.operator),
        field('result', choice(
          $.parameter_list,
          $._type,
          alias($.Token_Not, $.keyword)
        ))
      )),
    )),

    parameter_list: $ => prec.right(10, seq(
      $.Token_OpenParen,
      list($.Token_Comma,
					 $.parameter_declaration,
					),
      $.Token_CloseParen
    )),

    parameter_declaration: $ => prec.left(1, choice(
			$.Token_Ident,
			seq(
				field('name', seq(
					list1($.Token_Comma, seq(
						optional(alias($.Token_using, $.keyword)),
						choice(seq(alias($.Token_Dollar, $.operator),
											 $._type_identifier),
									 $.Token_Ident)
					)),
					$.Token_Colon,
				)),
				field('type', seq(
					optional(alias($.Token_Ellipsis, $.operator)),
					//optional(alias($.Token_Dollar, $.operator)),
					optional($._type),
					optional(seq(alias($.Token_Quo, $.operator),
											 //optional(alias($.Token_Dollar, $.operator)),
											 $._type))
				)),
				optional(seq($.Token_Eq, field('value', $._expression))))
		)),

    _calling_convention: $ => alias($.Token_String, $.calling_convention),

    proc_call: $ => prec(op_prec.r_unary, seq(
      choice(
				field('procedure', $._expression),
				seq(
					field('caller', $._expression),
					alias($.Token_ArrowRight, $.operator),
					field('member_proc', $.Token_Ident)
				),
			),
      $.Token_OpenParen,
			optional($.Token_Comma), // eat a comma for fmt.println(#procedure, "args here")
			optional(field('arguments', $.initializer_list)),
			$.Token_CloseParen,
    )),

    initializer_list: $ => prec.right(seq(
      list1($.Token_Comma, choice($._expression, $.single_assignment, $.compound_literal)),
      optional($.Token_Comma),
    )),

    single_assignment: $ => seq(
      field('lhs', $._expression), $.Token_Eq, field('rhs', choice($._expression, $.compound_literal)),
    ),

    assignment_statement: $ => prec.right(seq(
      field('lhs', choice($._expression, list1($.Token_Comma, $._var_identifier))),
      alias(choice($.Token_AddEq, $.Token_SubEq, $.Token_MulEq, $.Token_QuoEq, $.Token_ModEq, $.Token_ModModEq,
									 $.Token_AndEq, $.Token_OrEq, $.Token_XorEq, $.Token_AndNotEq, $.Token_ShlEq, $.Token_ShrEq,
									 $.Token_CmpAndEq, $.Token_CmpOrEq, $.Token_Eq), $.operator),
      field('rhs', list1($.Token_Comma, choice($._expression, $.compound_literal))),
    )),

    case_statement: $ => prec.right(seq(
      alias($.Token_case, $.keyword),
      optional(list($.Token_Comma, $._expression)),
			alias($.Token_Colon, $.operator),
			list($.Token_Semicolon, optional($._statement)),
    )),

    continue_statement: $ => seq(alias($.Token_continue, $.keyword), optional($._label_identifier)),

    break_statement: $ => seq(alias($.Token_break, $.keyword), optional($._label_identifier)),

    if_statement: $ => prec.right(seq(
      optional(field('label', seq($._label_identifier, $.Token_Colon))),
      alias(choice($.Token_if, $.Token_when), $.keyword),
      optional(seq(
        field('initializer', $._statement),
        $.Token_Semicolon,
      )),
      field('condition', $._expression),
      field('if_true', choice(
        seq(alias($.Token_do, $.keyword), $._statement, ),
				$.block,
      )),
      optional(seq(
        alias($.Token_else, $.keyword),
        field('if_false', choice(
          $.if_statement,
          seq(alias($.Token_do, $.keyword), $._statement, ),
          $.block,
        )),
      )),
    )),

    switch_statement: $ => prec.right(1, seq(
      optional(field('label', seq($._label_identifier, $.Token_Colon))),
      alias($.Token_switch, $.keyword),
      choice(
        seq(
          optional(seq(
            field('initializer', $._simple_statement),
            $.Token_Semicolon,
          )),
          optional(field('expression', $._expression)),
        ),
        seq(
          optional(field('bind', $.Token_Ident)),
          alias($.Token_in, $.keyword),
          field('expression',$._expression),
        ),
      ),
      field('body', choice(
        seq(alias($.Token_do, $.keyword), $._statement, optional($.Token_Semicolon)),
				seq($.Token_OpenBrace,
						repeat($.case_statement),
						$.Token_CloseBrace)),
      ),
    )),

    for_statement: $ => prec.right(1, seq(
      optional(field('label', seq($._label_identifier, $.Token_Colon))),
      alias($.Token_for, $.keyword),
      optional(choice(
        $._range_clause,
        $._for_empty_clause,
        $._for_full_clause,
        $._for_conditional_clause,
        $._for_conditional_and_update_clause,
        $._for_init_clause,
        $._for_init_and_update_clause,
      )),
      field('body', choice(
        seq(alias($.Token_do, $.keyword), $._statement),
        $.block,
      )),
    )),

    _for_full_clause: $ => seq(
      field('initializer', $._simple_statement),
      $.Token_Semicolon,
      field('condition', $._expression),
      $.Token_Semicolon,
      field('update', $._simple_statement),
    ),

    _for_empty_clause: $ => seq($.Token_Semicolon, $.Token_Semicolon),

    _for_conditional_clause: $=> seq(field('condition', $._expression)),

    _for_conditional_and_update_clause: $ => prec.left(seq(
      $.Token_Semicolon,
      field('condition', $._expression),
      $.Token_Semicolon,
      optional(field('update', $._simple_statement)),
    )),

    _for_init_and_update_clause: $ => seq(
      field('initializer', $._simple_statement),
      $.Token_Semicolon,
      $.Token_Semicolon,
      field('update', $._simple_statement),
    ),

    _for_init_clause: $ => seq(
      field('initializer', $._simple_statement),
      $.Token_Semicolon,
      $.Token_Semicolon,
    ),

    _range_clause: $ => prec.right(10, seq(
      list($.Token_Comma, field('name', $.Token_Ident)),
      alias($.Token_in, $.keyword),
      field('range',$._expression),
    )),

    right_unary_expression: $ => prec(op_prec.r_unary, seq(
      field('operand', $._expression),
      field('operator', alias(choice($.Token_Pointer, $.Token_or_return), $.operator)),
    )),

    left_unary_expression: $ => prec.right(op_prec.l_unary, seq(
      field('operator', alias(choice($.Token_Add, $.Token_Sub, $.Token_Xor, $.Token_And, $.Token_Not, $.Token_Ellipsis), $.operator)),
			choice(
				field('operand', $._expression),
				$.compound_literal),
    )),

    binary_expression: $ => {
      const table = [
        [op_prec.control_flow, $.Token_or_else],
        [op_prec.multiplicative, choice($.Token_Mul, $.Token_Quo, $.Token_Mod, $.Token_Shl, $.Token_Shr, $.Token_And, $.Token_AndNot)],
        [op_prec.membership, choice($.Token_in, $.Token_not_in)],
        [op_prec.additive, choice($.Token_Add, $.Token_Sub, $.Token_Xor, $.Token_Or)],
        [op_prec.comparison, choice($.Token_Gt, $.Token_Lt, $.Token_GtEq, $.Token_LtEq, $.Token_CmpEq, $.Token_NotEq)],
        [op_prec.range, choice($.Token_Ellipsis, $.Token_RangeHalf, $.Token_RangeFull)],
        [op_prec.and, $.Token_CmpAnd],
        [op_prec.or, $.Token_CmpOr],
      ];
      return choice(...table.map(
        ([p, o]) => prec.left(p, seq(
          field('left', $._expression),
					field('operator', alias(o, $.operator)),
					field('right', choice($._expression, $.compound_literal)),
        )),
      ));
    },

    compound_literal_list: $ => seq(
      prec(1, list1($.Token_Comma, $.compound_literal)),
      optional($.Token_Comma),
    ),
		
    compound_literal: $ => prec.dynamic(-1, seq(
      optional(field('type', $._simple_type)),
      $.Token_OpenBrace,
			optional(field('contents', choice($.initializer_list, $.compound_literal_list))),
			$.Token_CloseBrace,
    )),

    compiler_directive: $ => seq(
			$.Token_Hash,
			$.Token_Ident,
    ),

    _label_identifier: $ => alias($.Token_Ident, $.label_identifier),
    _type_identifier: $ => alias($.Token_Ident, $.type_identifier),
    _pragma_identifier: $ => alias($.Token_Ident, $.pragma_identifier),
    _field_identifier: $ => alias($.Token_Ident, $.field_identifier),
		_var_identifier: $ => alias($.Token_Ident, $.var_identifier),
    _package_identifier: $ => alias($.Token_Ident, $.package_identifier),

		comment: $ => $.Token_Comment,
	}
})

function list1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)))
}

function list(sep, rule) {
  return optional(list1(sep, rule))
}
