#include <string.h>

u32 gb_fnv32a(void const *data, isize len) {
	isize i;
	u32 h = 0x811c9dc5;
	u8 const *c = cast(u8 const *)data;

	for (i = 0; i < len; i++) {
		h = (h ^ c[i]) * 0x01000193;
	}

	return h;
}

/** Unicode categories. */
typedef enum {
  UTF8PROC_CATEGORY_CN  = 0, /**< Other, not assigned */
  UTF8PROC_CATEGORY_LU  = 1, /**< Letter, uppercase */
  UTF8PROC_CATEGORY_LL  = 2, /**< Letter, lowercase */
  UTF8PROC_CATEGORY_LT  = 3, /**< Letter, titlecase */
  UTF8PROC_CATEGORY_LM  = 4, /**< Letter, modifier */
  UTF8PROC_CATEGORY_LO  = 5, /**< Letter, other */
  UTF8PROC_CATEGORY_MN  = 6, /**< Mark, nonspacing */
  UTF8PROC_CATEGORY_MC  = 7, /**< Mark, spacing combining */
  UTF8PROC_CATEGORY_ME  = 8, /**< Mark, enclosing */
  UTF8PROC_CATEGORY_ND  = 9, /**< Number, decimal digit */
  UTF8PROC_CATEGORY_NL = 10, /**< Number, letter */
  UTF8PROC_CATEGORY_NO = 11, /**< Number, other */
  UTF8PROC_CATEGORY_PC = 12, /**< Punctuation, connector */
  UTF8PROC_CATEGORY_PD = 13, /**< Punctuation, dash */
  UTF8PROC_CATEGORY_PS = 14, /**< Punctuation, open */
  UTF8PROC_CATEGORY_PE = 15, /**< Punctuation, close */
  UTF8PROC_CATEGORY_PI = 16, /**< Punctuation, initial quote */
  UTF8PROC_CATEGORY_PF = 17, /**< Punctuation, final quote */
  UTF8PROC_CATEGORY_PO = 18, /**< Punctuation, other */
  UTF8PROC_CATEGORY_SM = 19, /**< Symbol, math */
  UTF8PROC_CATEGORY_SC = 20, /**< Symbol, currency */
  UTF8PROC_CATEGORY_SK = 21, /**< Symbol, modifier */
  UTF8PROC_CATEGORY_SO = 22, /**< Symbol, other */
  UTF8PROC_CATEGORY_ZS = 23, /**< Separator, space */
  UTF8PROC_CATEGORY_ZL = 24, /**< Separator, line */
  UTF8PROC_CATEGORY_ZP = 25, /**< Separator, paragraph */
  UTF8PROC_CATEGORY_CC = 26, /**< Other, control */
  UTF8PROC_CATEGORY_CF = 27, /**< Other, format */
  UTF8PROC_CATEGORY_CS = 28, /**< Other, surrogate */
  UTF8PROC_CATEGORY_CO = 29, /**< Other, private use */
} utf8proc_category_t;

bool rune_is_letter(Rune r) {
	if (r < 0x80) {
		if (r == '_') {
			return true;
		}
		return ((cast(u32)r | 0x20) - 0x61) < 26;
	}
	switch (r) { //utf8proc_category(r)) {
	case UTF8PROC_CATEGORY_LU:
	case UTF8PROC_CATEGORY_LL:
	case UTF8PROC_CATEGORY_LT:
	case UTF8PROC_CATEGORY_LM:
	case UTF8PROC_CATEGORY_LO:
		return true;
	}
	return false;
}

bool rune_is_letter_or_digit(Rune r) {
	if (r < 0x80) {
		if (r == '_') {
			return true;
		}
		if (((cast(u32)r | 0x20) - 0x61) < 26) {
			return true;
		}
		return (cast(u32)r - '0') < 10;
	}
	switch (r) {//utf8proc_category(r)) {
	case UTF8PROC_CATEGORY_LU:
	case UTF8PROC_CATEGORY_LL:
	case UTF8PROC_CATEGORY_LT:
	case UTF8PROC_CATEGORY_LM:
	case UTF8PROC_CATEGORY_LO:
		return true;
	case UTF8PROC_CATEGORY_ND:
		return true;
	}
	return false;
}

// --------------------------------

#define TOKEN_KINDS                                                     \
	TOKEN_KIND(Token_Invalid, "Invalid"), \
	TOKEN_KIND(Token_EOF,     "EOF"), \
	TOKEN_KIND(Token_Comment, "Comment"), \
\
TOKEN_KIND(Token__LiteralBegin, ""), \
	TOKEN_KIND(Token_Ident,     "identifier"), \
	TOKEN_KIND(Token_Integer,   "integer"), \
	TOKEN_KIND(Token_Float,     "float"), \
	TOKEN_KIND(Token_Imag,      "imaginary"), \
	TOKEN_KIND(Token_Rune,      "rune"), \
	TOKEN_KIND(Token_String,    "string"), \
TOKEN_KIND(Token__LiteralEnd,   ""), \
\
TOKEN_KIND(Token__OperatorBegin, ""), \
	TOKEN_KIND(Token_Eq,       "="), \
	TOKEN_KIND(Token_Not,      "!"), \
	TOKEN_KIND(Token_Hash,     "#"), \
	TOKEN_KIND(Token_At,       "@"), \
	TOKEN_KIND(Token_Dollar,   "$"), \
	TOKEN_KIND(Token_Pointer,  "^"), \
	TOKEN_KIND(Token_Question, "?"), \
	TOKEN_KIND(Token_Add,      "+"), \
	TOKEN_KIND(Token_Sub,      "-"), \
	TOKEN_KIND(Token_Mul,      "*"), \
	TOKEN_KIND(Token_Quo,      "/"), \
	TOKEN_KIND(Token_Mod,      "%"), \
	TOKEN_KIND(Token_ModMod,   "%%"), \
	TOKEN_KIND(Token_And,      "&"), \
	TOKEN_KIND(Token_Or,       "|"), \
	TOKEN_KIND(Token_Xor,      "~"), \
	TOKEN_KIND(Token_AndNot,   "&~"), \
	TOKEN_KIND(Token_Shl,      "<<"), \
	TOKEN_KIND(Token_Shr,      ">>"), \
	TOKEN_KIND(Token_CmpAnd,   "&&"), \
	TOKEN_KIND(Token_CmpOr,    "||"), \
\
TOKEN_KIND(Token__AssignOpBegin, ""), \
	TOKEN_KIND(Token_AddEq,    "+="), \
	TOKEN_KIND(Token_SubEq,    "-="), \
	TOKEN_KIND(Token_MulEq,    "*="), \
	TOKEN_KIND(Token_QuoEq,    "/="), \
	TOKEN_KIND(Token_ModEq,    "%="), \
	TOKEN_KIND(Token_ModModEq, "%%="), \
	TOKEN_KIND(Token_AndEq,    "&="), \
	TOKEN_KIND(Token_OrEq,     "|="), \
	TOKEN_KIND(Token_XorEq,    "~="), \
	TOKEN_KIND(Token_AndNotEq, "&~="), \
	TOKEN_KIND(Token_ShlEq,    "<<="), \
	TOKEN_KIND(Token_ShrEq,    ">>="), \
	TOKEN_KIND(Token_CmpAndEq, "&&="), \
	TOKEN_KIND(Token_CmpOrEq,  "||="), \
TOKEN_KIND(Token__AssignOpEnd, ""), \
	TOKEN_KIND(Token_Increment, "++"), \
	TOKEN_KIND(Token_Decrement, "--"), \
	TOKEN_KIND(Token_ArrowRight,"->"), \
	TOKEN_KIND(Token_Undef,     "---"), \
\
TOKEN_KIND(Token__ComparisonBegin, ""), \
	TOKEN_KIND(Token_CmpEq, "=="), \
	TOKEN_KIND(Token_NotEq, "!="), \
	TOKEN_KIND(Token_Lt,    "<"), \
	TOKEN_KIND(Token_Gt,    ">"), \
	TOKEN_KIND(Token_LtEq,  "<="), \
	TOKEN_KIND(Token_GtEq,  ">="), \
TOKEN_KIND(Token__ComparisonEnd, ""), \
\
	TOKEN_KIND(Token_OpenParen,     "("),   \
	TOKEN_KIND(Token_CloseParen,    ")"),   \
	TOKEN_KIND(Token_OpenBracket,   "["),   \
	TOKEN_KIND(Token_CloseBracket,  "]"),   \
	TOKEN_KIND(Token_OpenBrace,     "{"),   \
	TOKEN_KIND(Token_CloseBrace,    "}"),   \
	TOKEN_KIND(Token_Colon,         ":"),   \
	TOKEN_KIND(Token_Semicolon,     ";"),   \
	TOKEN_KIND(Token_Period,        "."),   \
	TOKEN_KIND(Token_Comma,         ","),   \
	TOKEN_KIND(Token_Ellipsis,      ".."),  \
	TOKEN_KIND(Token_RangeFull,     "..="), \
	TOKEN_KIND(Token_RangeHalf,     "..<"), \
	TOKEN_KIND(Token_BackSlash,     "\\"),  \
TOKEN_KIND(Token__OperatorEnd, ""), \
\
TOKEN_KIND(Token__KeywordBegin, ""), \
	TOKEN_KIND(Token_import,      "import"),      \
	TOKEN_KIND(Token_foreign,     "foreign"),     \
	TOKEN_KIND(Token_package,     "package"),     \
	TOKEN_KIND(Token_typeid,      "typeid"),      \
	TOKEN_KIND(Token_when,        "when"),        \
	TOKEN_KIND(Token_where,       "where"),       \
	TOKEN_KIND(Token_if,          "if"),          \
	TOKEN_KIND(Token_else,        "else"),        \
	TOKEN_KIND(Token_for,         "for"),         \
	TOKEN_KIND(Token_switch,      "switch"),      \
	TOKEN_KIND(Token_in,          "in"),          \
	TOKEN_KIND(Token_not_in,      "not_in"),      \
	TOKEN_KIND(Token_do,          "do"),          \
	TOKEN_KIND(Token_case,        "case"),        \
	TOKEN_KIND(Token_break,       "break"),       \
	TOKEN_KIND(Token_continue,    "continue"),    \
	TOKEN_KIND(Token_fallthrough, "fallthrough"), \
	TOKEN_KIND(Token_defer,       "defer"),       \
	TOKEN_KIND(Token_return,      "return"),      \
	TOKEN_KIND(Token_proc,        "proc"),        \
	TOKEN_KIND(Token_struct,      "struct"),      \
	TOKEN_KIND(Token_union,       "union"),       \
	TOKEN_KIND(Token_enum,        "enum"),        \
	TOKEN_KIND(Token_bit_set,     "bit_set"),     \
	TOKEN_KIND(Token_map,         "map"),         \
	TOKEN_KIND(Token_dynamic,     "dynamic"),     \
	TOKEN_KIND(Token_auto_cast,   "auto_cast"),   \
	TOKEN_KIND(Token_cast,        "cast"),        \
	TOKEN_KIND(Token_transmute,   "transmute"),   \
	TOKEN_KIND(Token_distinct,    "distinct"),    \
	TOKEN_KIND(Token_using,       "using"),       \
	TOKEN_KIND(Token_context,     "context"),     \
	TOKEN_KIND(Token_or_else,     "or_else"),     \
	TOKEN_KIND(Token_or_return,   "or_return"),   \
	TOKEN_KIND(Token_asm,         "asm"),         \
	TOKEN_KIND(Token_matrix,      "matrix"),      \
TOKEN_KIND(Token__KeywordEnd, ""), \
	TOKEN_KIND(Token_Count, "")


struct String 
{
	u8      *text;
	u32      len;
};

enum TokenKind : u8 {
#define TOKEN_KIND(e, s) e
	TOKEN_KINDS
#undef TOKEN_KIND
};

String const token_strings[] = {
#define TOKEN_KIND(e, s) {cast(u8 *)s, gb_size_of(s)-1}
	TOKEN_KINDS
#undef TOKEN_KIND
};

struct KeywordHashEntry {
	u32       hash;
	TokenKind kind;
	String    text;
};

enum {
	KEYWORD_HASH_TABLE_COUNT = 1<<9,
	KEYWORD_HASH_TABLE_MASK = KEYWORD_HASH_TABLE_COUNT-1,
};
gb_global KeywordHashEntry keyword_hash_table[KEYWORD_HASH_TABLE_COUNT] = {};
GB_STATIC_ASSERT(Token__KeywordEnd-Token__KeywordBegin <= gb_count_of(keyword_hash_table));
gb_global isize const min_keyword_size = 2;
gb_global isize max_keyword_size = 11;
gb_global bool keyword_indices[16] = {};


gb_inline u32 keyword_hash(u8 const *text, isize len) {
	return gb_fnv32a(text, len);
}
void add_keyword_hash_entry(String const &s, TokenKind kind) {
	max_keyword_size = gb_max(max_keyword_size, s.len);

	keyword_indices[s.len] = true;

	u32 hash = keyword_hash(s.text, s.len);

	// NOTE(bill): This is a bit of an empirical hack in order to speed things up
	u32 index = hash & KEYWORD_HASH_TABLE_MASK;
	KeywordHashEntry *entry = &keyword_hash_table[index];
	//GB_ASSERT_MSG(entry->kind == Token_Invalid, "Keyword hash table initialtion collision: %.*s %.*s 0x%08x 0x%08x", LIT(s), LIT(token_strings[entry->kind]), hash, entry->hash);
	entry->hash = hash;
	entry->kind = kind;
	entry->text = s;
}
void init_keyword_hash_table(void) {
	for (i32 kind = Token__KeywordBegin+1; kind < Token__KeywordEnd; kind++) {
		add_keyword_hash_entry(token_strings[kind], cast(TokenKind)kind);
	}
//
//	static struct {
//		String s;
//		TokenKind kind;
//	} const legacy_keywords[] = {
//		{str_lit("notin"), Token_not_in},
//	};
//
//	for (i32 i = 0; i < gb_count_of(legacy_keywords); i++) {
//		add_keyword_hash_entry(legacy_keywords[i].s, legacy_keywords[i].kind);
//	}

	GB_ASSERT(max_keyword_size < 16);
}

//gb_global Array<String>           global_file_path_strings; // index is file id
//gb_global Array<struct AstFile *> global_files; // index is file id

//String   get_file_path_string(i32 index);
//struct AstFile *thread_safe_get_ast_file_from_id(i32 index);

struct TokenPos {
	i32 file_id;
	i32 offset; // starting at 0
	i32 line;   // starting at 1
	i32 column; // starting at 1
};

//i32 token_pos_cmp(TokenPos const &a, TokenPos const &b) {
//	if (a.offset != b.offset) {
//		return (a.offset < b.offset) ? -1 : +1;
//	}
//	if (a.line != b.line) {
//		return (a.line < b.line) ? -1 : +1;
//	}
//	if (a.column != b.column) {
//		return (a.column < b.column) ? -1 : +1;
//	}
//	return string_compare(get_file_path_string(a.file_id), get_file_path_string(b.file_id));
//}

//bool operator==(TokenPos const &a, TokenPos const &b) { return token_pos_cmp(a, b) == 0; }
//bool operator!=(TokenPos const &a, TokenPos const &b) { return token_pos_cmp(a, b) != 0; }
//bool operator< (TokenPos const &a, TokenPos const &b) { return token_pos_cmp(a, b) <  0; }
//bool operator<=(TokenPos const &a, TokenPos const &b) { return token_pos_cmp(a, b) <= 0; }
//bool operator> (TokenPos const &a, TokenPos const &b) { return token_pos_cmp(a, b) >  0; }
//bool operator>=(TokenPos const &a, TokenPos const &b) { return token_pos_cmp(a, b) >= 0; }


TokenPos token_pos_add_column(TokenPos pos) {
	pos.column += 1;
	pos.offset += 1;
	return pos;
}

enum TokenFlag : u8 {
	TokenFlag_Remove  = 1<<1,
	TokenFlag_Replace = 1<<2,
};

struct Token {
	TokenKind kind;
	u8        flags;
	String    string;
	TokenPos  pos;
};

Token empty_token = {Token_Invalid};
Token blank_token = {Token_Ident, 0, {cast(u8 *)"_", 1}};

Token make_token_ident(String s) {
	Token t = {Token_Ident, 0, s};
	return t;
}
//Token make_token_ident(char const *s) {
//	Token t = {Token_Ident, 0, make_string_c(s)};
//	return t;
//}

bool token_is_newline(Token const &tok) {
	return tok.kind == Token_Semicolon && strcmp((char*)tok.string.text, "\n")==0;
}

gb_inline bool token_is_literal(TokenKind t) {
	return gb_is_between(t, Token__LiteralBegin+1, Token__LiteralEnd-1);
}
gb_inline bool token_is_operator(TokenKind t) {
	return gb_is_between(t, Token__OperatorBegin+1, Token__OperatorEnd-1);
}
gb_inline bool token_is_keyword(TokenKind t) {
	return gb_is_between(t, Token__KeywordBegin+1, Token__KeywordEnd-1);
}
gb_inline bool token_is_comparison(TokenKind t) {
	return gb_is_between(t, Token__ComparisonBegin+1, Token__ComparisonEnd-1);
}
gb_inline bool token_is_shift(TokenKind t) {
	return t == Token_Shl || t == Token_Shr;
}

gb_inline void print_token(Token t) { printf("%s\n", t.string.text); }

//#include "error.cpp"

enum TokenizerInitError {
	TokenizerInit_None,

	TokenizerInit_Invalid,
	TokenizerInit_NotExists,
	TokenizerInit_Permission,
	TokenizerInit_Empty,
	TokenizerInit_FileTooLarge,

	TokenizerInit_Count,
};

struct Tokenizer {
	i32 curr_file_id;
  String fullpath;
	//u8 *start;
	//u8 *end;

	Rune  curr_rune;   // current character
	//u8 *  curr;        // character pos
	//u8 *  read_curr;   // pos from start
	i32   column_minus_one;
	i32   line_count;

	i32 error_count;

	bool insert_semicolon;
	
	//LoadedFile loaded_file;
};

void init_tokenizer_with_data(Tokenizer *t, String const &fullpath, void const *data, isize size) {
	t->fullpath = fullpath;
	t->line_count = 1;

	//t->start = cast(u8 *)data;
	//t->read_curr = t->curr = t->start;
	//t->end = t->start + size;

	//advance_to_next_rune(t);
	//if (t->curr_rune == GB_RUNE_BOM) {
		//advance_to_next_rune(t); // Ignore BOM at file beginning
	//}
}

//TokenizerInitError loaded_file_error_map_to_tokenizer[LoadedFile_COUNT] = {
//	TokenizerInit_None,         /*LoadedFile_None*/
//	TokenizerInit_Empty,        /*LoadedFile_Empty*/
//	TokenizerInit_FileTooLarge, /*LoadedFile_FileTooLarge*/
//	TokenizerInit_Invalid,      /*LoadedFile_Invalid*/
//	TokenizerInit_NotExists,    /*LoadedFile_NotExists*/
//	TokenizerInit_Permission,   /*LoadedFile_Permission*/
//};

//TokenizerInitError init_tokenizer_from_fullpath(Tokenizer *t, String const &fullpath, bool copy_file_contents) {
//	LoadedFileError file_err = load_file_32(
//		alloc_cstring(temporary_allocator(), fullpath), 
//		&t->loaded_file,
//		copy_file_contents
//	);
//	
//	TokenizerInitError err = loaded_file_error_map_to_tokenizer[file_err];
//	switch (file_err) {
//	case LoadedFile_None:
//		init_tokenizer_with_data(t, fullpath, t->loaded_file.data, cast(isize)t->loaded_file.size);
//		break;
//	case LoadedFile_FileTooLarge:
//	case LoadedFile_Empty:
//		t->fullpath = fullpath;
//		t->line_count = 1;
//		break;
//	}	
//	return err;
//}

gb_inline i32 digit_value(Rune r) {
	switch (r) {
	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
		return r - '0';
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
		return r - 'a' + 10;
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
		return r - 'A' + 10;
	}
	return 16; // NOTE(bill): Larger than highest possible
}

void tokenizer_err(Tokenizer *t, char const *msg, ...) {
	va_list va;
	va_start(va, msg);
	vprintf(msg, va);
	va_end(va);
	t->error_count++;
}

void advance_to_next_rune(TSLexer *lexer, Tokenizer *t, bool skip=false) {
	if (t->curr_rune == '\n') {
		t->column_minus_one = -1;
		t->line_count++;
	}
	if (lexer->lookahead != 0) {
		lexer->advance(lexer, skip);
		Rune rune = lexer->lookahead;		
		
		if (rune == 0) {
			//tokenizer_err(t, "Illegal character NUL");
		}
		t->curr_rune = rune;
		t->column_minus_one++;
	} else {
		t->curr_rune = GB_RUNE_EOF;
	}
}

int scan_mantissa(TSLexer *lexer, Tokenizer *t, i32 base) {
	int digit_cnt = 0;
	
	while (digit_value(t->curr_rune) < base || t->curr_rune == '_') {
		if (t->curr_rune != '_') {
			digit_cnt++;
		}
		advance_to_next_rune(lexer, t);
	}
	return digit_cnt;
}

Rune peek_rune(TSLexer *lexer, Tokenizer *t, isize offset=0) {
	lexer->mark_end(lexer); // allows multiple lookahead..
	// TODO: only do once at beginning of get_token
	// mark_end will say this is the end of a token.. then any advance will "just look"
	// until mark_end is called again

	for (int i=0; i<offset; i++) {
		lexer->advance(lexer, false);
	}
	return lexer->lookahead;
}

void scan_number_to_token(TSLexer *lexer, Tokenizer *t, Token *token, bool seen_decimal_point) {
	token->kind = Token_Integer;
	token->pos.file_id = t->curr_file_id;
	token->pos.line = t->line_count;
	token->pos.column = t->column_minus_one+1;

	int l = 0; // digit_cnt from scan_mantissa
	
	if (seen_decimal_point) {
		token->pos.column -= 1;
		token->kind = Token_Float;
		scan_mantissa(lexer, t, 10);
		goto exponent;
	}
	
	if (t->curr_rune == '0') {
		advance_to_next_rune(lexer, t);
		switch (t->curr_rune) {
		case 'b': // Binary
			advance_to_next_rune(lexer, t);
			l = scan_mantissa(lexer, t, 2);
			if (l <= 2) {
				token->kind = Token_Invalid;
			}
			goto end;
		case 'o': // Octal
			advance_to_next_rune(lexer, t);
			l = scan_mantissa(lexer, t, 8);
			if (l <= 2) {
				token->kind = Token_Invalid;
			}
			goto end;
		case 'd': // Decimal
			advance_to_next_rune(lexer, t);
			l = scan_mantissa(lexer, t, 10);
			if (l <= 2) {
				token->kind = Token_Invalid;
			}
			goto end;
		case 'z': // Dozenal
			advance_to_next_rune(lexer, t);
			l = scan_mantissa(lexer, t, 12);
			if (l <= 2) {
				token->kind = Token_Invalid;
			}
			goto end;
		case 'x': // Hexadecimal
			advance_to_next_rune(lexer, t);
			l = scan_mantissa(lexer, t, 16);
			if (l <= 2) {
				token->kind = Token_Invalid;
			}
			goto end;
		case 'h': // Hexadecimal Float
			token->kind = Token_Float;
			advance_to_next_rune(lexer, t);
			l = scan_mantissa(lexer, t, 16);
			if (l <= 2) {
				token->kind = Token_Invalid;
			} else {
				switch (l) {
				case 4:
				case 8:
				case 16:
					break;
				default:
					tokenizer_err(t, "Invalid hexadecimal float, expected 4, 8, or 16 digits, got %td", l);
					break;
				}
			}
			goto end;
		default:
			scan_mantissa(lexer, t, 10);
			goto fraction;
		}
	}

	scan_mantissa(lexer, t, 10);


fraction:
	if (t->curr_rune == '.') {
		if (peek_rune(lexer, t) == '.') {
			// NOTE(bill): this is kind of ellipsis
			goto end;
		}
		advance_to_next_rune(lexer, t);

		token->kind = Token_Float;
		scan_mantissa(lexer, t, 10);
	}

exponent:
	if (t->curr_rune == 'e' || t->curr_rune == 'E') {
		token->kind = Token_Float;
		advance_to_next_rune(lexer, t);
		if (t->curr_rune == '-' || t->curr_rune == '+') {
			advance_to_next_rune(lexer, t);
		}
		scan_mantissa(lexer, t, 10);
	}

	switch (t->curr_rune) {
	case 'i': case 'j': case 'k':
		token->kind = Token_Imag;
		advance_to_next_rune(lexer, t);
		break;
	}

end:
	return;
}

bool scan_escape(TSLexer *lexer, Tokenizer *t) {
	isize len = 0;
	u32 base = 0, max = 0, x = 0;

	Rune r = t->curr_rune;
	switch (r) {
	case 'a':
	case 'b':
	case 'e':
	case 'f':
	case 'n':
	case 'r':
	case 't':
	case 'v':
	case '\\':
	case '\'':
	case '\"':
		advance_to_next_rune(lexer, t);
		return true;

	case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7':
		len = 3; base = 8; max = 255;
		break;

	case 'x':
		advance_to_next_rune(lexer, t);
		len = 2; base = 16; max = 255;
		break;

	case 'u':
		advance_to_next_rune(lexer, t);
		len = 4; base = 16; max = GB_RUNE_MAX;
		break;

	case 'U':
		advance_to_next_rune(lexer, t);
		len = 8; base = 16; max = GB_RUNE_MAX;
		break;

	default:
		if (t->curr_rune < 0) {
			tokenizer_err(t, "Escape sequence was not terminated");
		} else {
			tokenizer_err(t, "Unknown escape sequence");
		}
		return false;
	}

	while (len --> 0) {
		u32 d = cast(u32)digit_value(t->curr_rune);
		if (d >= base) {
			if (t->curr_rune < 0) {
				tokenizer_err(t, "Escape sequence was not terminated");
			} else {
				tokenizer_err(t, "Illegal character %d in escape sequence", t->curr_rune);
			}
			return false;
		}

		x = x*base + d;
		advance_to_next_rune(lexer, t);
	}

	return true;
}


void tokenizer_skip_line(TSLexer *lexer, Tokenizer *t) {
	while (t->curr_rune != '\n' && t->curr_rune != GB_RUNE_EOF) {
		advance_to_next_rune(lexer, t, false);
	}
}

void tokenizer_skip_whitespace(TSLexer *lexer, Tokenizer *t, bool on_newline) {
	if (on_newline) {
		for (;;) {
			switch (t->curr_rune) {
			case ' ':
			case '\t':
			case '\r':
				advance_to_next_rune(lexer, t, true);
				continue;
			}
			break;
		}
	} else {
		for (;;) {
			switch (t->curr_rune) {
			case '\n':
			case ' ':
			case '\t':
			case '\r':
				advance_to_next_rune(lexer, t, true);
				continue;
			}
			break;
		}
	}
}

void tokenizer_get_token(TSLexer *lexer, Tokenizer *t, Token *token, int repeat=0) {
	tokenizer_skip_whitespace(lexer, t, t->insert_semicolon);

	token->kind = Token_Invalid;
	token->pos.line = t->line_count;
	token->pos.column = t->column_minus_one+1;

	Rune curr_rune = t->curr_rune;
	if (rune_is_letter(curr_rune)) {
		token->kind = Token_Ident;

		token->string.text = (u8*) malloc(max_keyword_size);
		token->string.len = 0;

		while (rune_is_letter_or_digit(t->curr_rune)) {
			if (token->string.len < max_keyword_size) {
				token->string.text[token->string.len] = t->curr_rune; // TODO: only add if ascii char?
				token->string.len++;
				token->string.text[token->string.len] = 0;
			}
			advance_to_next_rune(lexer, t);
		}

		// NOTE(bill): Heavily optimize to make it faster to find keywords
		if (1 < token->string.len && token->string.len <= max_keyword_size && keyword_indices[token->string.len]) {
			u32 hash = keyword_hash(token->string.text, token->string.len);
			u32 index = hash & KEYWORD_HASH_TABLE_MASK;
			KeywordHashEntry *entry = &keyword_hash_table[index];
			if (entry->kind != Token_Invalid && entry->hash == hash) {
				if (strcmp((char*)entry->text.text, (char*)token->string.text)==0) {
					token->kind = entry->kind;
					if (token->kind == Token_not_in && strcmp((char*)entry->text.text, "notin")==0) {
						printf("'notin' is deprecated in favour of 'not_in'");
						//syntax_warning(*token, "'notin' is deprecated in favour of 'not_in'");
					}
				}
			}
		}

		free(token->string.text);
		token->string.text = NULL;

		goto semicolon_check;
	} else {
		switch (curr_rune) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			scan_number_to_token(lexer, t, token, false);
			goto semicolon_check;
		}

		advance_to_next_rune(lexer, t);
		
		switch (curr_rune) {
		case GB_RUNE_EOF:
			token->kind = Token_EOF;
			if (t->insert_semicolon) {
				t->insert_semicolon = false; // EOF consumed
				token->string.text = (u8*)"\n";
				token->string.len = 1;
				token->kind = Token_Semicolon;
				return;
			}
			break;

		case '\n':
			t->insert_semicolon = false;
			token->string.text = (u8*)"\n";
			token->string.len = 1;
			token->kind = Token_Semicolon;
			return;

		case '\\':
			t->insert_semicolon = false;
			tokenizer_get_token(lexer, t, token);
			// NOTE(bill): tokenizer_get_token has been called already, return early
			return;

		case '\'': // Rune Literal
		{
			token->kind = Token_Rune;
			Rune quote = curr_rune;
			bool valid = true;
			i32 n = 0, success;
			for (;;) {
				Rune r = t->curr_rune;
				if (r == '\n' || r < 0) {
					tokenizer_err(t, "Rune literal not terminated");
					break;
				}
				advance_to_next_rune(lexer, t);
				if (r == quote) {
					break;
				}
				n++;
				if (r == '\\') {
					if (!scan_escape(lexer, t)) {
						valid = false;
					}
				}
			}

			// TODO(bill): Better Error Handling
			if (valid && n != 1) {
				tokenizer_err(t, "Invalid rune literal");
			}
			goto semicolon_check;
		} break;

		case '`': // Raw String Literal
		case '"': // String Literal
		{
			bool has_carriage_return = false;
			i32 success;
			Rune quote = curr_rune;
			token->kind = Token_String;
			if (curr_rune == '"') {
				for (;;) {
					Rune r = t->curr_rune;
					if (r == '\n' || r < 0) {
						tokenizer_err(t, "String literal not terminated");
						break;
					}
					advance_to_next_rune(lexer, t);
					if (r == quote) {
						break;
					}
					if (r == '\\') {
						scan_escape(lexer, t);
					}
				}
			} else {
				for (;;) {
					Rune r = t->curr_rune;
					if (r < 0) {
						tokenizer_err(t, "String literal not terminated");
						break;
					}
					advance_to_next_rune(lexer, t);
					if (r == quote) {
						break;
					}
					if (r == '\r') {
						has_carriage_return = true;
					}
				}
			}
			goto semicolon_check;
		} break;

		case '.':
			token->kind = Token_Period;
			switch (t->curr_rune) {
			case '.':
				advance_to_next_rune(lexer, t);
				token->kind = Token_Ellipsis;
				if (t->curr_rune == '<') {
					advance_to_next_rune(lexer, t);
					token->kind = Token_RangeHalf;
				} else if (t->curr_rune == '=') {
					advance_to_next_rune(lexer, t);
					token->kind = Token_RangeFull;
				}
				break;
			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9':
				scan_number_to_token(lexer, t, token, true);
				break;
			}
			break;
		case '@': token->kind = Token_At;           break;
		case '$': token->kind = Token_Dollar;       break;
		case '?': token->kind = Token_Question;     break;
		case '^': token->kind = Token_Pointer;      break;
		case ';': token->kind = Token_Semicolon;    break;
		case ',': token->kind = Token_Comma;        break;
		case ':': token->kind = Token_Colon;        break;
		case '(': token->kind = Token_OpenParen;    break;
		case ')': token->kind = Token_CloseParen;   break;
		case '[': token->kind = Token_OpenBracket;  break;
		case ']': token->kind = Token_CloseBracket; break;
		case '{': token->kind = Token_OpenBrace;    break;
		case '}': token->kind = Token_CloseBrace;   break;
		case '%':
			token->kind = Token_Mod;
			switch (t->curr_rune) {
			case '=':
				advance_to_next_rune(lexer, t);
				token->kind = Token_ModEq;
				break;
			case '%':
				token->kind = Token_ModMod;
				advance_to_next_rune(lexer, t);
				if (t->curr_rune == '=') {
					token->kind = Token_ModModEq;
					advance_to_next_rune(lexer, t);
				}
				break;
			}
			break;

		case '*':
			token->kind = Token_Mul;
			if (t->curr_rune == '=') {
				advance_to_next_rune(lexer, t);
				token->kind = Token_MulEq;
			}
			break;
		case '=':
			token->kind = Token_Eq;
			if (t->curr_rune == '=') {
				advance_to_next_rune(lexer, t);
				token->kind = Token_CmpEq;
			}
			break;
		case '~':
			token->kind = Token_Xor;
			if (t->curr_rune == '=') {
				advance_to_next_rune(lexer, t);
				token->kind = Token_XorEq;
			}
			break;
		case '!':
			token->kind = Token_Not;
			if (t->curr_rune == '=') {
				advance_to_next_rune(lexer, t);
				token->kind = Token_NotEq;
			}
			break;
		case '+':
			token->kind = Token_Add;
			switch (t->curr_rune) {
			case '=':
				advance_to_next_rune(lexer, t);
				token->kind = Token_AddEq;
				break;
			case '+':
				advance_to_next_rune(lexer, t);
				token->kind = Token_Increment;
				break;
			}
			break;
		case '-':
			token->kind = Token_Sub;
			switch (t->curr_rune) {
			case '=':
				advance_to_next_rune(lexer, t);
				token->kind = Token_SubEq;
				break;
			case '-':
				advance_to_next_rune(lexer, t);
				token->kind = Token_Decrement;
				if (t->curr_rune == '-') {
					advance_to_next_rune(lexer, t);
					token->kind = Token_Undef;
				}
				break;
			case '>':
				advance_to_next_rune(lexer, t);
				token->kind = Token_ArrowRight;
				break;
			}
			break;
		case '#':
			token->kind = Token_Hash;
			if (t->curr_rune == '!') {
				token->kind = Token_Comment;
				tokenizer_skip_line(lexer, t);
			}
			break;
		case '/':
			token->kind = Token_Quo;
			switch (t->curr_rune) {
			case '/':
				token->kind = Token_Comment;
				tokenizer_skip_line(lexer, t);
				break;
			case '*':
				token->kind = Token_Comment;
				advance_to_next_rune(lexer, t);
				for (isize comment_scope = 1; comment_scope > 0; /**/) {
					if (t->curr_rune == GB_RUNE_EOF) {
						break;
					} else if (t->curr_rune == '/') {
						advance_to_next_rune(lexer, t);
						if (t->curr_rune == '*') {
							advance_to_next_rune(lexer, t);
							comment_scope++;
						}
					} else if (t->curr_rune == '*') {
						advance_to_next_rune(lexer, t);
						if (t->curr_rune == '/') {
							advance_to_next_rune(lexer, t);
							comment_scope--;
						}
					} else {
						advance_to_next_rune(lexer, t);
					}
				}
				break;
			case '=':
				advance_to_next_rune(lexer, t);
				token->kind = Token_QuoEq;
				break;
			}
			break;
		case '<':
			token->kind = Token_Lt;
			switch (t->curr_rune) {
			case '=':
				token->kind = Token_LtEq;
				advance_to_next_rune(lexer, t);
				break;
			case '<':
				token->kind = Token_Shl;
				advance_to_next_rune(lexer, t);
				if (t->curr_rune == '=') {
					token->kind = Token_ShlEq;
					advance_to_next_rune(lexer, t);
				}
				break;
			}
			break;
		case '>':
			token->kind = Token_Gt;
			switch (t->curr_rune) {
			case '=':
				token->kind = Token_GtEq;
				advance_to_next_rune(lexer, t);
				break;
			case '>':
				token->kind = Token_Shr;
				advance_to_next_rune(lexer, t);
				if (t->curr_rune == '=') {
					token->kind = Token_ShrEq;
					advance_to_next_rune(lexer, t);
				}
				break;
			}
			break;
		case '&':
			token->kind = Token_And;
			switch (t->curr_rune) {
			case '~':
				token->kind = Token_AndNot;
				advance_to_next_rune(lexer, t);
				if (t->curr_rune == '=') {
					token->kind = Token_AndNotEq;
					advance_to_next_rune(lexer, t);
				}
				break;
			case '=':
				token->kind = Token_AndEq;
				advance_to_next_rune(lexer, t);
				break;
			case '&':
				token->kind = Token_CmpAnd;
				advance_to_next_rune(lexer, t);
				if (t->curr_rune == '=') {
					token->kind = Token_CmpAndEq;
					advance_to_next_rune(lexer, t);
				}
				break;
			}
			break;
		case '|':
			token->kind = Token_Or;
			switch (t->curr_rune) {
			case '=':
				token->kind = Token_OrEq;
				advance_to_next_rune(lexer, t);
				break;
			case '|':
				token->kind = Token_CmpOr;
				advance_to_next_rune(lexer, t);
				if (t->curr_rune == '=') {
					token->kind = Token_CmpOrEq;
					advance_to_next_rune(lexer, t);
				}
				break;
			}
			break;
		default:
			token->kind = Token_Invalid;
			//if (curr_rune != GB_RUNE_BOM && curr_rune != 0) {
			//	u8 str[4] = {};
			//	int len = cast(int)gb_utf8_encode_rune(str, curr_rune);
			//	tokenizer_err(t, "Illegal character: %.*s (%d) ", len, str, curr_rune);
			//}
			break;
		}
	}

semicolon_check:;
	switch (token->kind) {
	case Token_Invalid:
	case Token_Comment:
		// Preserve insert_semicolon info
		break;
	case Token_Ident:
	case Token_context:
	case Token_typeid:
	case Token_break:
	case Token_continue:
	case Token_fallthrough:
	case Token_return:
	case Token_or_return:
		/*fallthrough*/
	case Token_Integer:
	case Token_Float:
	case Token_Imag:
	case Token_Rune:
	case Token_String:
	case Token_Undef:
		/*fallthrough*/
	case Token_Question:
	case Token_Pointer:
	case Token_CloseParen:
	case Token_CloseBracket:
	case Token_OpenBrace:
	case Token_CloseBrace:
		/*fallthrough*/
	case Token_Increment:
	case Token_Decrement:
		/*fallthrough*/
		t->insert_semicolon = true;
		break;
	default:
		t->insert_semicolon = false;
		break;
	}

	return;
}
