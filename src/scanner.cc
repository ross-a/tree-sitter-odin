#include <tree_sitter/parser.h>
#include <wctype.h>
#include <stdio.h>

#include "odin_src/gb/gb.h"
#include "odin_src/tokenizer.cpp" // modified to work with tree-sitter

// Valid Tokens that match up in grammer.js
enum TokenType {
#define TOKEN_KIND(e, s) tso_ ## e
	TOKEN_KINDS
#undef TOKEN_KIND
};

extern "C" {
	void *tree_sitter_odin_external_scanner_create() {
		Tokenizer *t = (Tokenizer*) malloc(sizeof(Tokenizer));
		t->line_count = -1; // using to indicate start of scan

		String s;
		s.text = (u8*)"import";
		s.len = 6;
		u32 hash = keyword_hash(s.text, s.len);
		u32 index = hash & KEYWORD_HASH_TABLE_MASK;
		KeywordHashEntry *entry = &keyword_hash_table[index];
		if (entry->kind != Token_import) {
				init_keyword_hash_table();
		}
		
		return t;
	}
	
	void tree_sitter_odin_external_scanner_destroy(void *p) {
		Tokenizer *t = (Tokenizer*)p;
		free(t);
	}
	
	void tree_sitter_odin_external_scanner_reset(void *p) {}

	unsigned tree_sitter_odin_external_scanner_serialize(void *p, char *buffer) {
		// copy p into buffer
		memcpy(buffer, p, sizeof(Tokenizer));
		return sizeof(Tokenizer);
	}
	
	void tree_sitter_odin_external_scanner_deserialize(void *p, const char *b, unsigned n) {
		memcpy(p, b, n);
	}

	bool tree_sitter_odin_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols);
}

bool tree_sitter_odin_external_scanner_scan(void *payload, TSLexer *lexer,
                                            const bool *valid_symbols) {

	// lexer->lookahead = int32_t 32-bit unicode code point... convert to char* and insert into buf
	// lexer->result_symbol = TSSymbol to return (see TokenType enum)
	// lexer->advance(lexer, skip)... skip treats curr character as whitespace
	// lexer->mark_end(lexer)... mark end of token so you can advance more if needed without gobbling up more
	// valid_symbols[Token_...] expected valid tokens

	bool eof = (lexer->lookahead == 0);
	Tokenizer *t = (Tokenizer*)payload;
	bool start = (t->line_count == -1);

	if (start) {
		t->line_count = 1;
		t->curr_file_id = -1;
		t->curr_rune = lexer->lookahead;
	}

	Token tok;
	for (;;) {
		tokenizer_get_token(lexer, t, &tok);
		lexer->mark_end(lexer); // for peek_rune()

		if (tok.kind == Token_Semicolon && t->curr_file_id == -1) {
			t->curr_file_id = 1;
			continue;
		}
		if (tok.kind == Token_EOF && !eof) {
			continue;
		}
		if (tok.kind == Token_Invalid) {
			return false; // skip invalid to prevent inf loop
		}
		
		break;
	}

	lexer->result_symbol = tok.kind;

	if (!valid_symbols[tok.kind]) {
		printf("got unexpected %s (%d, %d)\n", token_strings[tok.kind], t->line_count, t->column_minus_one);
		for (int i=0; i<Token_Count; i++) {
			if (valid_symbols[i]) {
				printf("%d - %s\n", i, token_strings[i]);
			}
		}
		return false;
	}	
	return !eof;
}
