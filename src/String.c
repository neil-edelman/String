/** 2017 Neil Edelman, distributed under the terms of the MIT License;
 see readme.txt, or \url{ https://opensource.org/licenses/MIT }.

 A dynamic string, intended to be used with modified UTF-8 encoding,
 \url{ https://en.wikipedia.org/wiki/UTF-8#Modified_UTF-8 }. That is, this is a
 wrapper that automatically expands memory as needed around a standard {C}
 null-terminated string in a monolithic array and is compatible with {ASCII}.
 If you need to edit a potentially large string, just one of {String} will be
 generally linear-time and is unsuited alone for such a purpose.

 {struct String} is meant to used directly; it is in the header. It's initial
 state is zero where it will be inactive, for example
 {struct String s = { 0 };} or static data. Any calls to string functions
 generally make it active and one should destruct the string by \see{String_}.
 The only exception is \see{String}, which also initialises the string to be
 empty. All functions accept null pointers as a valid state, which means one
 can compose functions safely without
 \url{ https://en.wikipedia.org/wiki/Pyramid_of_doom_(programming) }.

 @title		String
 @author	Neil
 @std		C89/90, C99 {vsnprintf}, C99 {stdint.h}
 @version	2018-03 {Text -> String}; complete refactoring to work with {Text}.
 @since		2018-01
			2017-03
 @fixme uhh, {StringByteOffsetCodePoints()}?
 @fixme work with int instead of char, obviously. */

#include <stdlib.h> /* malloc realloc free */
#include <string.h> /* strlen memmove memcpy memchr */
#include <errno.h>  /* errno */
#include <assert.h> /* assert */
#include <ctype.h>  /* isspace */
#include <stdarg.h> /* va_* */
#include <stdint.h> /* C99 uint32_t */
#include "String.h"

/* This function was standardised in C99. */
int vsnprintf(char *s, size_t n, const char *format, va_list ap);

/** {strdup} is a {POSIX.1-2017} extension. This is better. Used in
 \see{StringTransform}.
 @throws {malloc} errors: {IEEE Std 1003.1-2001}. */
static void *memdup(const void *src, const size_t n) {
	void *copy;
	assert(src);
	if(!(copy = malloc(n))) return 0;
	memcpy(copy, src, n);
	return copy;
}

/* Used in \see{text_length}. */
static const size_t fibonacci11 = 89;
static const size_t fibonacci12 = 144;

/** Initialises {string} to inactive. */
static void inactive(struct String *const string) {
	assert(string);
	string->text        = 0;
	string->length      = 0;
	string->capacity[0] = 0;
	string->capacity[1] = 0;
}

/** Clears the text of an active {string}. */
static void clear(struct String *const string) {
	assert(string && string->text);
	string->length  = 0;
	string->text[0] = '\0';
}

/** Ensures {text} capacity.
 @param len: The length to which one wants it to grow.
 @return Success; otherwise, {errno} may be set.
 @throws ERANGE: Tried allocating more then can fit in {size_t}.
 @throws {realloc} errors: {IEEE Std 1003.1-2001}. */
static int text_length(struct String *const string, const size_t len) {
	const size_t size = len + 1;
	size_t c0, c1;
	char *text;
	assert(string);
	if(size <= len) return errno = ERANGE, 0;
	/* It's already that size. */
	if(string->capacity[0] >= size) return 1;
	if(!string->text) c0 = fibonacci11, c1 = fibonacci12;
	else c0 = string->capacity[0], c1 = string->capacity[1];
	while(c0 < size) {
		assert(c0 < c1);
		c0 ^= c1, c1 ^= c0, c0 ^= c1, c1 += c0;
		if(c1 <= c0) c1 = (size_t)-1;
	}
	/* If this fails, we are good with whatever {realloc} sets {errno},
	 including not setting it at all, by {C89} standards. Probably {ENOMEM}. */
	if(!(text = realloc(string->text, c0 * sizeof *string->text))) return 0;
	string->text = text, string->capacity[0] = c0, string->capacity[1] = c1;
	return 1;
}

/** @return Success.
 @throws ERANGE: Tried allocating more then can fit in {size_t}.
 @throws {realloc} errors: {IEEE Std 1003.1-2001}. */
static int cat(struct String *const string, const char *const str,
	const size_t str_len) {
	const size_t old_len = string->length, new_len = old_len + str_len;
	assert(string && str);
	if(!str_len) return 1;
	if(new_len <= old_len) { errno = ERANGE; return 0; }
	if(!text_length(string, new_len)) return 0;
	memcpy(string->text + old_len, str, str_len);
	string->text[new_len] = '\0';
	string->length = new_len;
	return 1;
}

/** Use this if {string} is already in an initialised state. The {String} text
 will be set to null and any memory will be freed.
 @param string: If null, does nothing.
 @order O(1) */
void String_(struct String *const string) {
	if(!string) return;
	free(string->text);
	inactive(string);
}

/** Use this if {string} is uninitialised. Sets the {String} text to be null,
 thus in a well-defined state. Static {String} variables do not need
 initialisation, though it will not hurt. Calling this on an active {string}
 results in a memory leak.
 @param string: A string whose text will be set to null. If null, does nothing.
 @order O(1) */
void String(struct String *const string) {
	if(!string) return;
	inactive(string);
}

/** Erases the text of {string} so the text is empty. If the text of {string}
 is null, initialises an empty string.
 @param string: If null, returns null.
 @return {string}.
 @throws {realloc} errors: {IEEE Std 1003.1-2001}.
 @order O(1) */
struct String *StringClear(struct String *const string) {
	if(!string) return 0;
	if(!string->text) {
		if(!text_length(string, 0)) return 0;
		assert(string->text);
	}
	clear(string);
	return string;
}

/** Volatile, in the sense that it exposes the text; specifically, not
 guaranteed to last between {String} calls to the same object. If you want a
 copy, do {strdup(StringGet(string))}.
 @return The text associated to {string} or null if there is no text or if
 {string} is null.
 @order O(1) */
const char *StringGet(const struct String *const string) {
	if(!string) return 0;
	return string->text;
}

/** @param string: If null, returns zero.
 @return The length in bytes.
 @order O(1) */
size_t StringLength(const struct String *const string) {
	if(!string) return 0;
	return string->length;
}

/** @param string: if null, returns zero.
 @return How many code-points in
 \url{ https://en.wikipedia.org/wiki/UTF-8#Modified_UTF-8 }. If it is not a
 valid string in {UTF-8}, string will return an undefined value between
 {[0, size]}.
 @order O({string.size})
 @fixme Untested.
 @fixme This is stupid, work with ints. */
size_t StringCodePoints(const struct String *const string) {
	char *text, ch;
	const char *end_null;
	size_t length;
	if(!string || !(text = string->text)) return 0;
	length = string->length;
	end_null = text + length;
	assert(*end_null == '\0');
	while(text < end_null) {
		/* Less and less likely; optimise for the top. */
		if((ch = *text) > 0)    { text++; continue; }
		if((ch & 0xE0) == 0xC0) { text += 2; length -= 1; continue; }
		if((ch & 0xF0) == 0xE0) { text += 3; length -= 2; continue; }
		if((ch & 0xF8) == 0xF0) { text += 4; length -= 3; continue; }
		/* RFC 3629: "treat any ill-formed code unit sequence as an error
		 condition." Skip? */
		text++, length--;
	}
	return length;
}

/** @return True if the text of {string} exists and is not empty. */
int StringHasContent(const struct String *const string) {
	return !(!string || !string->text || *string->text == '\0');
}

/** White-space trims the text associated with {string} using {isspace} only
 at the end.
 @param string: If null, returns null.
 @return {string}.
 @fixme Untested. */
struct String *StringRightTrim(struct String *const string) {
	char *str, *z;
	if(!string || !(str = string->text) || !string->length) return string;
	z = str + string->length - 1;
	while(z >= str && isspace(*z)) z--;
	z++, *z = '\0';
	string->length = (size_t)(z - str);
	return string;
}

/** White-space trims the text associated with {string} using {isspace}.
 @param string: If null, returns null.
 @return {string}. */
struct String *StringTrim(struct String *const string) {
	char *str, *a, *z;
	if(!string || !(str = string->text) || !string->length) return string;
	z = str + string->length - 1, a = str;
	while(z >= str && isspace(*z)) z--;
	z++, *z = '\0';
	while(isspace(*a)) a++;
	string->length = (size_t)(z - a);
	if(a - str) memmove(str, a, string->length + 1);
	return string;
}

/** Replaces the text in {string} with {str}.
 @param string: If null, returns null.
 @param str: If null, returns {string}.
 @return {string}.
 @throws ERANGE: Tried allocating more then can fit in {size_t}.
 @throws {realloc} errors: {IEEE Std 1003.1-2001}. */
struct String *StringCopy(struct String *const string, const char *const str) {
	size_t len;
	if(!string || !str) return string;
	len = strlen(str);
	if(!text_length(string, len)) return 0; /* Pre-allocate. */
	clear(string);
	if(!cat(string, str, len)) return 0; /* Unthinkable. */
	return string;
}

/** Concatenates {str} onto the text in {string}.
 @param string: If null, returns null.
 @param str: If null, returns {string}.
 @return {string}.
 @throws ERANGE: Tried allocating more then can fit in {size_t}.
 @throws {realloc} errors: {IEEE Std 1003.1-2001}. */
struct String *StringCat(struct String *const string, const char *const str) {
	if(!string || !str) return string;
	if(!cat(string, str, strlen(str))) return 0;
	return string;
}

/** Concatenates up to {str_len} bytes characters of {str} onto the text in
 {string}.
 @param string: If null, returns null.
 @param str: If null, returns {string}.
 @param str_len: If the bytes one has access to is smaller then this value, the
 results are technically undefined, if using a compiler mode before {C11}.
 \url{ https://stackoverflow.com/questions/47315902/is-it-legal-to-call-memchr-with-a-too-long-length-if-you-know-the-character-wil?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa }
 @return {string}.
 @throws ERANGE: Tried allocating more then can fit in {size_t}.
 @throws {realloc} errors: {IEEE Std 1003.1-2001}. */
struct String *StringNCat(struct String *const string, const char *const str,
	const size_t str_len) {
	const char *end;
	if(!string || !str) return string;
	end = memchr(str, 0, str_len);
	if(!cat(string, str, end ? (size_t)(end - str) : str_len)) return 0;
	return string;
}

/** Concatenates {string} with {[a, b]}.
 @param string: If null, returns null.
 @param a, b: If {a} or {b} are null or {a} > {b}, returns {string}.
 @return {string}.
 @throws ERANGE: Tried allocating more then can fit in {size_t}.
 @throws {realloc} errors: {IEEE Std 1003.1-2001}. */
struct String *StringBetweenCat(struct String *const string,
	const char *const a, const char *const b) {
	if(!string || !a || !b || a > b) return string;
	/* @fixme ?? end = memchr(a, 0, (size_t)(b - a + 1));
	 to make sure it doesn't contain nulls? do we want that? */
	if(!cat(string, a, (size_t)(b - a + 1))) return 0;
	return string;
}

/** Concatenates the text with an {fprintf};
 \url{http://pubs.opengroup.org/onlinepubs/007908799/xsh/fprintf.html}.
 @param string: If null, returns null.
 @param fmt: If null, returns {string}.
 @return {string}.
 @throws ERANGE: Tried allocating more then can fit in {size_t}.
 @throws {vsnprintf/realloc} errors: {IEEE Std 1003.1-2001}. */
struct String *StringPrintCat(struct String *const string,
	const char *const fmt, ...) {
	va_list argp;
	char garbage;
	int length;
	size_t total_length;
	if(!string || !fmt) return string;
	/* Check the length first by printing to garbage. */
	va_start(argp, fmt);
	length = vsnprintf(&garbage, 0ul, fmt, argp);
	va_end(argp);
	if(length < 0) return 0; /* {vsnprintf} error. */
	total_length = string->length + length;
	if(total_length < string->length) { errno = ERANGE; return 0; }
	if(!text_length(string, total_length)) return 0;
	/* Now that we have enough space, do the actual printing. */
	va_start(argp, fmt);
	length = vsnprintf(string->text + string->length, string->capacity[0], fmt,
		argp);
	va_end(argp);
	if(length < 0) return 0; /* {vsnprintf} error. */
	string->length += length;
	return string;
}

/** Transforms the original text according to {fmt}.
 @param string: If null, returns null.
 @param fmt: Accepts %% as '%' and %s as the original string. If null, returns
 {string}.
 @return {string}.
 @throws ERANGE: Tried allocating more then can fit in {size_t}.
 @throws {strdup/malloc/realloc} errors: {IEEE Std 1003.1-2001}. */
struct String *StringTransform(struct String *const string, const char *fmt) {
	int is_old_ref = 0, is_old_dup = 0;
	const char *f;
	char *t, null_termiate = '\0', *old = &null_termiate;
	size_t copy_len = 0, old_len = 0;
	if(!string || !fmt) return string;
	/* Count. */
	old_len = string->length;
	for(f = fmt; *f; f++) {
		if(*f != '%') { copy_len++; continue; }
		switch(*++f) {
			case '%': copy_len++; break;
			case 's': copy_len += old_len; is_old_ref = 1; break;
		}
	}
	/* Copy the string into {old}. */
	if(is_old_ref && string->text && string->text != '\0') {
		assert(string->length > 0);
		if(!(old = memdup(string->text, old_len + 1))) return 0;
		is_old_dup = 1;
	} else {
		old_len = 0; /* Paranoid. */
	}
	/* Allocate. */
	if(!text_length(string, copy_len))
		{ if(is_old_dup) free(old); return 0; }
	/* New string is the transform. */
	for(t = string->text, f = fmt; *f; f++) {
		if(*f != '%') { *t++ = *f; continue; }
		switch(*++f) {
			case '%': *t++ = '%'; break;
			case 's': memcpy(t, old, old_len), t += old_len; break;
		}
	}
	*t = '\0';
	string->length = copy_len;
	/* Free {old}. */
	if(is_old_dup) free(old);
	return string;
}



#include <stdio.h>

/*typedef int (*Match)(char *const, struct State *const);

struct StateVt {
	
};

struct State {
	const Match *const match;
};
#define LIST_NAME State
#define LIST_TYPE struct Match


struct MatchString {

};*/

/* {State} used in {Regex}. */
struct State {
	uint32_t bit[8]; /* Bit field, one byte, {256/32 = 8}. {!out} -> {bit}. */
	struct State *out[2];
};
struct Migrate;
static void state_migrate_each(struct State *const state,
	const struct Migrate *const migrate);
#define STACK_NAME State
#define STACK_TYPE struct State
#define STACK_MIGRATE_EACH &state_migrate_each
#include "Stack.h"
static void state_migrate_each(struct State *const state,
	const struct Migrate *const migrate) {
	StateStackMigratePointer(&state->out[0], migrate);
	StateStackMigratePointer(&state->out[1], migrate);
}
static void bit_clear(uint32_t bit[8]) {
	bit[0] = bit[1] = bit[3] = bit[4] = bit[5] = bit[6] = bit[7] = 0;
}
static void bit_invert(uint32_t bit[8]) {
	bit[0] = ~bit[0], bit[1] = ~bit[1], bit[2] = ~bit[2], bit[3] = ~bit[3],
	bit[4] = ~bit[4], bit[5] = ~bit[5], bit[6] = ~bit[6], bit[7] = ~bit[7];
}
static void bit_set(uint32_t bit[8], char b) {
	uint32_t *const hi = bit + (b >> 5);
	*hi |= (1 << (b & 31));
}
static int bit_test(uint32_t bit[8], char b) {
	uint32_t *const hi = bit + (b >> 5);
	return *hi & (uint32_t)(1 << (b & 31));
}

/** Initialises an empty {state}.
 @param state: Has to be valid. */
static void State(struct State *const state) {
	assert(state);
	bit_clear(state->bit);
	state->out[0] = state->out[1] = 0;
}
/** Adds {match} to state. */
static void StateMatchAdd(struct State *const state, const char match) {
	assert(state);
	bit_set(state->bit, match);
}
/** Inverts the matches that will get though {state}.
 @fixme UTF-8 does not do as expecected. */
static void StateMatchInvert(struct State *const state) {
	assert(state);
	bit_invert(state->bit);
}
/** Tests {match} agaist {state}. */
static int StateMatch(struct State *const state, const char match) {
	assert(state);
	return bit_test(state->bit, match);
}

/* Temporary variable when compiling a {Regex}; indexes into {StateStack}. */
struct Pair { size_t from, to; };
#define STACK_NAME Pair
#define STACK_TYPE struct Pair
#include "Stack.h"

/* All wrapped up one one object for convenience. */
struct MakeRe {
	struct StateStack *states;
	struct PairStack pairs;
	int is_in_text;
};

/** Helper for \see{re_compile}.
 @throws ERANGE: Tried allocating more then can fit in {size_t}.
 @throws {realloc} errors: {IEEE Std 1003.1-2001}. */
static int add_state(struct MakeRe *const make, const char byte) {
	struct State *const state = StateStackNew(make->states);
	assert(make);
	/* Add the state. */
	if(!state) return 0; /* {realloc} error. */
	State(state);
	StateMatchAdd(state, byte);
	/* Add/update temporary pairs. */
	if(make->is_in_text) {
		/* We are currently in the pair. */
		struct Pair *const pair = PairStackPeek(&make->pairs);
		struct State *const last = StateStackGet(make->states, pair->to);
		assert(pair && last && !last->out[0] && !last->out[1]);
		last->out[0] = state;
		pair->to = StateStackIndex(make->states, state);
	} else {
		/* Make a new pair. */
		struct Pair *const pair = PairStackNew(&make->pairs);
		if(!pair) return 0; /* {realloc} error. */
		pair->from = pair->to = StateStackIndex(make->states, state);
		make->is_in_text = 1;
	}
	printf("%c", byte);
	return 1;
}

/** Helper for \see{re_compile}. */
static void repeat(struct MakeRe *const make, int low, int high) {
	assert(make && low >= 0 && (high == -1 || low < high));
}

/** Clears all memory and resets {make}. */
static void MakeRe_(struct MakeRe *const make) {
	if(!make) return;
	PairStack_(&make->pairs);
	make->is_in_text = 0;
}

/** Initialises {make} and clears {states}. */
static void MakeRe(struct MakeRe *const make, struct StateStack *const states) {
	assert(make && states);
	make->states = states;
	PairStack(&make->pairs);
	make->is_in_text = 0;
	StateStackClear(states);
}

/* {Regex} uses {State}. */
struct Regex {
	struct StateStack states;
};

/** \url{ https://swtch.com/~rsc/regexp/regexp1.html }.
 @param re: A valid {Regex}; will be erased.
 @param compile: A non-null string. */
static int re_compile(struct Regex *const re, const char *const compile) {
	struct MakeRe make;
	int is_done = 0, is_escape = 0;
	const char *byte;
	printf("<<re_compile:\n");
	assert(re && compile);
	MakeRe(&make, &re->states);
	/* Compile char by char. */
	for(byte = compile; ; byte++) {
		/* The previous was a '\'. */
		if(is_escape) {
			if(byte) {
				if(!add_state(&make, *byte)) return 0;
				continue;
			} else {
				/* Add lone backslash at the end. */
				if(!add_state(&make, '\\')) return 0;
				break;
			}
		}
		switch(*byte) {
		case '\\': is_escape = 1; break;
		case '|': break;
		case '*': repeat(&make, 0, -1); break;
		case '+': repeat(&make, 1, -1); break;
		case '?': repeat(&make, 0,  1); break;
		/* @fixme case '{', '}'' */
		case '(': /*make.pairs @fixme */
		case ')': make.is_in_text = 0; break;
		case '\0': is_done = 1; break;
		default: if(!add_state(&make, *byte)) return 0; break;
		}
		if(is_done) break;
	}
	printf("\nre_compile>>\n");
	MakeRe_(&make);
	return 1;
}

/** Destructor. One can desruct anything in a valid state, including null and
 zero, it just does nothing.
 @param re: If null, does nothing, otherwise it is set to match zero characters
 and frees the memory. */
void Regex_(struct Regex **const pre) {
	struct Regex *re;
	if(!pre || !(re = *pre)) return;
	StateStack_(&re->states);
	free(re);
	*pre = 0;
}

/** Compiles a regex into an uninitalised or empty {re}.
 @param re: If null, does nothing and returns false, otherwise on error, this
 is initailialised to empty. On success, requires \see{Regex_} destructor when
 done.
 @param compile: If null or empty, {re} is initailialised to empty and it
 returns true. Otherwise, this is a null-terminated modified UTF-8 string that
 gets compiled into a regular expression.
 @return Success.
 @throws {malloc/realloc} errors: {IEEE Std 1003.1-2001}.
 @throws EILSEQ: The {re} could not be compiled, (required since 1994
 Amendment 1 to C89 standard.) */
struct Regex *Regex(const char *const compile) {
	struct Regex *re;
	if(!compile || !*compile || !(re = malloc(sizeof *re))) return 0;
	StateStack(&re->states);
	if(!re_compile(re, compile)) errno = EILSEQ, Regex_(&re);
	return re;
}

/** Compare at this point. */
static int re_match(struct Regex *const re, const char *m) {
	struct State *s = 0;
	assert(re && m);
	/* Starting state; if there is none, the expression trivally matches. */
	for(s = StateStackNext(&re->states, 0); s; s = s->out[0], m++)
		if(!StateMatch(s, *m)) return 0;
	return 1;
}

/** Match {re} to {match}.
 @return The first match or null. */
const char *RegexMatch(struct Regex *const re, const char *const match) {
	const char *m = match;
	if(!re || !match) return 0;
	for( ; ; m++) {
		if(re_match(re, m)) return m;
		if(*m == '\0') break;
	}
	return 0;
}
