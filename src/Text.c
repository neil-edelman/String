/** Copyright 2017 Neil Edelman, distributed under the terms of the MIT License;
 see readme.txt, or \url{ https://opensource.org/licenses/MIT }.

 For reading and parsing text files.

 @param TEXT_DEBUG
 Prints things to stderr.

 @file		Text
 @author	Neil
 @version	1.0; 2017-02
 @since		1.0; 2017-02 */

#define TEXT_DEBUG

#include <stdlib.h> /* malloc free */
#include <stdio.h>  /* fprintf */
#include <errno.h>
#include <string.h>	/* strerror */
#include <limits.h>	/* INT_MAX */
#ifdef TEXT_DEBUG /* <-- */
#include <stdarg.h>
#endif /* --> */
#include "Text.h"

/* <-- ugly compiler-specific code, ie, MSVC/not-MSVC */
#ifdef _UNUSED
#undef _UNUSED
#endif
#ifndef _MSC_VER /* <-- not msvc */
/* don't produce warnings, yet optimise away the obviously useless while? hmm,
 I have not seen a clean solution for this */
#define _UNUSED(a) while(0 && (a));
#else /* not msvc --><-- msvc: not a C89/90 compiler; needs a little help */
#pragma warning(push)
/* "Assignment within conditional expression." That is a C language feature
 that I use all the time and is perfectly valid. */
#pragma warning(disable: 4706)
/* "<ANSI/ISO name>: The POSIX name for this item is deprecated. Instead use
 the ISO C and C++ conformant name <ISO C11 name>." No. */
#pragma warning(disable: 4996)
/* the pre-compiler is a little too smart for it's own good,
 http://stackoverflow.com/questions/4851075/universally-compiler-independent-
 way-of-implementing-an-unused-macro-in-c-c . . . what does that even mean?
 that can't be valid C? but works with MSVC */
#define _UNUSED(a) (void)(sizeof((a), 0))
#endif /* msvc --> */
/* ugly --> */



/* for allocating recursively */
static const unsigned fibonacci6  = 8;
static const unsigned fibonacci7  = 13;

/* for allocating buffer */
static const unsigned fibonacci11 = 89;
static const unsigned fibonacci12 = 144;

enum Error {
	E_NO_ERROR,
	E_ERRNO,
	E_PARAMETER,
	E_OVERFLOW,
	E_SYNTAX,
	E_ASSERT
};

static const char *const error_explination[] = {
	"no error",
	0,
	"parameter out-of-range",
	"overflow",
	"syntax error",
	"assertion failed"
};

/* globals */

static enum Error global_error = E_NO_ERROR;
static int        global_errno_copy;

struct Text {
	struct Text *up;
	size_t up_begin, up_end;
	char *name;
	char *buffer;
	size_t buffer_size, buffer_capacity[2];
	struct Text *downs;
	size_t downs_size, downs_capacity[2];
	enum Error error;
	int errno_copy;
};

/* private */

static struct Text *Text(char *const name);
static struct Text *Text_string_recursive(struct Text *const up,
	const size_t up_begin, const size_t up_end, char *const name,
	char *const str);
static int buffer_capacity_up(struct Text *const this);
static void debug(struct Text *const this, const char *const fn,
	const char *const fmt, ...);



/**********
 * Public */

/** Constructs an empty {Text} out of the filename, {fn}, having the name {fn}.
 @fixme		Assumes 8-bit encoding.
 @fixme		Hmmmm, Text() and TextFile()? can't decide wether to make it big or
			small. */
struct Text *TextFile(char *const fn) {
	struct Text *this;
	FILE *fp;
	size_t cursor = 0;
	int size, err;

	/* param check */
	if(!fn || !fn[0])
		{ global_error = E_PARAMETER; return 0; }

	/* generic constructor */
	if(!(this = Text(fn)))
		{ global_error = E_ERRNO, global_errno_copy = errno; return 0; }

	/* open */
	if(!(fp = fopen(fn, "r"))) {
		global_error = E_ERRNO, global_errno_copy = errno;
		Text_(&this);
		return 0;
	}

	/* allocate buffer; there is no real way to portably and correctly get the
	 number of characters in a text file, so allocate incrementally */
	if(!(this->buffer=malloc(this->buffer_capacity[0] *sizeof *this->buffer))) {
		global_error = E_ERRNO, global_errno_copy = errno;
		if(fclose(fp) == EOF) perror(fn); /* unreported error */
		Text_(&this);
		return 0;
	}

	/* read */
	while(size = (this->buffer_capacity[0] - cursor > INT_MAX) ?
		INT_MAX : (int)(this->buffer_capacity[0] - cursor),
		fgets(this->buffer + cursor, size, fp)) {
		cursor += strlen(this->buffer + cursor);
		if(cursor >= this->buffer_capacity[0] - 1 && !buffer_capacity_up(this)){
			global_error = this->error;
			if(fclose(fp) == EOF) perror(fn); /* unreported error */
			Text_(&this);
			return 0;
		}
	}
	if((err = ferror(fp))) {
		global_error = E_ERRNO;
		global_errno_copy = err;
		Text_(&this);
		return 0;
	}

	/* close */
	if(fclose(fp) == EOF) perror(fn); /* unreported error */

	return this;
}

/** Constructs an empty {Text} out of the string, {str}, having the name,
 {name}. */
struct Text *TextString(char *const name, char *const str) {
	struct Text *this;
	size_t str_size;

	if(!str || !name) return 0;

	str_size = strlen(str) + 1; /* danger */

	if(!(this = Text(name)))
		{ global_error = E_ERRNO, global_errno_copy = errno; return 0; }

	this->buffer_capacity[0] += str_size; /* danger */
	this->buffer_capacity[1] += str_size; /* danger */

	if(!(this->buffer=malloc(this->buffer_capacity[0] * sizeof *this->buffer))){
		global_error = E_ERRNO, global_errno_copy = errno;
		Text_(&this);
		return 0;
	}
	memcpy(this->buffer, str, str_size);

	return this;
}

/** Destructs a {Text}. */
void Text_(struct Text **const this_ptr) {
	struct Text *this, *child;
	size_t i;

	if(!this_ptr || !(this = *this_ptr)) return;

	debug(this, "~", "erase \"%s.\"\n", this->name);

	for(i = 0; i < this->downs_size; i++) {
		child = this->downs + i;
		Text_(&child);
	}
	free(this->downs);
	free(this->buffer);
	free(this);

	*this_ptr = 0;
}

/** Gets a string from the text; valid until the text size changes. */
char *TextGetString(struct Text *const this) {
	if(!this) return 0;
	return this->buffer;
}

/** @return		The last error associated with {this} (can be null.) */
const char *TextGetError(struct Text *const this) {
	const char *str;
	enum Error *perr;
	int *perrno;

	perr   = this ? &this->error      : &global_error;
	perrno = this ? &this->errno_copy : &global_errno_copy;
	if(!(str = error_explination[*perr])) str = strerror(*perrno);
	*perr = 0;
	return str;
}

/** Transforms {this} according to all specified {patterns} array of
 {patterns_size}.
 @param patterns	An array of {TextPattern}; when the {begin} of a pattern
 					encompasses another pattern, it should be before in the
					array. All patterns must have {begin} and {transform}, but
					{end} is optional; where missing, it will just call
					{transform} with {begin}.
 @param find_name	Searches though text to find a name. Can be null, in which
					case, the name will be "null" (sic.) */
int TextMatch(struct Text *this, struct TextPattern *const patterns,
	const size_t patterns_size, const StringOperator find_name) {
	struct TextPattern *pattern, *first_pat = 0;
	size_t p, begin_length;
	int is_replace = 0;
	char replace = 0, *pos, *first_pos = 0, *replace_pos = 0;

	if(!this) return 0;

	for(p = 0; p < patterns_size; p++) {
		pattern = patterns + p;
		printf("matching(\"%s\"..\"%s\") in \"%.30s..\".\n", pattern->begin,
			pattern->end, this->buffer);
		if(!(pos = strstr(this->buffer, pattern->begin))) continue;
		/*printf("begin \"%.40s..\".\n", pos);*/
		/* this happens when first_pos is [abcdefg] and [cdef] is matched */
		if(first_pos && pos >= first_pos) continue;
		/* else it's the first */
		first_pat = pattern, first_pos = pos + strlen(pattern->begin);
		/* move the temporary null ahead */
		if(is_replace) *replace_pos = replace;
		begin_length = strlen(pattern->begin);
		replace_pos = pos + strlen(pattern->begin);
		replace = *replace_pos, *replace_pos = '\0', is_replace = -1;
		/*printf("now first \"%.40s..\".\n", pos);*/
	}
	/* didn't find any patterns */
	if(!first_pat) return 0;
	/* if the {first_pat} has an ending, move the cursor to ending */
	if(first_pat->end) {
		*replace_pos = replace;
		if(!(pos = strstr(replace_pos, first_pat->end)))
			{ this->error = E_SYNTAX; return 0; }
		replace_pos = pos/* + strlen(first_pat->end) <- don't want the 'end' */;
		replace = *replace_pos, *replace_pos = '\0';
	}
	/* copy first_pos into a separate temporary buffer */
	/*printf("first_pos: \"%s\" from %lu to %lu.\n", first_pos,
		first_pos - this->buffer, replace_pos - this->buffer);*/
	/* fixme: call find_name; find_name will go to the end and search for a
	 function, transform it into a temp buffer */
	if(!Text_string_recursive(this, (size_t)(first_pos - this->buffer),
		(size_t)(replace_pos - this->buffer), "null", first_pos)) return 0;
	/* reset the buffer back to normal */
	*replace_pos = replace;
	/*printf("now buffer \"%.40s..\" and first \"%s\" at \"%.40s..\".\n",
		this->buffer, first_pat ? first_pat->begin : "(null)", first_pos);*/

	return -1;
}



/************
 * Private. */

/** Constructs a generic buffer, but buffer is null to give the calling fn room
 to expand; set it to something. If returning 0, there was a allocation error,
 and {errno} is set. */
static struct Text *Text(char *const name) {
	struct Text *this;
	size_t name_size;

	name_size = strlen(name) + 1;
	if(!(this = malloc(sizeof *this + name_size))) return 0;
	this->up                 = 0;
	this->up_begin           = 0;
	this->up_end             = 0;
	this->name               = (char *)(this + 1);
	memcpy(this->name, name, name_size);
	this->buffer             = 0;
	this->buffer_size        = 0;
	this->buffer_capacity[0] = fibonacci11;
	this->buffer_capacity[1] = fibonacci12;
	this->downs              = 0;
	this->downs_size         = 0;
	this->downs_capacity[0]  = fibonacci6;
	this->downs_capacity[1]  = fibonacci7;
	this->error              = E_NO_ERROR;
	this->errno_copy         = 0;

	if(!(this->downs = malloc(this->downs_capacity[0] * sizeof *this->downs)))
		{ Text_(&this); return 0; }

	debug(this, "cons", "new, \"%s,\" capacity %d.\n", name,
		this->buffer_capacity[0]);

	return this;
}

static struct Text *Text_string_recursive(struct Text *const up,
	const size_t up_begin, const size_t up_end, char *const name,
	char *const str) {
	struct Text *this;
	size_t str_size;

	if(!str || !name) return 0;

	str_size = strlen(str) + 1; /* danger */

	if(up && str_size != up_end - up_begin + 1) {
		up->error = E_ASSERT;
		return 0;
	}

	if(!(this = Text(name)))
		{ up->error = E_ERRNO, up->errno_copy = errno; return 0; }

	this->up       = up;
	this->up_begin = up_begin;
	this->up_end   = up_end;
	this->buffer_capacity[0] += str_size; /* danger */
	this->buffer_capacity[1] += str_size; /* danger */

	if(!(this->buffer=malloc(this->buffer_capacity[0] * sizeof *this->buffer))){
		up->error = E_ERRNO, up->errno_copy = errno;
		Text_(&this);
		return 0;
	}
	memcpy(this->buffer, str, str_size);
	printf("recursive: \"%s\"\n", this->buffer);

	return this;
}

/** Ensures buffer capacity.
 @return	Success.
 @throws	E_OVERFLOW, E_ERRNO */
static int buffer_capacity_up(struct Text *const this) {
	size_t c0, c1;
	char *buffer;

	/* fibonacci */
	c0 = this->buffer_capacity[0];
	c1 = this->buffer_capacity[1];
	if(c0 == (size_t)-1) { this->error = E_OVERFLOW; return 0; }
	c0 ^= c1;
	c1 ^= c0;
	c0 ^= c1;
	c1 += c0;
	if(c1 <= c0) c1 = (size_t)-1;
	debug(this, "buffer_capacity_up", "capacity %lu->%lu.\n",
		this->buffer_capacity[0], c0);
	if(!(buffer = realloc(this->buffer, c0 * sizeof *this->buffer))) {
		this->error = E_ERRNO, this->errno_copy = errno;
		return 0;
	}
	this->buffer = buffer;
	this->buffer_capacity[0] = c0;
	this->buffer_capacity[1] = c1;

	return -1;
}

/** Private debug messages from list functions; turn on using {LIST_DEBUG}. */
static void debug(struct Text *const this, const char *const fn,
	const char *const fmt, ...) {
#ifdef LIST_DEBUG
	va_list parg;
	char scratch[9];

	va_start(parg, fmt);
	fprintf(stderr, "Text<%s>#%p.%s: ", this->fn, (void *)this, fn);
	vfprintf(stderr, fmt, parg);
	va_end(parg);
#else
	_UNUSED(this); _UNUSED(fn); _UNUSED(fmt);
#endif
}
