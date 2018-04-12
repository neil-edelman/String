/** Copyright 2015 Neil Edelman, distributed under the terms of the MIT License;
 see readme.txt, or \url{ https://opensource.org/licenses/MIT }.

 This is a test of String.

 @author	Neil
 @version	2018-03 Re-factored code to split up {String} and {Text}.
 @since		2017-03 */

#include <stdlib.h>	/* EXIT_* */
#include <stdio.h>  /* fprintf */
#include <string.h>	/* strcmp */
#include <assert.h>
#include "../src/String.h"

static void verify(const struct String *const string, const size_t bytes,
	const size_t codep) {
	size_t b, c;
	assert(string);
	b = StringLength(string);
	c = StringCodePoints(string);
	printf("The string <%s> has %lu bytes and %lu code-points.\n",
		StringGet(string), (unsigned long)b, (unsigned long)c);
	assert(b == bytes);
	assert(c == codep);
	assert(!StringHasContent(string) == !bytes);
}

static void test_regex(void) {
	struct Regex *re_null = Regex(0), *re_empty = Regex(""), *re;
	const char *str1 = "hellohithere", *str2 = "thsdoesnot", *m;
	re = Regex("hi");
	assert(!re_null && !re_empty && re);

	if((m = RegexMatch(re, str1))) printf("matches <%s> <%s>\n", str1, m);
	else assert(0);
	printf("str2: <%s>\n", str2);
	if((m = RegexMatch(re, str2)))
		printf("matches <%s> <%s>\n", str2, m), assert(0);

	Regex_(&re_null);
	Regex_(&re);
	assert(!re_null && !re_empty && !re);
}

/** The is a test of String. */
int main(void) {
	const char *bit1x30 = "all your base are belong to us",
		*bit2x2 = "¥æ",
		*bit3x7 = "煮爫禎ﬀﭖﳼﷺ",
		*bit4x4 = "𐑫𐑣𐑟𐐥";
	struct String s = { 0, 0, { 0, 0 } }, t;
	const char *a, *b;
	size_t bytes = 0, codep = 0, i;

	printf("Testing:\n");

	String(&t);
	a = StringGet(&s);
	b = StringGet(&t);
	assert(!a && !b && StringLength(&s) == 0
		&& !StringHasContent(0) && !StringHasContent(&s));

	String_(&s);
	a = StringGet(&s);
	assert(!a);

	StringClear(&s);
	a = StringGet(&s);
	assert(a && !strcmp(a, "") && StringLength(&s) == 0
		&& !StringHasContent(&s));

	StringClear(&s);
	for(i = 0; i < 300; i++) StringPrintCat(&s, "%c", '0' + i % 10);
	printf("t: \"%s\"\n", StringGet(&s));
	i = StringLength(&s);
	assert(i == 300);

	printf("StringNCat:\n");
	StringClear(&t);
	StringNCat(&t, "TestString", (size_t)4);
	printf("String: %s\n", StringGet(&t));
	assert((a = StringGet(&t)) && !strcmp(b = "Test", a));
	printf("StringTransform:\n");
	StringTransform(&t, "\\url{%s%%%s} yo {YO}");
	printf("String: %s\n", StringGet(&t));
	assert((a = StringGet(&t))
		&& !strcmp(b = "\\url{Test%Test} yo {YO}", a));

	printf("StringBetweenCat:\n");
	{
		const char *const fn = "foo/bar/baz/qux/quxx";
		const char *s0, *s1;
		StringClear(&t);
		s0 = strchr(fn + 1, '/');
		s1 = strchr(s0 + 1, '/');
		StringBetweenCat(&t, s0, s1);
		assert((a = StringGet(&t)) && !strcmp(b = "/bar/", a));
		s0 = strchr(s1 + 1, '/');
		s1 = strchr(s0 + 1, '/');
		StringBetweenCat(&t, s0, s1);
		assert((a = StringGet(&t)) && !strcmp(b = "/bar//qux/", a));
		printf("String: %s\n", StringGet(&t));
		StringClear(&t);
	}

	/* @fixme This is a pitiful test. */
	StringCopy(&s, bit3x7);
	StringCopy(&s, bit1x30), bytes += 30, codep += 30;
	StringCat(&s, bit2x2), bytes += 2*2, codep += 2;
	StringCat(&s, bit3x7), bytes += 3*7, codep += 7;
	StringCat(&s, bit4x4), bytes += 4*4, codep += 4;
	verify(&s, bytes, codep);
	StringNCat(&s, bit1x30, (size_t)5), bytes += 5, codep += 5;
	verify(&s, bytes, codep);
	StringBetweenCat(&s, bit1x30 + 10, bit1x30 + 20), bytes += 11, codep += 11;
	verify(&s, bytes, codep);
	StringPrintCat(&s, "%s%s%.3s \t", bit2x2, bit4x4, bit1x30),
		bytes += 2*2 + 4*4 + 3 + 2, codep += 2 + 4 + 3 + 2;
	verify(&s, bytes, codep);
	StringTransform(&s, "    %s%%%s\n\f");
	bytes *= 2, bytes += 4 + 1 + 2, codep *= 2, codep += 4 + 1 + 2;
	verify(&s, bytes, codep);
	StringCopy(&t, StringGet(&s));
	verify(&t, bytes, codep);
	StringRightTrim(&t);
	verify(&t, bytes - 4, codep - 4);
	StringTrim(&s);
	bytes -= 4 + 4, codep -= 4 + 4;
	verify(&s, bytes, codep);

	StringClear(&s);
	verify(&s, 0, 0);
	String_(&t);
	verify(&t, 0, 0);
	String_(&s);
	verify(&s, 0, 0);

	test_regex();

	return EXIT_SUCCESS;
}

