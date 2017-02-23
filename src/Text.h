struct Text;

typedef void (*TextTransform)(struct Text *const match);

struct TextPattern {
	char *begin;
	char *end;
	TextTransform transform;
};

struct Text *TextFile(char *const fn);
struct Text *TextString(char *const name, char *const str);
char *TextGetString(struct Text *const this);
void Text_(struct Text **const this_ptr);
const char *TextGetError(struct Text *const this);
int TextMatch(struct Text *this, struct TextPattern *const patterns,
	const size_t patterns_size);
