
#include <SDL.h>
#include <SDL_ttf.h>

SDL_Surface * renderTextSolid(TTF_Font *font, const char *text, SDL_Color *fg);
SDL_Surface * renderUTF8Solid(TTF_Font *font, const char *text, SDL_Color *fg);
SDL_Surface * renderGlyphSolid(TTF_Font *font, Uint16 ch, SDL_Color *fg);

SDL_Surface * renderTextShaded(TTF_Font *font, const char *text, SDL_Color *fg, SDL_Color *bg);
SDL_Surface * renderUTF8Shaded(TTF_Font *font, const char *text, SDL_Color *fg, SDL_Color *bg);
SDL_Surface * renderGlyphShaded(TTF_Font *font, Uint16 ch, SDL_Color *fg, SDL_Color *bg);

SDL_Surface * renderTextBlended(TTF_Font *font, const char *text, SDL_Color *fg);
SDL_Surface * renderUTF8Blended(TTF_Font *font, const char *text, SDL_Color *fg);
SDL_Surface * renderGlyphBlended(TTF_Font *font, Uint16 ch, SDL_Color *fg);


