

#include <SDL_ttf.h>

#include "Wrapper.h"

SDL_Surface * renderTextSolid(TTF_Font *font, const char *text, SDL_Color *fg)
{
  return TTF_RenderText_Solid(font,text,*fg);
}
SDL_Surface * renderUTF8Solid(TTF_Font *font, const char *text, SDL_Color *fg)
{
  return TTF_RenderUTF8_Solid(font,text,*fg);
}
SDL_Surface * renderGlyphSolid(TTF_Font *font, Uint16 ch, SDL_Color *fg)
{
  return TTF_RenderGlyph_Solid(font,ch,*fg);
}


SDL_Surface * renderTextShaded(TTF_Font *font, const char *text, SDL_Color *fg, SDL_Color *bg)
{
  return TTF_RenderText_Shaded(font,text,*fg,*bg);
}
SDL_Surface * renderUTF8Shaded(TTF_Font *font, const char *text, SDL_Color *fg, SDL_Color *bg)
{
  return TTF_RenderUTF8_Shaded(font,text,*fg,*bg);
}
SDL_Surface * renderGlyphShaded(TTF_Font *font, Uint16 ch, SDL_Color *fg, SDL_Color *bg)
{
  return TTF_RenderGlyph_Shaded(font,ch,*fg,*bg);
}


SDL_Surface * renderTextBlended(TTF_Font *font, const char *text, SDL_Color *fg)
{
  return TTF_RenderText_Blended(font,text,*fg);
}
SDL_Surface * renderUTF8Blended(TTF_Font *font, const char *text, SDL_Color *fg)
{
  return TTF_RenderUTF8_Blended(font,text,*fg);
}
SDL_Surface * renderGlyphBlended(TTF_Font *font, Uint16 ch, SDL_Color *fg)
{
  return TTF_RenderGlyph_Blended(font,ch,*fg);
}

