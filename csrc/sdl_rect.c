#include "SDL_stdinc.h"
#include "SDL_rect.h"

SDL_bool SDL_RectEmpty_Wrapper(const SDL_Rect *r)
{
    return SDL_RectEmpty(r);
}

SDL_bool SDL_RectEquals_Wrapper(const SDL_Rect *a, const SDL_Rect *b)
{
    return SDL_RectEquals(a, b);
}

