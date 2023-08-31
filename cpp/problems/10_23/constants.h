/* 10_23/constants.h */
#ifndef CONSTANTS_SENTRY
#define CONSTANTS_SENTRY

enum {
    btn_sz = 150,
    spacing = 15,
    padding = 75,
    font_size = 50,

    space_before_btns = 45,
    btn_w = 150,
    btn_h = 90,
    btn_spacing = 150,

    board_sz = 4,
    board_area = board_sz*board_sz,
    filled_board_area = board_area-1
};

extern const char default_save_path[];

static inline int idx_to_coord(int idx)
{ 
    return padding + idx*btn_sz + idx*spacing; 
}

static inline int xy_to_lin_idx(int x, int y)
{ 
    return board_sz*y + x; 
}

#endif
