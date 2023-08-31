/* 10_23/fifteen_history.cpp */
#include "fifteen.h"
#include "utils.h"

FifteenHistory::FifteenHistory(FifteenBoard *a_master, const char *sv_filename)
    : master(a_master), save_f(0), turn_stack(0) 
{
    if (sv_filename) {
        save_f = fopen(sv_filename, "r");
        if (!save_f) {
            fprintf(stderr, 
                    "Couldn't open save file, reverting to generation\n");
            goto generate;
        }

        int read_res = ReadInitStateFromSave() && ReadHistoryFromSave();
        if (!read_res) {
            fprintf(stderr, "Save file invalid, reverting to generation\n");
            goto generate;
        }

        return;
    }

    // else
generate:
    GenerateNewInitState();
}

FifteenHistory::~FifteenHistory()
{
    WriteToSave();
    ResetHistory();
}

void FifteenHistory::ResetHistory()
{
    while (turn_stack) {
        turn_t *tmp = turn_stack;
        turn_stack = turn_stack->prev;
        delete tmp;
    }

    turn_stack = 0;
}

void FifteenHistory::PushTurn(int dest_x, int dest_y)
{
    turn_t *turn = new turn_t;
    turn->dest_x = dest_x;
    turn->dest_y = dest_y;
    turn->prev = turn_stack;
    turn_stack = turn;
}

bool FifteenHistory::PopTurn(int *dest_x, int *dest_y)
{
    if (!turn_stack)
        return false;

    turn_t *tmp = turn_stack;
    if (dest_x)
        *dest_x = tmp->dest_x;
    if (dest_y)
        *dest_y = tmp->dest_y;
    turn_stack = turn_stack->prev;
    delete tmp;

    return true;
}

bool FifteenHistory::PeekTurn(int *dest_x, int *dest_y)
{
    if (!turn_stack)
        return false;

    *dest_x = turn_stack->dest_x;
    *dest_y = turn_stack->dest_y;
    return true;
}

void FifteenHistory::WriteToSave()
{
    if (save_f)
        save_f = freopen(0, "w", save_f);
    else
        save_f = fopen(default_save_path, "w");

    if (!save_f) {
        fprintf(stderr, 
                "Failed to write save, your latest progress was lost\n");
        return;
    }
    
    for (int y = 0; y < board_sz; y++) {
        for (int x = 0; x < board_sz; x++) {
            fprintf(save_f, x == board_sz-1 ? "%d" : "%d ",
                    init_board_state[y][x]);
        }
        fputc('\n', save_f);
    }

    if (turn_stack) {
        fputc('\n', save_f);
        WriteTurns(turn_stack);
    }

    fclose(save_f);
}

void FifteenHistory::WriteTurns(turn_t *head)
{
    if (!head)
        return;

    WriteTurns(head->prev);
    fprintf(save_f, "%d %d\n", head->dest_x, head->dest_y);
}

bool FifteenHistory::ReadInitStateFromSave()
{
    bool number_indicators[board_area] = { 0 };
    int perm[filled_board_area] = { 0 };

    for (int y = 0; y < board_sz; y++) {
        char buf[64];
        if (!fgets(buf, sizeof(buf), save_f))
            return false;

        char *p = buf;
        char *endptr = p;
        for (int x = 0; x < board_sz; x++) {
            while (*p == ' ' || *p == '\t')
                p++;

            int n = strtol(p, &endptr, 10);

            // Correct format checks
            if (n == 0 && endptr == p)
                return false;
            if (n < 0 || n > filled_board_area)
                return false;
            if ((x != board_sz-1 || y != board_sz-1) && n == 0)
                return false;
            if (x == board_sz-1 && y == board_sz-1 && n != 0)
                return false;

            init_board_state[y][x] = n;

            if (number_indicators[n])
                return false;
            number_indicators[n] = true;
            if (x != board_sz || y != board_sz)
                perm[xy_to_lin_idx(x, y)] = n;

            p = endptr;
        }

        while (*p == ' ' || *p == '\t')
            p++;
        if (*p != '\n')
            return false;
    }

    // Init pos must be valid
    if (
            permutation_is_id(perm, filled_board_area) ||
            permutation_is_odd(perm, filled_board_area)
       )
    {
        return false;
    }

    return true;
}

bool FifteenHistory::ReadHistoryFromSave()
{
    int c;
    while ((c = fgetc(save_f)) != EOF) {
        if (c != ' ' && c != '\t' && c != '\r' && c != '\n')
            break;
    }
    if (c == EOF)
        return true;
    ungetc(c, save_f);

    for (;;) {
        char buf[64];
        if (!fgets(buf, sizeof(buf), save_f))
            break;

        int x, y;
        char *p = buf;
        char *endptr = p;

        while (*p == ' ' || *p == '\t')
            p++;
        x = strtol(p, &endptr, 10);
        if (x == 0 && endptr == p)
            return false;
        if (x < 0 || x >= board_sz)
            return false;
        p = endptr;

        while (*p == ' ' || *p == '\t')
            p++;
        y = strtol(p, &endptr, 10);
        if (y == 0 && endptr == p)
            return false;
        if (y < 0 || y >= board_sz)
            return false;
        p = endptr;

        PushTurn(x, y);

        while (*p == ' ' || *p == '\t')
            p++;
        if (*p != '\n')
            return false;
    }

    return true;
}

void FifteenHistory::GenerateNewInitState()
{
    int perm[filled_board_area];
    generate_even_permutation(perm, filled_board_area);

    for (int y = 0; y < board_sz; y++)
        for (int x = 0; x < board_sz; x++) {
            int i = xy_to_lin_idx(x, y); 
            init_board_state[y][x] = i < filled_board_area ? perm[i]+1 : 0;
        }
}
