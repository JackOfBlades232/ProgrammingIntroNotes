/* 4_24.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum { init_line_capacity = 32 };

typedef struct tag_string {
    char *content;
    int len;
    int capacity;
} string;

string resize_string(string str)
{
    str.capacity *= 2;
    str.content = realloc(str.content, str.capacity);

    return str;
}

typedef struct tag_file_info {
    const char *filename;
    int max_line_len;
    char *max_line;
} file_info;

file_info create_empty_file_info(const char *filename)
{
    file_info info;

    info.filename = filename;
    info.max_line_len = 0;
    info.max_line = NULL;

    return info;
}

void update_max_line(string *current_line, file_info *info)
{
    current_line->content[current_line->len] = 0;

    if (current_line->len > info->max_line_len) {
        info->max_line = 
            realloc(info->max_line, current_line->len + 1);
        strcpy(info->max_line, current_line->content);

        info->max_line_len = current_line->len;
    }

    current_line->len = 0;
}

int construct_file_info(file_info *info, const char *filename) 
{
    FILE *fp;
    string current_line;
    int c;

    fp = fopen(filename, "r");
    if (!fp) 
        return 0;

    *info = create_empty_file_info(filename);
    current_line.len = 0;
    current_line.capacity = init_line_capacity;
    current_line.content = malloc(current_line.capacity * sizeof(char));

    /* TODO : refac this */
    while ((c = fgetc(fp)) != EOF) {
        if (current_line.len >= current_line.capacity - 1)
            current_line = resize_string(current_line);

        if (c != '\n') {
            current_line.content[current_line.len] = c;
            (current_line.len)++;
        } else 
            update_max_line(&current_line, info);
    }

    if (current_line.len > 0)
        update_max_line(&current_line, info);

    free(current_line.content);
    return 1;
}

int main (int argc, char **argv)
{
    file_info *file_infos;
    int i, longest_line_len, longest_line_file_idx;

    if (argc < 2) {
        fprintf(stderr, "Provide at least 1 file name arg\n");
        return 1;
    }

    file_infos = malloc((argc-1) * sizeof(file_info));
    longest_line_len = 0;
    longest_line_file_idx = 0;

    for (i = 0; i < argc-1; i++) {
        int res;

        res = construct_file_info(file_infos + i, argv[i+1]);
        if (!res) {
            perror(argv[i+1]);
            return 2;
        }

        if (file_infos[i].max_line_len > longest_line_len) {
            longest_line_len = file_infos[i].max_line_len;
            longest_line_file_idx = i;
        }
    }

    for (i = 0; i < argc-1; i++) {
        if (i == longest_line_file_idx)
            putchar('*');

        printf("%s: %s\n", argv[i+1], file_infos[i].max_line);
        free(file_infos[i].max_line);
    }

    free(file_infos);
    
    return 0;
}
