/* 5_3/2_54_remake.c */
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <limits.h>

void update_min(int *min, int val)
{
    if (*min > val)
        *min = val;
}

void update_max(int *max, int val)
{
    if (*max < val)
        *max = val;
}

int main(int argc, char **argv)
{
    int status = 0;

    int num_fd;
    FILE *out_f;
    char **next_bin_fnm;

    if (argc < 3) {
        fprintf(stderr, "Specify at least 1 binary file and the output file\n");
        return 1;
    }

    out_f = fopen(argv[argc-1], "w");
    if (!out_f) {
        perror(argv[argc-1]);
        return 2;
    }

    for (next_bin_fnm = argv+1; next_bin_fnm-argv < argc-1; next_bin_fnm++) {
        int num, read_res;
        int min = INT_MAX, max = INT_MIN, cnt = 0;

        num_fd = open(*next_bin_fnm, O_RDONLY);
        if (num_fd == -1) {
            status = 3;
            goto cleanup;
        }

        while ((read_res = read(num_fd, &num, sizeof(num))) == sizeof(num)) {
            update_min(&min, num);            
            update_max(&max, num);            
            cnt++;
        }

        fprintf(out_f, "%s: %d %d %d\n", *next_bin_fnm, cnt, min, max);
        close(num_fd);
    }

    cleanup:
        fclose(out_f); 
        return status;
}
