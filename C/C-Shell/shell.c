#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/sysinfo.h>
#include <sys/resource.h>

// Based off of project from: https://gist.github.com/parse/966049
// Name: Ethan Krug
// Assignment: Lab 1

/* The array below will hold the arguments: args[0] is the command. */
static char *args[512];
char *prompt = "cwushell> ";

/* Final cleanup, 'wait' for processes to terminate.
 *  n : Number of times 'command' was invoked.
 */
static void cleanup(int n)
{
    int i;
    for (i = 0; i < n; ++i)
        wait(NULL);
}

static char line[1024];
char *last_cmd;
static int n = 0; /* number of calls to 'command' */

int main()
{
    while (1)
    {
        /* Print the command prompt */
        printf("%s", prompt);
        fflush(NULL);

        /* Read a command line */
        if (!fgets(line, 1024, stdin))
            return 0;

        char *cmd = line;
        char *next = strchr(cmd, '|'); /* Find first '|' */

        while (next != NULL)
        {
            /* 'next' points to '|' */
            *next = '\0';
            run(cmd);

            cmd = next + 1;
            next = strchr(cmd, '|'); /* Find next '|' */
        }
        run(cmd);
        cleanup(n);
        n = 0;
        last_cmd = cmd;
    }
    return 0;
}

// made for ignoring whitespace
static char *skipwhite(char *s)
{
    while (isspace(*s))
        ++s;
    return s;
}

// Splits the command, options, etc. apart
static int split(char *cmd)
{
    cmd = skipwhite(cmd);
    char *next = strchr(cmd, ' ');
    int i = 0;

    while (next != NULL)
    {
        next[0] = '\0';
        args[i] = cmd;
        ++i;
        cmd = skipwhite(next + 1);
        next = strchr(cmd, ' ');
    }

    if (cmd[0] != '\0')
    {
        args[i] = cmd;
        next = strchr(cmd, '\n');
        next[0] = '\0';
        ++i;
    }

    args[i] = NULL;
    return i;
}

// Tokenizes the current command and its options, and runs the command
void run(char *cmd)
{
    int num_cmds = split(cmd);
    if (args[0] != NULL)
    {
        if (strcmp(args[0], "exit") == 0) // handling for the exit command
        {
            if (num_cmds == 2)
            {
                if (strcmp(args[1], "-h") == 0 || strcmp(args[1], "--help") == 0)
                {
                    FILE *manual = fopen("./manual.txt", "rb");
                    char s[100];
                    while (fgets(s, 100, manual) != NULL)
                    {
                        if (strstr(s, "exit"))
                        {
                            printf(s);
                            break;
                        }
                    }
                    fclose(manual);
                }
                else
                {
                    int exit_code;
                    sscanf(args[1], "%d", &exit_code);
                    exit(exit_code);
                }
            }
            else if (num_cmds == 1)
            {
                exit(0);
            }
            else
            {
                exit((int)last_cmd);
            }
        }
        else if (strcmp(args[0], "prompt") == 0) // handling for prompt command
        {
            if (num_cmds > 1)
            {
                if (strcmp(args[1], "-h") == 0 || strcmp(args[1], "--help") == 0)
                {
                    FILE *manual = fopen("./manual.txt", "rb");
                    char s[100];
                    while (fgets(s, 100, manual) != NULL)
                    {
                        if (strstr(s, "prompt"))
                        {
                            printf(s);
                            break;
                        }
                    }
                    fclose(manual);
                }
                else
                {
                    char *new_prompt = malloc(strlen(args[1]) + 1);
                    new_prompt = strcat(args[1], " ");
                    prompt = new_prompt;
                }
            }
            else
            {
                prompt = "cwushell> ";
            }
        }
        else if (strcmp(args[0], "cpuinfo") == 0) // handling for cpuinfo command
        {
            if (num_cmds > 1)
            {
                if (strcmp(args[1], "-h") == 0 || strcmp(args[1], "--help") == 0)
                {
                    FILE *manual = fopen("./manual.txt", "rb");
                    char s[250];
                    while (fgets(s, 250, manual) != NULL)
                    {
                        if (strstr(s, "cpuinfo"))
                        {
                            printf(s);
                            printf("\n");
                        }
                    }
                    fclose(manual);
                }
                else
                {
                    FILE *cpuinfo = popen("lscpu", "r");
                    char s[100];
                    while (fgets(s, 100, cpuinfo) != NULL)
                    {
                        if ((strstr(s, "CPU(s):") && strcmp(args[1], "-n") == 0) ||
                            (strstr(s, "Model name:") && strcmp(args[1], "-t") == 0) ||
                            (strstr(s, "CPU MHz:") && strcmp(args[1], "-c") == 0))
                        {
                            printf(s);
                            break;
                        }
                    }
                    fclose(cpuinfo);
                }
            }
        }
        else if (strcmp(args[0], "meminfo") == 0) // handling for the meminfo command
        {
            if (num_cmds > 1)
            {
                if (strcmp(args[1], "-h") == 0 || strcmp(args[1], "--help") == 0)
                {
                    FILE *manual = fopen("./manual.txt", "rb");
                    char s[200];
                    while (fgets(s, 200, manual) != NULL)
                    {
                        if (strstr(s, "meminfo"))
                        {
                            printf(s);
                            printf("\n");
                        }
                    }
                    fclose(manual);
                }
                else if (strcmp(args[1], "-t") == 0)
                {
                    FILE *stats = fopen("/proc/meminfo", "rb");
                    char s[100];
                    while (fgets(s, 100, stats) != NULL)
                    {
                        if ((strstr(s, "MemTotal:") && strcmp(args[1], "-t") == 0))
                        {
                            puts(s);
                            break;
                        }
                    }
                    fclose(stats);
                }
                else if (strcmp(args[1], "-c") == 0)
                {
                    FILE *cpuinfo = popen("lscpu", "r");
                    char s[100];
                    while (fgets(s, 100, cpuinfo) != NULL)
                    {
                        if (strstr(s, "L2 cache:") && strcmp(args[1], "-c") == 0)
                        {
                            printf(s);
                            break;
                        }
                    }
                    fclose(cpuinfo);
                }
            }
        }
        else if (strcmp(args[0], "manual") == 0) // handling for the manual command
        {
            if (num_cmds > 1)
            {
                if (strcmp(args[1], "exit") == 0)
                {
                    FILE *manual = fopen("./manual.txt", "rb");
                    char s[100];
                    while (fgets(s, 100, manual) != NULL)
                    {
                        if (strstr(s, "exit"))
                        {
                            printf(s);
                            break;
                        }
                    }
                    fclose(manual);
                }
                else if (strcmp(args[1], "prompt") == 0)
                {
                    FILE *manual = fopen("./manual.txt", "rb");
                    char s[100];
                    while (fgets(s, 100, manual) != NULL)
                    {
                        if (strstr(s, "prompt"))
                        {
                            printf(s);
                            break;
                        }
                    }
                    fclose(manual);
                }
                else if (strcmp(args[1], "cpuinfo") == 0)
                {
                    FILE *manual = fopen("./manual.txt", "rb");
                    char s[250];
                    while (fgets(s, 250, manual) != NULL)
                    {
                        if (strstr(s, "cpuinfo"))
                        {
                            printf(s);
                            printf("\n");
                        }
                    }
                    fclose(manual);
                }
                else if (strcmp(args[1], "meminfo") == 0)
                {
                    FILE *manual = fopen("./manual.txt", "rb");
                    char s[200];
                    while (fgets(s, 200, manual) != NULL)
                    {
                        if (strstr(s, "meminfo"))
                        {
                            printf(s);
                            printf("\n");
                        }
                    }
                    fclose(manual);
                }
            }
            else
            {
                printf("Try something like: manual cpuinfo\n");
            }
        }
        else // non-custom commands
        {
            char new_cmd[1024] = {0};
            for (int i = 0; i < num_cmds; i++)
            {
                strcat(new_cmd, args[i]);
                strcat(new_cmd, " ");
            }
            system(new_cmd);
        }
        n += 1;
    }
}