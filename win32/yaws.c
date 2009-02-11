#include <windows.h>
#include <stdio.h>

#define BSIZ 16000

static unsigned char path[BSIZ];
static unsigned char *fpath;

static void readreg() {
    HKEY hKey;
    unsigned int bsz = BSIZ;
    // Check where yaws is installed, we need to -pa that path
    if (RegOpenKeyEx(HKEY_LOCAL_MACHINE,
                     "SOFTWARE\\Hyber\\Yaws",
                     0, KEY_READ, &hKey) != ERROR_SUCCESS)
        {
            printf ("Cannot find yaws installation in the registry\n");
            exit(1);
        }
    memset(path, 0, BSIZ);
    int ret = RegQueryValueEx(hKey, "DIR", NULL, NULL,
                              path, &bsz);
    fpath = (unsigned char*)malloc(strlen(path));
    int i;
    for(i = 0; i<strlen(path); i++ ) {
        if (path[i] == '\\') {
            fpath[i] = '/';
        }
        else 
            fpath[i] = path[i];
    }
    fpath[i] = 0;
    
}


int charg(char **argv, char *s1, char *s2, int p) {
    if (strcmp(s1, argv[p]) == 0) return 1;
    if ((s2 != NULL) && strcmp(s2, argv[p]) == 0) return 1;
    return 0;
}


int main(int argc, char**argv) {

    int interactive  = 0; 
    int winteractive  = 0; 
    char *conf = NULL;
    int debug = 0;
    char *runmod = NULL;

    readreg();

    int p = 1;
    while (p < argc) {
        if (charg(argv, "-i", "--interactive", p))  
            interactive = 1;
        else if (charg(argv, "-w", "--winteractive", p))
            winteractive = 1;
        else if (charg(argv, "--debug", NULL, p))
            debug = 1;
        else if (charg(argv, "-c", "--conf", p)) 
            conf = argv[++p];
        else if (charg(argv, "--runmod", NULL, p))
            runmod = argv[++p];
        else {
            fprintf(stderr, "Unknown arg: %s\n", argv[p]);
            exit(1);
        }
        p++;
    }
    printf("interactive = %d\n"
           "winteractive = %d\n"
           "conf = %s\n"
           "debug = %d\n"
           "runmod = %s\n", 
           interactive, winteractive,
           conf, debug, runmod);


    STARTUPINFO          si = { sizeof(si) };
    PROCESS_INFORMATION  pi;
    char                 szExe[] = "erl.exe -s test test";
    char                 execString[BSIZ];
    char                 paBuf[BSIZ];
    int i;
    int len;
    char yawsargs[BUFSIZ];

    if (winteractive)
        sprintf(execString, "werl.exe  ");
    else
        sprintf(execString, "erl.exe  ");

    sprintf(paBuf, " -pa \"%s/ebin\" ", fpath);
    strcat(execString, paBuf);

    if (debug)
        strcat(execString, " -boot start_sasl -yaws debug ");
    if (interactive == 0 && winteractive == 0)
        strcat(execString, " --detach ");

    if (conf == NULL) 
        sprintf(yawsargs, " -conf \"%s\\yaws.conf\" ", path);
    else 
        sprintf(yawsargs, " -conf \"%s\" ", conf);
    strcat(execString, yawsargs);
    strcat(execString, " -run yaws -yaws id default ");
    

    printf("execString = %s\n", execString);
    if(CreateProcess(0, execString, NULL, NULL, FALSE, 0, 0, 0, &si, &pi))
        {
            unsigned long ret = 0;
            
            // wait for process to finish
            WaitForSingleObject(pi.hProcess, INFINITE);
            if (GetExitCodeProcess(pi.hProcess, &ret) == 0)
                ret = 1;
            CloseHandle(pi.hProcess);
            CloseHandle(pi.hThread);
            return ret;
        }
    printf("Failed to create the process entirely\n");
    return 1;
}

               

