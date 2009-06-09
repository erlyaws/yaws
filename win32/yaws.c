/* win32 version of yaws starup script */
/* Author: klacke@hyber.org (Jan 2009) */


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
    int ret = RegQueryValueEx(hKey, "DIR", NULL, NULL, path, &bsz);
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


int runwait(char *execString) {
    
    STARTUPINFO          si = { sizeof(si) };
    PROCESS_INFORMATION  pi;

    //printf("execString: %s \n", execString);

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
    printf("Tried to invoke: <%s> \n", execString);
    printf("Make sure you have <erl> in your environment PATH \n"); 
    return 1;
}



int nosh(char *s) {
    char buf[BUFSIZ];
    sprintf (buf, "erl -noshell -pa \"%s/ebin\" %s ", fpath, s);
    return runwait(buf);
}

int help() {
    printf("usage:  "
""
"       yaws -i | --interactive       -- interactive (no daemon) mode"
"       yaws -w | --winteractive      -- interactive (werl) "
"       yaws --daemon                 -- daemon mode"
""
""
""
"       Auxiliary flags for the daemon: "
"            --id Id          --  set system id"
"            --debug          --  debug mode "
"            --conf File      --  set config file"
"            --tracetraf      --  trace traffic"
"            --tracehttp      --  trace http traffic"
"            --traceout       --  trace output to stdout"
"            --version        --  print version"
"            --pa path        --  add load path"
"            --mnesiadir dir  --  start Mnesia in dir"
"            --sname xxx      --  start with sname xxx"
"            --name xxx       --  start with name xxx"
"            --runmod mod     --  call mod:start/0 at startup"
"            --erlarg X       --  pass argument X to $erl"
""
"ctl functions ... "
"        yaws --hup [--id ID]                 -- hup the daemon, reload conf"
"        yaws --stop [--id ID]                -- stop the daemon "
"        yaws --debug-dump [--id ID]          -- produce a debug dump "
"        yaws --status [--id ID]              -- query the daemon status "
"        yaws --load Modules                  -- load modules "
"        yaws --ls                            -- list Yaws nodes and their status"
"        yaws --ctltrace traffic|http         -- toggle trace of running daemon");
    
    return(0);
}



int main(int argc, char**argv) {

    int interactive  = 1; 
    int winteractive  = 0; 
    int daemon = 0;
    char *conf = NULL;
    char *sname = NULL;
    char *name = NULL;
    int debug = 0;
    char *runmod = NULL;
    char *trace = NULL;
    char *traceoutput = NULL;
    char tbuf[BUFSIZ];
    char paBuf[BUFSIZ];
    char execString[BSIZ];
    char mnesia[255];
    char erlarg[255];
    char *id = "default";

    memset(paBuf, 0, BUFSIZ);
    memset(tbuf, 0, BUFSIZ);
    memset(execString, 0, BSIZ);

    readreg();
    sprintf(paBuf, " -pa \"%s/ebin\" ", fpath);

    int p = 1;
    while (p < argc) {
        if (charg(argv, "-i", "--interactive", p))   {
            interactive = 1;
        }
        else if (charg(argv, "-w", "--winteractive", p)) {
            winteractive = 1;
        }
        else if (charg(argv, "-D", "--daemon", p)) {
            interactive = winteractive = 0;
            daemon = 1;
        }
        else if (charg(argv, "--debug", NULL, p)) {
            debug = 1;
        }
        else if (charg(argv, "-t", "--tracetraf", p)) {
             trace=" -yaws trace traffic ";
        }
        else if (charg(argv, "-T", "--tracehttp", p)) {
             trace=" -yaws trace http ";
        }
        else if (charg(argv, "-x", "--traceout", p)) {
            traceoutput = " -yaws traceoutput ";
        }
        else if (charg(argv, "--trace", NULL, p)) {
            traceoutput = " -yaws traceoutput ";
            trace = " -yaws trace traffic ";
        }
        else if (charg(argv, "--mnesiadir", "-M", p)) {
            sprintf(mnesia, "-mnesia dir \"%s\" -run mnesia start ",argv[++p]);
        }
        else if (charg(argv, "-c", "--conf", p))  {
            conf = argv[++p];
        }
        else if (charg(argv, "--id", NULL, p))  {
            id = argv[++p];
        }
        else if (charg(argv, "-pa", NULL, p))  {
            sprintf(tbuf, " -pa \"%s\" ", argv[++p]);
            strcat(paBuf, tbuf);
        }
        else if (charg(argv, "--runmod", "-r", p)) {
            runmod = argv[++p];
        }
        else if (charg(argv, "--hup", "-h", p)) {
            return nosh("-s yaws_ctl hup default");
        }
        else if (charg(argv, "--stop", "-s", p)) {
            return nosh("-s yaws_ctl stop default");
        }
        else if (charg(argv, "--status", "-S", p)) {
            return nosh("-s yaws_ctl status default");
        }
        else if (charg(argv, "--debug-dump", NULL, p)) {
            return nosh("-s yaws_ctl debug_dump default ");
        }
        else   if (charg(argv, "--version", "-v", p)) {  
            nosh("-s yaws printversion");
            exit(0);
        }
        else   if (charg(argv, "--sname", "-sname", p)) {  
             sname = argv[++p];
        }
        else   if (charg(argv, "--name", "-name", p)) {  
             name = argv[++p];
        }
        else   if (charg(argv, "--erlarg", "-erlarg", p)) {  
            sprintf(tbuf, " %s ", argv[++p]);
            strcat(erlarg, tbuf);
        }
        else {
            return help();
        }
        p++;
    }

    if (winteractive)
        sprintf(execString, "werl.exe  %s %s ", erlarg, paBuf);
    else
        sprintf(execString, "erl.exe  %s %s ", erlarg, paBuf);

    if (debug)
        strcat(execString, " -boot start_sasl -yaws debug ");
    if ((interactive == 0 && winteractive == 0) || daemon == 1)
        strcat(execString, " -detached ");
    
    if (conf == NULL) 
        sprintf(tbuf, " -conf \"%s\\yaws.conf\" ", path);
    else 
        sprintf(tbuf, " -conf \"%s\" ", conf);
    strcat(execString, tbuf);
    strcat(execString, " -run yaws -yaws id default ");

    if (mnesia != NULL) 
        strcat(execString, mnesia);
    if (trace != NULL) 
        strcat(execString, trace);
    if (traceoutput != NULL)
        strcat(execString, traceoutput);

    if (sname != NULL) {
        sprintf(tbuf," -sname %s ", sname);
        strcat(execString, tbuf);
    } else if (name != NULL) {
        sprintf(tbuf, " -name %s ", name);
        strcat(execString, tbuf);
    } 
    if (runmod != NULL) {
        sprintf(tbuf, " -runmod %s ", runmod);
        strcat(execString, tbuf);
    }

    return runwait(execString);
}

               

