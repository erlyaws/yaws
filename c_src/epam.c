
#include <pam_appl.h>
#include <stdio.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>


#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))


#define put_int16(i, s) {((unsigned char*)(s))[0] = ((i) >> 8) & 0xff; \
                        ((unsigned char*)(s))[1] = (i)         & 0xff;}

#ifndef D
/*#define D(str) fprintf(stderr, (str)) */
#define D(str)
#endif

static int read_fill(int fd, unsigned char *buf, int len)
{
    int i, got = 0;

    do {
        if ((i = read(fd, buf+got, len-got)) <= 0) {
            if (i == 0) return got;
            if (errno != EINTR)
                return got;
            i = 0;
        }
        got += i;
    } while (got < len);
    return (len);
}


    
static int write_fill(int fd, char *buf, int len)
{
    int i, done = 0; 
    
    do {
        if ((i = write(fd, buf+done, len-done)) < 0) {
            if (errno != EINTR)
                return (i);
            i = 0;
        }
        done += i;
    } while (done < len);
    return (len);
}


#if 0
/*
 * These functions are for binary prompt manipulation.
 * The manner in which a binary prompt is processed is application
 * specific, so these function pointers are provided and can be
 * initialized by the application prior to the conversation function
 * being used.
 */

static void pam_misc_conv_delete_binary(void *appdata,
                                        pamc_bp_t *delete_me)
{
    PAM_BP_RENEW(delete_me, 0, 0);
}

int (*pam_binary_handler_fn)(void *appdata, pamc_bp_t *prompt_p) = NULL;
void (*pam_binary_handler_free)(void *appdata, pamc_bp_t *prompt_p)
      = pam_misc_conv_delete_binary;
#endif

/*
 * This conversation function is supposed to be a generic PAM one.
 * Unfortunately, it is _not_ completely compatible with the Solaris PAM
 * codebase.
 *
 * Namely, for msgm's that contain multiple prompts, this function
 * interprets "const struct pam_message **msgm" as equivalent to
 * "const struct pam_message *msgm[]". The Solaris module
 * implementation interprets the **msgm object as a pointer to a
 * pointer to an array of "struct pam_message" objects (that is, a
 * confusing amount of pointer indirection).
 */

int misc_conv(int num_msg, const struct pam_message **msgm,
              struct pam_response **response, void *appdata_ptr)
{
    int count=0;
    struct pam_response *reply;

    if (num_msg <= 0)
        return PAM_CONV_ERR;

    D(("allocating empty response structure array."));

    reply = (struct pam_response *) calloc(num_msg,
                                           sizeof(struct pam_response));
    if (reply == NULL) {
        D(("no memory for responses"));
        return PAM_CONV_ERR;
    }

    D(("entering conversation function."));

    for (count=0; count < num_msg; ++count) {
        char *string=NULL;

        switch (msgm[count]->msg_style) {
        case PAM_PROMPT_ECHO_OFF:
            string = (char*)appdata_ptr;
            break;
        case PAM_PROMPT_ECHO_ON:
            string = (char*)appdata_ptr;
            break;
        case PAM_ERROR_MSG:
            if (fprintf(stderr,"%s\n",msgm[count]->msg) < 0) {
                goto failed_conversation;
            }
            break;
        case PAM_TEXT_INFO:
            if (fprintf(stdout,"%s\n",msgm[count]->msg) < 0) {
                goto failed_conversation;
            }
            break;
#if 0
        case PAM_BINARY_PROMPT:
        {
            pamc_bp_t binary_prompt = NULL;

            if (!msgm[count]->msg || !pam_binary_handler_fn) {
                goto failed_conversation;
            }

            PAM_BP_RENEW(&binary_prompt,
                         PAM_BP_RCONTROL(msgm[count]->msg),
                         PAM_BP_LENGTH(msgm[count]->msg));
            PAM_BP_FILL(binary_prompt, 0, PAM_BP_LENGTH(msgm[count]->msg),
                        PAM_BP_RDATA(msgm[count]->msg));

            if (pam_binary_handler_fn(appdata_ptr,
                                      &binary_prompt) != PAM_SUCCESS
                || (binary_prompt == NULL)) {
                goto failed_conversation;
            }
            string = (char *) binary_prompt;
            binary_prompt = NULL;

            break;
        }
#endif
        default:
            fprintf(stderr, "erroneous conversation (%d)\n"
                    ,msgm[count]->msg_style);
            goto failed_conversation;
        }

        if (string) {                         /* must add to reply array */
            /* add string to list of responses */

            reply[count].resp_retcode = 0;
            reply[count].resp = string;
            string = NULL;
        }
    }

    *response = reply;
    reply = NULL;

    return PAM_SUCCESS;

failed_conversation:

    D(("the conversation failed"));

    if (reply) {
        for (count=0; count<num_msg; ++count) {
            if (reply[count].resp == NULL) {
                continue;
            }
            switch (msgm[count]->msg_style) {
            case PAM_PROMPT_ECHO_ON:
            case PAM_PROMPT_ECHO_OFF:
#if 0
                _pam_overwrite(reply[count].resp);
#endif
                free(reply[count].resp);
                break;
#if 0
            case PAM_BINARY_PROMPT:
                pam_binary_handler_free(appdata_ptr,
                                        (pamc_bp_t *) &reply[count].resp);
                break;
#endif
            case PAM_ERROR_MSG:
            case PAM_TEXT_INFO:
                /* should not actually be able to get here... */
                free(reply[count].resp);
            }                                            
            reply[count].resp = NULL;
        }
        /* forget reply too */
        free(reply);
        reply = NULL;
    }

    return PAM_CONV_ERR;
}






static struct pam_conv conv = {
    misc_conv,
    NULL
};

static void werr(pam_handle_t *pamh, int sid, int ecode, char *phase)
{
    char buf[BUFSIZ];
    int len;

    sprintf(&buf[2], "pam %d no %s %s",
            sid, phase, pam_strerror(pamh, ecode));
    len = strlen(&buf[2]);
    put_int16(len, &buf[0]);
    if (write_fill(1, buf, len+2) != len+2)
        exit(1);
}


static void wok(int sid)
{
    char buf[BUFSIZ];
    int len;

    sprintf(&buf[2], "pam %d yes", sid);
    len = strlen(&buf[2]);
    put_int16(len, &buf[0]);
    if (write_fill(1, buf, len+2) != len+2)
        exit(1);
}

static void wstart()
{
    char buf[5];


    sprintf(&buf[2], "ok");
    put_int16(2, &buf[0]);
    if (write_fill(1, buf, 4) != 4) {
        exit(1);
    }
}


struct session {
    pam_handle_t *pamh;
    int sid;
    int session_mode;
    struct session *next;
};

static struct session *sessions = NULL;

static struct session *del_session(struct session **sp, int sid)
{

    struct session *tmp;

    if (*sp == NULL) return NULL;
    if ((*sp)->sid == sid) {
        tmp = *sp;
        *sp = tmp->next;
        return tmp;
    }
    tmp = (*sp)->next;
    while (tmp != NULL) {
        if (tmp->sid == sid) {
            (*sp)->next = tmp->next;
            return tmp;
        }
        sp = &((*sp)->next);
        tmp = tmp->next;
    }
    return NULL;
}

        
static void do_auth(char *service, char*user, char*pwd, char* mode, int sid)
{
    pam_handle_t *pamh=NULL;
    int retval;
    struct session *sessp;

    conv.appdata_ptr = (void*)strdup(pwd);
    retval = pam_start(service, user, &conv, &pamh);
    
    if (retval != PAM_SUCCESS) {
        werr(pamh, sid, retval, "start");
        return;
    }
    pam_set_item(pamh, PAM_RUSER, user);

    retval = pam_authenticate(pamh, 0); 
    if (retval != PAM_SUCCESS) {
        werr(pamh, sid, retval, "auth");
        return;
    }
    if (mode[0] == 'A') {
        retval = pam_acct_mgmt(pamh, 0); 
        if (retval != PAM_SUCCESS) {
            werr(pamh, sid, retval, "accounting");
            return;
        }
        /*fprintf(stderr, "did ok acct \n\r");*/
    }
    if (mode[1] == 'S') {
        retval = pam_open_session(pamh, 0);
        if (retval != PAM_SUCCESS) {
            werr(pamh, sid, retval, "session");
            return;
        }
        /*fprintf(stderr, "did ok open sess \n\r"); */
    }
    if ((sessp = malloc(sizeof(struct session))) == NULL) {
        werr(pamh, sid, -1, "malloc");
        return;
    }
    if (mode[1] == 'S') 
        sessp->session_mode = 1;
    else
        sessp->session_mode = 0;
    sessp->sid = sid;
    sessp->pamh = pamh;
    sessp->next = sessions;
    sessions = sessp;
    
    wok(sid);
}


int main(int argc, char *argv[])
{
    pam_handle_t *pamh=NULL;
    unsigned char lb[2];
    unsigned char buf[BUFSIZ];
    char *user;
    char *pwd;
    char *mode;
    int sid;
    int rval;
    struct session *sessp;

    // test clause
    if (argc == 4 ) {
        /* ./epam authmodule user passwd */
        printf("testing service=%s u=%s pwd=%s\n", argv[1],argv[2], argv[3]);
        do_auth(argv[1], argv[2], argv[3], "AS", 33);
        exit(0);
    }
    wstart();
    while (1) {
        if (read_fill(0, lb, 2) != 2)
            exit(1);
        rval = get_int16(lb);
        if (read_fill(0, buf, rval) != rval)
            exit(1);
        switch (buf[0]) {
        case 'a': 
            // auth a user
            pamh = NULL;
            user = (char *)&buf[1];
            pwd = user + strlen(user) + 1;
            mode= pwd + strlen(pwd) + 1;
            sid = atoi(mode + strlen(mode) + 1);
            
            do_auth(argv[1], user, pwd, mode, sid);
            break;
        case 'c': 
            // close session
            sid = atoi((char *)&buf[1]);
            if ((sessp = del_session(&sessions, sid)) == NULL) {
                fprintf(stderr, "Couldn't find session %d\r\n", sid); 
                break;
            }
            if (sessp->session_mode == 1) {
                pam_close_session(sessp->pamh, 0);
                /*fprintf(stderr, "did ok close sess \n\r");*/
            }
            pam_end(sessp->pamh, PAM_SUCCESS); 
            free(sessp);
            break;
        default:
            fprintf(stderr, "Bad op \n\r");
        }
    }
}


