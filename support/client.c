extern char **environ;
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>


void usr1(int i) {
    exit(0);
}

void usr2(int i) {
    exit(1);
}

struct sigaction usr1sa,usr2sa;

char buf[4096];

int main(int argc, char *argv[])
{
    char *fdstring;
    int fd;
    char rv;
    int i;
    char **env;
    int bytes;
    int flags;

    fdstring=getenv("REDO_SERVER_SOCKFD");

    usr1sa.sa_handler=usr1;
    usr2sa.sa_handler=usr2;

    sigaction(SIGUSR1,&usr1sa,NULL);
    sigaction(SIGUSR2,&usr2sa,NULL);

    /*
    for(i=0;i<argc;++i)
    {
        fprintf(stderr,"%s ",argv[i]);
    }
    fprintf(stderr,"\n");

    for(env=environ; *env; ++env)
    {
        fprintf(stderr,"%s\n",*env);
    }

    fprintf(stderr, "REDO_CLIENT: %s\n",fdstring);

    */
    fd=atoi(fdstring);

    flags=fcntl(fd,F_GETFL);
    fcntl(fd,F_SETFL,flags&~O_NONBLOCK);

    for(i=0;i<argc;++i)
    {
        fprintf(stderr,"%s ",argv[i]);
        write(fd,argv[i],1+strlen(argv[i]));
    }

    fprintf(stderr,"\n");

    write(fd,"",1);

    sprintf(buf,"%d",getpid());
    write(fd,buf,1+strlen(buf));

    assert(getcwd(buf,sizeof(buf)));

    write(fd,buf,1+strlen(buf));

    for(env=environ; *env; ++env)
    {
        write(fd,*env,1+strlen(*env));
    }
    write(fd,"",1);

    for(;;)
        pause();
}

