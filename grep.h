#ifndef GREP_H
#define GREP_H

void commands(void);
unsigned int *address(void);
int advance(char *lp, char *ep);
int append(int (*f)(void), unsigned int *a);
void compile(int eof);
void error(char *s);
int execute(unsigned int *addr);
void exfile(void);
void filename(int comm);
char *getblock(unsigned int atl, int iof);
int getchr(void);
int getfile(void);
char *getline_blk(unsigned int tl);
int getnum(void);
void global(int k);
void init(void);
void newline(void);
void nonzero(void);
void print(void);
void putchr_(int ac);
void putd(void);
void putfile(void);
int putline(void);
void puts_(char *sp);
void quit(int n);
void setwide(void);
void squeeze(int);

#endif
