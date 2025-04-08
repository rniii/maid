# include <ctype.h>
#   include <fcntl.h>
#     include <getopt.h>
#       include <stdbool.h>
#         include <stdio.h>
#           include <stdlib.h>
#             include <unistd.h>
#               include <sys/mman.h>
//              </sys/mman.h>
//            </unistd.h>
//          </stdlib.h>
//        </stdio.h>
//      </stdbool.h>
//    </getopt.h>
//  </fcntl.h>
//</ctype.h>

typedef struct { size_t len; char *ptr; } string;

string nil = { 0, 0 };

bool dryRun = false;
bool quiet  = false;

#define CSI     "\033["
#define SGR(c)  CSI c "m"

char *p = SGR("95;1"); // primary
char *s = SGR("0;1");  // secondary
char *e = SGR("31;1"); // error
char *r = SGR("");     // reset

bool isnil(char *str) { return str == 0 || *str == 0; }

string readFile(char *path) {
  int prot  = PROT_READ;
  int flags = MAP_PRIVATE;

  int    fd  = open(path, O_RDONLY);    if (fd  == -1)         return nil;
  size_t len = lseek(fd, 0, SEEK_END);  if (len == (size_t)-1) return nil;
  void  *ptr = mmap(0, len, prot, flags, fd, 0);

  close(fd);

  if (ptr == MAP_FAILED) perror("mmap"), exit(1);

  return (string){ len, ptr };
}

bool iseol(char c) { return c == 0 || c == '\n'; }

bool cmpmagic(char *src) {
  char *magic = "<!-- maid-tasks -->";

  while (*src && (*src == *magic)) {
    src++;
    if (*magic != ' ' || *src == ' ') magic++;
  }

  return *src == *magic;
}

bool parseTaskfile(char *file) {
  string str = readFile(file);
  char  *end = str.ptr + str.len;
  char  *eol = str.ptr;
  char  *src = str.ptr;

  int heading = 0;
  int code    = 0;
  int state   = 0;
  int line    = 0;

  for (; src < end; src = ++eol) {
    bool  has_text    = false;
    bool  start       = true;
    bool  is_magic    = true;
    int   cur_heading = 0;
    int   cur_code    = 0;
    char *magic       = "<!-- maid-tasks -->";

    line++;

    while (eol < end && *eol != '\n') {
      if (*eol != ' ' && *eol != '\t') {
        has_text = true;
      }
      if (start) {
        if (*eol == '#') cur_heading++;
        else if (*eol == '`' || *eol == '~') cur_code++;
        else start = false;
      }
      if (is_magic) {
        if (*eol != *magic) is_magic = false;
        else if (*magic != ' ' || *eol == ' ') magic++;
      }

      eol++;
    }

    if (!has_text) continue;
    if (code && cur_code < code) continue;

    if (cur_heading) {
      printf("%2i: heading %i   | ", line, cur_heading);
      while (src < eol) putchar(*src++);
      puts("");
      continue;
    }

    if (cur_code) {
      printf("%2i: code %i < %i  | ", line,cur_code,code);
      while (src < eol) putchar(*src++);
      puts("");
      code = cur_code;
      continue;
    }

    if (is_magic) {
      printf("%2i: magic       | ",line);
      while (src < eol) putchar(*src++);
      puts("");
      continue;
    }
  }

  munmap(str.ptr, str.len);

  return true;
}

void maid(void) {
  parseTaskfile("README.md");
}
void list(void) {}
void help(void) {
  printf("%sUsage:%s maid [options] [task]\n\n", p, s);
  printf("%sOptions:%s\n", p, r);
  puts("  -h       --help           Display this message");
  puts("  -l       --list           List tasks concisely");
  puts("  -n       --dry-run        Don't run anything, just display commands");
  puts("  -q       --quiet          Don't display anything");
  puts("  -f FILE  --taskfile=FILE  Use tasks in FILE");
}

int main(int argc, char *argv[]) {
  if (!isnil(getenv("NO_COLOR")) || !isatty(1)) {
    p = s = e = r = "";
  }

  struct option longopts[] = {
    {"help",    no_argument,       0, 'h'},
    {"list",    no_argument,       0, 'l'},
    {"dry-run", no_argument,       0, 'n'},
    {"quiet",   no_argument,       0, 'q'},
    {"file",    required_argument, 0, 'f'},
    {0},
  };

  while (1) switch (getopt_long(argc, argv, "hlnqf:", longopts, 0)) {
    case 'h': help(); exit(0);
    case 'l': list(); exit(0);
    case 'n': dryRun = true; break;
    case 'q': quiet  = true; break;
    case '?': help(); exit(1);
    case -1:  maid(); exit(0);
  }
}
