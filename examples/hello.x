
enum hello_stat {
  HELLO_OK = 0,
  HELLO_ERROR = 1
};

union hello_res switch(hello_stat stat) {
  case HELLO_OK:
    string msg<>;
  case HELLO_ERROR:
    void;    
};

struct hello_goodbye_arg {
  int n;
  string msg<>;
};

typedef string hello_msg<>;

union hello_goodbye_res switch(hello_stat stat) {
  case HELLO_OK:
    hello_msg msgs<>;
  case HELLO_ERROR:
    void;
};

program TESTPROG {
  version HELLOV1 {
    void HELLO_NULL(void) = 0;
    hello_res HELLO_UPCASE(string<>) = 1;    
    hello_goodbye_res HELLO_GOODBYE(hello_goodbye_arg) = 2;
  } = 1;
} = 1;

