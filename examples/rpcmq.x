
enum mqstat {
  MQ_OK,
  MQ_NOTFOUND = 1,
  MQ_ERROR = 2
};

union mq_res switch(mqstat stat) {
  case MQ_OK: unsigned int handle;
  default: void;
};

struct mqstat {
  unsigned int id;
  string name<>;
};

union stat_res switch(mqstat stat) {
  case MQ_OK: mqstat stat;
  default: void;
};

struct mqinfo {
  string name<>;
  unsigned int handle;
};

struct postinfo {
  unsigned int handle;
  opaque data<>;
};

typedef string mqopen_arg<>;

typedef mqdump_res mqinfo<>;

program MQPROG {
  version MQV1 {

    void
    mqnull( void ) = 0;
    
    mq_res 
    mqopen( mqopen_arg ) = 1;

    mq_res
    mqpost( postinfo ) = 2;

    stat_res 
    mqstat( unsigned int ) = 3;
    
    mqdump_res
    mqdump( void ) = 4;

  } = 1;
} = 0x2666385D;
