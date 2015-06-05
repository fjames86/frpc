
typedef string frpc_des_name<>;

struct frpc_des_entry {
  frpc_des_name name;
  unsigned hyper timestamp;
  opaque key<>;
};

struct *frpc_des_entry_list {
  frpc_des_entry entry;
  frpc_des_entry_list *next;
};

program FRPC_DES_PROG {
  version FRPC_DES_V1 {

    void FRPC_DES_NULL( void ) = 0;
    
    frpc_des_entry FRPC_DES_GET( frpc_des_name ) = 1;
 
    void FRPC_DES_SET( frpc_des_entry ) =  2;

    frpc_des_entry_list FRPC_DES_LIST( void ) = 3;

  } = 1;
} = 814857052;

