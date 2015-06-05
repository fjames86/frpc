
typedef string frpc_des_name<>;

typedef opaque frpc_des_keybuf<>;

struct frpc_des_entry {
  frpc_des_name name;
  frpc_des_keybuf public;
};

struct *frpc_des_entry_list {
  frpc_des_entry entry;
  frpc_des_entry_list *next;
};

program FRPC_DES_PROG {
  version FRPC_DES_V1 {

    void FRPC_DES_NULL( void ) = 0;
    
    frpc_des_keybuf FRPC_DES_GET( frpc_des_name ) = 1;
 
    void FRPC_DES_SET( frpc_des_entry ) =  2;

    void FRPC_DES_UNSET( frpc_des_name ) = 3;

    frpc_des_entry_list FRPC_DES_LIST( void ) = 4;

  } = 1;
} = 814857052;

