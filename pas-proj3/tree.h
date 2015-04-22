#ifndef TREE_C
#define TREE_C
#include "types.h" 
#include "symtab.h" 
#include "message.h" 

/* Used for recording the current function id */
/* Set the func_id_stack to block stack depth */
extern ST_ID func_id_stack[BS_DEPTH];
/* For recording where we are at in the stack */
extern int stack_counter;

/* Procedure and function prototype directives */
typedef enum { DIR_EXTERNAL, DIR_FORWARD } DIRECTIVE;

/* Possible expression types (tags) */
typedef enum {
    INTCONST, REALCONST, STRCONST, GID, LVAR, LFUN, NULLOP, UNOP, BINOP,
    FCALL, ERROR
} EXPR_TAG;

/* Possible nullary operators (tags) */
typedef enum {
    NULL_EOF_OP, NULL_EOLN_OP, NIL_OP
} EXPR_NULLOP;

/* Possible unary operators (tags) */
typedef enum {
    CONVERT_OP, DEREF_OP, NEG_OP, PLUS_OP, ORD_OP, CHR_OP, SUCC_OP, PRED_OP,
    NEW_OP, DISPOSE_OP, NOT_OP, INDIR_OP, ADDRESS_OP, SET_RETURN_OP
} EXPR_UNOP;

/* Possible binary operators (tags) */
typedef enum {
    ADD_OP, SUB_OP, MUL_OP, DIV_OP, MOD_OP, REALDIV_OP, EQ_OP, LESS_OP, LE_OP,
    NE_OP, GE_OP, GREATER_OP, SYMDIFF_OP, OR_OP, XOR_OP, AND_OP, BIN_SUCC_OP,
    BIN_PRED_OP, ASSIGN_OP
} EXPR_BINOP;

/* List for multiple variables */
typedef struct var_id {
    ST_ID id;
    struct var_id *next;
} VAR_ID, *VAR_ID_LIST;

/* Node to store identifier and return type for function_heading production in gram.y */
typedef struct func_heading {
    ST_ID	id;
    TYPE	ret_type;
} FUNC_HEADING;

/* Node to store identifier and expression node */
typedef struct {
    struct exprnode * expr;
    ST_ID id;
} EXPR_ID;

/* Used for lists of actual arguments to functions/procedures */
typedef struct exprlistnode {
    struct exprnode * expr;
    struct exprlistnode * next;
} EXPR_LIST_NODE, * EXPR_LIST;

/* The syntax tree node for an expression
   (includes the type of the expression)
*/
typedef struct exprnode {
    EXPR_TAG tag;
    TYPE type;
    union {
		long intval;
		double realval;
		char * strval;
		ST_ID gid;	    	/* For global variables and global functions */
		struct {            /* For local variables and formal parameters */
		    BOOLEAN is_ref; /* TRUE if expr is a VAR (reference) parameter */
		    int offset;     /* storage location relative to frame pointer */
		    int link_count; /* Number of ref links to follow to find the var */
		} lvar;
		struct {            /* For local functions */
		    char * global_name; /* The assembler entry point label */
		    int link_count; /* Number of ref links to follow to find the fcn */
		} lfun;
		struct {            /* For nullary operators */
		    EXPR_NULLOP op;
		} nullop;
		struct {            /* For unary operators */
		    EXPR_UNOP op;
		    struct exprnode * operand;
		} unop;
		struct {            /* For binary operators */
		    EXPR_BINOP op;
		    struct exprnode * left, * right;
		} binop;
		struct {            /* For procedure and function calls */
		    struct exprnode * function;
		    EXPR_LIST args;
		} fcall;
    } u;
} EXPR_NODE, *EXPR;
   
TYPE build_unresolved_pointer(TYPE ret_type, TYPE object);

void create_typename(ST_ID id,TYPE new_type);

void create_gdecl(VAR_ID_LIST list,TYPE type);

TYPE check_typename( ST_ID id );

//TYPE check_subrange( long a, long b);

TYPE check_array(TYPE array, INDEX_LIST i);

TYPE check_function_type(TYPE t);

PARAM_LIST check_param(PARAM_LIST p);

VAR_ID_LIST build_var_id_list (VAR_ID_LIST list,ST_ID id);

PARAM_LIST build_param_list(VAR_ID_LIST id_list,TYPE type,BOOLEAN value);

PARAM_LIST concatenate_param_list (PARAM_LIST list1,PARAM_LIST list2);

INDEX_LIST concatenate_index_lists (INDEX_LIST list1,TYPE type);

INDEX_LIST create_list_from_type(TYPE type);

void resolve_all_ptr();

/* Definitions for part 2 */
void enter_main_body();

void exit_main_body();

void install_func_head(ST_ID id, TYPE type, DIRECTIVE dir);

int prepare_to_enter_func_body(ST_ID id, TYPE ret_type);

void enter_func_body(ST_ID id, TYPE ret_type, int loc_var_offset);

void exit_func_body(ST_ID id, TYPE ret_type);

void install_params(PARAM_LIST list);

EXPR_LIST expr_list_reverse(EXPR_LIST list);

EXPR_LIST expr_prepend(EXPR expr, EXPR_LIST list);

void expr_free(EXPR expr);

void expr_list_free(EXPR_LIST list);

EXPR make_intconst_expr(long val, TYPE type);

EXPR make_realconst_expr(double val);

EXPR make_strconst_expr(char * str);

EXPR make_id_expr(ST_ID id);

EXPR make_null_expr(EXPR_NULLOP op);

EXPR make_un_expr(EXPR_UNOP op, EXPR sub);

EXPR make_bin_expr(EXPR_BINOP op, EXPR left, EXPR right);

EXPR make_fcall_expr(EXPR func, EXPR_LIST args);

EXPR make_error_expr();

EXPR check_func_or_proc_or_assign(EXPR lhs, ST_ID id, EXPR rhs);

BOOLEAN is_lval(EXPR expr);

TYPE check_subrange(EXPR lo, EXPR hi);
#endif

