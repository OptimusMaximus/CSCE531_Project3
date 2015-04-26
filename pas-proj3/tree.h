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

extern char* exit_case_labels[100];
extern int exit_case_top;

/* Procedure and function prototype directives */
typedef enum { DIR_EXTERNAL, DIR_FORWARD } DIRECTIVE;

/* Possible expression types (tags) */
typedef enum {
    INTCONST, REALCONST, STRCONST, GID, LVAR, LFUN, NULLOP, UNOP, BINOP,
    FCALL, ERROR, ARRAY_ACCESS
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

/* Stores the type and the low and high values used in a case constant
   (assumed to be a subrange).  If the constant is a single value, then the
   high and low values are both set to this value. */
typedef struct val_node {
    long lo, hi;
    TYPETAG type;
    struct val_node * next;
} VAL_LIST_REC, *VAL_LIST;

/* Record for holding attributes of a case statement on the semantic stack.
   One such record is used per case statement.
   Fields:
       type: the type of the case expression
       label: the exit label for the case statement (a fresh label)
       values: the linked list of case constants (initially empty)
   This record is initialized and placed on the semantic stack in an
   intermediate action in the case_statement production, after LEX_OF but
   before any case_element's.  The list of case constants is built up
   inside each case_element.  Alternatively, one could use a global
   stack of CASE_RECORDs instead of using the semantic stack.  (A stack is
   required because case statements can be nested, and information for an
   inner case statement and its surrounding case statement should be kept
   separate.
*/
typedef struct {
	TYPETAG type;
	char * label;
	VAL_LIST values;
} CASE_REC;
   
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

/* Project 3 */

/* Returns the number of possible values of the given type, assumed to be
   an integral (ordinal) type smaller than long (usually a subrange).
   Also sets the output parameter low to the lowest possible value of the
   given type.
   Used to compute offsets and element sizes in array accesses.  Return
   value can also be used in size_of() to help find the size of an array.
*/
unsigned long get_value_range(TYPE type, long * low);


/* gram: variable_or_function_access_no_id (6th production)
   Returns a new ARRAY_ACCESS expr with the given array (in u.fcall.function)
   and given list of indices (in u.fcall.args).  These fields are "borrowed"
   from the function call expression.  Checks that the array is of array type
   (tag TYARRAY), else error.  Gets the r-value of each index expression, then
   unary converts it.  Also checks that the type of each index expression
   (after unary conversion) matches the corresponding "formal" index type
   (after base types are substituted for subranges), else error.  Also checks
   that the number of index expressions matches that of the array type.

   Note: things of array type are always l-values in Pascal, although they
   are always r-values in C.  Array accesses are always l-values (in both
   languages).
*/
EXPR make_array_access_expr(EXPR array, EXPR_LIST indices);


/* gram: one_case_constant (both productions), case_statement (intermediate action after LEX_OF, just to make an inital dummy record)
   Returns a new VAL_LIST node with the given information for a single case
   constant (obtained from an earlier call or calls to get_case_value()).
*/
VAL_LIST new_case_value(TYPETAG type, long lo, long hi);


/* gram: case_element (intermediate action after ':')
   Parameters:
       type: the type of the case expression
       vals: the list of current case constants
       prev_vals: the list of all previous case constants (for previous
           case elements)
   For each case constant on the vals list: Checks that its type (of both low
   and high values if subrange) matches that of the case expression, and if a
   subrange, checks that the low value is <= the high value (else ignored with
   warning), then checks for any overlap between the current value and any of
   the previous case constants (if so, error).  If all is ok, appends the case
   constant onto the list of previous values.

   Returns true iff no errors.

   Note: the case expression should be internally converted to a long,
   regardless of its type.  All comparisons of the case expression with
   case constants should be as TYSIGNEDLONGINT.  This internal conversion
   does not affect the TYPE being passed to check_case_values().
*/
BOOLEAN check_case_values(TYPETAG type, VAL_LIST vals, VAL_LIST prev_vals);


/* gram: case_constant_list (2nd production -- if error occurred), optional_semicolon_or_else_branch (both productions)
   De-allocates a list of case constants.
*/
void case_value_list_free(VAL_LIST vals);


/* gram: one_case_constant (once in 1st production; twice in 2nd production)
   Here, expr is a static (constant) expression, either alone or part of
   a subrange.  Sets the output parameters to the value and type of the
   expression.  The expr (after converting a STRCONST if possible or necessary)
   must be an INTCONST (else error).  Returns true iff no errors.
*/
BOOLEAN get_case_value(EXPR expr, long * val, TYPETAG * type);


/* gram: for_statement (in intermediate action after LEX_DO)
   Expressions passed are for the loop control variable, the initial value
   and the limit value, respectively.  Checks that the var is an l-value of
   ordinal type, and that its type matches those of the init and limit
   (after base types are substituted for subranges, as usual).
   Returns true iff all checks are ok.
*/
BOOLEAN check_for_preamble(EXPR var, EXPR init, EXPR limit);

#endif

