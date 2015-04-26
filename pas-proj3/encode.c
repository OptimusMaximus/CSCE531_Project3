/****************************************************************/
/*																*/
/*	CSCE531 - "Pascal" and "C" Compilers						*/
/*																*/
/*	--encode.c--												*/
/*																*/
/*	This File Contains routines that support the				*/
/*	"Encode Module".											*/
/*																*/
/*	Maximus Brandel												*/
/*  Konstantin Rubin											*/
/*	Ibrahim Elsayed												*/
/****************************************************************/

#include <stdlib.h>
#include "defs.h"
#include "types.h"
#include "symtab.h"
#include "message.h"
#include "backend-x86.h"
#include "encode.h"

int exit_label = -1;
char* exit_label_stack[100];

void declare_var(VAR_ID_LIST list_id, TYPE type){
	/* Return if type tag is TYERROR OR TYFUNC */
	if (ty_query(type) == TYERROR || ty_query(type) == TYFUNC) return;

	/* While list_id != NULL */
	while(list_id){
		ST_ID st_id = list_id->id;	
		/* Get the id from the Symbol Table */
		char *id = st_get_id_str(st_id);
		/* Get the size and alignment of the type */
		unsigned int size = get_size(type);
		int alignment = get_align(type);

		if (size != 0 && alignment != 0){
			/* Call backend functions */
			/* Declare variable*/
			b_global_decl(id, alignment, size);
			/* Allocate memory for the variable */
			b_skip(size);
		}else
			bug("Something messed up in declare_var");
		/* Go to next item in list */	
		list_id = list_id->next;
	}
}

unsigned int get_size(TYPE type){

    /* Get the TYPETAG from ty_query */
    TYPETAG tag;
    tag = ty_query(type);

    /* For arrays */
    INDEX_LIST indices;
    unsigned int array_size;
    TYPE temp_type = type;

    /* For subrange */
    long low, high;

    switch(tag){
    	case TYUNSIGNEDCHAR:
    		return 1;//sizeof(unsigned char);
    	case TYSIGNEDCHAR:
    		return 1;//sizeof(signed char);
    	case TYUNSIGNEDSHORTINT:
    		return 2;//sizeof(unsigned short int);
    	case TYSIGNEDSHORTINT:
    		return 2;//sizeof(signed short int);
    	case TYUNSIGNEDINT:
    		return 4;//sizeof(unsigned int);
    	case TYSIGNEDINT:
    		return 4;//sizeof(signed int);
    	case TYUNSIGNEDLONGINT:
    		return 4;//sizeof(unsigned long int);
    	case TYSIGNEDLONGINT:
    		return 4;//sizeof(signed long int);
    	case TYFLOAT:
    		return 4;//sizeof(float);
    	case TYDOUBLE:
    		return 8;//sizeof(double);
    	case TYLONGDOUBLE:
    		return 8;//sizeof(long double);
    	case TYARRAY:
    		/* Get array size recursively */
    		array_size = get_size(ty_query_array(type, &indices));
    		/* While indices != NULL */
			while (indices) { 
				/* Get the low and high values */
				ty_query_subrange(indices->type,&low,&high);
				/* Calculate new array_size */
				array_size *= high - low + 1;
				/* Go to next indice */		
				indices = indices->next;
			}		
			return array_size;	
    	case TYSUBRANGE:
    		return get_size(ty_query_subrange(type, &low, &high));
    	case TYPTR:
    		return 4;//sizeof(char *);
    	// case TYVOID:
    	// 	return 0;
    	// case TYSTRUCT:
    	// 	return 0;
    	// case TYUNION:
    	// 	return 0;
    	// case TYENUM:
    	// 	return 0;
    	// case TYSET:
    	// 	return 0;
    	//case TYFUNC:
    	// 	return -98;
    	// case TYBITFIELD:
    	// 	return 0;
    	//case TYERROR:
    	//	return -99;
    	default:
    		bug("Illegal type tag %d in get_size", tag);
    		return 0;
    }
}

int get_align(TYPE type){

    /* Get the TYPETAG from ty_query */
    TYPETAG tag;
    tag = ty_query(type);

    /* For arrays */
    INDEX_LIST indices;

    /* For subrange */
    long low, high;

    switch(tag){
    	case TYUNSIGNEDCHAR:
    		return 1;//sizeof(unsigned char);
    	case TYSIGNEDCHAR:
    		return 1;//sizeof(signed char);
    	case TYUNSIGNEDSHORTINT:
    		return 2;//sizeof(unsigned short int);
    	case TYSIGNEDSHORTINT:
    		return 2;//sizeof(signed short int);
    	case TYUNSIGNEDINT:
    		return 4;//sizeof(unsigned int);
    	case TYSIGNEDINT:
    		return 4;//sizeof(signed int);
    	case TYUNSIGNEDLONGINT:
    		return 4;//sizeof(unsigned long int);
    	case TYSIGNEDLONGINT:
    		return 4;//sizeof(signed long int);
    	case TYFLOAT:
    		return 4;//sizeof(float);
    	case TYDOUBLE:
    		return 8;//sizeof(double);
    	case TYLONGDOUBLE:
    		return 8;//sizeof(long double);
    	case TYARRAY:
    		return get_align(ty_query_array(type, &indices));
    	case TYSUBRANGE:
    		return get_align(ty_query_subrange(type, &low, &high));
    	case TYPTR:
    	 	return 4;//sizeof(char *);
    	// case TYVOID:
    	// 	return 0;
    	// case TYSTRUCT:
    	// 	return 0;
    	// case TYUNION:
    	// 	return 0;
    	// case TYENUM:
    	// 	return 0;
    	// case TYSET:
    	// 	return 0;
    	//case TYFUNC:
    	// 	return -98;
    	// case TYBITFIELD:
    	// 	return 0;
    	//case TYERROR:
    	// 	return -99;
    	default:
    		bug("Illegal type tag %d in get_align", tag);
    		return 0;
    }
}

/* Function to encode an expression
   Calls the appropriate encode function for unary, binop, and function calls.
*/
void encode_expr(EXPR expr)
{    
    if (expr == NULL)
        bug("Expression is null ");
    switch (expr->tag) {
        case LFUN: //expr->tag = 4
        case ERROR: break; //expr->tag = 10
        case INTCONST:  //expr->tag = 0
            b_push_const_int(expr->u.intval);
            if(ty_query(expr->type) == TYUNSIGNEDCHAR)
                b_convert(TYSIGNEDLONGINT, TYUNSIGNEDCHAR);
            if(ty_query(expr->type) == TYSIGNEDCHAR)
                b_convert(TYSIGNEDLONGINT, TYSIGNEDCHAR);
            break;
        case REALCONST:   //expr->tag = 1   
            b_push_const_double(expr->u.realval);
            break;
        case STRCONST: //expr->tag = 2
            b_push_const_string(expr->u.strval);
            break;
        case GID: //expr->tag = 3
            b_push_ext_addr(st_get_id_str(expr->u.gid));
            break;
        case LVAR:     //expr->tag = 4 
            b_push_loc_addr(0);
            int i = 0;
            for (i = 0; i < expr->u.lvar.link_count; i++) {
                b_offset(FUNC_LINK_OFFSET);
                b_deref(TYPTR);
            }
            b_offset(expr->u.lvar.offset);
            if (expr->u.lvar.is_ref == TRUE)
                b_deref(TYPTR);
            break;
        case NULLOP:   //expr->tag = 6    
            b_push_const_int(0);
            break;   
        case UNOP:  //expr->tag = 7      
            encode_unop(expr->u.unop.op, expr);
            break;
        case BINOP://expr->tag = 8
            encode_binop(expr->u.binop.op, expr);
            break;
        case FCALL:  //expr->tag = 9      
            encode_fcall(expr->u.fcall.function, expr->u.fcall.args);
            break;
		case ARRAY_ACCESS:
			encode_arrayaccess(expr->u.fcall.function, expr->u.fcall.args);
			break;
    }
}

void encode_arrayaccess(EXPR expr, EXPR_LIST indices) {
   long low_index, high_index, range;
   int align;
   unsigned int size;
   TYPE array_type, subrange_type;
   INDEX_LIST index_list;
   EXPR_LIST rev_indices;

   encode_expr(expr);
   array_type = ty_query_array(expr->type, &index_list);
   align = get_align(array_type);
   size = get_size(array_type);

   rev_indices = expr_list_reverse(indices);

   while (rev_indices != NULL && index_list != NULL) {
      encode_expr(rev_indices->expr);
      subrange_type = ty_query_subrange(index_list->type, &low_index, &high_index);
      range = high_index - low_index + 1;
      
      if (index_list->next == NULL) {
         b_push_const_int(low_index);
         b_arith_rel_op(B_SUB,TYSIGNEDLONGINT);
         b_ptr_arith_op(B_ADD, TYSIGNEDLONGINT, size);
      }
      //else if (subrange do later)
      rev_indices = rev_indices->next;
      index_list = index_list->next;
   }
}

/* Function to encode a unary operation */
void encode_unop(EXPR_UNOP op, EXPR expr)
{
    long low, high;
    TYPE baseT;
    TYPETAG tag, rval_tag;
    ST_ID id;
    TYPE type, base_type;
    BOOLEAN converted_to_int;

	if(op == DISPOSE_OP)
		b_alloc_arglist(4);
	
    encode_expr(expr->u.unop.operand);
	//printf("unop op is %d \n", op);
    converted_to_int = FALSE;
    tag = ty_query(expr->u.unop.operand->type);
    rval_tag = ty_query(expr->type);
	//printf("tag is %d \n", tag);
	
    switch(op) {
        case INDIR_OP:
        case PLUS_OP: break;
        case CONVERT_OP:     
            if(tag==TYSUBRANGE) { 
                base_type = ty_query_subrange(expr->u.unop.operand->type, &low, &high);
                b_convert(TYSUBRANGE, ty_query(base_type));
            } 
            else if(tag==TYPTR) { 
				
            } 
            else
                b_convert(tag, rval_tag);
            break;
         case NEG_OP:   
            b_negate(tag); 
            break;
         case ORD_OP:
            if(tag==TYUNSIGNEDCHAR)
                b_convert(tag, TYSIGNEDLONGINT);
            break;
         case CHR_OP:
            if(tag==TYSIGNEDLONGINT) 
                b_convert(tag, TYUNSIGNEDCHAR);
            break;
         case SUCC_OP:   
            if(tag!=TYSIGNEDLONGINT) {
                b_convert(tag,TYSIGNEDLONGINT);
                converted_to_int = TRUE;
            }
            b_push_const_int(1);
            b_arith_rel_op(B_ADD, TYSIGNEDLONGINT);
            if(converted_to_int == TRUE) 
                b_convert(TYSIGNEDLONGINT,tag);
            break;
        case PRED_OP:
            if(tag != TYSIGNEDLONGINT) {
                b_convert(tag,TYSIGNEDLONGINT);
                converted_to_int = TRUE;
            }

            b_push_const_int(-1);
            b_arith_rel_op(B_ADD, TYSIGNEDLONGINT);

            if(converted_to_int == TRUE)
                b_convert(TYSIGNEDLONGINT, tag);
            break;
        case NEW_OP:       
            b_alloc_arglist(4);
            b_push_const_int(get_size(ty_query_ptr(expr->u.unop.operand->type, &id)));
            b_load_arg(TYUNSIGNEDINT);
            b_funcall_by_name("malloc", TYPTR);
            b_assign(TYPTR);
            b_pop();
            break;
        case DISPOSE_OP:   
            b_load_arg(TYPTR);
            b_funcall_by_name("free", TYVOID);
            break;
        case DEREF_OP:
			b_deref(tag);
            break;
        case SET_RETURN_OP: 
            b_set_return(ty_query(expr->u.unop.operand->type));
            break;
    }
}

/* Function to encode a binary operation */
void encode_binop(EXPR_BINOP out, EXPR expr)
{
  TYPETAG type_tag;
  TYPETAG left_type_tag, right_type_tag;

//TODO: Still working on constant folding
//Constant folding done here
//add SUB, MUL, succ, chr, ord, pred, DIV, MOD, 
if(expr->u.binop.left->tag==INTCONST && expr->u.binop.right->tag==INTCONST)
{
	EXPR ret;
	switch (out) 
	{
		case ADD_OP:			
			ret = make_intconst_expr((expr->u.binop.left->u.intval + expr->u.binop.right->u.intval), ty_build_basic(TYSIGNEDLONGINT));
			encode_expr(ret);
			break;
		case SUB_OP:
			ret = make_intconst_expr((expr->u.binop.left->u.intval - expr->u.binop.right->u.intval), ty_build_basic(TYSIGNEDLONGINT));
			encode_expr(ret);
			break;
		case MUL_OP: 
			ret = make_intconst_expr((expr->u.binop.left->u.intval * expr->u.binop.right->u.intval), ty_build_basic(TYSIGNEDLONGINT));
			encode_expr(ret);			
			break;
    	case DIV_OP: 
			ret = make_intconst_expr((expr->u.binop.left->u.intval / expr->u.binop.right->u.intval), ty_build_basic(TYSIGNEDLONGINT));
			encode_expr(ret);			
			break;
    	case MOD_OP: 
			ret = make_intconst_expr((expr->u.binop.left->u.intval % expr->u.binop.right->u.intval), ty_build_basic(TYSIGNEDLONGINT));
			encode_expr(ret);
 			break;    
    	case REALDIV_OP: 
			//This is only for reals not ints!!!
			break;

	}
}/*
else if (expr->u.binop.left->tag==UNOP && expr->u.binop.right->tag==REALCONST){
	EXPR ret;
	switch(out){
		case ADD_OP: 
			ret = make_realconst_expr(((double)(expr->u.binop.left->u.unop.operand->u.intval) + expr->u.binop.right->u.realval));
			encode_expr(ret);
			break;
		case SUB_OP:
			ret = make_realconst_expr(((double)(expr->u.binop.left->u.unop.operand->u.intval) - expr->u.binop.right->u.realval));
			encode_expr(ret);
			break;
		case MUL_OP: 
			ret = make_realconst_expr(((double)(expr->u.binop.left->u.unop.operand->u.intval) * expr->u.binop.right->u.realval));
			encode_expr(ret);			
			break;
    	case DIV_OP: 
			//This is for ints
			break;
    	case MOD_OP: 
			//This is for ints
 			break;    
    	case REALDIV_OP: 
			ret = make_realconst_expr(((double)(expr->u.binop.left->u.unop.operand->u.intval) / expr->u.binop.right->u.realval));
			encode_expr(ret);
			break;
	}
}
else if(expr->u.binop.left->tag==REALCONST && expr->u.binop.right->tag==UNOP){
	EXPR ret;
	switch(out){
		case ADD_OP: 
			ret = make_realconst_expr(expr->u.binop.left->u.realval + (double)(expr->u.binop.right->u.unop.operand->u.intval));
			encode_expr(ret);
			break;
		case SUB_OP:
			ret = make_realconst_expr(expr->u.binop.left->u.realval - (double)(expr->u.binop.right->u.unop.operand->u.intval));
			encode_expr(ret);
			break;
		case MUL_OP: 
			ret = make_realconst_expr(expr->u.binop.left->u.realval * (double)(expr->u.binop.right->u.unop.operand->u.intval));
			encode_expr(ret);			
			break;
    	case DIV_OP: 
			//This is for ints
			break;
    	case MOD_OP: 
			//This is for ints
 			break;    
    	case REALDIV_OP: 
			ret = make_realconst_expr(expr->u.binop.left->u.realval / (double)(expr->u.binop.right->u.unop.operand->u.intval));
			encode_expr(ret);
			break;
	}
}*/
// else if(expr->u.binop.left->tag == UNOP && expr->u.binop.right->tag == INTCONST){
//     EXPR ret;
//     switch(out){
//         case ADD_OP: 
//             ret = make_realconst_expr(expr->u.binop.left->u.unop.operand->u.realval + (double)(expr->u.binop.right->u.intval));
//             encode_expr(ret);
//             break;
//         case SUB_OP:
//             ret = make_realconst_expr(expr->u.binop.left->u.unop.operand->u.realval - (double)(expr->u.binop.right->u.intval));
//             encode_expr(ret);
//             break;
//         case MUL_OP: 
//             ret = make_realconst_expr(expr->u.binop.left->u.unop.operand->u.realval * (double)(expr->u.binop.right->u.intval));
//             encode_expr(ret);           
//             break;
//         case DIV_OP: 
//             //This is for ints
//             break;
//         case MOD_OP: 
//             //This is for ints
//             break;    
//         case REALDIV_OP: 
//             ret = make_realconst_expr(expr->u.binop.left->u.unop.operand->u.realval / (double)(expr->u.binop.right->u.intval));
//             encode_expr(ret);
//             break;
//     }
// }
else {
	////printf("\n\n\nbefore   encode_expr(expr->u.binop.left); \n\n\n");
  encode_expr(expr->u.binop.left);
//if(expr->u.binop.right->tag==INTCONST)
 ////printf("\n\nexpr->u.binop.right->tag=INTCONST\n\n");
//	//printf("\n\n\n after encode_expr(expr->u.binop.left); \n\n\n");
  encode_expr(expr->u.binop.right);

  type_tag = ty_query(expr->type);
  if(type_tag == TYFLOAT) 
	type_tag = TYDOUBLE;

  left_type_tag = ty_query(expr->u.binop.left->type);
  right_type_tag = ty_query(expr->u.binop.right->type);
  
  switch (out) {
    case SYMDIFF_OP:
    case OR_OP:
    case XOR_OP:
    case AND_OP:  break;
    case ADD_OP:  b_arith_rel_op(B_ADD, type_tag); break;

    case SUB_OP: b_arith_rel_op(B_SUB, type_tag); break;

    case MUL_OP: b_arith_rel_op(B_MULT, type_tag);break;

    case DIV_OP: b_arith_rel_op(B_DIV, type_tag); break;

    case MOD_OP: b_arith_rel_op(B_MOD, type_tag); break;
    
    case REALDIV_OP: b_arith_rel_op(B_DIV, type_tag); break;
    
    case EQ_OP:b_arith_rel_op(B_EQ, type_tag);   break;

    case LESS_OP: b_arith_rel_op(B_LT, type_tag); break;
    
    case LE_OP: b_arith_rel_op(B_LE, type_tag); break;
    
    case NE_OP: b_arith_rel_op(B_NE, type_tag); break;
    
    case GE_OP: b_arith_rel_op(B_GE, type_tag); break;
    
    case GREATER_OP: b_arith_rel_op(B_GT, type_tag); break;
   
    case ASSIGN_OP:  
        if(expr->u.binop.left->tag == LVAR)
            b_push_loc_addr(expr->u.binop.left->u.lvar.offset);
        if(left_type_tag != right_type_tag){
			if(expr->u.binop.right->tag==INTCONST)
				b_convert(TYSIGNEDLONGINT, left_type_tag);
			else
            	b_convert(right_type_tag, left_type_tag);
		}
        b_assign(left_type_tag);
        b_pop();
        break;
   }
  }
}

/* Function to encode a function call */
void encode_fcall(EXPR func, EXPR_LIST args)
{
    int arg_list_size;
    char *func_glob_name;
    TYPE func_ret_type;
    TYPETAG arg_tag;
    PARAM_LIST func_params;
    BOOLEAN check_args;
	EXPR_LIST args_copy;

    func_ret_type = ty_query_func(func->type, &func_params, &check_args);
    arg_list_size = 0;
    args_copy = args;

    if(func->tag == GID)
        func_glob_name = st_get_id_str(func->u.gid);

    while(args_copy != NULL) {
        if(ty_query(args_copy->expr->type)==TYDOUBLE||ty_query(args_copy->expr->type)==TYFLOAT)
            arg_list_size += 8;
        else
            arg_list_size += 4;

        args_copy = args_copy->next;
    }

    b_alloc_arglist(arg_list_size);
    args_copy = args;
    while(args_copy != NULL) {

        encode_expr(args_copy->expr);
        arg_tag = ty_query(args_copy->expr->type);
        if(func_params != NULL) {
            if(func_params->is_ref==TRUE) { 
                if(is_lval(args_copy->expr)==FALSE)
                  bug("Function argument expected to be lval in encode_fcall_expr");

                if(ty_test_equality(args_copy->expr->type, func_params->type)==FALSE) 
                   error("Parameter types do not match");
				
                b_load_arg(TYPTR);
            } 
            else { 
                if(is_lval(args_copy->expr)==TRUE) 
                    b_deref(arg_tag);
                if(arg_tag==TYSIGNEDCHAR||arg_tag==TYUNSIGNEDCHAR) { 
                    b_convert(arg_tag,TYSIGNEDLONGINT);
                    b_load_arg(TYSIGNEDLONGINT);
                } 
                else if(arg_tag==TYFLOAT) { 
                    b_convert(arg_tag,TYDOUBLE);
                    b_load_arg(TYDOUBLE);
                } 
                else 
                    b_load_arg(arg_tag);
            }
         } 
        else {     
            //if(is_lval(args_copy->expr)==TRUE) 
              //  b_deref(arg_tag); // Maximus commented out
            if(arg_tag==TYSIGNEDCHAR || arg_tag==TYUNSIGNEDCHAR) { 
                b_convert(arg_tag, TYSIGNEDLONGINT);
                b_load_arg(TYSIGNEDLONGINT);
            } 
            else if(arg_tag==TYFLOAT) { 
                b_convert(arg_tag,TYDOUBLE);
                b_load_arg(TYDOUBLE);
            } 
            else 
                b_load_arg(arg_tag);
        }

        args_copy=args_copy->next;

        if(func_params!=NULL) 
            func_params=func_params->next;
    }
    b_funcall_by_name(func_glob_name,ty_query(func_ret_type));
}

/* Project III */

/* NOTE: In my solution code, many back-end routines are called directly
   from actions within the grammar.  Also, array access encoding is handled
   in helper functions to encode_expr()  This means that the following
   functions do not include all the encoding that is done.
*/

/* gram: simple_if (intermediate action after LEX_THEN), case_statement (intermediate action after LEX_OF), while_statement (intermediate action after LEX_DO), for_statement (reduce action -- to push the address of the loop control variable for incrementing/decrementing), simple_statement (3rd and 4th productions)
   This function is also modified from Project II to encode ARRAY_ACCESS
   expressions.
*/
//void encode_expr(EXPR expr);


/* gram: structured_statement (4th production, intermediate action before repetitive_statement)
   Generates a new label (calling new_symbol()) and pushes it onto a private
   global stack of exit labels.  (The stack is initially empty when parsing
   starts.)
*/
void new_exit_label(){
  /*Gets the new label*/
  char * new_label = new_symbol();

  /*Increments*/
  exit_label_curr++;

  /*Pushes the label onto the stack*/
  exit_labels[exit_label_curr] = new_label;
}


/* gram: structured_statement (4th production's reduce action)
   Pops an exit label off of the private global stack of exit labels and
   returns it.  (BUG message if the stack is empty.)
*/
char * old_exit_label(){
  /*If the current index is zero, bug*/
  if(exit_label_curr < 0)
  {
    /*Bug, return*/
    bug("Exit label stack empty");
    return;
  }

  /*Gets the top label off the stack, decrements, returns the label*/
  char * ret_label = exit_labels[exit_label_curr];
  exit_label_curr--;
  return ret_label;
}


/* gram: while_statement (intermediate action after LEX_DO), statement_extensions (3rd production)
   Returns the label at the top of the private global stack of exit labels
   without popping it.  (BUG message if the stack is empty)
*/
char * current_exit_label(){
  /*If the current index is zero, bug*/
  if(exit_label_curr < 0)
  {
    /*Bug, return*/
    bug("Exit label stack empty");
    return;
  }

  /*Gets the top label off the stack, returns the label*/
  char * ret_label = exit_labels[exit_label_curr];
  return ret_label;
}


/* gram: statement_extensions (3rd production)
   Returns true iff the private global stack of exit labels is nonempty.
*/
BOOLEAN is_exit_label(){
  /*If index greater than or equal to zero, not empty*/
  if(exit_label_curr >= 0)
    return TRUE;
  else
    return FALSE;
}


/* gram: case_element (intermediate action after ':')
   At run time, it is assumed that the value of the case expression is on
   top of the control stack.  Each case constant in vals is compared with
   the case expression using b_dispatch().  The label parameter is the
   destination to jump to if there are no matches among the current constants.
   (This destination will be the start of the next case element if there is
   one, otherwise the start of the else-clause if there is one, otherwise the
   end of the entire case statement itself.)

   Before looping through the constants, a new "match" label is generated,
   to be used as the destination to jump to if a match is found.  This will
   be the beginning of the subsequent statement inside the current case
   element.

   In each loop iteration:
   -- For a single case constant value (low==high), B_EQ is used for
      the comparison, and only one call to b_dispatch() is made (with the
      match label).
   -- For a nontrivial subrange (low < high):
      1. A new label l is generated.
      2. The low value is compared with B_LT and label l.
      3. The high value is compared with B_LE and the match label.
      4. The label l is emitted with b_label().
   The point of the label l is that if the case expresion is less than the
   low value, we want to avoid the subsequent comparison with the high value,
   so we are branching around it.

   After the loop:
   1. Unconditional jump to the "no match" label given by the parameter.
   2. Emit the match label using b_label()

   In all instances of b_dispatch(), the comparison uses TYSIGNEDLONGINT,
   which is assumed to be how the values are stored internally.

   The last parameter to b_dispatch() should be TRUE (thus popping the
   case expression) if and only if there is a match (branch to the match
   label -- see the comments for b_dispatch() in backend.h).
*/
void encode_dispatch(VAL_LIST vals, char * label)
{
  /*New match label*/
  char * match_label = new_symbol();
 
  /*If there are elements in the val list*/
  if(vals != NULL)
    while(vals != NULL)
    {
      /*Comparison based on whether type is subrange or not*/
      if(vals->lo == vals->hi){
		/*Calls b_dispatch for comparison*/
		b_dispatch(B_EQ, TYSIGNEDLONGINT, vals->lo, match_label, TRUE);		
      } else {
		/*Subrange case*/
		char *  new_label = new_symbol();
		b_dispatch(B_LT, TYSIGNEDLONGINT, vals->lo, new_label, FALSE);
		b_dispatch(B_LE, TYSIGNEDLONGINT, vals->hi, match_label, FALSE);
		b_label(new_label);
      }
      /*Moves to the next item*/
      vals = vals->next;
    }

  /*Unconditional jump to the no match label*/
  b_jump(label);

  /*Emits the match label*/
  b_label(match_label);
}


/* gram: for_statement (part of intermediate action after LEX_DO)
   Encodes the preamble to a for-loop:
   1. Encodes the limit expression, implicitly converting it to
      TYSIGNEDLONGINT if it isn't already (b_convert()).  (This pushes the
      value onto the control stack at runtime.)
   2. Emits code to duplicate this value (b_duplicate()).
   3. Encodes what amounts to an assignment of the initial value to the
      loop control variable, except tha the value assigned is not popped
      off the control stack afterwards.
   4. Emits a new "return" label (with b_label()).
   5. Emits code to convert the initial value (on top of the control stack) to
      TYSIGNEDLONGING (b_convert()) if it isn't already.
   6. Emits code to compare the duplicate limit value with the top stack
      value (b_arith_rel_op()) with either B_LT or B_GT,
      depending on the direction (dir).  (There are two possible directions,
      corresponding to the two token types LEX_TO and LEX_DOWNTO.)
   7. Emits a conditional jump on true (B_NONZERO) to the current exit label
      (current_exit_label()).

   Note: For this to work, at the end of each iteration, when the loop
   control variable is incremented (or decremented, according to the direction),
   the condition of the control stack must be similar to the above upon
   jumping back to the "return" label.  That is, the new value of the
   loop control variable must be on top, with a duplicate of the limit
   value immediately beneath it.  This latter conditioning should be done
   in the reduce action to the for_statement production.  The easiest way
   to increment or decrement the variable so that the the new value is left
   on the stack is to use b_inc_dec() with either B_PRE_INC or B_PRE_DEC,
   depending on the direction.
*/
char * encode_for_preamble(EXPR var, EXPR init, int dir, EXPR limit){
	return;
}
