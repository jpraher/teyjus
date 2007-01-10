/***************************************************************************/ 
/*                                                                         */  
/*  File simdispatch.c. The instruction dispatch table used by the         */  
/*  simulator is defined here as an array of function pointers, each of    */  
/*  which refers to a function realizing a corresponding instruction.      */  
/*  These functions are defined in the file ./siminstr.c.                  */  
/***************************************************************************/  

#include "../tables/instructions.h" //to be modified                        
#include "siminstr.h"                                                        
#include "simdispatch.h"

SDP_InstrFunctionPtr SDP_dispatchTable[INSTR_NUM_INSTRS] = {
    SINSTR_put_variable_t,
    SINSTR_put_variable_p,
    SINSTR_put_value_t,
    SINSTR_put_value_p,
    SINSTR_put_unsafe_value,
    SINSTR_copy_value,
    SINSTR_put_m_const,
    SINSTR_put_p_const,
    SINSTR_put_nil,
    SINSTR_put_integer,
    SINSTR_put_float,
    SINSTR_put_string,
    SINSTR_put_index,
    SINSTR_put_app,
    SINSTR_put_list,
    SINSTR_put_lambda,
    SINSTR_set_variable_t,
    SINSTR_set_variable_te,
    SINSTR_set_variable_p,
    SINSTR_set_value_t,
    SINSTR_set_value_p,
    SINSTR_globalize_pt,
    SINSTR_globalize_t,
    SINSTR_set_m_const,
    SINSTR_set_p_const,
    SINSTR_set_nil,
    SINSTR_set_integer,
    SINSTR_set_float,
    SINSTR_set_string,
    SINSTR_set_index,
    SINSTR_set_void,
    SINSTR_deref,
    SINSTR_set_lambda,
    SINSTR_get_variable_t,
    SINSTR_get_variable_p,
    SINSTR_init_variable_t,
    SINSTR_init_variable_p,
    SINSTR_get_m_constant,
    SINSTR_get_p_constant,
    SINSTR_get_integer,
    SINSTR_get_float,
    SINSTR_get_string,
    SINSTR_get_nil,
    SINSTR_get_m_structure,
    SINSTR_get_p_structure,
    SINSTR_get_list,
    SINSTR_unify_variable_t,
    SINSTR_unify_variable_p,
    SINSTR_unify_value_t,
    SINSTR_unify_value_p,
    SINSTR_unify_local_value_t,
    SINSTR_unify_local_value_p,
    SINSTR_unify_m_constant,
    SINSTR_unify_p_constant,
    SINSTR_unify_integer,
    SINSTR_unify_float,
    SINSTR_unify_string,
    SINSTR_unify_nil,
    SINSTR_unify_void,
    SINSTR_put_type_variable_t,
    SINSTR_put_type_variable_p,
    SINSTR_put_type_value_t,
    SINSTR_put_type_value_p,
    SINSTR_put_type_unsafe_value,
    SINSTR_put_type_const,
    SINSTR_put_type_structure,
    SINSTR_put_type_arrow,
    SINSTR_set_type_variable_t,
    SINSTR_set_type_variable_p,
    SINSTR_set_type_value_t,
    SINSTR_set_type_value_p,
    SINSTR_set_type_local_value_t,
    SINSTR_set_type_local_value_p,
    SINSTR_set_type_constant,
    SINSTR_get_type_variable_t,
    SINSTR_get_type_variable_p,
    SINSTR_init_type_variable_t,
    SINSTR_init_type_variable_p,
    SINSTR_get_type_value_t,
    SINSTR_get_type_value_p,
    SINSTR_get_type_constant,
    SINSTR_get_type_structure,
    SINSTR_get_type_arrow,
    SINSTR_unify_type_variable_t,
    SINSTR_unify_type_variable_p,
    SINSTR_unify_type_value_t,
    SINSTR_unify_type_value_p,
    SINSTR_unify_envty_value_t,
    SINSTR_unify_envty_value_p,
    SINSTR_unify_type_local_value_t,
    SINSTR_unify_type_local_value_p,
    SINSTR_unify_envty_local_value_t,
    SINSTR_unify_envty_local_value_p,
    SINSTR_unify_type_constant,
    SINSTR_pattern_unify_t,
    SINSTR_pattern_unify_p,
    SINSTR_finish_unify,
    SINSTR_head_normalize_t,
    SINSTR_head_normalize_p,
    SINSTR_incr_universe,
    SINSTR_decr_universe,
    SINSTR_set_univ_tag,
    SINSTR_tag_exists_t,
    SINSTR_tag_exists_p,
    SINSTR_tag_variable,
    SINSTR_push_impl_point,
    SINSTR_pop_impl_point,
    SINSTR_add_imports,
    SINSTR_remove_imports,
    SINSTR_push_import,
    SINSTR_pop_imports,
    SINSTR_allocate,
    SINSTR_deallocate,
    SINSTR_call,
    SINSTR_call_name,
    SINSTR_execute,
    SINSTR_execute_name,
    SINSTR_proceed,
    SINSTR_try_me_else,
    SINSTR_retry_me_else,
    SINSTR_trust_me,
    SINSTR_try,
    SINSTR_retry,
    SINSTR_trust,
    SINSTR_trust_ext,
    SINSTR_try_else,
    SINSTR_retry_else,
    SINSTR_branch,
    SINSTR_switch_on_term,
    SINSTR_switch_on_constant,
    SINSTR_switch_on_bvar,
    SINSTR_switch_on_reg,
    SINSTR_neck_cut,
    SINSTR_get_level,
    SINSTR_put_level,
    SINSTR_cut,
    SINSTR_call_builtin,
    SINSTR_builtin,
    SINSTR_stop,
    SINSTR_halt,
    SINSTR_fail
};

