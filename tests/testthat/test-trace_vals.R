test_that("test tracing a simple call", {
  r <- trace_vals(code = {
    stringr::str_detect("AB", "B")
    stringr::str_detect("AB", "A")
  })
  browser()

  if (is.null(r$result$error)) {
    expect_equal(length(r$output$arguments), 0)
  }

})


## r$result
##  $error
##  $output
##  $output$call_ref
##  $output$arg_ref
##  $output$effects
##  $output$metaprogramming
##  $output$environments
##  $output$functions (cols: fun_id, fun_name, anonymous, qual_name, parent_fun_ie, fun_env_id, call_count, fun_hash, fun_def)
## $output$arguments (cols: arg_id, call_id, fun_id, call_env_id, formal_pos, dot_pos, force_pos, actual_pos, default, arg_name, vararg, missing, arg_type, expr_type, val_type, preforced, cap_force, cap_meta, cap_lookup, escaped, esc_force, esc_meta, esc_lookup, con_force, con_lookup, force_depth, meta_depth, comp_pos, event_seq, self_effect_seq, effect_seq, self_ref_seq, parent_fun_id, parent_formal_pos, parent_call_id, parent_arg_id)
## $output$calls (cols: call_id, fun_id, env_id, successful, result_type, force_order, esc_env, call_expr)
## $statistics
## $statistics$allocation
## $statistics$execution




