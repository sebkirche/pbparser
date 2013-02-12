forward
global type nv_term_test from nv_term
end type
end forward

global type nv_term_test from nv_term
end type
global nv_term_test nv_term_test

forward prototypes
public function boolean accept (ref string as_input, readonly long al_inplen, readonly long al_startoff, ref long al_endoff, ref nv_termqueue atq_terms, readonly string as_ops, readonly string as_funcs)
end prototypes

public function boolean accept (ref string as_input, readonly long al_inplen, readonly long al_startoff, ref long al_endoff, ref nv_termqueue atq_terms, readonly string as_ops, readonly string as_funcs);
return true

end function

on nv_term_test.create
call super::create
end on

on nv_term_test.destroy
call super::destroy
end on

