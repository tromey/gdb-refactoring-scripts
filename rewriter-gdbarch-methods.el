
(defconst gdbarch-methods
  '("floatformat_for_type"
    "virtual_frame_pointer"
    "pseudo_register_read"
    "pseudo_register_read_value"
    "pseudo_register_write"
    "deprecated_pseudo_register_write"
    "ax_pseudo_register_collect"
    "ax_pseudo_register_push_stack"
    "report_signal_info"
    "stab_reg_to_regnum"
    "ecoff_reg_to_regnum"
    "sdb_reg_to_regnum"
    "dwarf2_reg_to_regnum"
    "register_name"
    "register_type"
    "dummy_id"
    "push_dummy_call"
    "push_dummy_code"
    "code_of_frame_writable"
    "print_registers_info"
    "print_float_info"
    "print_vector_info"
    "register_sim_regno"
    "cannot_fetch_register"
    "cannot_store_register"
    "convert_register_p"
    "value_from_register"
    "pointer_to_address"
    "address_to_pointer"
    "integer_to_address"
    "return_value"
    "return_value_as_value"
    "update_call_site_pc"
    "return_in_first_hidden_param_p"
    "skip_prologue"
    "skip_main_prologue"
    "skip_entrypoint"
    "breakpoint_from_pc"
    "breakpoint_kind_from_pc"
    "sw_breakpoint_from_kind"
    "breakpoint_kind_from_current_state"
    "adjust_breakpoint_address"
    "memory_insert_breakpoint"
    "memory_remove_breakpoint"
    "remote_register_number"
    "get_thread_local_address"
    "unwind_pc"
    "unwind_sp"
    "frame_align"
    "stabs_argument_has_addr"
    "convert_from_func_ptr_addr"
    "addr_bits_remove"
    "remove_non_address_bits"
    "memtag_to_string"
    "tagged_address_p"
    "memtag_matches_p"
    "set_memtags"
    "get_memtag"
    "single_step_through_delay"
    "skip_solib_resolver"
    "in_solib_return_trampoline"
    "in_indirect_branch_thunk"
    "stack_frame_destroyed_p"
    "address_class_type_flags_to_name"
    "execute_dwarf_cfa_vendor_op"
    "address_class_name_to_type_flags"
    "register_reggroup_p"
    "iterate_over_regset_sections"
    "make_corefile_notes"
    "find_memory_regions"
    "create_memtag_section"
    "fill_memtag_section"
    "decode_memtag_section"
    "core_xfer_shared_libraries"
    "core_xfer_shared_libraries_aix"
    "core_pid_to_str"
    "core_thread_name"
    "core_xfer_siginfo"
    "core_read_x86_xsave_layout"
    "displaced_step_copy_insn"
    "displaced_step_hw_singlestep"
    "displaced_step_fixup"
    "displaced_step_prepare"
    "displaced_step_finish"
    "relocate_instruction"
    "core_read_description"
    "process_record"
    "process_record_signal"
    "gdb_signal_from_target"
    "gdb_signal_to_target"
    "get_siginfo_type"
    "record_special_symbol"
    "get_syscall_number"
    "stap_is_single_operand"
    "stap_parse_special_token"
    "stap_adjust_register"
    "dtrace_parse_probe_argument"
    "dtrace_probe_is_enabled"
    "dtrace_enable_probe"
    "dtrace_disable_probe"
    "has_shared_address_space"
    "fast_tracepoint_valid_at"
    "guess_tracepoint_registers"
    "gen_return_address"
    "info_proc"
    "core_info_proc"
    "iterate_over_objfiles_in_search_order"
    "insn_is_call"
    "insn_is_ret"
    "insn_is_jump"
    "program_breakpoint_here_p"
    "auxv_parse"
    "print_auxv_entry"
    "vsyscall_range"
    "gcc_target_options"
    "gnu_triplet_regexp"
    "addressable_memory_unit_size"
    "type_align"
    "read_core_file_mappings"
    "use_target_description_from_corefile_notes"))

(defconst set-gdbarch-re
  (concat "^\\s +set_gdbarch_\\("
	  (regexp-opt gdbarch-methods)
	  "\\)\\_>"))

(defvar all-callbacks nil)

(defun rw-find-callbacks-in-file ()
  (while (re-search-forward set-gdbarch-re nil t)
    ;; found set_gdbarch_mumble, skip whitespace and "(".
    (skip-chars-forward " \t\n")
    (if (looking-at "(")
	(progn
	  (forward-char)
	  ;; Skip first arg.
	  (rw-skip-expr ",")
	  (forward-char)
	  (skip-chars-forward " \t\n")
	  (when (looking-at "[a-zA-Z0-9_]+")
	    (let ((cb (match-string 0)))
	      ;; NULL appears on occasion intentionally
	      (unless (equal cb "NULL")
		(message " ... found %s" cb)
		(push cb all-callbacks)))))
      (rw-error "NO PAREN"))))

(rw-rewrite #'rw-find-callbacks-in-file)

(defconst rw-method-impls
  (concat "^"
	  (regexp-opt (delete-dups all-callbacks))
	  "\\_>"))

(defun rw-rewrite-callbacks ()
  (while (re-search-forward rw-method-impls nil t)
    ;; Found method impl, skip "(" and rewrite the first arg.
    (skip-chars-forward " \t\n")
    (if (looking-at "(")
	(progn
	  (forward-char)
	  ;; I did a few by hand
	  (unless (looking-at "const")
	    (insert "const ")))
      (rw-error "NO PAREN 2"))))

(rw-rewrite #'rw-rewrite-callbacks)
