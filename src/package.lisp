(defpackage llvm
  (:documentation
   "In general, names are from the C interface (see include/llvm-c), translated
    as follows:

  • “LLVM” prefix is dropped
  • intraCaps are replaced with hyphens (except common abbrevs, like “PPC”,
    which are enumerated in cffi.lisp)
  • “Is” prefix replaced by “p” suffix (hyphenated using the usual CL rules)
  • “Get” prefix is dropped
  • “Set” prefix is dropped, replaced with setf function
  • “Create*” is replaced by MAKE-INSTANCE, to look more like a defclass
    constructor

    EG, LLVMX86FP80TypeInContext -> x86-fp80-type-in-context
    and LLVMIsPackedStruct -> packed-struct-p

  • paired parameters like (LLVMTypeRef *ElementTypes, int ElementCount) are
    passed as a single sequence argument
  • “out parameters” are returned instead (using VALUES if necessary)
  • dropped the enum name suffix from enum values
    (EG, LLVMByValAttribute -> by-val)
  • predicate enums are a bit more heavily modified. We use the CL forms (=, /=,
    etc.) and prefix with UNSIGNED- or UNORDERED where appropriate.
  • instead of having both global and -IN-CONTEXT variants of many functions,
    there is a :CONTEXT keyword parameter
  • some abbreviations are expanded (EG, CallConv -> calling-convention)

  • some things should be abbreviated as methods (dispose, dump)

  • in addition to the Create/Dispose functions, you can do
      (with-objects ((memory-buffer 'memory-buffer :path #p\"/some/path\"))
        (do-stuff memory-buffer))
    and it will handle the disposal for you. This is _better_. Using the
    disposal directly can lead to leaks if you forget UNWIND-PROTECT, etc.")
  (:use #:cl #:cffi)
  (:shadow #:constantp #:type #:type-of)
  (:export #:with-object #:with-objects
           ;; modules
           #:context #:global-context #:context-dispose
           #:module
           #:with-module
           #:make-module
           #:dispose-module
           #:data-layout
           #:target
           #:add-type-name #:delete-type-name #:get-type-by-name
           #:dump-module
           ;; types
           #:type-kind
           #:type-context
           #:int1-type #:int8-type #:int16-type #:int32-type #:int64-type
           #:int-type #:width
           #:float-type #:double-type #:x86-fp80-type #:fp128-type
           #:ppc-fp128-type
           #:function-type #:var-arg-p #:return-type #:count-param-types
           #:param-types
           #:struct-type #:count-struct-element-types #:struct-element-types
           #:struct-create-named #:get-struct-name #:struct-set-body
           #:packedp
           #:array-type #:pointer-type #:vector-type
           #:element-type #:array-length #:pointer-address-space #:vector-size
           #:void-type #:label-type #:opaque-type
           #:type-handle #:refine-type #:resolve-type-handle
           #:dispose-type-handle
           ;; values
           #:type-of #:value-name #:dump-value
           #:get-operand #:set-operand #:get-num-operands
           #:const-null #:const-all-ones #:undef #:constantp #:nullp #:undefp
           #:const-pointer-null
           #:const-int #:const-real #:z-ext-value #:s-ext-value
           #:const-string #:const-array #:const-struct #:const-vector
           #:const-opcode
           #:align-of #:sign-of #:const-neg #:const-f-neg #:const-not
           #:const-add #:const-nsw-add #:const-f-add #:const-sub #:const-f-sub
           #:const-mul #:const-f-mul #:const-u-div #:const-s-div
           #:const-exact-s-div #:const-f-div #:const-u-rem #:const-s-rem
           #:const-f-rem #:const-and #:const-or #:const-xor #:const-i-cmp
           #:const-f-cmp #:const-shl #:const-l-shr #:const-a-shr #:const-gep
           #:const-in-bounds-gep #:const-trunc #:const-s-ext #:const-z-ext
           #:const-fp-trunc #:const-fp-ext #:const-ui-to-fp #:const-si-to-fp
           #:const-fp-to-ui #:const-fp-to-si #:const-pointer-to-int
           #:const-int-to-pointer #:const-bit-cast #:const-z-ext-or-bit-cast
           #:const-s-ext-or-bit-cast #:const-trunc-or-bit-cast
           #:const-pointer-cast #:const-int-cast #:const-fp-cast #:const-select
           #:const-extract-element #:const-insert-element #:const-shuffle-vector
           #:const-extract-value #:const-insert-value #:const-inline-asm
           #:block-address #:global-parent #:declarationp
           #:linkage #:section #:visibility #:alignment
           #:add-global #:named-global #:first-global #:last-global
           #:next-global #:previous-global #:delete-global #:initializer
           #:set-initializer
           #:thread-local-p #:global-constant-p
           #:add-alias
           #:add-function #:named-function #:first-function #:last-function
           #:next-function #:previous-function #:delete-function #:intrinsic-id
           #:function-calling-convention #:gc
           #:add-function-attributes #:remove-function-attributes
           #:count-params #:params #:param #:param-parent #:first-param
           #:last-param #:next-param #:previous-param
           #:add-attributes #:remove-attributes #:param-alignment
           #:basic-block-as-value #:value-is-basic-block-p
           #:value-as-basic-block #:basic-block-parent #:count-basic-blocks
           #:basic-blocks #:first-basic-block #:last-basic-block
           #:next-basic-block #:previous-basic-block #:entry-basic-block
           #:append-basic-block #:insert-basic-block #:delete-basic-block
           #:move-basic-block-before #:move-basic-block-after
           #:instruction-parent #:first-instruction #:last-instruction
           #:next-instruction #:previous-instruction
           #:instruction-calling-convention #:add-instruction-attributes
           #:remove-instruction-attributes #:instruction-param-alignment
           #:tail-call-p
           #:add-incoming #:count-incoming #:incoming-value #:incoming-block
           ;; instruction builders
           #:builder #:with-builder #:make-builder #:dispose-builder
           #:position-builder #:position-builder-before #:position-builder-at-end
           #:insertion-block #:clear-insertion-position #:insert-into-builder
           #:build-ret #:build-br #:build-cond-br #:build-switch #:build-invoke
           #:build-unwind #:build-unreachable
           #:add-case
           #:build-add #:build-nsw-add #:build-nuw-add #:build-f-add
           #:build-sub #:build-nsw-sub #:build-nuw-sub #:build-f-sub
           #:build-mul #:build-nsw-mul #:build-nuw-mul #:build-f-mul
           #:build-u-div #:build-s-div #:build-exact-s-div #:build-f-div
           #:build-u-rem #:build-s-rem #:build-f-rem
           #:build-and #:build-or #:build-xor #:build-neg
           #:build-f-neg #:build-not
           #:build-malloc #:build-array-malloc #:build-alloca
           #:build-array-alloca #:build-free #:build-load #:build-store
           #:build-gep #:build-in-bounds-gep #:build-struct-gep
           #:build-global-string #:build-global-string-pointer
           #:build-trunc #:build-s-ext #:build-z-ext #:build-fp-trunc
           #:build-fp-ext #:build-ui-to-fp #:build-si-to-fp #:build-fp-to-ui
           #:build-fp-to-si #:build-pointer-to-int #:build-int-to-pointer
           #:build-bit-cast #:build-z-ext-or-bit-cast #:build-s-ext-or-bit-cast
           #:build-trunc-or-bit-cast #:build-pointer-cast #:build-int-cast
           #:build-fp-cast
           #:build-i-cmp #:build-f-cmp
           #:build-phi #:build-call #:build-select #:build-va-arg
           #:build-extract-element #:build-insert-element #:build-shuffle-vector
           #:build-extract-value #:build-insert-value
           #:build-nullp #:build-not-null-p #:build-pointer-diff
           ;; memory buffers
           #:memory-buffer #:dispose-memory-buffer
           ;; pass managers
           #:pass-manager
           #:function-pass-manager
           #:create-pass-manager
           #:create-function-pass-manager-for-module
           #:run-pass-manager
           #:initialize-function-pass-manager
           #:run-function-pass-manager
           #:finalize-function-pass-manager
           #:dispose-pass-manager
           ;; analysis
           #:verify-module
           #:verify-function
           #:view-function-cfg
           #:view-function-cfg-only
           ;; bit-reader
           #:parse-bitcode
           #:bitcode-module
           ;; bit-writer
           #:write-bitcode-to-file-handle
           #:write-bitcode-to-file
           ;; target
           ;; NOTE: The individual INITIALIZE-*-TARGET[-INFO] functions are
           ;;       also exported, but they are created dynamically, and
           ;;       exported at the point of creation (see target.lisp)
           #:initialize-all-target-infos
           #:initialize-all-targets
           #:initialize-native-target
           #:target-data
           #:add-target-data
           #:string-representation
           #:byte-order
           #:pointer-size
           #:int-pointer-type
           #:size-of-type-in-bits
           #:storage-size-of-type
           #:abi-size-of-type
           #:abi-alignment-of-type
           #:call-frame-alignment-of-type
           #:preferred-alignment-of-type
           #:preferred-alignment-of-global
           #:element-at-offset
           #:offset-of-element
           #:invalidate-struct-layout
           #:dispose-target-data
           ;; execution-engine
           #:create-generic-value-of-int
           #:create-generic-value-of-pointer
           #:create-generic-value-of-float
           #:generic-value-of-int
           #:generic-value-of-pointer
           #:generic-value-of-float
           #:generic-value-int-width
           #:generic-value-to-int
           #:generic-value-to-pointer
           #:generic-value-to-float
           #:dispose-generic-value
           #:execution-engine
           #:interpreter
           #:jit-compiler
           #:dispose-execution-engine
           #:run-static-constructors
           #:run-static-destructors
           #:run-function-as-main
           #:run-function
           #:free-machine-code-for-function
           #:add-module
           #:remove-module
           #:find-function
           #:execution-engine-target-data
           #:add-global-mapping
           #:pointer-to-global
           ;; scalar transforms
           #:add-aggressive-dce-pass
           #:add-cfg-simplification-pass
           #:add-cond-propagation-pass
           #:add-dead-store-elimination-pass
           #:add-gvn-pass
           #:add-independent-variable-simplification-pass
           #:add-instruction-combining-pass
           #:add-jump-threading-pass
           #:add-licm-pass
           #:add-loop-deletion-pass
           #:add-loop-index-split-pass
           #:add-loop-rotate-pass
           #:add-loop-unroll-pass
           #:add-loop-unswitch-pass
           #:add-mem-cpy-opt-pass
           #:add-promote-memory-to-register-pass
           #:add-reassociate-pass
           #:add-sccp-pass
           #:add-scalar-repl-aggregates-pass
           #:add-simplify-lib-calls-pass
           #:add-tail-call-elimination-pass
           #:add-constant-propagation-pass
           #:add-demote-memory-to-register-pass))
