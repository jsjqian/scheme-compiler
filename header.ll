; ModuleID = 'header.cpp'
source_filename = "header.cpp"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

%class.hamt = type { [7 x %class.KV], i64 }
%class.KV = type { %"union.KV<key, key, 0>::Key", %"union.KV<key, key, 0>::Val" }
%"union.KV<key, key, 0>::Key" = type { i64 }
%"union.KV<key, key, 0>::Val" = type { %class.KV.0* }
%class.KV.0 = type { %"union.KV<key, key, 1>::Key", %"union.KV<key, key, 1>::Val" }
%"union.KV<key, key, 1>::Key" = type { i64 }
%"union.KV<key, key, 1>::Val" = type { %class.KV.1* }
%class.KV.1 = type { %"union.KV<key, key, 2>::Key", %"union.KV<key, key, 2>::Val" }
%"union.KV<key, key, 2>::Key" = type { i64 }
%"union.KV<key, key, 2>::Val" = type { %class.KV.2* }
%class.KV.2 = type { %"union.KV<key, key, 3>::Key", %"union.KV<key, key, 3>::Val" }
%"union.KV<key, key, 3>::Key" = type { i64 }
%"union.KV<key, key, 3>::Val" = type { %class.KV.3* }
%class.KV.3 = type { %"union.KV<key, key, 4>::Key", %"union.KV<key, key, 4>::Val" }
%"union.KV<key, key, 4>::Key" = type { i64 }
%"union.KV<key, key, 4>::Val" = type { %class.KV.4* }
%class.KV.4 = type { %"union.KV<key, key, 5>::Key", %"union.KV<key, key, 5>::Val" }
%"union.KV<key, key, 5>::Key" = type { i64 }
%"union.KV<key, key, 5>::Val" = type { %class.KV.5* }
%class.KV.5 = type { %"union.KV<key, key, 6>::Key", %"union.KV<key, key, 6>::Val" }
%"union.KV<key, key, 6>::Key" = type { i64 }
%"union.KV<key, key, 6>::Val" = type { %class.KV.6* }
%class.KV.6 = type { %"union.KV<key, key, 7>::Key", %"union.KV<key, key, 7>::Val" }
%"union.KV<key, key, 7>::Key" = type { i64 }
%"union.KV<key, key, 7>::Val" = type { %class.KV.7* }
%class.KV.7 = type { %"union.KV<key, key, 8>::Key", %"union.KV<key, key, 8>::Val" }
%"union.KV<key, key, 8>::Key" = type { i64 }
%"union.KV<key, key, 8>::Val" = type { %class.KV.8* }
%class.KV.8 = type { %"union.KV<key, key, 9>::Key", %"union.KV<key, key, 9>::Val" }
%"union.KV<key, key, 9>::Key" = type { i64 }
%"union.KV<key, key, 9>::Val" = type { %class.KV.9* }
%class.KV.9 = type { %"union.KV<key, key, 10>::Key", %"union.KV<key, key, 10>::Val" }
%"union.KV<key, key, 10>::Key" = type { i64 }
%"union.KV<key, key, 10>::Val" = type { %class.LL* }
%class.LL = type { %class.key*, %class.key*, %class.LL* }
%class.key = type { i64 }

$_ZN4hamtI3keyS0_EC2Ev = comdat any

$_ZN3keyC2Em = comdat any

$_ZNK4hamtI3keyS0_E6insertEPKS0_S3_ = comdat any

$_ZNK4hamtI3keyS0_E3getEPKS0_ = comdat any

$_ZNK4hamtI3keyS0_E6removeEPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj0EEC2Ev = comdat any

$_ZN2KVI3keyS0_Lj0EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj0EE3ValC2EPKS0_ = comdat any

$_ZNK3key4hashEv = comdat any

$_ZN2KVI3keyS0_Lj0EEC2EPKS0_S3_ = comdat any

$_ZNK3keyeqERKS_ = comdat any

$_ZN2KVI3keyS0_Lj0EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj0EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI3keyS0_Lj0EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj1EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj1EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE = comdat any

$_ZN2KVI3keyS0_Lj1EEC2EPKS0_S3_ = comdat any

$_ZN2KVI3keyS0_Lj2EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj2EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE = comdat any

$_ZN2KVI3keyS0_Lj2EEC2EPKS0_S3_ = comdat any

$_ZN2KVI3keyS0_Lj3EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj3EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE = comdat any

$_ZN2KVI3keyS0_Lj3EEC2EPKS0_S3_ = comdat any

$_ZN2KVI3keyS0_Lj4EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj4EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE = comdat any

$_ZN2KVI3keyS0_Lj4EEC2EPKS0_S3_ = comdat any

$_ZN2KVI3keyS0_Lj5EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj5EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE = comdat any

$_ZN2KVI3keyS0_Lj5EEC2EPKS0_S3_ = comdat any

$_ZN2KVI3keyS0_Lj6EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj6EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE = comdat any

$_ZN2KVI3keyS0_Lj6EEC2EPKS0_S3_ = comdat any

$_ZN2KVI3keyS0_Lj7EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj7EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE = comdat any

$_ZN2KVI3keyS0_Lj7EEC2EPKS0_S3_ = comdat any

$_ZN2KVI3keyS0_Lj8EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj8EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE = comdat any

$_ZN2KVI3keyS0_Lj8EEC2EPKS0_S3_ = comdat any

$_ZN2KVI3keyS0_Lj9EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj9EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE = comdat any

$_ZN2KVI3keyS0_Lj9EEC2EPKS0_S3_ = comdat any

$_ZN2KVI3keyS0_Lj10EE14new_inner_nodeEmPKS0_S3_mS3_S3_ = comdat any

$_ZN2KVI3keyS0_Lj10EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE = comdat any

$_ZN2KVI3keyS0_Lj10EEC2EPKS0_S3_ = comdat any

$_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_ = comdat any

$_ZN2KVI3keyS0_Lj10EEC2EmPK2LLIS0_S0_E = comdat any

$_ZN2KVI3keyS0_Lj10EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj10EE3ValC2EPK2LLIS0_S0_E = comdat any

$_ZN2KVI3keyS0_Lj9EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj9EE3ValC2EPKS_IS0_S0_Lj10EE = comdat any

$_ZN2KVI3keyS0_Lj10EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj10EE3ValC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj8EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj8EE3ValC2EPKS_IS0_S0_Lj9EE = comdat any

$_ZN2KVI3keyS0_Lj9EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj9EE3ValC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj7EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj7EE3ValC2EPKS_IS0_S0_Lj8EE = comdat any

$_ZN2KVI3keyS0_Lj8EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj8EE3ValC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj6EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj6EE3ValC2EPKS_IS0_S0_Lj7EE = comdat any

$_ZN2KVI3keyS0_Lj7EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj7EE3ValC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj5EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj5EE3ValC2EPKS_IS0_S0_Lj6EE = comdat any

$_ZN2KVI3keyS0_Lj6EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj6EE3ValC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj4EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj4EE3ValC2EPKS_IS0_S0_Lj5EE = comdat any

$_ZN2KVI3keyS0_Lj5EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj5EE3ValC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj3EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj3EE3ValC2EPKS_IS0_S0_Lj4EE = comdat any

$_ZN2KVI3keyS0_Lj4EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj4EE3ValC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj2EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj2EE3ValC2EPKS_IS0_S0_Lj3EE = comdat any

$_ZN2KVI3keyS0_Lj3EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj3EE3ValC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj1EE3KeyC2Em = comdat any

$_ZN2KVI3keyS0_Lj1EE3ValC2EPKS_IS0_S0_Lj2EE = comdat any

$_ZN2KVI3keyS0_Lj2EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj2EE3ValC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj0EE3ValC2EPKS_IS0_S0_Lj1EE = comdat any

$_ZN2KVI3keyS0_Lj1EE3KeyC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj1EE3ValC2EPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj1EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI3keyS0_Lj1EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI3keyS0_Lj2EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI3keyS0_Lj2EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI3keyS0_Lj3EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI3keyS0_Lj3EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI3keyS0_Lj4EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI3keyS0_Lj4EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI3keyS0_Lj5EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI3keyS0_Lj5EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI3keyS0_Lj6EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI3keyS0_Lj6EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI3keyS0_Lj7EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI3keyS0_Lj7EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI3keyS0_Lj8EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI3keyS0_Lj8EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI3keyS0_Lj9EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI3keyS0_Lj9EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZN2KVI3keyS0_Lj10EE11update_nodeEPKS1_jjRS2_ = comdat any

$_ZN2KVI3keyS0_Lj10EE12insert_innerERKS1_mPKS0_S5_Pm = comdat any

$_ZNK2LLI3keyS0_E6insertEPKS0_S3_Pm = comdat any

$_ZN2KVI3keyS0_Lj0EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj1EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj2EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj3EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj4EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj5EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj6EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj7EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj8EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj9EE10inner_findERKS1_mPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj10EE10inner_findERKS1_mPKS0_ = comdat any

$_ZNK2LLI3keyS0_E4findEPKS0_ = comdat any

$_ZN2KVI3keyS0_Lj0EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj0EEeqERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj0EEC2ERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj1EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj1EEeqERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj2EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj2EEeqERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj3EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj3EEeqERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj4EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj4EEeqERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj5EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj5EEeqERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj6EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj6EEeqERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj7EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj7EEeqERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj8EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj8EEeqERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj9EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj9EEeqERKS1_ = comdat any

$_ZN2KVI3keyS0_Lj10EE12remove_innerERKS1_mPKS0_Pm = comdat any

$_ZNK2KVI3keyS0_Lj10EEeqERKS1_ = comdat any

$_ZNK2LLI3keyS0_E6removeEPKS0_Pm = comdat any

@.str = private unnamed_addr constant [25 x i8] c"library run-time error: \00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.2 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.3 = private unnamed_addr constant [5 x i8] c"%lu\0A\00", align 1
@.str.4 = private unnamed_addr constant [68 x i8] c"Expected value: null (in expect_args0). Prim cannot take arguments.\00", align 1
@.str.5 = private unnamed_addr constant [79 x i8] c"Expected cons value (in expect_args1). Prim applied on an empty argument list.\00", align 1
@.str.6 = private unnamed_addr constant [70 x i8] c"Expected null value (in expect_args1). Prim can only take 1 argument.\00", align 1
@.str.7 = private unnamed_addr constant [37 x i8] c"Expected a cons value. (expect_cons)\00", align 1
@.str.8 = private unnamed_addr constant [51 x i8] c"Expected a vector or special value. (expect_other)\00", align 1
@.str.9 = private unnamed_addr constant [27 x i8] c"'\22Error: Key not in hash.\22\00", align 1
@.str.10 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.11 = private unnamed_addr constant [13 x i8] c"#<procedure>\00", align 1
@.str.12 = private unnamed_addr constant [2 x i8] c"(\00", align 1
@.str.13 = private unnamed_addr constant [4 x i8] c" . \00", align 1
@.str.14 = private unnamed_addr constant [2 x i8] c")\00", align 1
@.str.15 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.16 = private unnamed_addr constant [5 x i8] c"\22%s\22\00", align 1
@.str.17 = private unnamed_addr constant [3 x i8] c"#(\00", align 1
@.str.18 = private unnamed_addr constant [2 x i8] c",\00", align 1
@.str.19 = private unnamed_addr constant [36 x i8] c"(print.. v); unrecognized value %lu\00", align 1
@.str.20 = private unnamed_addr constant [4 x i8] c"'()\00", align 1
@.str.21 = private unnamed_addr constant [3 x i8] c"#t\00", align 1
@.str.22 = private unnamed_addr constant [3 x i8] c"#f\00", align 1
@.str.23 = private unnamed_addr constant [8 x i8] c"#<void>\00", align 1
@.str.24 = private unnamed_addr constant [3 x i8] c"'(\00", align 1
@.str.25 = private unnamed_addr constant [4 x i8] c"'%s\00", align 1
@.str.26 = private unnamed_addr constant [34 x i8] c"(print v); unrecognized value %lu\00", align 1
@.str.27 = private unnamed_addr constant [49 x i8] c"first argument to make-vector must be an integer\00", align 1
@.str.28 = private unnamed_addr constant [39 x i8] c"prim applied on more than 2 arguments.\00", align 1
@.str.29 = private unnamed_addr constant [49 x i8] c"second argument to vector-ref must be an integer\00", align 1
@.str.30 = private unnamed_addr constant [46 x i8] c"first argument to vector-ref must be a vector\00", align 1
@.str.31 = private unnamed_addr constant [46 x i8] c"vector-ref not given a properly formed vector\00", align 1
@.str.32 = private unnamed_addr constant [48 x i8] c"first argument to vector-ref must be an integer\00", align 1
@.str.33 = private unnamed_addr constant [34 x i8] c"(prim + a b); a is not an integer\00", align 1
@.str.34 = private unnamed_addr constant [34 x i8] c"(prim + a b); b is not an integer\00", align 1
@.str.35 = private unnamed_addr constant [36 x i8] c"Tried to apply + on non list value.\00", align 1
@.str.36 = private unnamed_addr constant [34 x i8] c"(prim - a b); b is not an integer\00", align 1
@.str.37 = private unnamed_addr constant [34 x i8] c"(prim * a b); a is not an integer\00", align 1
@.str.38 = private unnamed_addr constant [34 x i8] c"(prim * a b); b is not an integer\00", align 1
@.str.39 = private unnamed_addr constant [34 x i8] c"(prim / a b); a is not an integer\00", align 1
@.str.40 = private unnamed_addr constant [34 x i8] c"(prim / a b); b is not an integer\00", align 1
@.str.41 = private unnamed_addr constant [34 x i8] c"(prim = a b); a is not an integer\00", align 1
@.str.42 = private unnamed_addr constant [34 x i8] c"(prim = a b); b is not an integer\00", align 1
@.str.43 = private unnamed_addr constant [34 x i8] c"(prim < a b); a is not an integer\00", align 1
@.str.44 = private unnamed_addr constant [34 x i8] c"(prim < a b); b is not an integer\00", align 1
@.str.45 = private unnamed_addr constant [35 x i8] c"(prim <= a b); a is not an integer\00", align 1
@.str.46 = private unnamed_addr constant [35 x i8] c"(prim <= a b); b is not an integer\00", align 1

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64* @alloc(i64) #0 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = call noalias i8* @malloc(i64 %3) #8
  %5 = bitcast i8* %4 to i64*
  ret i64* %5
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #1

; Function Attrs: noinline optnone sspstrong uwtable
define void @fatal_err(i8*) #2 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str, i32 0, i32 0))
  %4 = load i8*, i8** %2, align 8
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0), i8* %4)
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 1) #9
  unreachable
                                                  ; No predecessors!
  ret void
}

declare i32 @printf(i8*, ...) #3

; Function Attrs: noreturn nounwind
declare void @exit(i32) #4

; Function Attrs: noinline optnone sspstrong uwtable
define void @print_u64(i64) #2 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.3, i32 0, i32 0), i64 %3)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @expect_args0(i64) #2 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp ne i64 %3, 0
  br i1 %4, label %5, label %6

; <label>:5:                                      ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([68 x i8], [68 x i8]* @.str.4, i32 0, i32 0))
  br label %6

; <label>:6:                                      ; preds = %5, %1
  ret i64 0
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @expect_args1(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64*, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 1
  br i1 %6, label %7, label %8

; <label>:7:                                      ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([79 x i8], [79 x i8]* @.str.5, i32 0, i32 0))
  br label %8

; <label>:8:                                      ; preds = %7, %1
  %9 = load i64, i64* %2, align 8
  %10 = and i64 %9, -8
  %11 = inttoptr i64 %10 to i64*
  store i64* %11, i64** %3, align 8
  %12 = load i64*, i64** %3, align 8
  %13 = getelementptr inbounds i64, i64* %12, i64 1
  %14 = load i64, i64* %13, align 8
  %15 = icmp ne i64 %14, 0
  br i1 %15, label %16, label %17

; <label>:16:                                     ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([70 x i8], [70 x i8]* @.str.6, i32 0, i32 0))
  br label %17

; <label>:17:                                     ; preds = %16, %8
  %18 = load i64*, i64** %3, align 8
  %19 = getelementptr inbounds i64, i64* %18, i64 0
  %20 = load i64, i64* %19, align 8
  ret i64 %20
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @expect_cons(i64, i64*) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64*, align 8
  %5 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  store i64* %1, i64** %4, align 8
  %6 = load i64, i64* %3, align 8
  %7 = and i64 %6, 7
  %8 = icmp ne i64 %7, 1
  br i1 %8, label %9, label %10

; <label>:9:                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.7, i32 0, i32 0))
  br label %10

; <label>:10:                                     ; preds = %9, %2
  %11 = load i64, i64* %3, align 8
  %12 = and i64 %11, -8
  %13 = inttoptr i64 %12 to i64*
  store i64* %13, i64** %5, align 8
  %14 = load i64*, i64** %5, align 8
  %15 = getelementptr inbounds i64, i64* %14, i64 1
  %16 = load i64, i64* %15, align 8
  %17 = load i64*, i64** %4, align 8
  store i64 %16, i64* %17, align 8
  %18 = load i64*, i64** %5, align 8
  %19 = getelementptr inbounds i64, i64* %18, i64 0
  %20 = load i64, i64* %19, align 8
  ret i64 %20
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @expect_other(i64, i64*) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64*, align 8
  %5 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  store i64* %1, i64** %4, align 8
  %6 = load i64, i64* %3, align 8
  %7 = and i64 %6, 7
  %8 = icmp ne i64 %7, 6
  br i1 %8, label %9, label %10

; <label>:9:                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([51 x i8], [51 x i8]* @.str.8, i32 0, i32 0))
  br label %10

; <label>:10:                                     ; preds = %9, %2
  %11 = load i64, i64* %3, align 8
  %12 = and i64 %11, -8
  %13 = inttoptr i64 %12 to i64*
  store i64* %13, i64** %5, align 8
  %14 = load i64*, i64** %5, align 8
  %15 = getelementptr inbounds i64, i64* %14, i64 1
  %16 = load i64, i64* %15, align 8
  %17 = load i64*, i64** %4, align 8
  store i64 %16, i64* %17, align 8
  %18 = load i64*, i64** %5, align 8
  %19 = getelementptr inbounds i64, i64* %18, i64 0
  %20 = load i64, i64* %19, align 8
  ret i64 %20
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @const_init_int(i64) #0 {
  %2 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = trunc i64 %3 to i32
  %5 = zext i32 %4 to i64
  %6 = shl i64 %5, 32
  %7 = or i64 %6, 2
  ret i64 %7
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @const_init_void() #0 {
  ret i64 39
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @const_init_null() #0 {
  ret i64 0
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @const_init_true() #0 {
  ret i64 31
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @const_init_false() #0 {
  ret i64 15
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @const_init_string(i8*) #0 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = load i8*, i8** %2, align 8
  %4 = ptrtoint i8* %3 to i64
  %5 = or i64 %4, 3
  ret i64 %5
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @const_init_symbol(i8*) #0 {
  %2 = alloca i8*, align 8
  store i8* %0, i8** %2, align 8
  %3 = load i8*, i8** %2, align 8
  %4 = ptrtoint i8* %3 to i64
  %5 = or i64 %4, 4
  ret i64 %5
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_set() #2 {
  %1 = alloca %class.hamt*, align 8
  %2 = call noalias i8* @malloc(i64 120) #8
  %3 = bitcast i8* %2 to %class.hamt*
  %4 = bitcast %class.hamt* %3 to i8*
  %5 = bitcast i8* %4 to %class.hamt*
  call void @_ZN4hamtI3keyS0_EC2Ev(%class.hamt* %5)
  store %class.hamt* %5, %class.hamt** %1, align 8
  %6 = load %class.hamt*, %class.hamt** %1, align 8
  %7 = bitcast %class.hamt* %6 to i64*
  %8 = ptrtoint i64* %7 to i64
  %9 = or i64 %8, 6
  ret i64 %9
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN4hamtI3keyS0_EC2Ev(%class.hamt*) unnamed_addr #2 comdat align 2 {
  %2 = alloca %class.hamt*, align 8
  store %class.hamt* %0, %class.hamt** %2, align 8
  %3 = load %class.hamt*, %class.hamt** %2, align 8
  %4 = getelementptr inbounds %class.hamt, %class.hamt* %3, i32 0, i32 0
  %5 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %4, i64 0, i64 0
  %6 = getelementptr inbounds %class.KV, %class.KV* %5, i64 7
  br label %7

; <label>:7:                                      ; preds = %7, %1
  %8 = phi %class.KV* [ %5, %1 ], [ %9, %7 ]
  call void @_ZN2KVI3keyS0_Lj0EEC2Ev(%class.KV* %8)
  %9 = getelementptr inbounds %class.KV, %class.KV* %8, i64 1
  %10 = icmp eq %class.KV* %9, %6
  br i1 %10, label %11, label %7

; <label>:11:                                     ; preds = %7
  %12 = getelementptr inbounds %class.hamt, %class.hamt* %3, i32 0, i32 1
  store i64 0, i64* %12, align 8
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_set_45add(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hamt*, align 8
  %6 = alloca %class.key*, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %7 = load i64, i64* %3, align 8
  %8 = and i64 %7, -8
  %9 = inttoptr i64 %8 to i64*
  %10 = bitcast i64* %9 to %class.hamt*
  store %class.hamt* %10, %class.hamt** %5, align 8
  %11 = call noalias i8* @malloc(i64 8) #8
  %12 = bitcast i8* %11 to %class.key*
  %13 = bitcast %class.key* %12 to i8*
  %14 = bitcast i8* %13 to %class.key*
  %15 = load i64, i64* %4, align 8
  call void @_ZN3keyC2Em(%class.key* %14, i64 %15)
  store %class.key* %14, %class.key** %6, align 8
  %16 = load %class.hamt*, %class.hamt** %5, align 8
  %17 = load %class.key*, %class.key** %6, align 8
  %18 = load %class.key*, %class.key** %6, align 8
  %19 = call %class.hamt* @_ZNK4hamtI3keyS0_E6insertEPKS0_S3_(%class.hamt* %16, %class.key* %17, %class.key* %18)
  %20 = ptrtoint %class.hamt* %19 to i64
  %21 = or i64 %20, 6
  ret i64 %21
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN3keyC2Em(%class.key*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.key*, align 8
  %4 = alloca i64, align 8
  store %class.key* %0, %class.key** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %class.key*, %class.key** %3, align 8
  %6 = getelementptr inbounds %class.key, %class.key* %5, i32 0, i32 0
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.hamt* @_ZNK4hamtI3keyS0_E6insertEPKS0_S3_(%class.hamt*, %class.key*, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.hamt*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  %7 = alloca i64, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.hamt*, align 8
  store %class.hamt* %0, %class.hamt** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %10 = load %class.hamt*, %class.hamt** %4, align 8
  %11 = load %class.key*, %class.key** %5, align 8
  %12 = call i64 @_ZNK3key4hashEv(%class.key* %11)
  store i64 %12, i64* %7, align 8
  %13 = load i64, i64* %7, align 8
  %14 = and i64 %13, 15
  %15 = urem i64 %14, 7
  store i64 %15, i64* %8, align 8
  %16 = call noalias i8* @malloc(i64 120) #8
  %17 = bitcast i8* %16 to %class.hamt*
  store %class.hamt* %17, %class.hamt** %9, align 8
  %18 = load %class.hamt*, %class.hamt** %9, align 8
  %19 = bitcast %class.hamt* %18 to i8*
  %20 = bitcast %class.hamt* %10 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %19, i8* %20, i64 120, i32 8, i1 false)
  %21 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %22 = load i64, i64* %8, align 8
  %23 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %21, i64 0, i64 %22
  %24 = getelementptr inbounds %class.KV, %class.KV* %23, i32 0, i32 0
  %25 = bitcast %"union.KV<key, key, 0>::Key"* %24 to i64*
  %26 = load i64, i64* %25, align 8
  %27 = icmp eq i64 %26, 0
  br i1 %27, label %28, label %41

; <label>:28:                                     ; preds = %3
  %29 = load %class.hamt*, %class.hamt** %9, align 8
  %30 = getelementptr inbounds %class.hamt, %class.hamt* %29, i32 0, i32 0
  %31 = load i64, i64* %8, align 8
  %32 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %30, i64 0, i64 %31
  %33 = bitcast %class.KV* %32 to i8*
  %34 = bitcast i8* %33 to %class.KV*
  %35 = load %class.key*, %class.key** %5, align 8
  %36 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2EPKS0_S3_(%class.KV* %34, %class.key* %35, %class.key* %36)
  %37 = load %class.hamt*, %class.hamt** %9, align 8
  %38 = getelementptr inbounds %class.hamt, %class.hamt* %37, i32 0, i32 1
  %39 = load i64, i64* %38, align 8
  %40 = add i64 %39, 1
  store i64 %40, i64* %38, align 8
  br label %121

; <label>:41:                                     ; preds = %3
  %42 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %43 = load i64, i64* %8, align 8
  %44 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %42, i64 0, i64 %43
  %45 = getelementptr inbounds %class.KV, %class.KV* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 0>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %104

; <label>:50:                                     ; preds = %41
  %51 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %52 = load i64, i64* %8, align 8
  %53 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %51, i64 0, i64 %52
  %54 = getelementptr inbounds %class.KV, %class.KV* %53, i32 0, i32 0
  %55 = bitcast %"union.KV<key, key, 0>::Key"* %54 to %class.key**
  %56 = load %class.key*, %class.key** %55, align 8
  %57 = load %class.key*, %class.key** %5, align 8
  %58 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %56, %class.key* dereferenceable(8) %57)
  br i1 %58, label %59, label %68

; <label>:59:                                     ; preds = %50
  %60 = load %class.hamt*, %class.hamt** %9, align 8
  %61 = getelementptr inbounds %class.hamt, %class.hamt* %60, i32 0, i32 0
  %62 = load i64, i64* %8, align 8
  %63 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %61, i64 0, i64 %62
  %64 = bitcast %class.KV* %63 to i8*
  %65 = bitcast i8* %64 to %class.KV*
  %66 = load %class.key*, %class.key** %5, align 8
  %67 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2EPKS0_S3_(%class.KV* %65, %class.key* %66, %class.key* %67)
  br label %103

; <label>:68:                                     ; preds = %50
  %69 = load %class.hamt*, %class.hamt** %9, align 8
  %70 = getelementptr inbounds %class.hamt, %class.hamt* %69, i32 0, i32 1
  %71 = load i64, i64* %70, align 8
  %72 = add i64 %71, 1
  store i64 %72, i64* %70, align 8
  %73 = load %class.hamt*, %class.hamt** %9, align 8
  %74 = getelementptr inbounds %class.hamt, %class.hamt* %73, i32 0, i32 0
  %75 = load i64, i64* %8, align 8
  %76 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %74, i64 0, i64 %75
  %77 = bitcast %class.KV* %76 to i8*
  %78 = bitcast i8* %77 to %class.KV*
  %79 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %80 = load i64, i64* %8, align 8
  %81 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %79, i64 0, i64 %80
  %82 = getelementptr inbounds %class.KV, %class.KV* %81, i32 0, i32 0
  %83 = bitcast %"union.KV<key, key, 0>::Key"* %82 to %class.key**
  %84 = load %class.key*, %class.key** %83, align 8
  %85 = call i64 @_ZNK3key4hashEv(%class.key* %84)
  %86 = lshr i64 %85, 4
  %87 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %88 = load i64, i64* %8, align 8
  %89 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %87, i64 0, i64 %88
  %90 = getelementptr inbounds %class.KV, %class.KV* %89, i32 0, i32 0
  %91 = bitcast %"union.KV<key, key, 0>::Key"* %90 to %class.key**
  %92 = load %class.key*, %class.key** %91, align 8
  %93 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %94 = load i64, i64* %8, align 8
  %95 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %93, i64 0, i64 %94
  %96 = getelementptr inbounds %class.KV, %class.KV* %95, i32 0, i32 1
  %97 = bitcast %"union.KV<key, key, 0>::Val"* %96 to %class.key**
  %98 = load %class.key*, %class.key** %97, align 8
  %99 = load i64, i64* %7, align 8
  %100 = lshr i64 %99, 4
  %101 = load %class.key*, %class.key** %5, align 8
  %102 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj0EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV* sret %78, i64 %86, %class.key* %92, %class.key* %98, i64 %100, %class.key* %101, %class.key* %102)
  br label %103

; <label>:103:                                    ; preds = %68, %59
  br label %120

; <label>:104:                                    ; preds = %41
  %105 = load %class.hamt*, %class.hamt** %9, align 8
  %106 = getelementptr inbounds %class.hamt, %class.hamt* %105, i32 0, i32 0
  %107 = load i64, i64* %8, align 8
  %108 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %106, i64 0, i64 %107
  %109 = bitcast %class.KV* %108 to i8*
  %110 = bitcast i8* %109 to %class.KV*
  %111 = getelementptr inbounds %class.hamt, %class.hamt* %10, i32 0, i32 0
  %112 = load i64, i64* %8, align 8
  %113 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %111, i64 0, i64 %112
  %114 = load i64, i64* %7, align 8
  %115 = lshr i64 %114, 4
  %116 = load %class.key*, %class.key** %5, align 8
  %117 = load %class.key*, %class.key** %6, align 8
  %118 = load %class.hamt*, %class.hamt** %9, align 8
  %119 = getelementptr inbounds %class.hamt, %class.hamt* %118, i32 0, i32 1
  call void @_ZN2KVI3keyS0_Lj0EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV* sret %110, %class.KV* dereferenceable(16) %113, i64 %115, %class.key* %116, %class.key* %117, i64* %119)
  br label %120

; <label>:120:                                    ; preds = %104, %103
  br label %121

; <label>:121:                                    ; preds = %120, %28
  %122 = load %class.hamt*, %class.hamt** %9, align 8
  ret %class.hamt* %122
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_set_45remove(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hamt*, align 8
  %6 = alloca %class.key*, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %7 = load i64, i64* %3, align 8
  %8 = and i64 %7, -8
  %9 = inttoptr i64 %8 to i64*
  %10 = bitcast i64* %9 to %class.hamt*
  store %class.hamt* %10, %class.hamt** %5, align 8
  %11 = call noalias i8* @malloc(i64 8) #8
  %12 = bitcast i8* %11 to %class.key*
  %13 = bitcast %class.key* %12 to i8*
  %14 = bitcast i8* %13 to %class.key*
  %15 = load i64, i64* %4, align 8
  call void @_ZN3keyC2Em(%class.key* %14, i64 %15)
  store %class.key* %14, %class.key** %6, align 8
  %16 = load %class.hamt*, %class.hamt** %5, align 8
  %17 = load %class.key*, %class.key** %6, align 8
  %18 = call %class.hamt* @_ZNK4hamtI3keyS0_E6insertEPKS0_S3_(%class.hamt* %16, %class.key* %17, %class.key* null)
  %19 = bitcast %class.hamt* %18 to i64*
  %20 = ptrtoint i64* %19 to i64
  %21 = or i64 %20, 6
  ret i64 %21
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_set_45member_63(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.hamt*, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca %class.key*, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %9 = load i64, i64* %4, align 8
  %10 = and i64 %9, -8
  %11 = inttoptr i64 %10 to i64*
  %12 = bitcast i64* %11 to %class.hamt*
  store %class.hamt* %12, %class.hamt** %6, align 8
  %13 = call noalias i8* @malloc(i64 8) #8
  %14 = bitcast i8* %13 to %class.key*
  %15 = bitcast %class.key* %14 to i8*
  %16 = bitcast i8* %15 to %class.key*
  %17 = load i64, i64* %5, align 8
  call void @_ZN3keyC2Em(%class.key* %16, i64 %17)
  store %class.key* %16, %class.key** %7, align 8
  %18 = load %class.hamt*, %class.hamt** %6, align 8
  %19 = load %class.key*, %class.key** %7, align 8
  %20 = call %class.key* @_ZNK4hamtI3keyS0_E3getEPKS0_(%class.hamt* %18, %class.key* %19)
  store %class.key* %20, %class.key** %8, align 8
  %21 = load %class.key*, %class.key** %8, align 8
  %22 = icmp ne %class.key* %21, null
  br i1 %22, label %23, label %24

; <label>:23:                                     ; preds = %2
  store i64 31, i64* %3, align 8
  br label %25

; <label>:24:                                     ; preds = %2
  store i64 15, i64* %3, align 8
  br label %25

; <label>:25:                                     ; preds = %24, %23
  %26 = load i64, i64* %3, align 8
  ret i64 %26
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZNK4hamtI3keyS0_E3getEPKS0_(%class.hamt*, %class.key*) #2 comdat align 2 {
  %3 = alloca %class.key*, align 8
  %4 = alloca %class.hamt*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  store %class.hamt* %0, %class.hamt** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  %8 = load %class.hamt*, %class.hamt** %4, align 8
  %9 = load %class.key*, %class.key** %5, align 8
  %10 = call i64 @_ZNK3key4hashEv(%class.key* %9)
  store i64 %10, i64* %6, align 8
  %11 = load i64, i64* %6, align 8
  %12 = and i64 %11, 15
  %13 = urem i64 %12, 7
  store i64 %13, i64* %7, align 8
  %14 = getelementptr inbounds %class.hamt, %class.hamt* %8, i32 0, i32 0
  %15 = load i64, i64* %7, align 8
  %16 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %14, i64 0, i64 %15
  %17 = getelementptr inbounds %class.KV, %class.KV* %16, i32 0, i32 0
  %18 = bitcast %"union.KV<key, key, 0>::Key"* %17 to i64*
  %19 = load i64, i64* %18, align 8
  %20 = icmp eq i64 %19, 0
  br i1 %20, label %21, label %22

; <label>:21:                                     ; preds = %2
  store %class.key* null, %class.key** %3, align 8
  br label %56

; <label>:22:                                     ; preds = %2
  %23 = getelementptr inbounds %class.hamt, %class.hamt* %8, i32 0, i32 0
  %24 = load i64, i64* %7, align 8
  %25 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %23, i64 0, i64 %24
  %26 = getelementptr inbounds %class.KV, %class.KV* %25, i32 0, i32 0
  %27 = bitcast %"union.KV<key, key, 0>::Key"* %26 to i64*
  %28 = load i64, i64* %27, align 8
  %29 = and i64 %28, 1
  %30 = icmp eq i64 %29, 0
  br i1 %30, label %31, label %48

; <label>:31:                                     ; preds = %22
  %32 = getelementptr inbounds %class.hamt, %class.hamt* %8, i32 0, i32 0
  %33 = load i64, i64* %7, align 8
  %34 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %32, i64 0, i64 %33
  %35 = getelementptr inbounds %class.KV, %class.KV* %34, i32 0, i32 0
  %36 = bitcast %"union.KV<key, key, 0>::Key"* %35 to %class.key**
  %37 = load %class.key*, %class.key** %36, align 8
  %38 = load %class.key*, %class.key** %5, align 8
  %39 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %37, %class.key* dereferenceable(8) %38)
  br i1 %39, label %40, label %47

; <label>:40:                                     ; preds = %31
  %41 = getelementptr inbounds %class.hamt, %class.hamt* %8, i32 0, i32 0
  %42 = load i64, i64* %7, align 8
  %43 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %41, i64 0, i64 %42
  %44 = getelementptr inbounds %class.KV, %class.KV* %43, i32 0, i32 1
  %45 = bitcast %"union.KV<key, key, 0>::Val"* %44 to %class.key**
  %46 = load %class.key*, %class.key** %45, align 8
  store %class.key* %46, %class.key** %3, align 8
  br label %56

; <label>:47:                                     ; preds = %31
  store %class.key* null, %class.key** %3, align 8
  br label %56

; <label>:48:                                     ; preds = %22
  %49 = getelementptr inbounds %class.hamt, %class.hamt* %8, i32 0, i32 0
  %50 = load i64, i64* %7, align 8
  %51 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %49, i64 0, i64 %50
  %52 = load i64, i64* %6, align 8
  %53 = lshr i64 %52, 4
  %54 = load %class.key*, %class.key** %5, align 8
  %55 = call %class.key* @_ZN2KVI3keyS0_Lj0EE10inner_findERKS1_mPKS0_(%class.KV* dereferenceable(16) %51, i64 %53, %class.key* %54)
  store %class.key* %55, %class.key** %3, align 8
  br label %56

; <label>:56:                                     ; preds = %48, %47, %40, %21
  %57 = load %class.key*, %class.key** %3, align 8
  ret %class.key* %57
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_hash() #2 {
  %1 = alloca %class.hamt*, align 8
  %2 = call noalias i8* @malloc(i64 120) #8
  %3 = bitcast i8* %2 to %class.hamt*
  %4 = bitcast %class.hamt* %3 to i8*
  %5 = bitcast i8* %4 to %class.hamt*
  call void @_ZN4hamtI3keyS0_EC2Ev(%class.hamt* %5)
  store %class.hamt* %5, %class.hamt** %1, align 8
  %6 = load %class.hamt*, %class.hamt** %1, align 8
  %7 = bitcast %class.hamt* %6 to i64*
  %8 = ptrtoint i64* %7 to i64
  %9 = or i64 %8, 6
  ret i64 %9
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_hash_45ref(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hamt*, align 8
  %6 = alloca %class.key*, align 8
  %7 = alloca %class.key*, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = and i64 %8, -8
  %10 = inttoptr i64 %9 to i64*
  %11 = bitcast i64* %10 to %class.hamt*
  store %class.hamt* %11, %class.hamt** %5, align 8
  %12 = call noalias i8* @malloc(i64 8) #8
  %13 = bitcast i8* %12 to %class.key*
  %14 = bitcast %class.key* %13 to i8*
  %15 = bitcast i8* %14 to %class.key*
  %16 = load i64, i64* %4, align 8
  call void @_ZN3keyC2Em(%class.key* %15, i64 %16)
  store %class.key* %15, %class.key** %6, align 8
  %17 = load %class.hamt*, %class.hamt** %5, align 8
  %18 = load %class.key*, %class.key** %6, align 8
  %19 = call %class.key* @_ZNK4hamtI3keyS0_E3getEPKS0_(%class.hamt* %17, %class.key* %18)
  store %class.key* %19, %class.key** %7, align 8
  %20 = load %class.key*, %class.key** %7, align 8
  %21 = icmp eq %class.key* %20, null
  br i1 %21, label %22, label %25

; <label>:22:                                     ; preds = %2
  %23 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([27 x i8], [27 x i8]* @.str.9, i32 0, i32 0))
  %24 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 0) #9
  unreachable

; <label>:25:                                     ; preds = %2
  %26 = load %class.key*, %class.key** %7, align 8
  %27 = getelementptr inbounds %class.key, %class.key* %26, i32 0, i32 0
  %28 = load i64, i64* %27, align 8
  ret i64 %28
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_hash_45set(i64, i64, i64) #2 {
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.hamt*, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.hamt*, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  store i64 %2, i64* %6, align 8
  %11 = load i64, i64* %4, align 8
  %12 = and i64 %11, -8
  %13 = inttoptr i64 %12 to i64*
  %14 = bitcast i64* %13 to %class.hamt*
  store %class.hamt* %14, %class.hamt** %7, align 8
  %15 = call noalias i8* @malloc(i64 8) #8
  %16 = bitcast i8* %15 to %class.key*
  %17 = bitcast %class.key* %16 to i8*
  %18 = bitcast i8* %17 to %class.key*
  %19 = load i64, i64* %5, align 8
  call void @_ZN3keyC2Em(%class.key* %18, i64 %19)
  store %class.key* %18, %class.key** %8, align 8
  %20 = call noalias i8* @malloc(i64 8) #8
  %21 = bitcast i8* %20 to %class.key*
  %22 = bitcast %class.key* %21 to i8*
  %23 = bitcast i8* %22 to %class.key*
  %24 = load i64, i64* %6, align 8
  call void @_ZN3keyC2Em(%class.key* %23, i64 %24)
  store %class.key* %23, %class.key** %9, align 8
  %25 = load %class.hamt*, %class.hamt** %7, align 8
  %26 = load %class.key*, %class.key** %8, align 8
  %27 = load %class.key*, %class.key** %9, align 8
  %28 = call %class.hamt* @_ZNK4hamtI3keyS0_E6insertEPKS0_S3_(%class.hamt* %25, %class.key* %26, %class.key* %27)
  store %class.hamt* %28, %class.hamt** %10, align 8
  %29 = load %class.hamt*, %class.hamt** %10, align 8
  %30 = bitcast %class.hamt* %29 to i64*
  %31 = ptrtoint i64* %30 to i64
  %32 = or i64 %31, 6
  ret i64 %32
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_hash_45remove(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca %class.hamt*, align 8
  %6 = alloca %class.key*, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %7 = load i64, i64* %3, align 8
  %8 = and i64 %7, -8
  %9 = inttoptr i64 %8 to i64*
  %10 = bitcast i64* %9 to %class.hamt*
  store %class.hamt* %10, %class.hamt** %5, align 8
  %11 = call noalias i8* @malloc(i64 8) #8
  %12 = bitcast i8* %11 to %class.key*
  %13 = bitcast %class.key* %12 to i8*
  %14 = bitcast i8* %13 to %class.key*
  %15 = load i64, i64* %4, align 8
  call void @_ZN3keyC2Em(%class.key* %14, i64 %15)
  store %class.key* %14, %class.key** %6, align 8
  %16 = load %class.hamt*, %class.hamt** %5, align 8
  %17 = load %class.key*, %class.key** %6, align 8
  %18 = call %class.hamt* @_ZNK4hamtI3keyS0_E6removeEPKS0_(%class.hamt* %16, %class.key* %17)
  %19 = ptrtoint %class.hamt* %18 to i64
  %20 = or i64 %19, 6
  ret i64 %20
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.hamt* @_ZNK4hamtI3keyS0_E6removeEPKS0_(%class.hamt*, %class.key*) #2 comdat align 2 {
  %3 = alloca %class.hamt*, align 8
  %4 = alloca %class.hamt*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca i64, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.hamt*, align 8
  %9 = alloca i64, align 8
  %10 = alloca %class.KV, align 8
  %11 = alloca %class.hamt*, align 8
  store %class.hamt* %0, %class.hamt** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  %12 = load %class.hamt*, %class.hamt** %4, align 8
  %13 = load %class.key*, %class.key** %5, align 8
  %14 = call i64 @_ZNK3key4hashEv(%class.key* %13)
  store i64 %14, i64* %6, align 8
  %15 = load i64, i64* %6, align 8
  %16 = and i64 %15, 15
  %17 = urem i64 %16, 7
  store i64 %17, i64* %7, align 8
  %18 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 0
  %19 = load i64, i64* %7, align 8
  %20 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %18, i64 0, i64 %19
  %21 = getelementptr inbounds %class.KV, %class.KV* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 0>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = icmp eq i64 %23, 0
  br i1 %24, label %25, label %26

; <label>:25:                                     ; preds = %2
  store %class.hamt* %12, %class.hamt** %3, align 8
  br label %91

; <label>:26:                                     ; preds = %2
  %27 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 0
  %28 = load i64, i64* %7, align 8
  %29 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %27, i64 0, i64 %28
  %30 = getelementptr inbounds %class.KV, %class.KV* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 0>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = and i64 %32, 1
  %34 = icmp eq i64 %33, 0
  br i1 %34, label %35, label %61

; <label>:35:                                     ; preds = %26
  %36 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 0
  %37 = load i64, i64* %7, align 8
  %38 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %36, i64 0, i64 %37
  %39 = getelementptr inbounds %class.KV, %class.KV* %38, i32 0, i32 0
  %40 = bitcast %"union.KV<key, key, 0>::Key"* %39 to %class.key**
  %41 = load %class.key*, %class.key** %40, align 8
  %42 = load %class.key*, %class.key** %5, align 8
  %43 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %41, %class.key* dereferenceable(8) %42)
  br i1 %43, label %44, label %60

; <label>:44:                                     ; preds = %35
  %45 = call noalias i8* @malloc(i64 120) #8
  %46 = bitcast i8* %45 to %class.hamt*
  store %class.hamt* %46, %class.hamt** %8, align 8
  %47 = load %class.hamt*, %class.hamt** %8, align 8
  %48 = bitcast %class.hamt* %47 to i8*
  %49 = bitcast %class.hamt* %12 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %48, i8* %49, i64 120, i32 8, i1 false)
  %50 = load %class.hamt*, %class.hamt** %8, align 8
  %51 = load i64, i64* %7, align 8
  %52 = getelementptr inbounds %class.hamt, %class.hamt* %50, i64 %51
  %53 = bitcast %class.hamt* %52 to i8*
  %54 = bitcast i8* %53 to %class.KV*
  call void @_ZN2KVI3keyS0_Lj0EEC2EPKS0_S3_(%class.KV* %54, %class.key* null, %class.key* null)
  %55 = load %class.hamt*, %class.hamt** %8, align 8
  %56 = getelementptr inbounds %class.hamt, %class.hamt* %55, i32 0, i32 1
  %57 = load i64, i64* %56, align 8
  %58 = add i64 %57, -1
  store i64 %58, i64* %56, align 8
  %59 = load %class.hamt*, %class.hamt** %8, align 8
  store %class.hamt* %59, %class.hamt** %3, align 8
  br label %91

; <label>:60:                                     ; preds = %35
  store %class.hamt* %12, %class.hamt** %3, align 8
  br label %91

; <label>:61:                                     ; preds = %26
  %62 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 1
  %63 = load i64, i64* %62, align 8
  store i64 %63, i64* %9, align 8
  %64 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 0
  %65 = load i64, i64* %7, align 8
  %66 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %64, i64 0, i64 %65
  %67 = load i64, i64* %6, align 8
  %68 = lshr i64 %67, 4
  %69 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj0EE12remove_innerERKS1_mPKS0_Pm(%class.KV* sret %10, %class.KV* dereferenceable(16) %66, i64 %68, %class.key* %69, i64* %9)
  %70 = getelementptr inbounds %class.hamt, %class.hamt* %12, i32 0, i32 0
  %71 = load i64, i64* %7, align 8
  %72 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %70, i64 0, i64 %71
  %73 = call zeroext i1 @_ZNK2KVI3keyS0_Lj0EEeqERKS1_(%class.KV* %10, %class.KV* dereferenceable(16) %72)
  br i1 %73, label %74, label %75

; <label>:74:                                     ; preds = %61
  store %class.hamt* %12, %class.hamt** %3, align 8
  br label %91

; <label>:75:                                     ; preds = %61
  %76 = call noalias i8* @malloc(i64 120) #8
  %77 = bitcast i8* %76 to %class.hamt*
  store %class.hamt* %77, %class.hamt** %11, align 8
  %78 = load %class.hamt*, %class.hamt** %11, align 8
  %79 = bitcast %class.hamt* %78 to i8*
  %80 = bitcast %class.hamt* %12 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %79, i8* %80, i64 120, i32 8, i1 false)
  %81 = load %class.hamt*, %class.hamt** %11, align 8
  %82 = getelementptr inbounds %class.hamt, %class.hamt* %81, i32 0, i32 0
  %83 = load i64, i64* %7, align 8
  %84 = getelementptr inbounds [7 x %class.KV], [7 x %class.KV]* %82, i64 0, i64 %83
  %85 = bitcast %class.KV* %84 to i8*
  %86 = bitcast i8* %85 to %class.KV*
  call void @_ZN2KVI3keyS0_Lj0EEC2ERKS1_(%class.KV* %86, %class.KV* dereferenceable(16) %10)
  %87 = load i64, i64* %9, align 8
  %88 = load %class.hamt*, %class.hamt** %11, align 8
  %89 = getelementptr inbounds %class.hamt, %class.hamt* %88, i32 0, i32 1
  store i64 %87, i64* %89, align 8
  %90 = load %class.hamt*, %class.hamt** %11, align 8
  store %class.hamt* %90, %class.hamt** %3, align 8
  br label %91

; <label>:91:                                     ; preds = %75, %74, %60, %44, %25
  %92 = load %class.hamt*, %class.hamt** %3, align 8
  ret %class.hamt* %92
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_hash_45has_45key_63(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.hamt*, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca %class.key*, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %9 = load i64, i64* %4, align 8
  %10 = and i64 %9, -8
  %11 = inttoptr i64 %10 to i64*
  %12 = bitcast i64* %11 to %class.hamt*
  store %class.hamt* %12, %class.hamt** %6, align 8
  %13 = call noalias i8* @malloc(i64 8) #8
  %14 = bitcast i8* %13 to %class.key*
  %15 = bitcast %class.key* %14 to i8*
  %16 = bitcast i8* %15 to %class.key*
  %17 = load i64, i64* %5, align 8
  call void @_ZN3keyC2Em(%class.key* %16, i64 %17)
  store %class.key* %16, %class.key** %7, align 8
  %18 = load %class.hamt*, %class.hamt** %6, align 8
  %19 = load %class.key*, %class.key** %7, align 8
  %20 = call %class.key* @_ZNK4hamtI3keyS0_E3getEPKS0_(%class.hamt* %18, %class.key* %19)
  store %class.key* %20, %class.key** %8, align 8
  %21 = load %class.key*, %class.key** %8, align 8
  %22 = icmp ne %class.key* %21, null
  br i1 %22, label %23, label %24

; <label>:23:                                     ; preds = %2
  store i64 31, i64* %3, align 8
  br label %25

; <label>:24:                                     ; preds = %2
  store i64 15, i64* %3, align 8
  br label %25

; <label>:25:                                     ; preds = %24, %23
  %26 = load i64, i64* %3, align 8
  ret i64 %26
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_print_aux(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64*, align 8
  %4 = alloca i64*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %7 = load i64, i64* %2, align 8
  %8 = icmp eq i64 %7, 0
  br i1 %8, label %9, label %11

; <label>:9:                                      ; preds = %1
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.10, i32 0, i32 0))
  br label %113

; <label>:11:                                     ; preds = %1
  %12 = load i64, i64* %2, align 8
  %13 = and i64 %12, 7
  %14 = icmp eq i64 %13, 0
  br i1 %14, label %15, label %17

; <label>:15:                                     ; preds = %11
  %16 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.11, i32 0, i32 0))
  br label %112

; <label>:17:                                     ; preds = %11
  %18 = load i64, i64* %2, align 8
  %19 = and i64 %18, 7
  %20 = icmp eq i64 %19, 1
  br i1 %20, label %21, label %36

; <label>:21:                                     ; preds = %17
  %22 = load i64, i64* %2, align 8
  %23 = and i64 %22, -8
  %24 = inttoptr i64 %23 to i64*
  store i64* %24, i64** %3, align 8
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.12, i32 0, i32 0))
  %26 = load i64*, i64** %3, align 8
  %27 = getelementptr inbounds i64, i64* %26, i64 0
  %28 = load i64, i64* %27, align 8
  %29 = call i64 @prim_print_aux(i64 %28)
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.13, i32 0, i32 0))
  %31 = load i64*, i64** %3, align 8
  %32 = getelementptr inbounds i64, i64* %31, i64 1
  %33 = load i64, i64* %32, align 8
  %34 = call i64 @prim_print_aux(i64 %33)
  %35 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i32 0, i32 0))
  br label %111

; <label>:36:                                     ; preds = %17
  %37 = load i64, i64* %2, align 8
  %38 = and i64 %37, 7
  %39 = icmp eq i64 %38, 2
  br i1 %39, label %40, label %45

; <label>:40:                                     ; preds = %36
  %41 = load i64, i64* %2, align 8
  %42 = lshr i64 %41, 32
  %43 = trunc i64 %42 to i32
  %44 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.15, i32 0, i32 0), i32 %43)
  br label %110

; <label>:45:                                     ; preds = %36
  %46 = load i64, i64* %2, align 8
  %47 = and i64 %46, 7
  %48 = icmp eq i64 %47, 3
  br i1 %48, label %49, label %54

; <label>:49:                                     ; preds = %45
  %50 = load i64, i64* %2, align 8
  %51 = and i64 %50, -8
  %52 = inttoptr i64 %51 to i8*
  %53 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.16, i32 0, i32 0), i8* %52)
  br label %109

; <label>:54:                                     ; preds = %45
  %55 = load i64, i64* %2, align 8
  %56 = and i64 %55, 7
  %57 = icmp eq i64 %56, 4
  br i1 %57, label %58, label %63

; <label>:58:                                     ; preds = %54
  %59 = load i64, i64* %2, align 8
  %60 = and i64 %59, -8
  %61 = inttoptr i64 %60 to i8*
  %62 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0), i8* %61)
  br label %108

; <label>:63:                                     ; preds = %54
  %64 = load i64, i64* %2, align 8
  %65 = and i64 %64, 7
  %66 = icmp eq i64 %65, 6
  br i1 %66, label %67, label %104

; <label>:67:                                     ; preds = %63
  %68 = load i64, i64* %2, align 8
  %69 = and i64 %68, -8
  %70 = inttoptr i64 %69 to i64*
  %71 = getelementptr inbounds i64, i64* %70, i64 0
  %72 = load i64, i64* %71, align 8
  %73 = and i64 %72, 7
  %74 = icmp eq i64 1, %73
  br i1 %74, label %75, label %104

; <label>:75:                                     ; preds = %67
  %76 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.17, i32 0, i32 0))
  %77 = load i64, i64* %2, align 8
  %78 = and i64 %77, -8
  %79 = inttoptr i64 %78 to i64*
  store i64* %79, i64** %4, align 8
  %80 = load i64*, i64** %4, align 8
  %81 = getelementptr inbounds i64, i64* %80, i64 0
  %82 = load i64, i64* %81, align 8
  %83 = lshr i64 %82, 3
  store i64 %83, i64* %5, align 8
  %84 = load i64*, i64** %4, align 8
  %85 = getelementptr inbounds i64, i64* %84, i64 1
  %86 = load i64, i64* %85, align 8
  %87 = call i64 @prim_print_aux(i64 %86)
  store i64 2, i64* %6, align 8
  br label %88

; <label>:88:                                     ; preds = %99, %75
  %89 = load i64, i64* %6, align 8
  %90 = load i64, i64* %5, align 8
  %91 = icmp ule i64 %89, %90
  br i1 %91, label %92, label %102

; <label>:92:                                     ; preds = %88
  %93 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.18, i32 0, i32 0))
  %94 = load i64*, i64** %4, align 8
  %95 = load i64, i64* %6, align 8
  %96 = getelementptr inbounds i64, i64* %94, i64 %95
  %97 = load i64, i64* %96, align 8
  %98 = call i64 @prim_print_aux(i64 %97)
  br label %99

; <label>:99:                                     ; preds = %92
  %100 = load i64, i64* %6, align 8
  %101 = add i64 %100, 1
  store i64 %101, i64* %6, align 8
  br label %88

; <label>:102:                                    ; preds = %88
  %103 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i32 0, i32 0))
  br label %107

; <label>:104:                                    ; preds = %67, %63
  %105 = load i64, i64* %2, align 8
  %106 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.19, i32 0, i32 0), i64 %105)
  br label %107

; <label>:107:                                    ; preds = %104, %102
  br label %108

; <label>:108:                                    ; preds = %107, %58
  br label %109

; <label>:109:                                    ; preds = %108, %49
  br label %110

; <label>:110:                                    ; preds = %109, %40
  br label %111

; <label>:111:                                    ; preds = %110, %21
  br label %112

; <label>:112:                                    ; preds = %111, %15
  br label %113

; <label>:113:                                    ; preds = %112, %9
  ret i64 39
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_print(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64*, align 8
  %4 = alloca i64*, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %7 = load i64, i64* %2, align 8
  %8 = icmp eq i64 %7, 0
  br i1 %8, label %9, label %11

; <label>:9:                                      ; preds = %1
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.20, i32 0, i32 0))
  br label %131

; <label>:11:                                     ; preds = %1
  %12 = load i64, i64* %2, align 8
  %13 = icmp eq i64 %12, 31
  br i1 %13, label %14, label %16

; <label>:14:                                     ; preds = %11
  %15 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.21, i32 0, i32 0))
  br label %130

; <label>:16:                                     ; preds = %11
  %17 = load i64, i64* %2, align 8
  %18 = icmp eq i64 %17, 15
  br i1 %18, label %19, label %21

; <label>:19:                                     ; preds = %16
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.22, i32 0, i32 0))
  br label %129

; <label>:21:                                     ; preds = %16
  %22 = load i64, i64* %2, align 8
  %23 = icmp eq i64 %22, 39
  br i1 %23, label %24, label %26

; <label>:24:                                     ; preds = %21
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.23, i32 0, i32 0))
  br label %128

; <label>:26:                                     ; preds = %21
  %27 = load i64, i64* %2, align 8
  %28 = and i64 %27, 7
  %29 = icmp eq i64 %28, 0
  br i1 %29, label %30, label %32

; <label>:30:                                     ; preds = %26
  %31 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.11, i32 0, i32 0))
  br label %127

; <label>:32:                                     ; preds = %26
  %33 = load i64, i64* %2, align 8
  %34 = and i64 %33, 7
  %35 = icmp eq i64 %34, 1
  br i1 %35, label %36, label %51

; <label>:36:                                     ; preds = %32
  %37 = load i64, i64* %2, align 8
  %38 = and i64 %37, -8
  %39 = inttoptr i64 %38 to i64*
  store i64* %39, i64** %3, align 8
  %40 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.24, i32 0, i32 0))
  %41 = load i64*, i64** %3, align 8
  %42 = getelementptr inbounds i64, i64* %41, i64 0
  %43 = load i64, i64* %42, align 8
  %44 = call i64 @prim_print_aux(i64 %43)
  %45 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.13, i32 0, i32 0))
  %46 = load i64*, i64** %3, align 8
  %47 = getelementptr inbounds i64, i64* %46, i64 1
  %48 = load i64, i64* %47, align 8
  %49 = call i64 @prim_print_aux(i64 %48)
  %50 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i32 0, i32 0))
  br label %126

; <label>:51:                                     ; preds = %32
  %52 = load i64, i64* %2, align 8
  %53 = and i64 %52, 7
  %54 = icmp eq i64 %53, 2
  br i1 %54, label %55, label %60

; <label>:55:                                     ; preds = %51
  %56 = load i64, i64* %2, align 8
  %57 = lshr i64 %56, 32
  %58 = trunc i64 %57 to i32
  %59 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.15, i32 0, i32 0), i32 %58)
  br label %125

; <label>:60:                                     ; preds = %51
  %61 = load i64, i64* %2, align 8
  %62 = and i64 %61, 7
  %63 = icmp eq i64 %62, 3
  br i1 %63, label %64, label %69

; <label>:64:                                     ; preds = %60
  %65 = load i64, i64* %2, align 8
  %66 = and i64 %65, -8
  %67 = inttoptr i64 %66 to i8*
  %68 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.16, i32 0, i32 0), i8* %67)
  br label %124

; <label>:69:                                     ; preds = %60
  %70 = load i64, i64* %2, align 8
  %71 = and i64 %70, 7
  %72 = icmp eq i64 %71, 4
  br i1 %72, label %73, label %78

; <label>:73:                                     ; preds = %69
  %74 = load i64, i64* %2, align 8
  %75 = and i64 %74, -8
  %76 = inttoptr i64 %75 to i8*
  %77 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.25, i32 0, i32 0), i8* %76)
  br label %123

; <label>:78:                                     ; preds = %69
  %79 = load i64, i64* %2, align 8
  %80 = and i64 %79, 7
  %81 = icmp eq i64 %80, 6
  br i1 %81, label %82, label %119

; <label>:82:                                     ; preds = %78
  %83 = load i64, i64* %2, align 8
  %84 = and i64 %83, -8
  %85 = inttoptr i64 %84 to i64*
  %86 = getelementptr inbounds i64, i64* %85, i64 0
  %87 = load i64, i64* %86, align 8
  %88 = and i64 %87, 7
  %89 = icmp eq i64 1, %88
  br i1 %89, label %90, label %119

; <label>:90:                                     ; preds = %82
  %91 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.17, i32 0, i32 0))
  %92 = load i64, i64* %2, align 8
  %93 = and i64 %92, -8
  %94 = inttoptr i64 %93 to i64*
  store i64* %94, i64** %4, align 8
  %95 = load i64*, i64** %4, align 8
  %96 = getelementptr inbounds i64, i64* %95, i64 0
  %97 = load i64, i64* %96, align 8
  %98 = lshr i64 %97, 3
  store i64 %98, i64* %5, align 8
  %99 = load i64*, i64** %4, align 8
  %100 = getelementptr inbounds i64, i64* %99, i64 1
  %101 = load i64, i64* %100, align 8
  %102 = call i64 @prim_print(i64 %101)
  store i64 2, i64* %6, align 8
  br label %103

; <label>:103:                                    ; preds = %114, %90
  %104 = load i64, i64* %6, align 8
  %105 = load i64, i64* %5, align 8
  %106 = icmp ule i64 %104, %105
  br i1 %106, label %107, label %117

; <label>:107:                                    ; preds = %103
  %108 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.18, i32 0, i32 0))
  %109 = load i64*, i64** %4, align 8
  %110 = load i64, i64* %6, align 8
  %111 = getelementptr inbounds i64, i64* %109, i64 %110
  %112 = load i64, i64* %111, align 8
  %113 = call i64 @prim_print(i64 %112)
  br label %114

; <label>:114:                                    ; preds = %107
  %115 = load i64, i64* %6, align 8
  %116 = add i64 %115, 1
  store i64 %116, i64* %6, align 8
  br label %103

; <label>:117:                                    ; preds = %103
  %118 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i32 0, i32 0))
  br label %122

; <label>:119:                                    ; preds = %82, %78
  %120 = load i64, i64* %2, align 8
  %121 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.26, i32 0, i32 0), i64 %120)
  br label %122

; <label>:122:                                    ; preds = %119, %117
  br label %123

; <label>:123:                                    ; preds = %122, %73
  br label %124

; <label>:124:                                    ; preds = %123, %64
  br label %125

; <label>:125:                                    ; preds = %124, %55
  br label %126

; <label>:126:                                    ; preds = %125, %36
  br label %127

; <label>:127:                                    ; preds = %126, %30
  br label %128

; <label>:128:                                    ; preds = %127, %24
  br label %129

; <label>:129:                                    ; preds = %128, %19
  br label %130

; <label>:130:                                    ; preds = %129, %14
  br label %131

; <label>:131:                                    ; preds = %130, %9
  ret i64 39
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_print(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_print(i64 %6)
  ret i64 %7
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_halt(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = call i64 @prim_print(i64 %4)
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 0) #9
  unreachable
                                                  ; No predecessors!
  %8 = load i64, i64* %2, align 8
  ret i64 %8
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_vector(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64*, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64*, align 8
  %6 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %7 = call noalias i8* @malloc(i64 4096) #8
  %8 = bitcast i8* %7 to i64*
  store i64* %8, i64** %3, align 8
  store i64 0, i64* %4, align 8
  br label %9

; <label>:9:                                      ; preds = %18, %1
  %10 = load i64, i64* %2, align 8
  %11 = and i64 %10, 7
  %12 = icmp eq i64 %11, 1
  br i1 %12, label %13, label %16

; <label>:13:                                     ; preds = %9
  %14 = load i64, i64* %4, align 8
  %15 = icmp ult i64 %14, 512
  br label %16

; <label>:16:                                     ; preds = %13, %9
  %17 = phi i1 [ false, %9 ], [ %15, %13 ]
  br i1 %17, label %18, label %25

; <label>:18:                                     ; preds = %16
  %19 = load i64, i64* %2, align 8
  %20 = call i64 @expect_cons(i64 %19, i64* %2)
  %21 = load i64*, i64** %3, align 8
  %22 = load i64, i64* %4, align 8
  %23 = add i64 %22, 1
  store i64 %23, i64* %4, align 8
  %24 = getelementptr inbounds i64, i64* %21, i64 %22
  store i64 %20, i64* %24, align 8
  br label %9

; <label>:25:                                     ; preds = %16
  %26 = load i64, i64* %4, align 8
  %27 = add i64 %26, 1
  %28 = mul i64 %27, 8
  %29 = call i64* @alloc(i64 %28)
  store i64* %29, i64** %5, align 8
  %30 = load i64, i64* %4, align 8
  %31 = shl i64 %30, 3
  %32 = or i64 %31, 1
  %33 = load i64*, i64** %5, align 8
  %34 = getelementptr inbounds i64, i64* %33, i64 0
  store i64 %32, i64* %34, align 8
  store i64 0, i64* %6, align 8
  br label %35

; <label>:35:                                     ; preds = %48, %25
  %36 = load i64, i64* %6, align 8
  %37 = load i64, i64* %4, align 8
  %38 = icmp ult i64 %36, %37
  br i1 %38, label %39, label %51

; <label>:39:                                     ; preds = %35
  %40 = load i64*, i64** %3, align 8
  %41 = load i64, i64* %6, align 8
  %42 = getelementptr inbounds i64, i64* %40, i64 %41
  %43 = load i64, i64* %42, align 8
  %44 = load i64*, i64** %5, align 8
  %45 = load i64, i64* %6, align 8
  %46 = add i64 %45, 1
  %47 = getelementptr inbounds i64, i64* %44, i64 %46
  store i64 %43, i64* %47, align 8
  br label %48

; <label>:48:                                     ; preds = %39
  %49 = load i64, i64* %6, align 8
  %50 = add i64 %49, 1
  store i64 %50, i64* %6, align 8
  br label %35

; <label>:51:                                     ; preds = %35
  %52 = load i64*, i64** %3, align 8
  %53 = icmp eq i64* %52, null
  br i1 %53, label %56, label %54

; <label>:54:                                     ; preds = %51
  %55 = bitcast i64* %52 to i8*
  call void @_ZdaPv(i8* %55) #10
  br label %56

; <label>:56:                                     ; preds = %54, %51
  %57 = load i64*, i64** %5, align 8
  %58 = ptrtoint i64* %57 to i64
  %59 = or i64 %58, 6
  ret i64 %59
}

; Function Attrs: nobuiltin nounwind
declare void @_ZdaPv(i8*) #5

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_make_45vector(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64*, align 8
  %7 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11:                                     ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.27, i32 0, i32 0))
  br label %12

; <label>:12:                                     ; preds = %11, %2
  %13 = load i64, i64* %3, align 8
  %14 = and i64 %13, -8
  %15 = lshr i64 %14, 32
  %16 = trunc i64 %15 to i32
  %17 = sext i32 %16 to i64
  store i64 %17, i64* %5, align 8
  %18 = load i64, i64* %5, align 8
  %19 = add i64 %18, 1
  %20 = mul i64 %19, 8
  %21 = call i64* @alloc(i64 %20)
  store i64* %21, i64** %6, align 8
  %22 = load i64, i64* %5, align 8
  %23 = shl i64 %22, 3
  %24 = or i64 %23, 1
  %25 = load i64*, i64** %6, align 8
  %26 = getelementptr inbounds i64, i64* %25, i64 0
  store i64 %24, i64* %26, align 8
  store i64 1, i64* %7, align 8
  br label %27

; <label>:27:                                     ; preds = %36, %12
  %28 = load i64, i64* %7, align 8
  %29 = load i64, i64* %5, align 8
  %30 = icmp ule i64 %28, %29
  br i1 %30, label %31, label %39

; <label>:31:                                     ; preds = %27
  %32 = load i64, i64* %4, align 8
  %33 = load i64*, i64** %6, align 8
  %34 = load i64, i64* %7, align 8
  %35 = getelementptr inbounds i64, i64* %33, i64 %34
  store i64 %32, i64* %35, align 8
  br label %36

; <label>:36:                                     ; preds = %31
  %37 = load i64, i64* %7, align 8
  %38 = add i64 %37, 1
  store i64 %38, i64* %7, align 8
  br label %27

; <label>:39:                                     ; preds = %27
  %40 = load i64*, i64** %6, align 8
  %41 = ptrtoint i64* %40 to i64
  %42 = or i64 %41, 6
  ret i64 %42
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_make_45vector(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %13

; <label>:13:                                     ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_make_45vector(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_vector_45ref(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load i64, i64* %4, align 8
  %6 = and i64 %5, 7
  %7 = icmp ne i64 %6, 2
  br i1 %7, label %8, label %9

; <label>:8:                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.29, i32 0, i32 0))
  br label %9

; <label>:9:                                      ; preds = %8, %2
  %10 = load i64, i64* %3, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 6
  br i1 %12, label %13, label %14

; <label>:13:                                     ; preds = %9
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.30, i32 0, i32 0))
  br label %14

; <label>:14:                                     ; preds = %13, %9
  %15 = load i64, i64* %3, align 8
  %16 = and i64 %15, -8
  %17 = inttoptr i64 %16 to i64*
  %18 = getelementptr inbounds i64, i64* %17, i64 0
  %19 = load i64, i64* %18, align 8
  %20 = and i64 %19, 7
  %21 = icmp ne i64 %20, 1
  br i1 %21, label %22, label %23

; <label>:22:                                     ; preds = %14
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.31, i32 0, i32 0))
  br label %23

; <label>:23:                                     ; preds = %22, %14
  %24 = load i64, i64* %3, align 8
  %25 = and i64 %24, -8
  %26 = inttoptr i64 %25 to i64*
  %27 = load i64, i64* %4, align 8
  %28 = and i64 %27, -8
  %29 = lshr i64 %28, 32
  %30 = trunc i64 %29 to i32
  %31 = add nsw i32 1, %30
  %32 = sext i32 %31 to i64
  %33 = getelementptr inbounds i64, i64* %26, i64 %32
  %34 = load i64, i64* %33, align 8
  ret i64 %34
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_vector_45ref(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %13

; <label>:13:                                     ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_vector_45ref(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_vector_45set_33(i64, i64, i64) #2 {
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  store i64 %2, i64* %6, align 8
  %7 = load i64, i64* %5, align 8
  %8 = and i64 %7, 7
  %9 = icmp ne i64 %8, 2
  br i1 %9, label %10, label %11

; <label>:10:                                     ; preds = %3
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.29, i32 0, i32 0))
  br label %11

; <label>:11:                                     ; preds = %10, %3
  %12 = load i64, i64* %4, align 8
  %13 = and i64 %12, 7
  %14 = icmp ne i64 %13, 6
  br i1 %14, label %15, label %16

; <label>:15:                                     ; preds = %11
  call void @fatal_err(i8* getelementptr inbounds ([48 x i8], [48 x i8]* @.str.32, i32 0, i32 0))
  br label %16

; <label>:16:                                     ; preds = %15, %11
  %17 = load i64, i64* %4, align 8
  %18 = and i64 %17, -8
  %19 = inttoptr i64 %18 to i64*
  %20 = getelementptr inbounds i64, i64* %19, i64 0
  %21 = load i64, i64* %20, align 8
  %22 = and i64 %21, 7
  %23 = icmp ne i64 %22, 1
  br i1 %23, label %24, label %25

; <label>:24:                                     ; preds = %16
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.31, i32 0, i32 0))
  br label %25

; <label>:25:                                     ; preds = %24, %16
  %26 = load i64, i64* %6, align 8
  %27 = load i64, i64* %4, align 8
  %28 = and i64 %27, -8
  %29 = inttoptr i64 %28 to i64*
  %30 = load i64, i64* %5, align 8
  %31 = and i64 %30, -8
  %32 = lshr i64 %31, 32
  %33 = trunc i64 %32 to i32
  %34 = add nsw i32 1, %33
  %35 = sext i32 %34 to i64
  %36 = getelementptr inbounds i64, i64* %29, i64 %35
  store i64 %26, i64* %36, align 8
  ret i64 39
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_vector_45set_33(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  %6 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %7 = load i64, i64* %2, align 8
  %8 = call i64 @expect_cons(i64 %7, i64* %3)
  store i64 %8, i64* %4, align 8
  %9 = load i64, i64* %3, align 8
  %10 = call i64 @expect_cons(i64 %9, i64* %3)
  store i64 %10, i64* %5, align 8
  %11 = load i64, i64* %3, align 8
  %12 = call i64 @expect_cons(i64 %11, i64* %3)
  store i64 %12, i64* %6, align 8
  %13 = load i64, i64* %3, align 8
  %14 = icmp ne i64 %13, 0
  br i1 %14, label %15, label %16

; <label>:15:                                     ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %16

; <label>:16:                                     ; preds = %15, %1
  %17 = load i64, i64* %4, align 8
  %18 = load i64, i64* %5, align 8
  %19 = load i64, i64* %6, align 8
  %20 = call i64 @prim_vector_45set_33(i64 %17, i64 %18, i64 %19)
  ret i64 %20
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_void() #0 {
  ret i64 39
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_eq_63(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = load i64, i64* %5, align 8
  %8 = icmp eq i64 %6, %7
  br i1 %8, label %9, label %10

; <label>:9:                                      ; preds = %2
  store i64 31, i64* %3, align 8
  br label %11

; <label>:10:                                     ; preds = %2
  store i64 15, i64* %3, align 8
  br label %11

; <label>:11:                                     ; preds = %10, %9
  %12 = load i64, i64* %3, align 8
  ret i64 %12
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_eq_63(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %13

; <label>:13:                                     ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_eq_63(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_eqv_63(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = load i64, i64* %5, align 8
  %8 = icmp eq i64 %6, %7
  br i1 %8, label %9, label %10

; <label>:9:                                      ; preds = %2
  store i64 31, i64* %3, align 8
  br label %11

; <label>:10:                                     ; preds = %2
  store i64 15, i64* %3, align 8
  br label %11

; <label>:11:                                     ; preds = %10, %9
  %12 = load i64, i64* %3, align 8
  ret i64 %12
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_eqv_63(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %13

; <label>:13:                                     ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_eqv_63(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_equal_63(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  ret i64 0
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_equal_63(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %13

; <label>:13:                                     ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_equal_63(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_number_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = and i64 %4, 7
  %6 = icmp eq i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7:                                      ; preds = %1
  store i64 31, i64* %2, align 8
  br label %9

; <label>:8:                                      ; preds = %1
  store i64 15, i64* %2, align 8
  br label %9

; <label>:9:                                      ; preds = %8, %7
  %10 = load i64, i64* %2, align 8
  ret i64 %10
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_number_63(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_number_63(i64 %6)
  ret i64 %7
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_integer_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = and i64 %4, 7
  %6 = icmp eq i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7:                                      ; preds = %1
  store i64 31, i64* %2, align 8
  br label %9

; <label>:8:                                      ; preds = %1
  store i64 15, i64* %2, align 8
  br label %9

; <label>:9:                                      ; preds = %8, %7
  %10 = load i64, i64* %2, align 8
  ret i64 %10
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_integer_63(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_integer_63(i64 %6)
  ret i64 %7
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_void_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = icmp eq i64 %4, 39
  br i1 %5, label %6, label %7

; <label>:6:                                      ; preds = %1
  store i64 31, i64* %2, align 8
  br label %8

; <label>:7:                                      ; preds = %1
  store i64 15, i64* %2, align 8
  br label %8

; <label>:8:                                      ; preds = %7, %6
  %9 = load i64, i64* %2, align 8
  ret i64 %9
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_void_63(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_void_63(i64 %6)
  ret i64 %7
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_procedure_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = and i64 %4, 7
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %7, label %8

; <label>:7:                                      ; preds = %1
  store i64 31, i64* %2, align 8
  br label %9

; <label>:8:                                      ; preds = %1
  store i64 15, i64* %2, align 8
  br label %9

; <label>:9:                                      ; preds = %8, %7
  %10 = load i64, i64* %2, align 8
  ret i64 %10
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_procedure_63(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_procedure_63(i64 %6)
  ret i64 %7
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_null_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = icmp eq i64 %4, 0
  br i1 %5, label %6, label %7

; <label>:6:                                      ; preds = %1
  store i64 31, i64* %2, align 8
  br label %8

; <label>:7:                                      ; preds = %1
  store i64 15, i64* %2, align 8
  br label %8

; <label>:8:                                      ; preds = %7, %6
  %9 = load i64, i64* %2, align 8
  ret i64 %9
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_null_63(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_null_63(i64 %6)
  ret i64 %7
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_cons_63(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = and i64 %4, 7
  %6 = icmp eq i64 %5, 1
  br i1 %6, label %7, label %8

; <label>:7:                                      ; preds = %1
  store i64 31, i64* %2, align 8
  br label %9

; <label>:8:                                      ; preds = %1
  store i64 15, i64* %2, align 8
  br label %9

; <label>:9:                                      ; preds = %8, %7
  %10 = load i64, i64* %2, align 8
  ret i64 %10
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_cons_63(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_cons_63(i64 %6)
  ret i64 %7
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_cons(i64, i64) #0 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %6 = call i64* @alloc(i64 16)
  store i64* %6, i64** %5, align 8
  %7 = load i64, i64* %3, align 8
  %8 = load i64*, i64** %5, align 8
  %9 = getelementptr inbounds i64, i64* %8, i64 0
  store i64 %7, i64* %9, align 8
  %10 = load i64, i64* %4, align 8
  %11 = load i64*, i64** %5, align 8
  %12 = getelementptr inbounds i64, i64* %11, i64 1
  store i64 %10, i64* %12, align 8
  %13 = load i64*, i64** %5, align 8
  %14 = ptrtoint i64* %13 to i64
  %15 = or i64 %14, 1
  ret i64 %15
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_cons(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %6 = load i64, i64* %2, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %3)
  store i64 %7, i64* %4, align 8
  %8 = load i64, i64* %3, align 8
  %9 = call i64 @expect_cons(i64 %8, i64* %3)
  store i64 %9, i64* %5, align 8
  %10 = load i64, i64* %3, align 8
  %11 = icmp ne i64 %10, 0
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %1
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.28, i32 0, i32 0))
  br label %13

; <label>:13:                                     ; preds = %12, %1
  %14 = load i64, i64* %4, align 8
  %15 = load i64, i64* %5, align 8
  %16 = call i64 @prim_cons(i64 %14, i64 %15)
  ret i64 %16
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_car(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %5 = load i64, i64* %2, align 8
  %6 = call i64 @expect_cons(i64 %5, i64* %3)
  store i64 %6, i64* %4, align 8
  %7 = load i64, i64* %4, align 8
  ret i64 %7
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_car(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_car(i64 %6)
  ret i64 %7
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim_cdr(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %5 = load i64, i64* %2, align 8
  %6 = call i64 @expect_cons(i64 %5, i64* %3)
  store i64 %6, i64* %4, align 8
  %7 = load i64, i64* %3, align 8
  ret i64 %7
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_cdr(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_cdr(i64 %6)
  ret i64 %7
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim__43(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load i64, i64* %3, align 8
  %6 = and i64 %5, 7
  %7 = icmp ne i64 %6, 2
  br i1 %7, label %8, label %9

; <label>:8:                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.33, i32 0, i32 0))
  br label %9

; <label>:9:                                      ; preds = %8, %2
  %10 = load i64, i64* %4, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 2
  br i1 %12, label %13, label %14

; <label>:13:                                     ; preds = %9
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.34, i32 0, i32 0))
  br label %14

; <label>:14:                                     ; preds = %13, %9
  %15 = load i64, i64* %3, align 8
  %16 = and i64 %15, -8
  %17 = lshr i64 %16, 32
  %18 = trunc i64 %17 to i32
  %19 = load i64, i64* %4, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = trunc i64 %21 to i32
  %23 = add nsw i32 %18, %22
  %24 = zext i32 %23 to i64
  %25 = shl i64 %24, 32
  %26 = or i64 %25, 2
  ret i64 %26
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim__43(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %7, label %8

; <label>:7:                                      ; preds = %1
  store i64 2, i64* %2, align 8
  br label %34

; <label>:8:                                      ; preds = %1
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 1
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.35, i32 0, i32 0))
  br label %13

; <label>:13:                                     ; preds = %12, %8
  %14 = load i64, i64* %3, align 8
  %15 = and i64 %14, -8
  %16 = inttoptr i64 %15 to i64*
  store i64* %16, i64** %4, align 8
  %17 = load i64*, i64** %4, align 8
  %18 = getelementptr inbounds i64, i64* %17, i64 0
  %19 = load i64, i64* %18, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = trunc i64 %21 to i32
  %23 = load i64*, i64** %4, align 8
  %24 = getelementptr inbounds i64, i64* %23, i64 1
  %25 = load i64, i64* %24, align 8
  %26 = call i64 @applyprim__43(i64 %25)
  %27 = and i64 %26, -8
  %28 = lshr i64 %27, 32
  %29 = trunc i64 %28 to i32
  %30 = add nsw i32 %22, %29
  %31 = zext i32 %30 to i64
  %32 = shl i64 %31, 32
  %33 = or i64 %32, 2
  store i64 %33, i64* %2, align 8
  br label %34

; <label>:34:                                     ; preds = %13, %7
  %35 = load i64, i64* %2, align 8
  ret i64 %35
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim__45(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load i64, i64* %3, align 8
  %6 = and i64 %5, 7
  %7 = icmp ne i64 %6, 2
  br i1 %7, label %8, label %9

; <label>:8:                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.33, i32 0, i32 0))
  br label %9

; <label>:9:                                      ; preds = %8, %2
  %10 = load i64, i64* %4, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 2
  br i1 %12, label %13, label %14

; <label>:13:                                     ; preds = %9
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.36, i32 0, i32 0))
  br label %14

; <label>:14:                                     ; preds = %13, %9
  %15 = load i64, i64* %3, align 8
  %16 = and i64 %15, -8
  %17 = lshr i64 %16, 32
  %18 = trunc i64 %17 to i32
  %19 = load i64, i64* %4, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = trunc i64 %21 to i32
  %23 = sub nsw i32 %18, %22
  %24 = zext i32 %23 to i64
  %25 = shl i64 %24, 32
  %26 = or i64 %25, 2
  ret i64 %26
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim__45(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %7, label %8

; <label>:7:                                      ; preds = %1
  store i64 2, i64* %2, align 8
  br label %50

; <label>:8:                                      ; preds = %1
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 1
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.35, i32 0, i32 0))
  br label %13

; <label>:13:                                     ; preds = %12, %8
  %14 = load i64, i64* %3, align 8
  %15 = and i64 %14, -8
  %16 = inttoptr i64 %15 to i64*
  store i64* %16, i64** %4, align 8
  %17 = load i64*, i64** %4, align 8
  %18 = getelementptr inbounds i64, i64* %17, i64 1
  %19 = load i64, i64* %18, align 8
  %20 = icmp eq i64 %19, 0
  br i1 %20, label %21, label %32

; <label>:21:                                     ; preds = %13
  %22 = load i64*, i64** %4, align 8
  %23 = getelementptr inbounds i64, i64* %22, i64 0
  %24 = load i64, i64* %23, align 8
  %25 = and i64 %24, -8
  %26 = lshr i64 %25, 32
  %27 = trunc i64 %26 to i32
  %28 = sub nsw i32 0, %27
  %29 = zext i32 %28 to i64
  %30 = shl i64 %29, 32
  %31 = or i64 %30, 2
  store i64 %31, i64* %2, align 8
  br label %50

; <label>:32:                                     ; preds = %13
  %33 = load i64*, i64** %4, align 8
  %34 = getelementptr inbounds i64, i64* %33, i64 0
  %35 = load i64, i64* %34, align 8
  %36 = and i64 %35, -8
  %37 = lshr i64 %36, 32
  %38 = trunc i64 %37 to i32
  %39 = load i64*, i64** %4, align 8
  %40 = getelementptr inbounds i64, i64* %39, i64 1
  %41 = load i64, i64* %40, align 8
  %42 = call i64 @applyprim__43(i64 %41)
  %43 = and i64 %42, -8
  %44 = lshr i64 %43, 32
  %45 = trunc i64 %44 to i32
  %46 = sub nsw i32 %38, %45
  %47 = zext i32 %46 to i64
  %48 = shl i64 %47, 32
  %49 = or i64 %48, 2
  store i64 %49, i64* %2, align 8
  br label %50

; <label>:50:                                     ; preds = %32, %21, %7
  %51 = load i64, i64* %2, align 8
  ret i64 %51
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim__42(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load i64, i64* %3, align 8
  %6 = and i64 %5, 7
  %7 = icmp ne i64 %6, 2
  br i1 %7, label %8, label %9

; <label>:8:                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.37, i32 0, i32 0))
  br label %9

; <label>:9:                                      ; preds = %8, %2
  %10 = load i64, i64* %4, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 2
  br i1 %12, label %13, label %14

; <label>:13:                                     ; preds = %9
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.38, i32 0, i32 0))
  br label %14

; <label>:14:                                     ; preds = %13, %9
  %15 = load i64, i64* %3, align 8
  %16 = and i64 %15, -8
  %17 = lshr i64 %16, 32
  %18 = trunc i64 %17 to i32
  %19 = load i64, i64* %4, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = trunc i64 %21 to i32
  %23 = mul nsw i32 %18, %22
  %24 = zext i32 %23 to i64
  %25 = shl i64 %24, 32
  %26 = or i64 %25, 2
  ret i64 %26
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim__42(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %4 = alloca i64*, align 8
  store i64 %0, i64* %3, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %5, 0
  br i1 %6, label %7, label %8

; <label>:7:                                      ; preds = %1
  store i64 4294967298, i64* %2, align 8
  br label %34

; <label>:8:                                      ; preds = %1
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 1
  br i1 %11, label %12, label %13

; <label>:12:                                     ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.35, i32 0, i32 0))
  br label %13

; <label>:13:                                     ; preds = %12, %8
  %14 = load i64, i64* %3, align 8
  %15 = and i64 %14, -8
  %16 = inttoptr i64 %15 to i64*
  store i64* %16, i64** %4, align 8
  %17 = load i64*, i64** %4, align 8
  %18 = getelementptr inbounds i64, i64* %17, i64 0
  %19 = load i64, i64* %18, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = trunc i64 %21 to i32
  %23 = load i64*, i64** %4, align 8
  %24 = getelementptr inbounds i64, i64* %23, i64 1
  %25 = load i64, i64* %24, align 8
  %26 = call i64 @applyprim__42(i64 %25)
  %27 = and i64 %26, -8
  %28 = lshr i64 %27, 32
  %29 = trunc i64 %28 to i32
  %30 = mul nsw i32 %22, %29
  %31 = zext i32 %30 to i64
  %32 = shl i64 %31, 32
  %33 = or i64 %32, 2
  store i64 %33, i64* %2, align 8
  br label %34

; <label>:34:                                     ; preds = %13, %7
  %35 = load i64, i64* %2, align 8
  ret i64 %35
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim__47(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load i64, i64* %3, align 8
  %6 = and i64 %5, 7
  %7 = icmp ne i64 %6, 2
  br i1 %7, label %8, label %9

; <label>:8:                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.39, i32 0, i32 0))
  br label %9

; <label>:9:                                      ; preds = %8, %2
  %10 = load i64, i64* %4, align 8
  %11 = and i64 %10, 7
  %12 = icmp ne i64 %11, 2
  br i1 %12, label %13, label %14

; <label>:13:                                     ; preds = %9
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.40, i32 0, i32 0))
  br label %14

; <label>:14:                                     ; preds = %13, %9
  %15 = load i64, i64* %3, align 8
  %16 = and i64 %15, -8
  %17 = lshr i64 %16, 32
  %18 = trunc i64 %17 to i32
  %19 = load i64, i64* %4, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = trunc i64 %21 to i32
  %23 = sdiv i32 %18, %22
  %24 = zext i32 %23 to i64
  %25 = shl i64 %24, 32
  %26 = or i64 %25, 2
  ret i64 %26
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim__61(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = and i64 %6, 7
  %8 = icmp ne i64 %7, 2
  br i1 %8, label %9, label %10

; <label>:9:                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.41, i32 0, i32 0))
  br label %10

; <label>:10:                                     ; preds = %9, %2
  %11 = load i64, i64* %5, align 8
  %12 = and i64 %11, 7
  %13 = icmp ne i64 %12, 2
  br i1 %13, label %14, label %15

; <label>:14:                                     ; preds = %10
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.42, i32 0, i32 0))
  br label %15

; <label>:15:                                     ; preds = %14, %10
  %16 = load i64, i64* %4, align 8
  %17 = and i64 %16, -8
  %18 = lshr i64 %17, 32
  %19 = trunc i64 %18 to i32
  %20 = load i64, i64* %5, align 8
  %21 = and i64 %20, -8
  %22 = lshr i64 %21, 32
  %23 = trunc i64 %22 to i32
  %24 = icmp eq i32 %19, %23
  br i1 %24, label %25, label %26

; <label>:25:                                     ; preds = %15
  store i64 31, i64* %3, align 8
  br label %27

; <label>:26:                                     ; preds = %15
  store i64 15, i64* %3, align 8
  br label %27

; <label>:27:                                     ; preds = %26, %25
  %28 = load i64, i64* %3, align 8
  ret i64 %28
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim__60(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = and i64 %6, 7
  %8 = icmp ne i64 %7, 2
  br i1 %8, label %9, label %10

; <label>:9:                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.43, i32 0, i32 0))
  br label %10

; <label>:10:                                     ; preds = %9, %2
  %11 = load i64, i64* %5, align 8
  %12 = and i64 %11, 7
  %13 = icmp ne i64 %12, 2
  br i1 %13, label %14, label %15

; <label>:14:                                     ; preds = %10
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.44, i32 0, i32 0))
  br label %15

; <label>:15:                                     ; preds = %14, %10
  %16 = load i64, i64* %4, align 8
  %17 = and i64 %16, -8
  %18 = lshr i64 %17, 32
  %19 = trunc i64 %18 to i32
  %20 = load i64, i64* %5, align 8
  %21 = and i64 %20, -8
  %22 = lshr i64 %21, 32
  %23 = trunc i64 %22 to i32
  %24 = icmp slt i32 %19, %23
  br i1 %24, label %25, label %26

; <label>:25:                                     ; preds = %15
  store i64 31, i64* %3, align 8
  br label %27

; <label>:26:                                     ; preds = %15
  store i64 15, i64* %3, align 8
  br label %27

; <label>:27:                                     ; preds = %26, %25
  %28 = load i64, i64* %3, align 8
  ret i64 %28
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @prim__60_61(i64, i64) #2 {
  %3 = alloca i64, align 8
  %4 = alloca i64, align 8
  %5 = alloca i64, align 8
  store i64 %0, i64* %4, align 8
  store i64 %1, i64* %5, align 8
  %6 = load i64, i64* %4, align 8
  %7 = and i64 %6, 7
  %8 = icmp ne i64 %7, 2
  br i1 %8, label %9, label %10

; <label>:9:                                      ; preds = %2
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.45, i32 0, i32 0))
  br label %10

; <label>:10:                                     ; preds = %9, %2
  %11 = load i64, i64* %5, align 8
  %12 = and i64 %11, 7
  %13 = icmp ne i64 %12, 2
  br i1 %13, label %14, label %15

; <label>:14:                                     ; preds = %10
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.46, i32 0, i32 0))
  br label %15

; <label>:15:                                     ; preds = %14, %10
  %16 = load i64, i64* %4, align 8
  %17 = and i64 %16, -8
  %18 = lshr i64 %17, 32
  %19 = trunc i64 %18 to i32
  %20 = load i64, i64* %5, align 8
  %21 = and i64 %20, -8
  %22 = lshr i64 %21, 32
  %23 = trunc i64 %22 to i32
  %24 = icmp sle i32 %19, %23
  br i1 %24, label %25, label %26

; <label>:25:                                     ; preds = %15
  store i64 31, i64* %3, align 8
  br label %27

; <label>:26:                                     ; preds = %15
  store i64 15, i64* %3, align 8
  br label %27

; <label>:27:                                     ; preds = %26, %25
  %28 = load i64, i64* %3, align 8
  ret i64 %28
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define i64 @prim_not(i64) #0 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %3, align 8
  %4 = load i64, i64* %3, align 8
  %5 = icmp eq i64 %4, 15
  br i1 %5, label %6, label %7

; <label>:6:                                      ; preds = %1
  store i64 31, i64* %2, align 8
  br label %8

; <label>:7:                                      ; preds = %1
  store i64 15, i64* %2, align 8
  br label %8

; <label>:8:                                      ; preds = %7, %6
  %9 = load i64, i64* %2, align 8
  ret i64 %9
}

; Function Attrs: noinline optnone sspstrong uwtable
define i64 @applyprim_not(i64) #2 {
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %0, i64* %2, align 8
  %4 = load i64, i64* %2, align 8
  %5 = call i64 @expect_args1(i64 %4)
  store i64 %5, i64* %3, align 8
  %6 = load i64, i64* %3, align 8
  %7 = call i64 @prim_not(i64 %6)
  ret i64 %7
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EEC2Ev(%class.KV*) unnamed_addr #2 comdat align 2 {
  %2 = alloca %class.KV*, align 8
  store %class.KV* %0, %class.KV** %2, align 8
  %3 = load %class.KV*, %class.KV** %2, align 8
  %4 = getelementptr inbounds %class.KV, %class.KV* %3, i32 0, i32 0
  call void @_ZN2KVI3keyS0_Lj0EE3KeyC2Em(%"union.KV<key, key, 0>::Key"* %4, i64 0)
  %5 = getelementptr inbounds %class.KV, %class.KV* %3, i32 0, i32 1
  call void @_ZN2KVI3keyS0_Lj0EE3ValC2EPKS0_(%"union.KV<key, key, 0>::Val"* %5, %class.key* null)
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EE3KeyC2Em(%"union.KV<key, key, 0>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 0>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 0>::Key"* %0, %"union.KV<key, key, 0>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 0>::Key"*, %"union.KV<key, key, 0>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 0>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EE3ValC2EPKS0_(%"union.KV<key, key, 0>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 0>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 0>::Val"* %0, %"union.KV<key, key, 0>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 0>::Val"*, %"union.KV<key, key, 0>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 0>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr i64 @_ZNK3key4hashEv(%class.key*) #0 comdat align 2 {
  %2 = alloca %class.key*, align 8
  %3 = alloca i8*, align 8
  %4 = alloca i64, align 8
  %5 = alloca i32, align 4
  store %class.key* %0, %class.key** %2, align 8
  %6 = load %class.key*, %class.key** %2, align 8
  %7 = bitcast %class.key* %6 to i8*
  store i8* %7, i8** %3, align 8
  store i64 -3750763034362895579, i64* %4, align 8
  store i32 0, i32* %5, align 4
  br label %8

; <label>:8:                                      ; preds = %28, %1
  %9 = load i32, i32* %5, align 4
  %10 = zext i32 %9 to i64
  %11 = icmp ult i64 %10, 8
  br i1 %11, label %12, label %30

; <label>:12:                                     ; preds = %8
  %13 = load i64, i64* %4, align 8
  %14 = load i8*, i8** %3, align 8
  %15 = load i8, i8* %14, align 1
  %16 = zext i8 %15 to i64
  %17 = xor i64 %13, %16
  store i64 %17, i64* %4, align 8
  %18 = load i64, i64* %4, align 8
  %19 = mul i64 %18, 1099511628211
  store i64 %19, i64* %4, align 8
  br label %20

; <label>:20:                                     ; preds = %12
  %21 = load i32, i32* %5, align 4
  %22 = add i32 %21, 1
  store i32 %22, i32* %5, align 4
  %23 = icmp ne i32 %22, 0
  br i1 %23, label %24, label %28

; <label>:24:                                     ; preds = %20
  %25 = load i8*, i8** %3, align 8
  %26 = getelementptr inbounds i8, i8* %25, i32 1
  store i8* %26, i8** %3, align 8
  %27 = icmp ne i8* %26, null
  br label %28

; <label>:28:                                     ; preds = %24, %20
  %29 = phi i1 [ false, %20 ], [ %27, %24 ]
  br label %8

; <label>:30:                                     ; preds = %8
  %31 = load i64, i64* %4, align 8
  ret i64 %31
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i32, i1) #6

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EEC2EPKS0_S3_(%class.KV*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV* %0, %class.KV** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV*, %class.KV** %4, align 8
  %8 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj0EE3KeyC2EPKS0_(%"union.KV<key, key, 0>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj0EE3ValC2EPKS0_(%"union.KV<key, key, 0>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK3keyeqERKS_(%class.key*, %class.key* dereferenceable(8)) #0 comdat align 2 {
  %3 = alloca %class.key*, align 8
  %4 = alloca %class.key*, align 8
  store %class.key* %0, %class.key** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %class.key*, %class.key** %3, align 8
  %6 = load %class.key*, %class.key** %4, align 8
  %7 = getelementptr inbounds %class.key, %class.key* %6, i32 0, i32 0
  %8 = load i64, i64* %7, align 8
  %9 = getelementptr inbounds %class.key, %class.key* %5, i32 0, i32 0
  %10 = load i64, i64* %9, align 8
  %11 = icmp eq i64 %8, %10
  ret i1 %11
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.0, align 8
  %17 = alloca %class.KV.0*, align 8
  %18 = alloca %class.KV.0*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %14, align 4
  %23 = load i64, i64* %11, align 8
  %24 = and i64 %23, 63
  %25 = urem i64 %24, 63
  %26 = trunc i64 %25 to i32
  store i32 %26, i32* %15, align 4
  %27 = load i32, i32* %14, align 4
  %28 = load i32, i32* %15, align 4
  %29 = icmp eq i32 %27, %28
  br i1 %29, label %30, label %51

; <label>:30:                                     ; preds = %7
  %31 = load i64, i64* %8, align 8
  %32 = lshr i64 %31, 6
  %33 = load %class.key*, %class.key** %9, align 8
  %34 = load %class.key*, %class.key** %10, align 8
  %35 = load i64, i64* %11, align 8
  %36 = lshr i64 %35, 6
  %37 = load %class.key*, %class.key** %12, align 8
  %38 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj1EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.0* sret %16, i64 %32, %class.key* %33, %class.key* %34, i64 %36, %class.key* %37, %class.key* %38)
  %39 = call noalias i8* @malloc(i64 16) #8
  %40 = bitcast i8* %39 to %class.KV.0*
  store %class.KV.0* %40, %class.KV.0** %17, align 8
  %41 = load %class.KV.0*, %class.KV.0** %17, align 8
  %42 = getelementptr inbounds %class.KV.0, %class.KV.0* %41, i64 0
  %43 = bitcast %class.KV.0* %42 to i8*
  %44 = bitcast i8* %43 to %class.KV.0*
  call void @_ZN2KVI3keyS0_Lj1EEC2ERKS1_(%class.KV.0* %44, %class.KV.0* dereferenceable(16) %16)
  %45 = load i32, i32* %14, align 4
  %46 = zext i32 %45 to i64
  %47 = shl i64 1, %46
  %48 = shl i64 %47, 1
  %49 = or i64 %48, 1
  %50 = load %class.KV.0*, %class.KV.0** %17, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %0, i64 %49, %class.KV.0* %50)
  br label %94

; <label>:51:                                     ; preds = %7
  %52 = call noalias i8* @malloc(i64 32) #8
  %53 = bitcast i8* %52 to %class.KV.0*
  store %class.KV.0* %53, %class.KV.0** %18, align 8
  %54 = load i32, i32* %15, align 4
  %55 = load i32, i32* %14, align 4
  %56 = icmp ult i32 %54, %55
  br i1 %56, label %57, label %70

; <label>:57:                                     ; preds = %51
  %58 = load %class.KV.0*, %class.KV.0** %18, align 8
  %59 = getelementptr inbounds %class.KV.0, %class.KV.0* %58, i64 0
  %60 = bitcast %class.KV.0* %59 to i8*
  %61 = bitcast i8* %60 to %class.KV.0*
  %62 = load %class.key*, %class.key** %12, align 8
  %63 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %61, %class.key* %62, %class.key* %63)
  %64 = load %class.KV.0*, %class.KV.0** %18, align 8
  %65 = getelementptr inbounds %class.KV.0, %class.KV.0* %64, i64 1
  %66 = bitcast %class.KV.0* %65 to i8*
  %67 = bitcast i8* %66 to %class.KV.0*
  %68 = load %class.key*, %class.key** %9, align 8
  %69 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %67, %class.key* %68, %class.key* %69)
  br label %83

; <label>:70:                                     ; preds = %51
  %71 = load %class.KV.0*, %class.KV.0** %18, align 8
  %72 = getelementptr inbounds %class.KV.0, %class.KV.0* %71, i64 0
  %73 = bitcast %class.KV.0* %72 to i8*
  %74 = bitcast i8* %73 to %class.KV.0*
  %75 = load %class.key*, %class.key** %9, align 8
  %76 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %74, %class.key* %75, %class.key* %76)
  %77 = load %class.KV.0*, %class.KV.0** %18, align 8
  %78 = getelementptr inbounds %class.KV.0, %class.KV.0* %77, i64 1
  %79 = bitcast %class.KV.0* %78 to i8*
  %80 = bitcast i8* %79 to %class.KV.0*
  %81 = load %class.key*, %class.key** %12, align 8
  %82 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %80, %class.key* %81, %class.key* %82)
  br label %83

; <label>:83:                                     ; preds = %70, %57
  %84 = load i32, i32* %14, align 4
  %85 = zext i32 %84 to i64
  %86 = shl i64 1, %85
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = shl i64 1, %88
  %90 = or i64 %86, %89
  %91 = shl i64 %90, 1
  %92 = or i64 %91, 1
  %93 = load %class.KV.0*, %class.KV.0** %18, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %0, i64 %92, %class.KV.0* %93)
  br label %94

; <label>:94:                                     ; preds = %83, %30
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV* noalias sret, %class.KV* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.0*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.0*, align 8
  %19 = alloca %class.KV.0, align 8
  %20 = alloca %class.KV.0, align 8
  %21 = alloca %class.KV.0*, align 8
  %22 = alloca %class.KV.0, align 8
  %23 = alloca %class.KV.0*, align 8
  %24 = alloca %class.KV.0*, align 8
  store %class.KV* %1, %class.KV** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV*, %class.KV** %7, align 8
  %26 = getelementptr inbounds %class.KV, %class.KV* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 0>::Val"* %26 to %class.KV.0**
  %28 = load %class.KV.0*, %class.KV.0** %27, align 8
  store %class.KV.0* %28, %class.KV.0** %12, align 8
  %29 = load %class.KV*, %class.KV** %7, align 8
  %30 = getelementptr inbounds %class.KV, %class.KV* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 0>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %14, align 4
  %38 = load i64, i64* %13, align 8
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %15, align 4
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i32, i32* %14, align 4
  %44 = sub i32 63, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %42, %45
  %47 = call i64 @llvm.ctpop.i64(i64 %46)
  %48 = trunc i64 %47 to i32
  store i32 %48, i32* %16, align 4
  %49 = load i64, i64* %13, align 8
  %50 = load i32, i32* %14, align 4
  %51 = zext i32 %50 to i64
  %52 = shl i64 1, %51
  %53 = and i64 %49, %52
  %54 = icmp ne i64 %53, 0
  %55 = zext i1 %54 to i8
  store i8 %55, i8* %17, align 1
  %56 = load i8, i8* %17, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %149

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.0*, %class.KV.0** %12, align 8
  %60 = load i32, i32* %16, align 4
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %class.KV.0, %class.KV.0* %59, i64 %61
  %63 = getelementptr inbounds %class.KV.0, %class.KV.0* %62, i32 0, i32 0
  %64 = bitcast %"union.KV<key, key, 1>::Key"* %63 to i64*
  %65 = load i64, i64* %64, align 8
  %66 = and i64 %65, 1
  %67 = icmp eq i64 %66, 0
  br i1 %67, label %68, label %130

; <label>:68:                                     ; preds = %58
  %69 = load %class.KV.0*, %class.KV.0** %12, align 8
  %70 = load i32, i32* %16, align 4
  %71 = zext i32 %70 to i64
  %72 = getelementptr inbounds %class.KV.0, %class.KV.0* %69, i64 %71
  %73 = getelementptr inbounds %class.KV.0, %class.KV.0* %72, i32 0, i32 0
  %74 = bitcast %"union.KV<key, key, 1>::Key"* %73 to %class.key**
  %75 = load %class.key*, %class.key** %74, align 8
  %76 = load %class.key*, %class.key** %9, align 8
  %77 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %75, %class.key* dereferenceable(8) %76)
  br i1 %77, label %78, label %90

; <label>:78:                                     ; preds = %68
  %79 = load %class.KV.0*, %class.KV.0** %12, align 8
  %80 = load i32, i32* %15, align 4
  %81 = load i32, i32* %16, align 4
  %82 = load %class.key*, %class.key** %9, align 8
  %83 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %19, %class.key* %82, %class.key* %83)
  %84 = call %class.KV.0* @_ZN2KVI3keyS0_Lj1EE11update_nodeEPKS1_jjRS2_(%class.KV.0* %79, i32 %80, i32 %81, %class.KV.0* dereferenceable(16) %19)
  store %class.KV.0* %84, %class.KV.0** %18, align 8
  %85 = load %class.KV*, %class.KV** %7, align 8
  %86 = getelementptr inbounds %class.KV, %class.KV* %85, i32 0, i32 0
  %87 = bitcast %"union.KV<key, key, 0>::Key"* %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = load %class.KV.0*, %class.KV.0** %18, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %0, i64 %88, %class.KV.0* %89)
  br label %198

; <label>:90:                                     ; preds = %68
  %91 = load i64*, i64** %11, align 8
  %92 = load i64, i64* %91, align 8
  %93 = add i64 %92, 1
  store i64 %93, i64* %91, align 8
  %94 = load %class.KV.0*, %class.KV.0** %12, align 8
  %95 = load i32, i32* %16, align 4
  %96 = zext i32 %95 to i64
  %97 = getelementptr inbounds %class.KV.0, %class.KV.0* %94, i64 %96
  %98 = getelementptr inbounds %class.KV.0, %class.KV.0* %97, i32 0, i32 0
  %99 = bitcast %"union.KV<key, key, 1>::Key"* %98 to %class.key**
  %100 = load %class.key*, %class.key** %99, align 8
  %101 = call i64 @_ZNK3key4hashEv(%class.key* %100)
  %102 = lshr i64 %101, 10
  %103 = load %class.KV.0*, %class.KV.0** %12, align 8
  %104 = load i32, i32* %16, align 4
  %105 = zext i32 %104 to i64
  %106 = getelementptr inbounds %class.KV.0, %class.KV.0* %103, i64 %105
  %107 = getelementptr inbounds %class.KV.0, %class.KV.0* %106, i32 0, i32 0
  %108 = bitcast %"union.KV<key, key, 1>::Key"* %107 to %class.key**
  %109 = load %class.key*, %class.key** %108, align 8
  %110 = load %class.KV.0*, %class.KV.0** %12, align 8
  %111 = load i32, i32* %16, align 4
  %112 = zext i32 %111 to i64
  %113 = getelementptr inbounds %class.KV.0, %class.KV.0* %110, i64 %112
  %114 = getelementptr inbounds %class.KV.0, %class.KV.0* %113, i32 0, i32 1
  %115 = bitcast %"union.KV<key, key, 1>::Val"* %114 to %class.key**
  %116 = load %class.key*, %class.key** %115, align 8
  %117 = load i64, i64* %8, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.key*, %class.key** %9, align 8
  %120 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj1EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.0* sret %20, i64 %102, %class.key* %109, %class.key* %116, i64 %118, %class.key* %119, %class.key* %120)
  %121 = load %class.KV.0*, %class.KV.0** %12, align 8
  %122 = load i32, i32* %15, align 4
  %123 = load i32, i32* %16, align 4
  %124 = call %class.KV.0* @_ZN2KVI3keyS0_Lj1EE11update_nodeEPKS1_jjRS2_(%class.KV.0* %121, i32 %122, i32 %123, %class.KV.0* dereferenceable(16) %20)
  store %class.KV.0* %124, %class.KV.0** %21, align 8
  %125 = load %class.KV*, %class.KV** %7, align 8
  %126 = getelementptr inbounds %class.KV, %class.KV* %125, i32 0, i32 0
  %127 = bitcast %"union.KV<key, key, 0>::Key"* %126 to i64*
  %128 = load i64, i64* %127, align 8
  %129 = load %class.KV.0*, %class.KV.0** %21, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %0, i64 %128, %class.KV.0* %129)
  br label %198

; <label>:130:                                    ; preds = %58
  %131 = load %class.KV.0*, %class.KV.0** %12, align 8
  %132 = load i32, i32* %16, align 4
  %133 = zext i32 %132 to i64
  %134 = getelementptr inbounds %class.KV.0, %class.KV.0* %131, i64 %133
  %135 = load i64, i64* %8, align 8
  %136 = lshr i64 %135, 6
  %137 = load %class.key*, %class.key** %9, align 8
  %138 = load %class.key*, %class.key** %10, align 8
  %139 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3keyS0_Lj1EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.0* sret %22, %class.KV.0* dereferenceable(16) %134, i64 %136, %class.key* %137, %class.key* %138, i64* %139)
  %140 = load %class.KV.0*, %class.KV.0** %12, align 8
  %141 = load i32, i32* %15, align 4
  %142 = load i32, i32* %16, align 4
  %143 = call %class.KV.0* @_ZN2KVI3keyS0_Lj1EE11update_nodeEPKS1_jjRS2_(%class.KV.0* %140, i32 %141, i32 %142, %class.KV.0* dereferenceable(16) %22)
  store %class.KV.0* %143, %class.KV.0** %23, align 8
  %144 = load %class.KV*, %class.KV** %7, align 8
  %145 = getelementptr inbounds %class.KV, %class.KV* %144, i32 0, i32 0
  %146 = bitcast %"union.KV<key, key, 0>::Key"* %145 to i64*
  %147 = load i64, i64* %146, align 8
  %148 = load %class.KV.0*, %class.KV.0** %23, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %0, i64 %147, %class.KV.0* %148)
  br label %198

; <label>:149:                                    ; preds = %6
  %150 = load i64*, i64** %11, align 8
  %151 = load i64, i64* %150, align 8
  %152 = add i64 %151, 1
  store i64 %152, i64* %150, align 8
  %153 = load i32, i32* %15, align 4
  %154 = add i32 %153, 1
  %155 = zext i32 %154 to i64
  %156 = mul i64 %155, 16
  %157 = call noalias i8* @malloc(i64 %156) #8
  %158 = bitcast i8* %157 to %class.KV.0*
  store %class.KV.0* %158, %class.KV.0** %24, align 8
  %159 = load %class.KV.0*, %class.KV.0** %24, align 8
  %160 = bitcast %class.KV.0* %159 to i8*
  %161 = load %class.KV.0*, %class.KV.0** %12, align 8
  %162 = bitcast %class.KV.0* %161 to i8*
  %163 = load i32, i32* %16, align 4
  %164 = zext i32 %163 to i64
  %165 = mul i64 %164, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %160, i8* %162, i64 %165, i32 8, i1 false)
  %166 = load %class.KV.0*, %class.KV.0** %24, align 8
  %167 = load i32, i32* %16, align 4
  %168 = add i32 %167, 1
  %169 = zext i32 %168 to i64
  %170 = getelementptr inbounds %class.KV.0, %class.KV.0* %166, i64 %169
  %171 = bitcast %class.KV.0* %170 to i8*
  %172 = load %class.KV.0*, %class.KV.0** %12, align 8
  %173 = load i32, i32* %16, align 4
  %174 = zext i32 %173 to i64
  %175 = getelementptr inbounds %class.KV.0, %class.KV.0* %172, i64 %174
  %176 = bitcast %class.KV.0* %175 to i8*
  %177 = load i32, i32* %15, align 4
  %178 = load i32, i32* %16, align 4
  %179 = sub i32 %177, %178
  %180 = zext i32 %179 to i64
  %181 = mul i64 %180, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %171, i8* %176, i64 %181, i32 8, i1 false)
  %182 = load %class.KV.0*, %class.KV.0** %24, align 8
  %183 = load i32, i32* %16, align 4
  %184 = zext i32 %183 to i64
  %185 = getelementptr inbounds %class.KV.0, %class.KV.0* %182, i64 %184
  %186 = bitcast %class.KV.0* %185 to i8*
  %187 = bitcast i8* %186 to %class.KV.0*
  %188 = load %class.key*, %class.key** %9, align 8
  %189 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EPKS0_S3_(%class.KV.0* %187, %class.key* %188, %class.key* %189)
  %190 = load i64, i64* %13, align 8
  %191 = load i32, i32* %14, align 4
  %192 = zext i32 %191 to i64
  %193 = shl i64 1, %192
  %194 = or i64 %190, %193
  %195 = shl i64 %194, 1
  %196 = or i64 %195, 1
  %197 = load %class.KV.0*, %class.KV.0** %24, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %0, i64 %196, %class.KV.0* %197)
  br label %198

; <label>:198:                                    ; preds = %149, %130, %90, %78
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EE3KeyC2EPKS0_(%"union.KV<key, key, 0>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 0>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 0>::Key"* %0, %"union.KV<key, key, 0>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 0>::Key"*, %"union.KV<key, key, 0>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 0>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj1EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.0* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.1, align 8
  %17 = alloca %class.KV.1*, align 8
  %18 = alloca %class.KV.1*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %14, align 4
  %23 = load i64, i64* %11, align 8
  %24 = and i64 %23, 63
  %25 = urem i64 %24, 63
  %26 = trunc i64 %25 to i32
  store i32 %26, i32* %15, align 4
  %27 = load i32, i32* %14, align 4
  %28 = load i32, i32* %15, align 4
  %29 = icmp eq i32 %27, %28
  br i1 %29, label %30, label %51

; <label>:30:                                     ; preds = %7
  %31 = load i64, i64* %8, align 8
  %32 = lshr i64 %31, 6
  %33 = load %class.key*, %class.key** %9, align 8
  %34 = load %class.key*, %class.key** %10, align 8
  %35 = load i64, i64* %11, align 8
  %36 = lshr i64 %35, 6
  %37 = load %class.key*, %class.key** %12, align 8
  %38 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj2EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.1* sret %16, i64 %32, %class.key* %33, %class.key* %34, i64 %36, %class.key* %37, %class.key* %38)
  %39 = call noalias i8* @malloc(i64 16) #8
  %40 = bitcast i8* %39 to %class.KV.1*
  store %class.KV.1* %40, %class.KV.1** %17, align 8
  %41 = load %class.KV.1*, %class.KV.1** %17, align 8
  %42 = getelementptr inbounds %class.KV.1, %class.KV.1* %41, i64 0
  %43 = bitcast %class.KV.1* %42 to i8*
  %44 = bitcast i8* %43 to %class.KV.1*
  call void @_ZN2KVI3keyS0_Lj2EEC2ERKS1_(%class.KV.1* %44, %class.KV.1* dereferenceable(16) %16)
  %45 = load i32, i32* %14, align 4
  %46 = zext i32 %45 to i64
  %47 = shl i64 1, %46
  %48 = shl i64 %47, 1
  %49 = or i64 %48, 1
  %50 = load %class.KV.1*, %class.KV.1** %17, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %0, i64 %49, %class.KV.1* %50)
  br label %94

; <label>:51:                                     ; preds = %7
  %52 = call noalias i8* @malloc(i64 32) #8
  %53 = bitcast i8* %52 to %class.KV.1*
  store %class.KV.1* %53, %class.KV.1** %18, align 8
  %54 = load i32, i32* %15, align 4
  %55 = load i32, i32* %14, align 4
  %56 = icmp ult i32 %54, %55
  br i1 %56, label %57, label %70

; <label>:57:                                     ; preds = %51
  %58 = load %class.KV.1*, %class.KV.1** %18, align 8
  %59 = getelementptr inbounds %class.KV.1, %class.KV.1* %58, i64 0
  %60 = bitcast %class.KV.1* %59 to i8*
  %61 = bitcast i8* %60 to %class.KV.1*
  %62 = load %class.key*, %class.key** %12, align 8
  %63 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %61, %class.key* %62, %class.key* %63)
  %64 = load %class.KV.1*, %class.KV.1** %18, align 8
  %65 = getelementptr inbounds %class.KV.1, %class.KV.1* %64, i64 1
  %66 = bitcast %class.KV.1* %65 to i8*
  %67 = bitcast i8* %66 to %class.KV.1*
  %68 = load %class.key*, %class.key** %9, align 8
  %69 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %67, %class.key* %68, %class.key* %69)
  br label %83

; <label>:70:                                     ; preds = %51
  %71 = load %class.KV.1*, %class.KV.1** %18, align 8
  %72 = getelementptr inbounds %class.KV.1, %class.KV.1* %71, i64 0
  %73 = bitcast %class.KV.1* %72 to i8*
  %74 = bitcast i8* %73 to %class.KV.1*
  %75 = load %class.key*, %class.key** %9, align 8
  %76 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %74, %class.key* %75, %class.key* %76)
  %77 = load %class.KV.1*, %class.KV.1** %18, align 8
  %78 = getelementptr inbounds %class.KV.1, %class.KV.1* %77, i64 1
  %79 = bitcast %class.KV.1* %78 to i8*
  %80 = bitcast i8* %79 to %class.KV.1*
  %81 = load %class.key*, %class.key** %12, align 8
  %82 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %80, %class.key* %81, %class.key* %82)
  br label %83

; <label>:83:                                     ; preds = %70, %57
  %84 = load i32, i32* %14, align 4
  %85 = zext i32 %84 to i64
  %86 = shl i64 1, %85
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = shl i64 1, %88
  %90 = or i64 %86, %89
  %91 = shl i64 %90, 1
  %92 = or i64 %91, 1
  %93 = load %class.KV.1*, %class.KV.1** %18, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %0, i64 %92, %class.KV.1* %93)
  br label %94

; <label>:94:                                     ; preds = %83, %30
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj1EEC2ERKS1_(%class.KV.0*, %class.KV.0* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV.0*, align 8
  %4 = alloca %class.KV.0*, align 8
  store %class.KV.0* %0, %class.KV.0** %3, align 8
  store %class.KV.0* %1, %class.KV.0** %4, align 8
  %5 = load %class.KV.0*, %class.KV.0** %3, align 8
  %6 = getelementptr inbounds %class.KV.0, %class.KV.0* %5, i32 0, i32 0
  %7 = load %class.KV.0*, %class.KV.0** %4, align 8
  %8 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 1>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 1>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.0, %class.KV.0* %5, i32 0, i32 1
  %12 = load %class.KV.0*, %class.KV.0** %4, align 8
  %13 = getelementptr inbounds %class.KV.0, %class.KV.0* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 1>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 1>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV*, i64, %class.KV.0*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.0*, align 8
  store %class.KV* %0, %class.KV** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.0* %2, %class.KV.0** %6, align 8
  %7 = load %class.KV*, %class.KV** %4, align 8
  %8 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj0EE3KeyC2Em(%"union.KV<key, key, 0>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 1
  %11 = load %class.KV.0*, %class.KV.0** %6, align 8
  call void @_ZN2KVI3keyS0_Lj0EE3ValC2EPKS_IS0_S0_Lj1EE(%"union.KV<key, key, 0>::Val"* %10, %class.KV.0* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj1EEC2EPKS0_S3_(%class.KV.0*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.0*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV.0* %0, %class.KV.0** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV.0*, %class.KV.0** %4, align 8
  %8 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj1EE3KeyC2EPKS0_(%"union.KV<key, key, 1>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj1EE3ValC2EPKS0_(%"union.KV<key, key, 1>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj2EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.1* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.2, align 8
  %17 = alloca %class.KV.2*, align 8
  %18 = alloca %class.KV.2*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %14, align 4
  %23 = load i64, i64* %11, align 8
  %24 = and i64 %23, 63
  %25 = urem i64 %24, 63
  %26 = trunc i64 %25 to i32
  store i32 %26, i32* %15, align 4
  %27 = load i32, i32* %14, align 4
  %28 = load i32, i32* %15, align 4
  %29 = icmp eq i32 %27, %28
  br i1 %29, label %30, label %51

; <label>:30:                                     ; preds = %7
  %31 = load i64, i64* %8, align 8
  %32 = lshr i64 %31, 6
  %33 = load %class.key*, %class.key** %9, align 8
  %34 = load %class.key*, %class.key** %10, align 8
  %35 = load i64, i64* %11, align 8
  %36 = lshr i64 %35, 6
  %37 = load %class.key*, %class.key** %12, align 8
  %38 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj3EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.2* sret %16, i64 %32, %class.key* %33, %class.key* %34, i64 %36, %class.key* %37, %class.key* %38)
  %39 = call noalias i8* @malloc(i64 16) #8
  %40 = bitcast i8* %39 to %class.KV.2*
  store %class.KV.2* %40, %class.KV.2** %17, align 8
  %41 = load %class.KV.2*, %class.KV.2** %17, align 8
  %42 = getelementptr inbounds %class.KV.2, %class.KV.2* %41, i64 0
  %43 = bitcast %class.KV.2* %42 to i8*
  %44 = bitcast i8* %43 to %class.KV.2*
  call void @_ZN2KVI3keyS0_Lj3EEC2ERKS1_(%class.KV.2* %44, %class.KV.2* dereferenceable(16) %16)
  %45 = load i32, i32* %14, align 4
  %46 = zext i32 %45 to i64
  %47 = shl i64 1, %46
  %48 = shl i64 %47, 1
  %49 = or i64 %48, 1
  %50 = load %class.KV.2*, %class.KV.2** %17, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %0, i64 %49, %class.KV.2* %50)
  br label %94

; <label>:51:                                     ; preds = %7
  %52 = call noalias i8* @malloc(i64 32) #8
  %53 = bitcast i8* %52 to %class.KV.2*
  store %class.KV.2* %53, %class.KV.2** %18, align 8
  %54 = load i32, i32* %15, align 4
  %55 = load i32, i32* %14, align 4
  %56 = icmp ult i32 %54, %55
  br i1 %56, label %57, label %70

; <label>:57:                                     ; preds = %51
  %58 = load %class.KV.2*, %class.KV.2** %18, align 8
  %59 = getelementptr inbounds %class.KV.2, %class.KV.2* %58, i64 0
  %60 = bitcast %class.KV.2* %59 to i8*
  %61 = bitcast i8* %60 to %class.KV.2*
  %62 = load %class.key*, %class.key** %12, align 8
  %63 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %61, %class.key* %62, %class.key* %63)
  %64 = load %class.KV.2*, %class.KV.2** %18, align 8
  %65 = getelementptr inbounds %class.KV.2, %class.KV.2* %64, i64 1
  %66 = bitcast %class.KV.2* %65 to i8*
  %67 = bitcast i8* %66 to %class.KV.2*
  %68 = load %class.key*, %class.key** %9, align 8
  %69 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %67, %class.key* %68, %class.key* %69)
  br label %83

; <label>:70:                                     ; preds = %51
  %71 = load %class.KV.2*, %class.KV.2** %18, align 8
  %72 = getelementptr inbounds %class.KV.2, %class.KV.2* %71, i64 0
  %73 = bitcast %class.KV.2* %72 to i8*
  %74 = bitcast i8* %73 to %class.KV.2*
  %75 = load %class.key*, %class.key** %9, align 8
  %76 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %74, %class.key* %75, %class.key* %76)
  %77 = load %class.KV.2*, %class.KV.2** %18, align 8
  %78 = getelementptr inbounds %class.KV.2, %class.KV.2* %77, i64 1
  %79 = bitcast %class.KV.2* %78 to i8*
  %80 = bitcast i8* %79 to %class.KV.2*
  %81 = load %class.key*, %class.key** %12, align 8
  %82 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %80, %class.key* %81, %class.key* %82)
  br label %83

; <label>:83:                                     ; preds = %70, %57
  %84 = load i32, i32* %14, align 4
  %85 = zext i32 %84 to i64
  %86 = shl i64 1, %85
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = shl i64 1, %88
  %90 = or i64 %86, %89
  %91 = shl i64 %90, 1
  %92 = or i64 %91, 1
  %93 = load %class.KV.2*, %class.KV.2** %18, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %0, i64 %92, %class.KV.2* %93)
  br label %94

; <label>:94:                                     ; preds = %83, %30
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj2EEC2ERKS1_(%class.KV.1*, %class.KV.1* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV.1*, align 8
  %4 = alloca %class.KV.1*, align 8
  store %class.KV.1* %0, %class.KV.1** %3, align 8
  store %class.KV.1* %1, %class.KV.1** %4, align 8
  %5 = load %class.KV.1*, %class.KV.1** %3, align 8
  %6 = getelementptr inbounds %class.KV.1, %class.KV.1* %5, i32 0, i32 0
  %7 = load %class.KV.1*, %class.KV.1** %4, align 8
  %8 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 2>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 2>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.1, %class.KV.1* %5, i32 0, i32 1
  %12 = load %class.KV.1*, %class.KV.1** %4, align 8
  %13 = getelementptr inbounds %class.KV.1, %class.KV.1* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 2>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 2>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0*, i64, %class.KV.1*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.0*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.1*, align 8
  store %class.KV.0* %0, %class.KV.0** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.1* %2, %class.KV.1** %6, align 8
  %7 = load %class.KV.0*, %class.KV.0** %4, align 8
  %8 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj1EE3KeyC2Em(%"union.KV<key, key, 1>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.0, %class.KV.0* %7, i32 0, i32 1
  %11 = load %class.KV.1*, %class.KV.1** %6, align 8
  call void @_ZN2KVI3keyS0_Lj1EE3ValC2EPKS_IS0_S0_Lj2EE(%"union.KV<key, key, 1>::Val"* %10, %class.KV.1* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj2EEC2EPKS0_S3_(%class.KV.1*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.1*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV.1* %0, %class.KV.1** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV.1*, %class.KV.1** %4, align 8
  %8 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj2EE3KeyC2EPKS0_(%"union.KV<key, key, 2>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj2EE3ValC2EPKS0_(%"union.KV<key, key, 2>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj3EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.2* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.3, align 8
  %17 = alloca %class.KV.3*, align 8
  %18 = alloca %class.KV.3*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %14, align 4
  %23 = load i64, i64* %11, align 8
  %24 = and i64 %23, 63
  %25 = urem i64 %24, 63
  %26 = trunc i64 %25 to i32
  store i32 %26, i32* %15, align 4
  %27 = load i32, i32* %14, align 4
  %28 = load i32, i32* %15, align 4
  %29 = icmp eq i32 %27, %28
  br i1 %29, label %30, label %51

; <label>:30:                                     ; preds = %7
  %31 = load i64, i64* %8, align 8
  %32 = lshr i64 %31, 6
  %33 = load %class.key*, %class.key** %9, align 8
  %34 = load %class.key*, %class.key** %10, align 8
  %35 = load i64, i64* %11, align 8
  %36 = lshr i64 %35, 6
  %37 = load %class.key*, %class.key** %12, align 8
  %38 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj4EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.3* sret %16, i64 %32, %class.key* %33, %class.key* %34, i64 %36, %class.key* %37, %class.key* %38)
  %39 = call noalias i8* @malloc(i64 16) #8
  %40 = bitcast i8* %39 to %class.KV.3*
  store %class.KV.3* %40, %class.KV.3** %17, align 8
  %41 = load %class.KV.3*, %class.KV.3** %17, align 8
  %42 = getelementptr inbounds %class.KV.3, %class.KV.3* %41, i64 0
  %43 = bitcast %class.KV.3* %42 to i8*
  %44 = bitcast i8* %43 to %class.KV.3*
  call void @_ZN2KVI3keyS0_Lj4EEC2ERKS1_(%class.KV.3* %44, %class.KV.3* dereferenceable(16) %16)
  %45 = load i32, i32* %14, align 4
  %46 = zext i32 %45 to i64
  %47 = shl i64 1, %46
  %48 = shl i64 %47, 1
  %49 = or i64 %48, 1
  %50 = load %class.KV.3*, %class.KV.3** %17, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %0, i64 %49, %class.KV.3* %50)
  br label %94

; <label>:51:                                     ; preds = %7
  %52 = call noalias i8* @malloc(i64 32) #8
  %53 = bitcast i8* %52 to %class.KV.3*
  store %class.KV.3* %53, %class.KV.3** %18, align 8
  %54 = load i32, i32* %15, align 4
  %55 = load i32, i32* %14, align 4
  %56 = icmp ult i32 %54, %55
  br i1 %56, label %57, label %70

; <label>:57:                                     ; preds = %51
  %58 = load %class.KV.3*, %class.KV.3** %18, align 8
  %59 = getelementptr inbounds %class.KV.3, %class.KV.3* %58, i64 0
  %60 = bitcast %class.KV.3* %59 to i8*
  %61 = bitcast i8* %60 to %class.KV.3*
  %62 = load %class.key*, %class.key** %12, align 8
  %63 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %61, %class.key* %62, %class.key* %63)
  %64 = load %class.KV.3*, %class.KV.3** %18, align 8
  %65 = getelementptr inbounds %class.KV.3, %class.KV.3* %64, i64 1
  %66 = bitcast %class.KV.3* %65 to i8*
  %67 = bitcast i8* %66 to %class.KV.3*
  %68 = load %class.key*, %class.key** %9, align 8
  %69 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %67, %class.key* %68, %class.key* %69)
  br label %83

; <label>:70:                                     ; preds = %51
  %71 = load %class.KV.3*, %class.KV.3** %18, align 8
  %72 = getelementptr inbounds %class.KV.3, %class.KV.3* %71, i64 0
  %73 = bitcast %class.KV.3* %72 to i8*
  %74 = bitcast i8* %73 to %class.KV.3*
  %75 = load %class.key*, %class.key** %9, align 8
  %76 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %74, %class.key* %75, %class.key* %76)
  %77 = load %class.KV.3*, %class.KV.3** %18, align 8
  %78 = getelementptr inbounds %class.KV.3, %class.KV.3* %77, i64 1
  %79 = bitcast %class.KV.3* %78 to i8*
  %80 = bitcast i8* %79 to %class.KV.3*
  %81 = load %class.key*, %class.key** %12, align 8
  %82 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %80, %class.key* %81, %class.key* %82)
  br label %83

; <label>:83:                                     ; preds = %70, %57
  %84 = load i32, i32* %14, align 4
  %85 = zext i32 %84 to i64
  %86 = shl i64 1, %85
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = shl i64 1, %88
  %90 = or i64 %86, %89
  %91 = shl i64 %90, 1
  %92 = or i64 %91, 1
  %93 = load %class.KV.3*, %class.KV.3** %18, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %0, i64 %92, %class.KV.3* %93)
  br label %94

; <label>:94:                                     ; preds = %83, %30
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj3EEC2ERKS1_(%class.KV.2*, %class.KV.2* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV.2*, align 8
  %4 = alloca %class.KV.2*, align 8
  store %class.KV.2* %0, %class.KV.2** %3, align 8
  store %class.KV.2* %1, %class.KV.2** %4, align 8
  %5 = load %class.KV.2*, %class.KV.2** %3, align 8
  %6 = getelementptr inbounds %class.KV.2, %class.KV.2* %5, i32 0, i32 0
  %7 = load %class.KV.2*, %class.KV.2** %4, align 8
  %8 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 3>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 3>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.2, %class.KV.2* %5, i32 0, i32 1
  %12 = load %class.KV.2*, %class.KV.2** %4, align 8
  %13 = getelementptr inbounds %class.KV.2, %class.KV.2* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 3>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 3>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1*, i64, %class.KV.2*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.1*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.2*, align 8
  store %class.KV.1* %0, %class.KV.1** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.2* %2, %class.KV.2** %6, align 8
  %7 = load %class.KV.1*, %class.KV.1** %4, align 8
  %8 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj2EE3KeyC2Em(%"union.KV<key, key, 2>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.1, %class.KV.1* %7, i32 0, i32 1
  %11 = load %class.KV.2*, %class.KV.2** %6, align 8
  call void @_ZN2KVI3keyS0_Lj2EE3ValC2EPKS_IS0_S0_Lj3EE(%"union.KV<key, key, 2>::Val"* %10, %class.KV.2* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj3EEC2EPKS0_S3_(%class.KV.2*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.2*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV.2* %0, %class.KV.2** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV.2*, %class.KV.2** %4, align 8
  %8 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj3EE3KeyC2EPKS0_(%"union.KV<key, key, 3>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj3EE3ValC2EPKS0_(%"union.KV<key, key, 3>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj4EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.3* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.4, align 8
  %17 = alloca %class.KV.4*, align 8
  %18 = alloca %class.KV.4*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %14, align 4
  %23 = load i64, i64* %11, align 8
  %24 = and i64 %23, 63
  %25 = urem i64 %24, 63
  %26 = trunc i64 %25 to i32
  store i32 %26, i32* %15, align 4
  %27 = load i32, i32* %14, align 4
  %28 = load i32, i32* %15, align 4
  %29 = icmp eq i32 %27, %28
  br i1 %29, label %30, label %51

; <label>:30:                                     ; preds = %7
  %31 = load i64, i64* %8, align 8
  %32 = lshr i64 %31, 6
  %33 = load %class.key*, %class.key** %9, align 8
  %34 = load %class.key*, %class.key** %10, align 8
  %35 = load i64, i64* %11, align 8
  %36 = lshr i64 %35, 6
  %37 = load %class.key*, %class.key** %12, align 8
  %38 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj5EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.4* sret %16, i64 %32, %class.key* %33, %class.key* %34, i64 %36, %class.key* %37, %class.key* %38)
  %39 = call noalias i8* @malloc(i64 16) #8
  %40 = bitcast i8* %39 to %class.KV.4*
  store %class.KV.4* %40, %class.KV.4** %17, align 8
  %41 = load %class.KV.4*, %class.KV.4** %17, align 8
  %42 = getelementptr inbounds %class.KV.4, %class.KV.4* %41, i64 0
  %43 = bitcast %class.KV.4* %42 to i8*
  %44 = bitcast i8* %43 to %class.KV.4*
  call void @_ZN2KVI3keyS0_Lj5EEC2ERKS1_(%class.KV.4* %44, %class.KV.4* dereferenceable(16) %16)
  %45 = load i32, i32* %14, align 4
  %46 = zext i32 %45 to i64
  %47 = shl i64 1, %46
  %48 = shl i64 %47, 1
  %49 = or i64 %48, 1
  %50 = load %class.KV.4*, %class.KV.4** %17, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %0, i64 %49, %class.KV.4* %50)
  br label %94

; <label>:51:                                     ; preds = %7
  %52 = call noalias i8* @malloc(i64 32) #8
  %53 = bitcast i8* %52 to %class.KV.4*
  store %class.KV.4* %53, %class.KV.4** %18, align 8
  %54 = load i32, i32* %15, align 4
  %55 = load i32, i32* %14, align 4
  %56 = icmp ult i32 %54, %55
  br i1 %56, label %57, label %70

; <label>:57:                                     ; preds = %51
  %58 = load %class.KV.4*, %class.KV.4** %18, align 8
  %59 = getelementptr inbounds %class.KV.4, %class.KV.4* %58, i64 0
  %60 = bitcast %class.KV.4* %59 to i8*
  %61 = bitcast i8* %60 to %class.KV.4*
  %62 = load %class.key*, %class.key** %12, align 8
  %63 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %61, %class.key* %62, %class.key* %63)
  %64 = load %class.KV.4*, %class.KV.4** %18, align 8
  %65 = getelementptr inbounds %class.KV.4, %class.KV.4* %64, i64 1
  %66 = bitcast %class.KV.4* %65 to i8*
  %67 = bitcast i8* %66 to %class.KV.4*
  %68 = load %class.key*, %class.key** %9, align 8
  %69 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %67, %class.key* %68, %class.key* %69)
  br label %83

; <label>:70:                                     ; preds = %51
  %71 = load %class.KV.4*, %class.KV.4** %18, align 8
  %72 = getelementptr inbounds %class.KV.4, %class.KV.4* %71, i64 0
  %73 = bitcast %class.KV.4* %72 to i8*
  %74 = bitcast i8* %73 to %class.KV.4*
  %75 = load %class.key*, %class.key** %9, align 8
  %76 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %74, %class.key* %75, %class.key* %76)
  %77 = load %class.KV.4*, %class.KV.4** %18, align 8
  %78 = getelementptr inbounds %class.KV.4, %class.KV.4* %77, i64 1
  %79 = bitcast %class.KV.4* %78 to i8*
  %80 = bitcast i8* %79 to %class.KV.4*
  %81 = load %class.key*, %class.key** %12, align 8
  %82 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %80, %class.key* %81, %class.key* %82)
  br label %83

; <label>:83:                                     ; preds = %70, %57
  %84 = load i32, i32* %14, align 4
  %85 = zext i32 %84 to i64
  %86 = shl i64 1, %85
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = shl i64 1, %88
  %90 = or i64 %86, %89
  %91 = shl i64 %90, 1
  %92 = or i64 %91, 1
  %93 = load %class.KV.4*, %class.KV.4** %18, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %0, i64 %92, %class.KV.4* %93)
  br label %94

; <label>:94:                                     ; preds = %83, %30
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj4EEC2ERKS1_(%class.KV.3*, %class.KV.3* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV.3*, align 8
  %4 = alloca %class.KV.3*, align 8
  store %class.KV.3* %0, %class.KV.3** %3, align 8
  store %class.KV.3* %1, %class.KV.3** %4, align 8
  %5 = load %class.KV.3*, %class.KV.3** %3, align 8
  %6 = getelementptr inbounds %class.KV.3, %class.KV.3* %5, i32 0, i32 0
  %7 = load %class.KV.3*, %class.KV.3** %4, align 8
  %8 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 4>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 4>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.3, %class.KV.3* %5, i32 0, i32 1
  %12 = load %class.KV.3*, %class.KV.3** %4, align 8
  %13 = getelementptr inbounds %class.KV.3, %class.KV.3* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 4>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 4>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2*, i64, %class.KV.3*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.2*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.3*, align 8
  store %class.KV.2* %0, %class.KV.2** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.3* %2, %class.KV.3** %6, align 8
  %7 = load %class.KV.2*, %class.KV.2** %4, align 8
  %8 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj3EE3KeyC2Em(%"union.KV<key, key, 3>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.2, %class.KV.2* %7, i32 0, i32 1
  %11 = load %class.KV.3*, %class.KV.3** %6, align 8
  call void @_ZN2KVI3keyS0_Lj3EE3ValC2EPKS_IS0_S0_Lj4EE(%"union.KV<key, key, 3>::Val"* %10, %class.KV.3* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj4EEC2EPKS0_S3_(%class.KV.3*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.3*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV.3* %0, %class.KV.3** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV.3*, %class.KV.3** %4, align 8
  %8 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj4EE3KeyC2EPKS0_(%"union.KV<key, key, 4>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj4EE3ValC2EPKS0_(%"union.KV<key, key, 4>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj5EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.4* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.5, align 8
  %17 = alloca %class.KV.5*, align 8
  %18 = alloca %class.KV.5*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %14, align 4
  %23 = load i64, i64* %11, align 8
  %24 = and i64 %23, 63
  %25 = urem i64 %24, 63
  %26 = trunc i64 %25 to i32
  store i32 %26, i32* %15, align 4
  %27 = load i32, i32* %14, align 4
  %28 = load i32, i32* %15, align 4
  %29 = icmp eq i32 %27, %28
  br i1 %29, label %30, label %51

; <label>:30:                                     ; preds = %7
  %31 = load i64, i64* %8, align 8
  %32 = lshr i64 %31, 6
  %33 = load %class.key*, %class.key** %9, align 8
  %34 = load %class.key*, %class.key** %10, align 8
  %35 = load i64, i64* %11, align 8
  %36 = lshr i64 %35, 6
  %37 = load %class.key*, %class.key** %12, align 8
  %38 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj6EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.5* sret %16, i64 %32, %class.key* %33, %class.key* %34, i64 %36, %class.key* %37, %class.key* %38)
  %39 = call noalias i8* @malloc(i64 16) #8
  %40 = bitcast i8* %39 to %class.KV.5*
  store %class.KV.5* %40, %class.KV.5** %17, align 8
  %41 = load %class.KV.5*, %class.KV.5** %17, align 8
  %42 = getelementptr inbounds %class.KV.5, %class.KV.5* %41, i64 0
  %43 = bitcast %class.KV.5* %42 to i8*
  %44 = bitcast i8* %43 to %class.KV.5*
  call void @_ZN2KVI3keyS0_Lj6EEC2ERKS1_(%class.KV.5* %44, %class.KV.5* dereferenceable(16) %16)
  %45 = load i32, i32* %14, align 4
  %46 = zext i32 %45 to i64
  %47 = shl i64 1, %46
  %48 = shl i64 %47, 1
  %49 = or i64 %48, 1
  %50 = load %class.KV.5*, %class.KV.5** %17, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %0, i64 %49, %class.KV.5* %50)
  br label %94

; <label>:51:                                     ; preds = %7
  %52 = call noalias i8* @malloc(i64 32) #8
  %53 = bitcast i8* %52 to %class.KV.5*
  store %class.KV.5* %53, %class.KV.5** %18, align 8
  %54 = load i32, i32* %15, align 4
  %55 = load i32, i32* %14, align 4
  %56 = icmp ult i32 %54, %55
  br i1 %56, label %57, label %70

; <label>:57:                                     ; preds = %51
  %58 = load %class.KV.5*, %class.KV.5** %18, align 8
  %59 = getelementptr inbounds %class.KV.5, %class.KV.5* %58, i64 0
  %60 = bitcast %class.KV.5* %59 to i8*
  %61 = bitcast i8* %60 to %class.KV.5*
  %62 = load %class.key*, %class.key** %12, align 8
  %63 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %61, %class.key* %62, %class.key* %63)
  %64 = load %class.KV.5*, %class.KV.5** %18, align 8
  %65 = getelementptr inbounds %class.KV.5, %class.KV.5* %64, i64 1
  %66 = bitcast %class.KV.5* %65 to i8*
  %67 = bitcast i8* %66 to %class.KV.5*
  %68 = load %class.key*, %class.key** %9, align 8
  %69 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %67, %class.key* %68, %class.key* %69)
  br label %83

; <label>:70:                                     ; preds = %51
  %71 = load %class.KV.5*, %class.KV.5** %18, align 8
  %72 = getelementptr inbounds %class.KV.5, %class.KV.5* %71, i64 0
  %73 = bitcast %class.KV.5* %72 to i8*
  %74 = bitcast i8* %73 to %class.KV.5*
  %75 = load %class.key*, %class.key** %9, align 8
  %76 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %74, %class.key* %75, %class.key* %76)
  %77 = load %class.KV.5*, %class.KV.5** %18, align 8
  %78 = getelementptr inbounds %class.KV.5, %class.KV.5* %77, i64 1
  %79 = bitcast %class.KV.5* %78 to i8*
  %80 = bitcast i8* %79 to %class.KV.5*
  %81 = load %class.key*, %class.key** %12, align 8
  %82 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %80, %class.key* %81, %class.key* %82)
  br label %83

; <label>:83:                                     ; preds = %70, %57
  %84 = load i32, i32* %14, align 4
  %85 = zext i32 %84 to i64
  %86 = shl i64 1, %85
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = shl i64 1, %88
  %90 = or i64 %86, %89
  %91 = shl i64 %90, 1
  %92 = or i64 %91, 1
  %93 = load %class.KV.5*, %class.KV.5** %18, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %0, i64 %92, %class.KV.5* %93)
  br label %94

; <label>:94:                                     ; preds = %83, %30
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj5EEC2ERKS1_(%class.KV.4*, %class.KV.4* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV.4*, align 8
  %4 = alloca %class.KV.4*, align 8
  store %class.KV.4* %0, %class.KV.4** %3, align 8
  store %class.KV.4* %1, %class.KV.4** %4, align 8
  %5 = load %class.KV.4*, %class.KV.4** %3, align 8
  %6 = getelementptr inbounds %class.KV.4, %class.KV.4* %5, i32 0, i32 0
  %7 = load %class.KV.4*, %class.KV.4** %4, align 8
  %8 = getelementptr inbounds %class.KV.4, %class.KV.4* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 5>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 5>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.4, %class.KV.4* %5, i32 0, i32 1
  %12 = load %class.KV.4*, %class.KV.4** %4, align 8
  %13 = getelementptr inbounds %class.KV.4, %class.KV.4* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 5>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 5>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3*, i64, %class.KV.4*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.3*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.4*, align 8
  store %class.KV.3* %0, %class.KV.3** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.4* %2, %class.KV.4** %6, align 8
  %7 = load %class.KV.3*, %class.KV.3** %4, align 8
  %8 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj4EE3KeyC2Em(%"union.KV<key, key, 4>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.3, %class.KV.3* %7, i32 0, i32 1
  %11 = load %class.KV.4*, %class.KV.4** %6, align 8
  call void @_ZN2KVI3keyS0_Lj4EE3ValC2EPKS_IS0_S0_Lj5EE(%"union.KV<key, key, 4>::Val"* %10, %class.KV.4* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj5EEC2EPKS0_S3_(%class.KV.4*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.4*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV.4* %0, %class.KV.4** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV.4*, %class.KV.4** %4, align 8
  %8 = getelementptr inbounds %class.KV.4, %class.KV.4* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj5EE3KeyC2EPKS0_(%"union.KV<key, key, 5>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.4, %class.KV.4* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj5EE3ValC2EPKS0_(%"union.KV<key, key, 5>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj6EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.5* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.6, align 8
  %17 = alloca %class.KV.6*, align 8
  %18 = alloca %class.KV.6*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %14, align 4
  %23 = load i64, i64* %11, align 8
  %24 = and i64 %23, 63
  %25 = urem i64 %24, 63
  %26 = trunc i64 %25 to i32
  store i32 %26, i32* %15, align 4
  %27 = load i32, i32* %14, align 4
  %28 = load i32, i32* %15, align 4
  %29 = icmp eq i32 %27, %28
  br i1 %29, label %30, label %51

; <label>:30:                                     ; preds = %7
  %31 = load i64, i64* %8, align 8
  %32 = lshr i64 %31, 6
  %33 = load %class.key*, %class.key** %9, align 8
  %34 = load %class.key*, %class.key** %10, align 8
  %35 = load i64, i64* %11, align 8
  %36 = lshr i64 %35, 6
  %37 = load %class.key*, %class.key** %12, align 8
  %38 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj7EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.6* sret %16, i64 %32, %class.key* %33, %class.key* %34, i64 %36, %class.key* %37, %class.key* %38)
  %39 = call noalias i8* @malloc(i64 16) #8
  %40 = bitcast i8* %39 to %class.KV.6*
  store %class.KV.6* %40, %class.KV.6** %17, align 8
  %41 = load %class.KV.6*, %class.KV.6** %17, align 8
  %42 = getelementptr inbounds %class.KV.6, %class.KV.6* %41, i64 0
  %43 = bitcast %class.KV.6* %42 to i8*
  %44 = bitcast i8* %43 to %class.KV.6*
  call void @_ZN2KVI3keyS0_Lj7EEC2ERKS1_(%class.KV.6* %44, %class.KV.6* dereferenceable(16) %16)
  %45 = load i32, i32* %14, align 4
  %46 = zext i32 %45 to i64
  %47 = shl i64 1, %46
  %48 = shl i64 %47, 1
  %49 = or i64 %48, 1
  %50 = load %class.KV.6*, %class.KV.6** %17, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %0, i64 %49, %class.KV.6* %50)
  br label %94

; <label>:51:                                     ; preds = %7
  %52 = call noalias i8* @malloc(i64 32) #8
  %53 = bitcast i8* %52 to %class.KV.6*
  store %class.KV.6* %53, %class.KV.6** %18, align 8
  %54 = load i32, i32* %15, align 4
  %55 = load i32, i32* %14, align 4
  %56 = icmp ult i32 %54, %55
  br i1 %56, label %57, label %70

; <label>:57:                                     ; preds = %51
  %58 = load %class.KV.6*, %class.KV.6** %18, align 8
  %59 = getelementptr inbounds %class.KV.6, %class.KV.6* %58, i64 0
  %60 = bitcast %class.KV.6* %59 to i8*
  %61 = bitcast i8* %60 to %class.KV.6*
  %62 = load %class.key*, %class.key** %12, align 8
  %63 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %61, %class.key* %62, %class.key* %63)
  %64 = load %class.KV.6*, %class.KV.6** %18, align 8
  %65 = getelementptr inbounds %class.KV.6, %class.KV.6* %64, i64 1
  %66 = bitcast %class.KV.6* %65 to i8*
  %67 = bitcast i8* %66 to %class.KV.6*
  %68 = load %class.key*, %class.key** %9, align 8
  %69 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %67, %class.key* %68, %class.key* %69)
  br label %83

; <label>:70:                                     ; preds = %51
  %71 = load %class.KV.6*, %class.KV.6** %18, align 8
  %72 = getelementptr inbounds %class.KV.6, %class.KV.6* %71, i64 0
  %73 = bitcast %class.KV.6* %72 to i8*
  %74 = bitcast i8* %73 to %class.KV.6*
  %75 = load %class.key*, %class.key** %9, align 8
  %76 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %74, %class.key* %75, %class.key* %76)
  %77 = load %class.KV.6*, %class.KV.6** %18, align 8
  %78 = getelementptr inbounds %class.KV.6, %class.KV.6* %77, i64 1
  %79 = bitcast %class.KV.6* %78 to i8*
  %80 = bitcast i8* %79 to %class.KV.6*
  %81 = load %class.key*, %class.key** %12, align 8
  %82 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %80, %class.key* %81, %class.key* %82)
  br label %83

; <label>:83:                                     ; preds = %70, %57
  %84 = load i32, i32* %14, align 4
  %85 = zext i32 %84 to i64
  %86 = shl i64 1, %85
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = shl i64 1, %88
  %90 = or i64 %86, %89
  %91 = shl i64 %90, 1
  %92 = or i64 %91, 1
  %93 = load %class.KV.6*, %class.KV.6** %18, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %0, i64 %92, %class.KV.6* %93)
  br label %94

; <label>:94:                                     ; preds = %83, %30
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj6EEC2ERKS1_(%class.KV.5*, %class.KV.5* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV.5*, align 8
  %4 = alloca %class.KV.5*, align 8
  store %class.KV.5* %0, %class.KV.5** %3, align 8
  store %class.KV.5* %1, %class.KV.5** %4, align 8
  %5 = load %class.KV.5*, %class.KV.5** %3, align 8
  %6 = getelementptr inbounds %class.KV.5, %class.KV.5* %5, i32 0, i32 0
  %7 = load %class.KV.5*, %class.KV.5** %4, align 8
  %8 = getelementptr inbounds %class.KV.5, %class.KV.5* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 6>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 6>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.5, %class.KV.5* %5, i32 0, i32 1
  %12 = load %class.KV.5*, %class.KV.5** %4, align 8
  %13 = getelementptr inbounds %class.KV.5, %class.KV.5* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 6>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 6>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4*, i64, %class.KV.5*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.4*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.5*, align 8
  store %class.KV.4* %0, %class.KV.4** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.5* %2, %class.KV.5** %6, align 8
  %7 = load %class.KV.4*, %class.KV.4** %4, align 8
  %8 = getelementptr inbounds %class.KV.4, %class.KV.4* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj5EE3KeyC2Em(%"union.KV<key, key, 5>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.4, %class.KV.4* %7, i32 0, i32 1
  %11 = load %class.KV.5*, %class.KV.5** %6, align 8
  call void @_ZN2KVI3keyS0_Lj5EE3ValC2EPKS_IS0_S0_Lj6EE(%"union.KV<key, key, 5>::Val"* %10, %class.KV.5* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj6EEC2EPKS0_S3_(%class.KV.5*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.5*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV.5* %0, %class.KV.5** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV.5*, %class.KV.5** %4, align 8
  %8 = getelementptr inbounds %class.KV.5, %class.KV.5* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj6EE3KeyC2EPKS0_(%"union.KV<key, key, 6>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.5, %class.KV.5* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj6EE3ValC2EPKS0_(%"union.KV<key, key, 6>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj7EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.6* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.7, align 8
  %17 = alloca %class.KV.7*, align 8
  %18 = alloca %class.KV.7*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %14, align 4
  %23 = load i64, i64* %11, align 8
  %24 = and i64 %23, 63
  %25 = urem i64 %24, 63
  %26 = trunc i64 %25 to i32
  store i32 %26, i32* %15, align 4
  %27 = load i32, i32* %14, align 4
  %28 = load i32, i32* %15, align 4
  %29 = icmp eq i32 %27, %28
  br i1 %29, label %30, label %51

; <label>:30:                                     ; preds = %7
  %31 = load i64, i64* %8, align 8
  %32 = lshr i64 %31, 6
  %33 = load %class.key*, %class.key** %9, align 8
  %34 = load %class.key*, %class.key** %10, align 8
  %35 = load i64, i64* %11, align 8
  %36 = lshr i64 %35, 6
  %37 = load %class.key*, %class.key** %12, align 8
  %38 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj8EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.7* sret %16, i64 %32, %class.key* %33, %class.key* %34, i64 %36, %class.key* %37, %class.key* %38)
  %39 = call noalias i8* @malloc(i64 16) #8
  %40 = bitcast i8* %39 to %class.KV.7*
  store %class.KV.7* %40, %class.KV.7** %17, align 8
  %41 = load %class.KV.7*, %class.KV.7** %17, align 8
  %42 = getelementptr inbounds %class.KV.7, %class.KV.7* %41, i64 0
  %43 = bitcast %class.KV.7* %42 to i8*
  %44 = bitcast i8* %43 to %class.KV.7*
  call void @_ZN2KVI3keyS0_Lj8EEC2ERKS1_(%class.KV.7* %44, %class.KV.7* dereferenceable(16) %16)
  %45 = load i32, i32* %14, align 4
  %46 = zext i32 %45 to i64
  %47 = shl i64 1, %46
  %48 = shl i64 %47, 1
  %49 = or i64 %48, 1
  %50 = load %class.KV.7*, %class.KV.7** %17, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %0, i64 %49, %class.KV.7* %50)
  br label %94

; <label>:51:                                     ; preds = %7
  %52 = call noalias i8* @malloc(i64 32) #8
  %53 = bitcast i8* %52 to %class.KV.7*
  store %class.KV.7* %53, %class.KV.7** %18, align 8
  %54 = load i32, i32* %15, align 4
  %55 = load i32, i32* %14, align 4
  %56 = icmp ult i32 %54, %55
  br i1 %56, label %57, label %70

; <label>:57:                                     ; preds = %51
  %58 = load %class.KV.7*, %class.KV.7** %18, align 8
  %59 = getelementptr inbounds %class.KV.7, %class.KV.7* %58, i64 0
  %60 = bitcast %class.KV.7* %59 to i8*
  %61 = bitcast i8* %60 to %class.KV.7*
  %62 = load %class.key*, %class.key** %12, align 8
  %63 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %61, %class.key* %62, %class.key* %63)
  %64 = load %class.KV.7*, %class.KV.7** %18, align 8
  %65 = getelementptr inbounds %class.KV.7, %class.KV.7* %64, i64 1
  %66 = bitcast %class.KV.7* %65 to i8*
  %67 = bitcast i8* %66 to %class.KV.7*
  %68 = load %class.key*, %class.key** %9, align 8
  %69 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %67, %class.key* %68, %class.key* %69)
  br label %83

; <label>:70:                                     ; preds = %51
  %71 = load %class.KV.7*, %class.KV.7** %18, align 8
  %72 = getelementptr inbounds %class.KV.7, %class.KV.7* %71, i64 0
  %73 = bitcast %class.KV.7* %72 to i8*
  %74 = bitcast i8* %73 to %class.KV.7*
  %75 = load %class.key*, %class.key** %9, align 8
  %76 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %74, %class.key* %75, %class.key* %76)
  %77 = load %class.KV.7*, %class.KV.7** %18, align 8
  %78 = getelementptr inbounds %class.KV.7, %class.KV.7* %77, i64 1
  %79 = bitcast %class.KV.7* %78 to i8*
  %80 = bitcast i8* %79 to %class.KV.7*
  %81 = load %class.key*, %class.key** %12, align 8
  %82 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %80, %class.key* %81, %class.key* %82)
  br label %83

; <label>:83:                                     ; preds = %70, %57
  %84 = load i32, i32* %14, align 4
  %85 = zext i32 %84 to i64
  %86 = shl i64 1, %85
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = shl i64 1, %88
  %90 = or i64 %86, %89
  %91 = shl i64 %90, 1
  %92 = or i64 %91, 1
  %93 = load %class.KV.7*, %class.KV.7** %18, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %0, i64 %92, %class.KV.7* %93)
  br label %94

; <label>:94:                                     ; preds = %83, %30
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj7EEC2ERKS1_(%class.KV.6*, %class.KV.6* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV.6*, align 8
  %4 = alloca %class.KV.6*, align 8
  store %class.KV.6* %0, %class.KV.6** %3, align 8
  store %class.KV.6* %1, %class.KV.6** %4, align 8
  %5 = load %class.KV.6*, %class.KV.6** %3, align 8
  %6 = getelementptr inbounds %class.KV.6, %class.KV.6* %5, i32 0, i32 0
  %7 = load %class.KV.6*, %class.KV.6** %4, align 8
  %8 = getelementptr inbounds %class.KV.6, %class.KV.6* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 7>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 7>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.6, %class.KV.6* %5, i32 0, i32 1
  %12 = load %class.KV.6*, %class.KV.6** %4, align 8
  %13 = getelementptr inbounds %class.KV.6, %class.KV.6* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 7>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 7>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5*, i64, %class.KV.6*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.5*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.6*, align 8
  store %class.KV.5* %0, %class.KV.5** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.6* %2, %class.KV.6** %6, align 8
  %7 = load %class.KV.5*, %class.KV.5** %4, align 8
  %8 = getelementptr inbounds %class.KV.5, %class.KV.5* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj6EE3KeyC2Em(%"union.KV<key, key, 6>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.5, %class.KV.5* %7, i32 0, i32 1
  %11 = load %class.KV.6*, %class.KV.6** %6, align 8
  call void @_ZN2KVI3keyS0_Lj6EE3ValC2EPKS_IS0_S0_Lj7EE(%"union.KV<key, key, 6>::Val"* %10, %class.KV.6* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj7EEC2EPKS0_S3_(%class.KV.6*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.6*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV.6* %0, %class.KV.6** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV.6*, %class.KV.6** %4, align 8
  %8 = getelementptr inbounds %class.KV.6, %class.KV.6* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj7EE3KeyC2EPKS0_(%"union.KV<key, key, 7>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.6, %class.KV.6* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj7EE3ValC2EPKS0_(%"union.KV<key, key, 7>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj8EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.7* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.8, align 8
  %17 = alloca %class.KV.8*, align 8
  %18 = alloca %class.KV.8*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %14, align 4
  %23 = load i64, i64* %11, align 8
  %24 = and i64 %23, 63
  %25 = urem i64 %24, 63
  %26 = trunc i64 %25 to i32
  store i32 %26, i32* %15, align 4
  %27 = load i32, i32* %14, align 4
  %28 = load i32, i32* %15, align 4
  %29 = icmp eq i32 %27, %28
  br i1 %29, label %30, label %51

; <label>:30:                                     ; preds = %7
  %31 = load i64, i64* %8, align 8
  %32 = lshr i64 %31, 6
  %33 = load %class.key*, %class.key** %9, align 8
  %34 = load %class.key*, %class.key** %10, align 8
  %35 = load i64, i64* %11, align 8
  %36 = lshr i64 %35, 6
  %37 = load %class.key*, %class.key** %12, align 8
  %38 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj9EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.8* sret %16, i64 %32, %class.key* %33, %class.key* %34, i64 %36, %class.key* %37, %class.key* %38)
  %39 = call noalias i8* @malloc(i64 16) #8
  %40 = bitcast i8* %39 to %class.KV.8*
  store %class.KV.8* %40, %class.KV.8** %17, align 8
  %41 = load %class.KV.8*, %class.KV.8** %17, align 8
  %42 = getelementptr inbounds %class.KV.8, %class.KV.8* %41, i64 0
  %43 = bitcast %class.KV.8* %42 to i8*
  %44 = bitcast i8* %43 to %class.KV.8*
  call void @_ZN2KVI3keyS0_Lj9EEC2ERKS1_(%class.KV.8* %44, %class.KV.8* dereferenceable(16) %16)
  %45 = load i32, i32* %14, align 4
  %46 = zext i32 %45 to i64
  %47 = shl i64 1, %46
  %48 = shl i64 %47, 1
  %49 = or i64 %48, 1
  %50 = load %class.KV.8*, %class.KV.8** %17, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %0, i64 %49, %class.KV.8* %50)
  br label %94

; <label>:51:                                     ; preds = %7
  %52 = call noalias i8* @malloc(i64 32) #8
  %53 = bitcast i8* %52 to %class.KV.8*
  store %class.KV.8* %53, %class.KV.8** %18, align 8
  %54 = load i32, i32* %15, align 4
  %55 = load i32, i32* %14, align 4
  %56 = icmp ult i32 %54, %55
  br i1 %56, label %57, label %70

; <label>:57:                                     ; preds = %51
  %58 = load %class.KV.8*, %class.KV.8** %18, align 8
  %59 = getelementptr inbounds %class.KV.8, %class.KV.8* %58, i64 0
  %60 = bitcast %class.KV.8* %59 to i8*
  %61 = bitcast i8* %60 to %class.KV.8*
  %62 = load %class.key*, %class.key** %12, align 8
  %63 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %61, %class.key* %62, %class.key* %63)
  %64 = load %class.KV.8*, %class.KV.8** %18, align 8
  %65 = getelementptr inbounds %class.KV.8, %class.KV.8* %64, i64 1
  %66 = bitcast %class.KV.8* %65 to i8*
  %67 = bitcast i8* %66 to %class.KV.8*
  %68 = load %class.key*, %class.key** %9, align 8
  %69 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %67, %class.key* %68, %class.key* %69)
  br label %83

; <label>:70:                                     ; preds = %51
  %71 = load %class.KV.8*, %class.KV.8** %18, align 8
  %72 = getelementptr inbounds %class.KV.8, %class.KV.8* %71, i64 0
  %73 = bitcast %class.KV.8* %72 to i8*
  %74 = bitcast i8* %73 to %class.KV.8*
  %75 = load %class.key*, %class.key** %9, align 8
  %76 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %74, %class.key* %75, %class.key* %76)
  %77 = load %class.KV.8*, %class.KV.8** %18, align 8
  %78 = getelementptr inbounds %class.KV.8, %class.KV.8* %77, i64 1
  %79 = bitcast %class.KV.8* %78 to i8*
  %80 = bitcast i8* %79 to %class.KV.8*
  %81 = load %class.key*, %class.key** %12, align 8
  %82 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %80, %class.key* %81, %class.key* %82)
  br label %83

; <label>:83:                                     ; preds = %70, %57
  %84 = load i32, i32* %14, align 4
  %85 = zext i32 %84 to i64
  %86 = shl i64 1, %85
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = shl i64 1, %88
  %90 = or i64 %86, %89
  %91 = shl i64 %90, 1
  %92 = or i64 %91, 1
  %93 = load %class.KV.8*, %class.KV.8** %18, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %0, i64 %92, %class.KV.8* %93)
  br label %94

; <label>:94:                                     ; preds = %83, %30
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj8EEC2ERKS1_(%class.KV.7*, %class.KV.7* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV.7*, align 8
  %4 = alloca %class.KV.7*, align 8
  store %class.KV.7* %0, %class.KV.7** %3, align 8
  store %class.KV.7* %1, %class.KV.7** %4, align 8
  %5 = load %class.KV.7*, %class.KV.7** %3, align 8
  %6 = getelementptr inbounds %class.KV.7, %class.KV.7* %5, i32 0, i32 0
  %7 = load %class.KV.7*, %class.KV.7** %4, align 8
  %8 = getelementptr inbounds %class.KV.7, %class.KV.7* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 8>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 8>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.7, %class.KV.7* %5, i32 0, i32 1
  %12 = load %class.KV.7*, %class.KV.7** %4, align 8
  %13 = getelementptr inbounds %class.KV.7, %class.KV.7* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 8>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 8>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6*, i64, %class.KV.7*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.6*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.7*, align 8
  store %class.KV.6* %0, %class.KV.6** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.7* %2, %class.KV.7** %6, align 8
  %7 = load %class.KV.6*, %class.KV.6** %4, align 8
  %8 = getelementptr inbounds %class.KV.6, %class.KV.6* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj7EE3KeyC2Em(%"union.KV<key, key, 7>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.6, %class.KV.6* %7, i32 0, i32 1
  %11 = load %class.KV.7*, %class.KV.7** %6, align 8
  call void @_ZN2KVI3keyS0_Lj7EE3ValC2EPKS_IS0_S0_Lj8EE(%"union.KV<key, key, 7>::Val"* %10, %class.KV.7* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj8EEC2EPKS0_S3_(%class.KV.7*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.7*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV.7* %0, %class.KV.7** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV.7*, %class.KV.7** %4, align 8
  %8 = getelementptr inbounds %class.KV.7, %class.KV.7* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj8EE3KeyC2EPKS0_(%"union.KV<key, key, 8>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.7, %class.KV.7* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj8EE3ValC2EPKS0_(%"union.KV<key, key, 8>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj9EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.8* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.9, align 8
  %17 = alloca %class.KV.9*, align 8
  %18 = alloca %class.KV.9*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %19 = load i64, i64* %8, align 8
  %20 = and i64 %19, 63
  %21 = urem i64 %20, 63
  %22 = trunc i64 %21 to i32
  store i32 %22, i32* %14, align 4
  %23 = load i64, i64* %11, align 8
  %24 = and i64 %23, 63
  %25 = urem i64 %24, 63
  %26 = trunc i64 %25 to i32
  store i32 %26, i32* %15, align 4
  %27 = load i32, i32* %14, align 4
  %28 = load i32, i32* %15, align 4
  %29 = icmp eq i32 %27, %28
  br i1 %29, label %30, label %51

; <label>:30:                                     ; preds = %7
  %31 = load i64, i64* %8, align 8
  %32 = lshr i64 %31, 6
  %33 = load %class.key*, %class.key** %9, align 8
  %34 = load %class.key*, %class.key** %10, align 8
  %35 = load i64, i64* %11, align 8
  %36 = lshr i64 %35, 6
  %37 = load %class.key*, %class.key** %12, align 8
  %38 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj10EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.9* sret %16, i64 %32, %class.key* %33, %class.key* %34, i64 %36, %class.key* %37, %class.key* %38)
  %39 = call noalias i8* @malloc(i64 16) #8
  %40 = bitcast i8* %39 to %class.KV.9*
  store %class.KV.9* %40, %class.KV.9** %17, align 8
  %41 = load %class.KV.9*, %class.KV.9** %17, align 8
  %42 = getelementptr inbounds %class.KV.9, %class.KV.9* %41, i64 0
  %43 = bitcast %class.KV.9* %42 to i8*
  %44 = bitcast i8* %43 to %class.KV.9*
  call void @_ZN2KVI3keyS0_Lj10EEC2ERKS1_(%class.KV.9* %44, %class.KV.9* dereferenceable(16) %16)
  %45 = load i32, i32* %14, align 4
  %46 = zext i32 %45 to i64
  %47 = shl i64 1, %46
  %48 = shl i64 %47, 1
  %49 = or i64 %48, 1
  %50 = load %class.KV.9*, %class.KV.9** %17, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %0, i64 %49, %class.KV.9* %50)
  br label %94

; <label>:51:                                     ; preds = %7
  %52 = call noalias i8* @malloc(i64 32) #8
  %53 = bitcast i8* %52 to %class.KV.9*
  store %class.KV.9* %53, %class.KV.9** %18, align 8
  %54 = load i32, i32* %15, align 4
  %55 = load i32, i32* %14, align 4
  %56 = icmp ult i32 %54, %55
  br i1 %56, label %57, label %70

; <label>:57:                                     ; preds = %51
  %58 = load %class.KV.9*, %class.KV.9** %18, align 8
  %59 = getelementptr inbounds %class.KV.9, %class.KV.9* %58, i64 0
  %60 = bitcast %class.KV.9* %59 to i8*
  %61 = bitcast i8* %60 to %class.KV.9*
  %62 = load %class.key*, %class.key** %12, align 8
  %63 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %61, %class.key* %62, %class.key* %63)
  %64 = load %class.KV.9*, %class.KV.9** %18, align 8
  %65 = getelementptr inbounds %class.KV.9, %class.KV.9* %64, i64 1
  %66 = bitcast %class.KV.9* %65 to i8*
  %67 = bitcast i8* %66 to %class.KV.9*
  %68 = load %class.key*, %class.key** %9, align 8
  %69 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %67, %class.key* %68, %class.key* %69)
  br label %83

; <label>:70:                                     ; preds = %51
  %71 = load %class.KV.9*, %class.KV.9** %18, align 8
  %72 = getelementptr inbounds %class.KV.9, %class.KV.9* %71, i64 0
  %73 = bitcast %class.KV.9* %72 to i8*
  %74 = bitcast i8* %73 to %class.KV.9*
  %75 = load %class.key*, %class.key** %9, align 8
  %76 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %74, %class.key* %75, %class.key* %76)
  %77 = load %class.KV.9*, %class.KV.9** %18, align 8
  %78 = getelementptr inbounds %class.KV.9, %class.KV.9* %77, i64 1
  %79 = bitcast %class.KV.9* %78 to i8*
  %80 = bitcast i8* %79 to %class.KV.9*
  %81 = load %class.key*, %class.key** %12, align 8
  %82 = load %class.key*, %class.key** %13, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %80, %class.key* %81, %class.key* %82)
  br label %83

; <label>:83:                                     ; preds = %70, %57
  %84 = load i32, i32* %14, align 4
  %85 = zext i32 %84 to i64
  %86 = shl i64 1, %85
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = shl i64 1, %88
  %90 = or i64 %86, %89
  %91 = shl i64 %90, 1
  %92 = or i64 %91, 1
  %93 = load %class.KV.9*, %class.KV.9** %18, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %0, i64 %92, %class.KV.9* %93)
  br label %94

; <label>:94:                                     ; preds = %83, %30
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj9EEC2ERKS1_(%class.KV.8*, %class.KV.8* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV.8*, align 8
  %4 = alloca %class.KV.8*, align 8
  store %class.KV.8* %0, %class.KV.8** %3, align 8
  store %class.KV.8* %1, %class.KV.8** %4, align 8
  %5 = load %class.KV.8*, %class.KV.8** %3, align 8
  %6 = getelementptr inbounds %class.KV.8, %class.KV.8* %5, i32 0, i32 0
  %7 = load %class.KV.8*, %class.KV.8** %4, align 8
  %8 = getelementptr inbounds %class.KV.8, %class.KV.8* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 9>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 9>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.8, %class.KV.8* %5, i32 0, i32 1
  %12 = load %class.KV.8*, %class.KV.8** %4, align 8
  %13 = getelementptr inbounds %class.KV.8, %class.KV.8* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 9>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 9>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7*, i64, %class.KV.8*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.7*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.8*, align 8
  store %class.KV.7* %0, %class.KV.7** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.8* %2, %class.KV.8** %6, align 8
  %7 = load %class.KV.7*, %class.KV.7** %4, align 8
  %8 = getelementptr inbounds %class.KV.7, %class.KV.7* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj8EE3KeyC2Em(%"union.KV<key, key, 8>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.7, %class.KV.7* %7, i32 0, i32 1
  %11 = load %class.KV.8*, %class.KV.8** %6, align 8
  call void @_ZN2KVI3keyS0_Lj8EE3ValC2EPKS_IS0_S0_Lj9EE(%"union.KV<key, key, 8>::Val"* %10, %class.KV.8* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj9EEC2EPKS0_S3_(%class.KV.8*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.8*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV.8* %0, %class.KV.8** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV.8*, %class.KV.8** %4, align 8
  %8 = getelementptr inbounds %class.KV.8, %class.KV.8* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj9EE3KeyC2EPKS0_(%"union.KV<key, key, 9>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.8, %class.KV.8* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj9EE3ValC2EPKS0_(%"union.KV<key, key, 9>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj10EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.9* noalias sret, i64, %class.key*, %class.key*, i64, %class.key*, %class.key*) #2 comdat align 2 {
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64, align 8
  %12 = alloca %class.key*, align 8
  %13 = alloca %class.key*, align 8
  %14 = alloca %class.LL*, align 8
  %15 = alloca %class.LL*, align 8
  store i64 %1, i64* %8, align 8
  store %class.key* %2, %class.key** %9, align 8
  store %class.key* %3, %class.key** %10, align 8
  store i64 %4, i64* %11, align 8
  store %class.key* %5, %class.key** %12, align 8
  store %class.key* %6, %class.key** %13, align 8
  %16 = call noalias i8* @malloc(i64 24) #8
  %17 = bitcast i8* %16 to %class.LL*
  %18 = bitcast %class.LL* %17 to i8*
  %19 = bitcast i8* %18 to %class.LL*
  %20 = load %class.key*, %class.key** %9, align 8
  %21 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL* %19, %class.key* %20, %class.key* %21, %class.LL* null)
  store %class.LL* %19, %class.LL** %14, align 8
  %22 = call noalias i8* @malloc(i64 24) #8
  %23 = bitcast i8* %22 to %class.LL*
  %24 = bitcast %class.LL* %23 to i8*
  %25 = bitcast i8* %24 to %class.LL*
  %26 = load %class.key*, %class.key** %12, align 8
  %27 = load %class.key*, %class.key** %13, align 8
  %28 = load %class.LL*, %class.LL** %14, align 8
  call void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL* %25, %class.key* %26, %class.key* %27, %class.LL* %28)
  store %class.LL* %25, %class.LL** %15, align 8
  %29 = load %class.LL*, %class.LL** %15, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EmPK2LLIS0_S0_E(%class.KV.9* %0, i64 1, %class.LL* %29)
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj10EEC2ERKS1_(%class.KV.9*, %class.KV.9* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV.9*, align 8
  %4 = alloca %class.KV.9*, align 8
  store %class.KV.9* %0, %class.KV.9** %3, align 8
  store %class.KV.9* %1, %class.KV.9** %4, align 8
  %5 = load %class.KV.9*, %class.KV.9** %3, align 8
  %6 = getelementptr inbounds %class.KV.9, %class.KV.9* %5, i32 0, i32 0
  %7 = load %class.KV.9*, %class.KV.9** %4, align 8
  %8 = getelementptr inbounds %class.KV.9, %class.KV.9* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 10>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 10>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV.9, %class.KV.9* %5, i32 0, i32 1
  %12 = load %class.KV.9*, %class.KV.9** %4, align 8
  %13 = getelementptr inbounds %class.KV.9, %class.KV.9* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 10>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 10>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8*, i64, %class.KV.9*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.8*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.KV.9*, align 8
  store %class.KV.8* %0, %class.KV.8** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.KV.9* %2, %class.KV.9** %6, align 8
  %7 = load %class.KV.8*, %class.KV.8** %4, align 8
  %8 = getelementptr inbounds %class.KV.8, %class.KV.8* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj9EE3KeyC2Em(%"union.KV<key, key, 9>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.8, %class.KV.8* %7, i32 0, i32 1
  %11 = load %class.KV.9*, %class.KV.9** %6, align 8
  call void @_ZN2KVI3keyS0_Lj9EE3ValC2EPKS_IS0_S0_Lj10EE(%"union.KV<key, key, 9>::Val"* %10, %class.KV.9* %11)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj10EEC2EPKS0_S3_(%class.KV.9*, %class.key*, %class.key*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.9*, align 8
  %5 = alloca %class.key*, align 8
  %6 = alloca %class.key*, align 8
  store %class.KV.9* %0, %class.KV.9** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  store %class.key* %2, %class.key** %6, align 8
  %7 = load %class.KV.9*, %class.KV.9** %4, align 8
  %8 = getelementptr inbounds %class.KV.9, %class.KV.9* %7, i32 0, i32 0
  %9 = load %class.key*, %class.key** %5, align 8
  call void @_ZN2KVI3keyS0_Lj10EE3KeyC2EPKS0_(%"union.KV<key, key, 10>::Key"* %8, %class.key* %9)
  %10 = getelementptr inbounds %class.KV.9, %class.KV.9* %7, i32 0, i32 1
  %11 = load %class.key*, %class.key** %6, align 8
  call void @_ZN2KVI3keyS0_Lj10EE3ValC2EPKS0_(%"union.KV<key, key, 10>::Val"* %10, %class.key* %11)
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL*, %class.key*, %class.key*, %class.LL*) unnamed_addr #0 comdat align 2 {
  %5 = alloca %class.LL*, align 8
  %6 = alloca %class.key*, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca %class.LL*, align 8
  store %class.LL* %0, %class.LL** %5, align 8
  store %class.key* %1, %class.key** %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  store %class.LL* %3, %class.LL** %8, align 8
  %9 = load %class.LL*, %class.LL** %5, align 8
  %10 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 0
  %11 = load %class.key*, %class.key** %6, align 8
  store %class.key* %11, %class.key** %10, align 8
  %12 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 1
  %13 = load %class.key*, %class.key** %7, align 8
  store %class.key* %13, %class.key** %12, align 8
  %14 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 2
  %15 = load %class.LL*, %class.LL** %8, align 8
  store %class.LL* %15, %class.LL** %14, align 8
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj10EEC2EmPK2LLIS0_S0_E(%class.KV.9*, i64, %class.LL*) unnamed_addr #2 comdat align 2 {
  %4 = alloca %class.KV.9*, align 8
  %5 = alloca i64, align 8
  %6 = alloca %class.LL*, align 8
  store %class.KV.9* %0, %class.KV.9** %4, align 8
  store i64 %1, i64* %5, align 8
  store %class.LL* %2, %class.LL** %6, align 8
  %7 = load %class.KV.9*, %class.KV.9** %4, align 8
  %8 = getelementptr inbounds %class.KV.9, %class.KV.9* %7, i32 0, i32 0
  %9 = load i64, i64* %5, align 8
  call void @_ZN2KVI3keyS0_Lj10EE3KeyC2Em(%"union.KV<key, key, 10>::Key"* %8, i64 %9)
  %10 = getelementptr inbounds %class.KV.9, %class.KV.9* %7, i32 0, i32 1
  %11 = load %class.LL*, %class.LL** %6, align 8
  call void @_ZN2KVI3keyS0_Lj10EE3ValC2EPK2LLIS0_S0_E(%"union.KV<key, key, 10>::Val"* %10, %class.LL* %11)
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj10EE3KeyC2Em(%"union.KV<key, key, 10>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 10>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 10>::Key"* %0, %"union.KV<key, key, 10>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 10>::Key"*, %"union.KV<key, key, 10>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 10>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj10EE3ValC2EPK2LLIS0_S0_E(%"union.KV<key, key, 10>::Val"*, %class.LL*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 10>::Val"*, align 8
  %4 = alloca %class.LL*, align 8
  store %"union.KV<key, key, 10>::Val"* %0, %"union.KV<key, key, 10>::Val"** %3, align 8
  store %class.LL* %1, %class.LL** %4, align 8
  %5 = load %"union.KV<key, key, 10>::Val"*, %"union.KV<key, key, 10>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 10>::Val"* %5 to %class.LL**
  %7 = load %class.LL*, %class.LL** %4, align 8
  store %class.LL* %7, %class.LL** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj9EE3KeyC2Em(%"union.KV<key, key, 9>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 9>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 9>::Key"* %0, %"union.KV<key, key, 9>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 9>::Key"*, %"union.KV<key, key, 9>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 9>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj9EE3ValC2EPKS_IS0_S0_Lj10EE(%"union.KV<key, key, 9>::Val"*, %class.KV.9*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 9>::Val"*, align 8
  %4 = alloca %class.KV.9*, align 8
  store %"union.KV<key, key, 9>::Val"* %0, %"union.KV<key, key, 9>::Val"** %3, align 8
  store %class.KV.9* %1, %class.KV.9** %4, align 8
  %5 = load %"union.KV<key, key, 9>::Val"*, %"union.KV<key, key, 9>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 9>::Val"* %5 to %class.KV.9**
  %7 = load %class.KV.9*, %class.KV.9** %4, align 8
  store %class.KV.9* %7, %class.KV.9** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj10EE3KeyC2EPKS0_(%"union.KV<key, key, 10>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 10>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 10>::Key"* %0, %"union.KV<key, key, 10>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 10>::Key"*, %"union.KV<key, key, 10>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 10>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj10EE3ValC2EPKS0_(%"union.KV<key, key, 10>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 10>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 10>::Val"* %0, %"union.KV<key, key, 10>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 10>::Val"*, %"union.KV<key, key, 10>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 10>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj8EE3KeyC2Em(%"union.KV<key, key, 8>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 8>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 8>::Key"* %0, %"union.KV<key, key, 8>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 8>::Key"*, %"union.KV<key, key, 8>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 8>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj8EE3ValC2EPKS_IS0_S0_Lj9EE(%"union.KV<key, key, 8>::Val"*, %class.KV.8*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 8>::Val"*, align 8
  %4 = alloca %class.KV.8*, align 8
  store %"union.KV<key, key, 8>::Val"* %0, %"union.KV<key, key, 8>::Val"** %3, align 8
  store %class.KV.8* %1, %class.KV.8** %4, align 8
  %5 = load %"union.KV<key, key, 8>::Val"*, %"union.KV<key, key, 8>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 8>::Val"* %5 to %class.KV.8**
  %7 = load %class.KV.8*, %class.KV.8** %4, align 8
  store %class.KV.8* %7, %class.KV.8** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj9EE3KeyC2EPKS0_(%"union.KV<key, key, 9>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 9>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 9>::Key"* %0, %"union.KV<key, key, 9>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 9>::Key"*, %"union.KV<key, key, 9>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 9>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj9EE3ValC2EPKS0_(%"union.KV<key, key, 9>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 9>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 9>::Val"* %0, %"union.KV<key, key, 9>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 9>::Val"*, %"union.KV<key, key, 9>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 9>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj7EE3KeyC2Em(%"union.KV<key, key, 7>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 7>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 7>::Key"* %0, %"union.KV<key, key, 7>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 7>::Key"*, %"union.KV<key, key, 7>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 7>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj7EE3ValC2EPKS_IS0_S0_Lj8EE(%"union.KV<key, key, 7>::Val"*, %class.KV.7*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 7>::Val"*, align 8
  %4 = alloca %class.KV.7*, align 8
  store %"union.KV<key, key, 7>::Val"* %0, %"union.KV<key, key, 7>::Val"** %3, align 8
  store %class.KV.7* %1, %class.KV.7** %4, align 8
  %5 = load %"union.KV<key, key, 7>::Val"*, %"union.KV<key, key, 7>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 7>::Val"* %5 to %class.KV.7**
  %7 = load %class.KV.7*, %class.KV.7** %4, align 8
  store %class.KV.7* %7, %class.KV.7** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj8EE3KeyC2EPKS0_(%"union.KV<key, key, 8>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 8>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 8>::Key"* %0, %"union.KV<key, key, 8>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 8>::Key"*, %"union.KV<key, key, 8>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 8>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj8EE3ValC2EPKS0_(%"union.KV<key, key, 8>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 8>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 8>::Val"* %0, %"union.KV<key, key, 8>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 8>::Val"*, %"union.KV<key, key, 8>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 8>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj6EE3KeyC2Em(%"union.KV<key, key, 6>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 6>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 6>::Key"* %0, %"union.KV<key, key, 6>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 6>::Key"*, %"union.KV<key, key, 6>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 6>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj6EE3ValC2EPKS_IS0_S0_Lj7EE(%"union.KV<key, key, 6>::Val"*, %class.KV.6*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 6>::Val"*, align 8
  %4 = alloca %class.KV.6*, align 8
  store %"union.KV<key, key, 6>::Val"* %0, %"union.KV<key, key, 6>::Val"** %3, align 8
  store %class.KV.6* %1, %class.KV.6** %4, align 8
  %5 = load %"union.KV<key, key, 6>::Val"*, %"union.KV<key, key, 6>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 6>::Val"* %5 to %class.KV.6**
  %7 = load %class.KV.6*, %class.KV.6** %4, align 8
  store %class.KV.6* %7, %class.KV.6** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj7EE3KeyC2EPKS0_(%"union.KV<key, key, 7>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 7>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 7>::Key"* %0, %"union.KV<key, key, 7>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 7>::Key"*, %"union.KV<key, key, 7>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 7>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj7EE3ValC2EPKS0_(%"union.KV<key, key, 7>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 7>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 7>::Val"* %0, %"union.KV<key, key, 7>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 7>::Val"*, %"union.KV<key, key, 7>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 7>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj5EE3KeyC2Em(%"union.KV<key, key, 5>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 5>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 5>::Key"* %0, %"union.KV<key, key, 5>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 5>::Key"*, %"union.KV<key, key, 5>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 5>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj5EE3ValC2EPKS_IS0_S0_Lj6EE(%"union.KV<key, key, 5>::Val"*, %class.KV.5*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 5>::Val"*, align 8
  %4 = alloca %class.KV.5*, align 8
  store %"union.KV<key, key, 5>::Val"* %0, %"union.KV<key, key, 5>::Val"** %3, align 8
  store %class.KV.5* %1, %class.KV.5** %4, align 8
  %5 = load %"union.KV<key, key, 5>::Val"*, %"union.KV<key, key, 5>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 5>::Val"* %5 to %class.KV.5**
  %7 = load %class.KV.5*, %class.KV.5** %4, align 8
  store %class.KV.5* %7, %class.KV.5** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj6EE3KeyC2EPKS0_(%"union.KV<key, key, 6>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 6>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 6>::Key"* %0, %"union.KV<key, key, 6>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 6>::Key"*, %"union.KV<key, key, 6>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 6>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj6EE3ValC2EPKS0_(%"union.KV<key, key, 6>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 6>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 6>::Val"* %0, %"union.KV<key, key, 6>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 6>::Val"*, %"union.KV<key, key, 6>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 6>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj4EE3KeyC2Em(%"union.KV<key, key, 4>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 4>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 4>::Key"* %0, %"union.KV<key, key, 4>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 4>::Key"*, %"union.KV<key, key, 4>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 4>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj4EE3ValC2EPKS_IS0_S0_Lj5EE(%"union.KV<key, key, 4>::Val"*, %class.KV.4*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 4>::Val"*, align 8
  %4 = alloca %class.KV.4*, align 8
  store %"union.KV<key, key, 4>::Val"* %0, %"union.KV<key, key, 4>::Val"** %3, align 8
  store %class.KV.4* %1, %class.KV.4** %4, align 8
  %5 = load %"union.KV<key, key, 4>::Val"*, %"union.KV<key, key, 4>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 4>::Val"* %5 to %class.KV.4**
  %7 = load %class.KV.4*, %class.KV.4** %4, align 8
  store %class.KV.4* %7, %class.KV.4** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj5EE3KeyC2EPKS0_(%"union.KV<key, key, 5>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 5>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 5>::Key"* %0, %"union.KV<key, key, 5>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 5>::Key"*, %"union.KV<key, key, 5>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 5>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj5EE3ValC2EPKS0_(%"union.KV<key, key, 5>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 5>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 5>::Val"* %0, %"union.KV<key, key, 5>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 5>::Val"*, %"union.KV<key, key, 5>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 5>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj3EE3KeyC2Em(%"union.KV<key, key, 3>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 3>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 3>::Key"* %0, %"union.KV<key, key, 3>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 3>::Key"*, %"union.KV<key, key, 3>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 3>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj3EE3ValC2EPKS_IS0_S0_Lj4EE(%"union.KV<key, key, 3>::Val"*, %class.KV.3*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 3>::Val"*, align 8
  %4 = alloca %class.KV.3*, align 8
  store %"union.KV<key, key, 3>::Val"* %0, %"union.KV<key, key, 3>::Val"** %3, align 8
  store %class.KV.3* %1, %class.KV.3** %4, align 8
  %5 = load %"union.KV<key, key, 3>::Val"*, %"union.KV<key, key, 3>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 3>::Val"* %5 to %class.KV.3**
  %7 = load %class.KV.3*, %class.KV.3** %4, align 8
  store %class.KV.3* %7, %class.KV.3** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj4EE3KeyC2EPKS0_(%"union.KV<key, key, 4>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 4>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 4>::Key"* %0, %"union.KV<key, key, 4>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 4>::Key"*, %"union.KV<key, key, 4>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 4>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj4EE3ValC2EPKS0_(%"union.KV<key, key, 4>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 4>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 4>::Val"* %0, %"union.KV<key, key, 4>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 4>::Val"*, %"union.KV<key, key, 4>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 4>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj2EE3KeyC2Em(%"union.KV<key, key, 2>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 2>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 2>::Key"* %0, %"union.KV<key, key, 2>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 2>::Key"*, %"union.KV<key, key, 2>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 2>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj2EE3ValC2EPKS_IS0_S0_Lj3EE(%"union.KV<key, key, 2>::Val"*, %class.KV.2*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 2>::Val"*, align 8
  %4 = alloca %class.KV.2*, align 8
  store %"union.KV<key, key, 2>::Val"* %0, %"union.KV<key, key, 2>::Val"** %3, align 8
  store %class.KV.2* %1, %class.KV.2** %4, align 8
  %5 = load %"union.KV<key, key, 2>::Val"*, %"union.KV<key, key, 2>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 2>::Val"* %5 to %class.KV.2**
  %7 = load %class.KV.2*, %class.KV.2** %4, align 8
  store %class.KV.2* %7, %class.KV.2** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj3EE3KeyC2EPKS0_(%"union.KV<key, key, 3>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 3>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 3>::Key"* %0, %"union.KV<key, key, 3>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 3>::Key"*, %"union.KV<key, key, 3>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 3>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj3EE3ValC2EPKS0_(%"union.KV<key, key, 3>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 3>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 3>::Val"* %0, %"union.KV<key, key, 3>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 3>::Val"*, %"union.KV<key, key, 3>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 3>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj1EE3KeyC2Em(%"union.KV<key, key, 1>::Key"*, i64) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 1>::Key"*, align 8
  %4 = alloca i64, align 8
  store %"union.KV<key, key, 1>::Key"* %0, %"union.KV<key, key, 1>::Key"** %3, align 8
  store i64 %1, i64* %4, align 8
  %5 = load %"union.KV<key, key, 1>::Key"*, %"union.KV<key, key, 1>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 1>::Key"* %5 to i64*
  %7 = load i64, i64* %4, align 8
  store i64 %7, i64* %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj1EE3ValC2EPKS_IS0_S0_Lj2EE(%"union.KV<key, key, 1>::Val"*, %class.KV.1*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 1>::Val"*, align 8
  %4 = alloca %class.KV.1*, align 8
  store %"union.KV<key, key, 1>::Val"* %0, %"union.KV<key, key, 1>::Val"** %3, align 8
  store %class.KV.1* %1, %class.KV.1** %4, align 8
  %5 = load %"union.KV<key, key, 1>::Val"*, %"union.KV<key, key, 1>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 1>::Val"* %5 to %class.KV.1**
  %7 = load %class.KV.1*, %class.KV.1** %4, align 8
  store %class.KV.1* %7, %class.KV.1** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj2EE3KeyC2EPKS0_(%"union.KV<key, key, 2>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 2>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 2>::Key"* %0, %"union.KV<key, key, 2>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 2>::Key"*, %"union.KV<key, key, 2>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 2>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj2EE3ValC2EPKS0_(%"union.KV<key, key, 2>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 2>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 2>::Val"* %0, %"union.KV<key, key, 2>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 2>::Val"*, %"union.KV<key, key, 2>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 2>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EE3ValC2EPKS_IS0_S0_Lj1EE(%"union.KV<key, key, 0>::Val"*, %class.KV.0*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 0>::Val"*, align 8
  %4 = alloca %class.KV.0*, align 8
  store %"union.KV<key, key, 0>::Val"* %0, %"union.KV<key, key, 0>::Val"** %3, align 8
  store %class.KV.0* %1, %class.KV.0** %4, align 8
  %5 = load %"union.KV<key, key, 0>::Val"*, %"union.KV<key, key, 0>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 0>::Val"* %5 to %class.KV.0**
  %7 = load %class.KV.0*, %class.KV.0** %4, align 8
  store %class.KV.0* %7, %class.KV.0** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj1EE3KeyC2EPKS0_(%"union.KV<key, key, 1>::Key"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 1>::Key"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 1>::Key"* %0, %"union.KV<key, key, 1>::Key"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 1>::Key"*, %"union.KV<key, key, 1>::Key"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 1>::Key"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj1EE3ValC2EPKS0_(%"union.KV<key, key, 1>::Val"*, %class.key*) unnamed_addr #0 comdat align 2 {
  %3 = alloca %"union.KV<key, key, 1>::Val"*, align 8
  %4 = alloca %class.key*, align 8
  store %"union.KV<key, key, 1>::Val"* %0, %"union.KV<key, key, 1>::Val"** %3, align 8
  store %class.key* %1, %class.key** %4, align 8
  %5 = load %"union.KV<key, key, 1>::Val"*, %"union.KV<key, key, 1>::Val"** %3, align 8
  %6 = bitcast %"union.KV<key, key, 1>::Val"* %5 to %class.key**
  %7 = load %class.key*, %class.key** %4, align 8
  store %class.key* %7, %class.key** %6, align 8
  ret void
}

; Function Attrs: nounwind readnone speculatable
declare i64 @llvm.ctpop.i64(i64) #7

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.KV.0* @_ZN2KVI3keyS0_Lj1EE11update_nodeEPKS1_jjRS2_(%class.KV.0*, i32, i32, %class.KV.0* dereferenceable(16)) #2 comdat align 2 {
  %5 = alloca %class.KV.0*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %class.KV.0*, align 8
  %9 = alloca %class.KV.0*, align 8
  store %class.KV.0* %0, %class.KV.0** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  store %class.KV.0* %3, %class.KV.0** %8, align 8
  %10 = load i32, i32* %6, align 4
  %11 = zext i32 %10 to i64
  %12 = mul i64 %11, 16
  %13 = call noalias i8* @malloc(i64 %12) #8
  %14 = bitcast i8* %13 to %class.KV.0*
  store %class.KV.0* %14, %class.KV.0** %9, align 8
  %15 = load %class.KV.0*, %class.KV.0** %9, align 8
  %16 = bitcast %class.KV.0* %15 to i8*
  %17 = load %class.KV.0*, %class.KV.0** %5, align 8
  %18 = bitcast %class.KV.0* %17 to i8*
  %19 = load i32, i32* %6, align 4
  %20 = zext i32 %19 to i64
  %21 = mul i64 %20, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %16, i8* %18, i64 %21, i32 8, i1 false)
  %22 = load %class.KV.0*, %class.KV.0** %9, align 8
  %23 = load i32, i32* %7, align 4
  %24 = zext i32 %23 to i64
  %25 = getelementptr inbounds %class.KV.0, %class.KV.0* %22, i64 %24
  %26 = bitcast %class.KV.0* %25 to i8*
  %27 = bitcast i8* %26 to %class.KV.0*
  %28 = load %class.KV.0*, %class.KV.0** %8, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2ERKS1_(%class.KV.0* %27, %class.KV.0* dereferenceable(16) %28)
  %29 = load %class.KV.0*, %class.KV.0** %9, align 8
  ret %class.KV.0* %29
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj1EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.0* noalias sret, %class.KV.0* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV.0*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.1*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.1*, align 8
  %19 = alloca %class.KV.1, align 8
  %20 = alloca %class.KV.1, align 8
  %21 = alloca %class.KV.1*, align 8
  %22 = alloca %class.KV.1, align 8
  %23 = alloca %class.KV.1*, align 8
  %24 = alloca %class.KV.1*, align 8
  store %class.KV.0* %1, %class.KV.0** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.0*, %class.KV.0** %7, align 8
  %26 = getelementptr inbounds %class.KV.0, %class.KV.0* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 1>::Val"* %26 to %class.KV.1**
  %28 = load %class.KV.1*, %class.KV.1** %27, align 8
  store %class.KV.1* %28, %class.KV.1** %12, align 8
  %29 = load %class.KV.0*, %class.KV.0** %7, align 8
  %30 = getelementptr inbounds %class.KV.0, %class.KV.0* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 1>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %14, align 4
  %38 = load i64, i64* %13, align 8
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %15, align 4
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i32, i32* %14, align 4
  %44 = sub i32 63, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %42, %45
  %47 = call i64 @llvm.ctpop.i64(i64 %46)
  %48 = trunc i64 %47 to i32
  store i32 %48, i32* %16, align 4
  %49 = load i64, i64* %13, align 8
  %50 = load i32, i32* %14, align 4
  %51 = zext i32 %50 to i64
  %52 = shl i64 1, %51
  %53 = and i64 %49, %52
  %54 = icmp ne i64 %53, 0
  %55 = zext i1 %54 to i8
  store i8 %55, i8* %17, align 1
  %56 = load i8, i8* %17, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %149

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.1*, %class.KV.1** %12, align 8
  %60 = load i32, i32* %16, align 4
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %class.KV.1, %class.KV.1* %59, i64 %61
  %63 = getelementptr inbounds %class.KV.1, %class.KV.1* %62, i32 0, i32 0
  %64 = bitcast %"union.KV<key, key, 2>::Key"* %63 to i64*
  %65 = load i64, i64* %64, align 8
  %66 = and i64 %65, 1
  %67 = icmp eq i64 %66, 0
  br i1 %67, label %68, label %130

; <label>:68:                                     ; preds = %58
  %69 = load %class.KV.1*, %class.KV.1** %12, align 8
  %70 = load i32, i32* %16, align 4
  %71 = zext i32 %70 to i64
  %72 = getelementptr inbounds %class.KV.1, %class.KV.1* %69, i64 %71
  %73 = getelementptr inbounds %class.KV.1, %class.KV.1* %72, i32 0, i32 0
  %74 = bitcast %"union.KV<key, key, 2>::Key"* %73 to %class.key**
  %75 = load %class.key*, %class.key** %74, align 8
  %76 = load %class.key*, %class.key** %9, align 8
  %77 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %75, %class.key* dereferenceable(8) %76)
  br i1 %77, label %78, label %90

; <label>:78:                                     ; preds = %68
  %79 = load %class.KV.1*, %class.KV.1** %12, align 8
  %80 = load i32, i32* %15, align 4
  %81 = load i32, i32* %16, align 4
  %82 = load %class.key*, %class.key** %9, align 8
  %83 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %19, %class.key* %82, %class.key* %83)
  %84 = call %class.KV.1* @_ZN2KVI3keyS0_Lj2EE11update_nodeEPKS1_jjRS2_(%class.KV.1* %79, i32 %80, i32 %81, %class.KV.1* dereferenceable(16) %19)
  store %class.KV.1* %84, %class.KV.1** %18, align 8
  %85 = load %class.KV.0*, %class.KV.0** %7, align 8
  %86 = getelementptr inbounds %class.KV.0, %class.KV.0* %85, i32 0, i32 0
  %87 = bitcast %"union.KV<key, key, 1>::Key"* %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = load %class.KV.1*, %class.KV.1** %18, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %0, i64 %88, %class.KV.1* %89)
  br label %198

; <label>:90:                                     ; preds = %68
  %91 = load i64*, i64** %11, align 8
  %92 = load i64, i64* %91, align 8
  %93 = add i64 %92, 1
  store i64 %93, i64* %91, align 8
  %94 = load %class.KV.1*, %class.KV.1** %12, align 8
  %95 = load i32, i32* %16, align 4
  %96 = zext i32 %95 to i64
  %97 = getelementptr inbounds %class.KV.1, %class.KV.1* %94, i64 %96
  %98 = getelementptr inbounds %class.KV.1, %class.KV.1* %97, i32 0, i32 0
  %99 = bitcast %"union.KV<key, key, 2>::Key"* %98 to %class.key**
  %100 = load %class.key*, %class.key** %99, align 8
  %101 = call i64 @_ZNK3key4hashEv(%class.key* %100)
  %102 = lshr i64 %101, 16
  %103 = load %class.KV.1*, %class.KV.1** %12, align 8
  %104 = load i32, i32* %16, align 4
  %105 = zext i32 %104 to i64
  %106 = getelementptr inbounds %class.KV.1, %class.KV.1* %103, i64 %105
  %107 = getelementptr inbounds %class.KV.1, %class.KV.1* %106, i32 0, i32 0
  %108 = bitcast %"union.KV<key, key, 2>::Key"* %107 to %class.key**
  %109 = load %class.key*, %class.key** %108, align 8
  %110 = load %class.KV.1*, %class.KV.1** %12, align 8
  %111 = load i32, i32* %16, align 4
  %112 = zext i32 %111 to i64
  %113 = getelementptr inbounds %class.KV.1, %class.KV.1* %110, i64 %112
  %114 = getelementptr inbounds %class.KV.1, %class.KV.1* %113, i32 0, i32 1
  %115 = bitcast %"union.KV<key, key, 2>::Val"* %114 to %class.key**
  %116 = load %class.key*, %class.key** %115, align 8
  %117 = load i64, i64* %8, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.key*, %class.key** %9, align 8
  %120 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj2EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.1* sret %20, i64 %102, %class.key* %109, %class.key* %116, i64 %118, %class.key* %119, %class.key* %120)
  %121 = load %class.KV.1*, %class.KV.1** %12, align 8
  %122 = load i32, i32* %15, align 4
  %123 = load i32, i32* %16, align 4
  %124 = call %class.KV.1* @_ZN2KVI3keyS0_Lj2EE11update_nodeEPKS1_jjRS2_(%class.KV.1* %121, i32 %122, i32 %123, %class.KV.1* dereferenceable(16) %20)
  store %class.KV.1* %124, %class.KV.1** %21, align 8
  %125 = load %class.KV.0*, %class.KV.0** %7, align 8
  %126 = getelementptr inbounds %class.KV.0, %class.KV.0* %125, i32 0, i32 0
  %127 = bitcast %"union.KV<key, key, 1>::Key"* %126 to i64*
  %128 = load i64, i64* %127, align 8
  %129 = load %class.KV.1*, %class.KV.1** %21, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %0, i64 %128, %class.KV.1* %129)
  br label %198

; <label>:130:                                    ; preds = %58
  %131 = load %class.KV.1*, %class.KV.1** %12, align 8
  %132 = load i32, i32* %16, align 4
  %133 = zext i32 %132 to i64
  %134 = getelementptr inbounds %class.KV.1, %class.KV.1* %131, i64 %133
  %135 = load i64, i64* %8, align 8
  %136 = lshr i64 %135, 6
  %137 = load %class.key*, %class.key** %9, align 8
  %138 = load %class.key*, %class.key** %10, align 8
  %139 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3keyS0_Lj2EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.1* sret %22, %class.KV.1* dereferenceable(16) %134, i64 %136, %class.key* %137, %class.key* %138, i64* %139)
  %140 = load %class.KV.1*, %class.KV.1** %12, align 8
  %141 = load i32, i32* %15, align 4
  %142 = load i32, i32* %16, align 4
  %143 = call %class.KV.1* @_ZN2KVI3keyS0_Lj2EE11update_nodeEPKS1_jjRS2_(%class.KV.1* %140, i32 %141, i32 %142, %class.KV.1* dereferenceable(16) %22)
  store %class.KV.1* %143, %class.KV.1** %23, align 8
  %144 = load %class.KV.0*, %class.KV.0** %7, align 8
  %145 = getelementptr inbounds %class.KV.0, %class.KV.0* %144, i32 0, i32 0
  %146 = bitcast %"union.KV<key, key, 1>::Key"* %145 to i64*
  %147 = load i64, i64* %146, align 8
  %148 = load %class.KV.1*, %class.KV.1** %23, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %0, i64 %147, %class.KV.1* %148)
  br label %198

; <label>:149:                                    ; preds = %6
  %150 = load i64*, i64** %11, align 8
  %151 = load i64, i64* %150, align 8
  %152 = add i64 %151, 1
  store i64 %152, i64* %150, align 8
  %153 = load i32, i32* %15, align 4
  %154 = add i32 %153, 1
  %155 = zext i32 %154 to i64
  %156 = mul i64 %155, 16
  %157 = call noalias i8* @malloc(i64 %156) #8
  %158 = bitcast i8* %157 to %class.KV.1*
  store %class.KV.1* %158, %class.KV.1** %24, align 8
  %159 = load %class.KV.1*, %class.KV.1** %24, align 8
  %160 = bitcast %class.KV.1* %159 to i8*
  %161 = load %class.KV.1*, %class.KV.1** %12, align 8
  %162 = bitcast %class.KV.1* %161 to i8*
  %163 = load i32, i32* %16, align 4
  %164 = zext i32 %163 to i64
  %165 = mul i64 %164, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %160, i8* %162, i64 %165, i32 8, i1 false)
  %166 = load %class.KV.1*, %class.KV.1** %24, align 8
  %167 = load i32, i32* %16, align 4
  %168 = add i32 %167, 1
  %169 = zext i32 %168 to i64
  %170 = getelementptr inbounds %class.KV.1, %class.KV.1* %166, i64 %169
  %171 = bitcast %class.KV.1* %170 to i8*
  %172 = load %class.KV.1*, %class.KV.1** %12, align 8
  %173 = load i32, i32* %16, align 4
  %174 = zext i32 %173 to i64
  %175 = getelementptr inbounds %class.KV.1, %class.KV.1* %172, i64 %174
  %176 = bitcast %class.KV.1* %175 to i8*
  %177 = load i32, i32* %15, align 4
  %178 = load i32, i32* %16, align 4
  %179 = sub i32 %177, %178
  %180 = zext i32 %179 to i64
  %181 = mul i64 %180, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %171, i8* %176, i64 %181, i32 8, i1 false)
  %182 = load %class.KV.1*, %class.KV.1** %24, align 8
  %183 = load i32, i32* %16, align 4
  %184 = zext i32 %183 to i64
  %185 = getelementptr inbounds %class.KV.1, %class.KV.1* %182, i64 %184
  %186 = bitcast %class.KV.1* %185 to i8*
  %187 = bitcast i8* %186 to %class.KV.1*
  %188 = load %class.key*, %class.key** %9, align 8
  %189 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EPKS0_S3_(%class.KV.1* %187, %class.key* %188, %class.key* %189)
  %190 = load i64, i64* %13, align 8
  %191 = load i32, i32* %14, align 4
  %192 = zext i32 %191 to i64
  %193 = shl i64 1, %192
  %194 = or i64 %190, %193
  %195 = shl i64 %194, 1
  %196 = or i64 %195, 1
  %197 = load %class.KV.1*, %class.KV.1** %24, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %0, i64 %196, %class.KV.1* %197)
  br label %198

; <label>:198:                                    ; preds = %149, %130, %90, %78
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.KV.1* @_ZN2KVI3keyS0_Lj2EE11update_nodeEPKS1_jjRS2_(%class.KV.1*, i32, i32, %class.KV.1* dereferenceable(16)) #2 comdat align 2 {
  %5 = alloca %class.KV.1*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %class.KV.1*, align 8
  %9 = alloca %class.KV.1*, align 8
  store %class.KV.1* %0, %class.KV.1** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  store %class.KV.1* %3, %class.KV.1** %8, align 8
  %10 = load i32, i32* %6, align 4
  %11 = zext i32 %10 to i64
  %12 = mul i64 %11, 16
  %13 = call noalias i8* @malloc(i64 %12) #8
  %14 = bitcast i8* %13 to %class.KV.1*
  store %class.KV.1* %14, %class.KV.1** %9, align 8
  %15 = load %class.KV.1*, %class.KV.1** %9, align 8
  %16 = bitcast %class.KV.1* %15 to i8*
  %17 = load %class.KV.1*, %class.KV.1** %5, align 8
  %18 = bitcast %class.KV.1* %17 to i8*
  %19 = load i32, i32* %6, align 4
  %20 = zext i32 %19 to i64
  %21 = mul i64 %20, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %16, i8* %18, i64 %21, i32 8, i1 false)
  %22 = load %class.KV.1*, %class.KV.1** %9, align 8
  %23 = load i32, i32* %7, align 4
  %24 = zext i32 %23 to i64
  %25 = getelementptr inbounds %class.KV.1, %class.KV.1* %22, i64 %24
  %26 = bitcast %class.KV.1* %25 to i8*
  %27 = bitcast i8* %26 to %class.KV.1*
  %28 = load %class.KV.1*, %class.KV.1** %8, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2ERKS1_(%class.KV.1* %27, %class.KV.1* dereferenceable(16) %28)
  %29 = load %class.KV.1*, %class.KV.1** %9, align 8
  ret %class.KV.1* %29
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj2EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.1* noalias sret, %class.KV.1* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV.1*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.2*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.2*, align 8
  %19 = alloca %class.KV.2, align 8
  %20 = alloca %class.KV.2, align 8
  %21 = alloca %class.KV.2*, align 8
  %22 = alloca %class.KV.2, align 8
  %23 = alloca %class.KV.2*, align 8
  %24 = alloca %class.KV.2*, align 8
  store %class.KV.1* %1, %class.KV.1** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.1*, %class.KV.1** %7, align 8
  %26 = getelementptr inbounds %class.KV.1, %class.KV.1* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 2>::Val"* %26 to %class.KV.2**
  %28 = load %class.KV.2*, %class.KV.2** %27, align 8
  store %class.KV.2* %28, %class.KV.2** %12, align 8
  %29 = load %class.KV.1*, %class.KV.1** %7, align 8
  %30 = getelementptr inbounds %class.KV.1, %class.KV.1* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 2>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %14, align 4
  %38 = load i64, i64* %13, align 8
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %15, align 4
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i32, i32* %14, align 4
  %44 = sub i32 63, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %42, %45
  %47 = call i64 @llvm.ctpop.i64(i64 %46)
  %48 = trunc i64 %47 to i32
  store i32 %48, i32* %16, align 4
  %49 = load i64, i64* %13, align 8
  %50 = load i32, i32* %14, align 4
  %51 = zext i32 %50 to i64
  %52 = shl i64 1, %51
  %53 = and i64 %49, %52
  %54 = icmp ne i64 %53, 0
  %55 = zext i1 %54 to i8
  store i8 %55, i8* %17, align 1
  %56 = load i8, i8* %17, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %149

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.2*, %class.KV.2** %12, align 8
  %60 = load i32, i32* %16, align 4
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %class.KV.2, %class.KV.2* %59, i64 %61
  %63 = getelementptr inbounds %class.KV.2, %class.KV.2* %62, i32 0, i32 0
  %64 = bitcast %"union.KV<key, key, 3>::Key"* %63 to i64*
  %65 = load i64, i64* %64, align 8
  %66 = and i64 %65, 1
  %67 = icmp eq i64 %66, 0
  br i1 %67, label %68, label %130

; <label>:68:                                     ; preds = %58
  %69 = load %class.KV.2*, %class.KV.2** %12, align 8
  %70 = load i32, i32* %16, align 4
  %71 = zext i32 %70 to i64
  %72 = getelementptr inbounds %class.KV.2, %class.KV.2* %69, i64 %71
  %73 = getelementptr inbounds %class.KV.2, %class.KV.2* %72, i32 0, i32 0
  %74 = bitcast %"union.KV<key, key, 3>::Key"* %73 to %class.key**
  %75 = load %class.key*, %class.key** %74, align 8
  %76 = load %class.key*, %class.key** %9, align 8
  %77 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %75, %class.key* dereferenceable(8) %76)
  br i1 %77, label %78, label %90

; <label>:78:                                     ; preds = %68
  %79 = load %class.KV.2*, %class.KV.2** %12, align 8
  %80 = load i32, i32* %15, align 4
  %81 = load i32, i32* %16, align 4
  %82 = load %class.key*, %class.key** %9, align 8
  %83 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %19, %class.key* %82, %class.key* %83)
  %84 = call %class.KV.2* @_ZN2KVI3keyS0_Lj3EE11update_nodeEPKS1_jjRS2_(%class.KV.2* %79, i32 %80, i32 %81, %class.KV.2* dereferenceable(16) %19)
  store %class.KV.2* %84, %class.KV.2** %18, align 8
  %85 = load %class.KV.1*, %class.KV.1** %7, align 8
  %86 = getelementptr inbounds %class.KV.1, %class.KV.1* %85, i32 0, i32 0
  %87 = bitcast %"union.KV<key, key, 2>::Key"* %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = load %class.KV.2*, %class.KV.2** %18, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %0, i64 %88, %class.KV.2* %89)
  br label %198

; <label>:90:                                     ; preds = %68
  %91 = load i64*, i64** %11, align 8
  %92 = load i64, i64* %91, align 8
  %93 = add i64 %92, 1
  store i64 %93, i64* %91, align 8
  %94 = load %class.KV.2*, %class.KV.2** %12, align 8
  %95 = load i32, i32* %16, align 4
  %96 = zext i32 %95 to i64
  %97 = getelementptr inbounds %class.KV.2, %class.KV.2* %94, i64 %96
  %98 = getelementptr inbounds %class.KV.2, %class.KV.2* %97, i32 0, i32 0
  %99 = bitcast %"union.KV<key, key, 3>::Key"* %98 to %class.key**
  %100 = load %class.key*, %class.key** %99, align 8
  %101 = call i64 @_ZNK3key4hashEv(%class.key* %100)
  %102 = lshr i64 %101, 22
  %103 = load %class.KV.2*, %class.KV.2** %12, align 8
  %104 = load i32, i32* %16, align 4
  %105 = zext i32 %104 to i64
  %106 = getelementptr inbounds %class.KV.2, %class.KV.2* %103, i64 %105
  %107 = getelementptr inbounds %class.KV.2, %class.KV.2* %106, i32 0, i32 0
  %108 = bitcast %"union.KV<key, key, 3>::Key"* %107 to %class.key**
  %109 = load %class.key*, %class.key** %108, align 8
  %110 = load %class.KV.2*, %class.KV.2** %12, align 8
  %111 = load i32, i32* %16, align 4
  %112 = zext i32 %111 to i64
  %113 = getelementptr inbounds %class.KV.2, %class.KV.2* %110, i64 %112
  %114 = getelementptr inbounds %class.KV.2, %class.KV.2* %113, i32 0, i32 1
  %115 = bitcast %"union.KV<key, key, 3>::Val"* %114 to %class.key**
  %116 = load %class.key*, %class.key** %115, align 8
  %117 = load i64, i64* %8, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.key*, %class.key** %9, align 8
  %120 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj3EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.2* sret %20, i64 %102, %class.key* %109, %class.key* %116, i64 %118, %class.key* %119, %class.key* %120)
  %121 = load %class.KV.2*, %class.KV.2** %12, align 8
  %122 = load i32, i32* %15, align 4
  %123 = load i32, i32* %16, align 4
  %124 = call %class.KV.2* @_ZN2KVI3keyS0_Lj3EE11update_nodeEPKS1_jjRS2_(%class.KV.2* %121, i32 %122, i32 %123, %class.KV.2* dereferenceable(16) %20)
  store %class.KV.2* %124, %class.KV.2** %21, align 8
  %125 = load %class.KV.1*, %class.KV.1** %7, align 8
  %126 = getelementptr inbounds %class.KV.1, %class.KV.1* %125, i32 0, i32 0
  %127 = bitcast %"union.KV<key, key, 2>::Key"* %126 to i64*
  %128 = load i64, i64* %127, align 8
  %129 = load %class.KV.2*, %class.KV.2** %21, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %0, i64 %128, %class.KV.2* %129)
  br label %198

; <label>:130:                                    ; preds = %58
  %131 = load %class.KV.2*, %class.KV.2** %12, align 8
  %132 = load i32, i32* %16, align 4
  %133 = zext i32 %132 to i64
  %134 = getelementptr inbounds %class.KV.2, %class.KV.2* %131, i64 %133
  %135 = load i64, i64* %8, align 8
  %136 = lshr i64 %135, 6
  %137 = load %class.key*, %class.key** %9, align 8
  %138 = load %class.key*, %class.key** %10, align 8
  %139 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3keyS0_Lj3EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.2* sret %22, %class.KV.2* dereferenceable(16) %134, i64 %136, %class.key* %137, %class.key* %138, i64* %139)
  %140 = load %class.KV.2*, %class.KV.2** %12, align 8
  %141 = load i32, i32* %15, align 4
  %142 = load i32, i32* %16, align 4
  %143 = call %class.KV.2* @_ZN2KVI3keyS0_Lj3EE11update_nodeEPKS1_jjRS2_(%class.KV.2* %140, i32 %141, i32 %142, %class.KV.2* dereferenceable(16) %22)
  store %class.KV.2* %143, %class.KV.2** %23, align 8
  %144 = load %class.KV.1*, %class.KV.1** %7, align 8
  %145 = getelementptr inbounds %class.KV.1, %class.KV.1* %144, i32 0, i32 0
  %146 = bitcast %"union.KV<key, key, 2>::Key"* %145 to i64*
  %147 = load i64, i64* %146, align 8
  %148 = load %class.KV.2*, %class.KV.2** %23, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %0, i64 %147, %class.KV.2* %148)
  br label %198

; <label>:149:                                    ; preds = %6
  %150 = load i64*, i64** %11, align 8
  %151 = load i64, i64* %150, align 8
  %152 = add i64 %151, 1
  store i64 %152, i64* %150, align 8
  %153 = load i32, i32* %15, align 4
  %154 = add i32 %153, 1
  %155 = zext i32 %154 to i64
  %156 = mul i64 %155, 16
  %157 = call noalias i8* @malloc(i64 %156) #8
  %158 = bitcast i8* %157 to %class.KV.2*
  store %class.KV.2* %158, %class.KV.2** %24, align 8
  %159 = load %class.KV.2*, %class.KV.2** %24, align 8
  %160 = bitcast %class.KV.2* %159 to i8*
  %161 = load %class.KV.2*, %class.KV.2** %12, align 8
  %162 = bitcast %class.KV.2* %161 to i8*
  %163 = load i32, i32* %16, align 4
  %164 = zext i32 %163 to i64
  %165 = mul i64 %164, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %160, i8* %162, i64 %165, i32 8, i1 false)
  %166 = load %class.KV.2*, %class.KV.2** %24, align 8
  %167 = load i32, i32* %16, align 4
  %168 = add i32 %167, 1
  %169 = zext i32 %168 to i64
  %170 = getelementptr inbounds %class.KV.2, %class.KV.2* %166, i64 %169
  %171 = bitcast %class.KV.2* %170 to i8*
  %172 = load %class.KV.2*, %class.KV.2** %12, align 8
  %173 = load i32, i32* %16, align 4
  %174 = zext i32 %173 to i64
  %175 = getelementptr inbounds %class.KV.2, %class.KV.2* %172, i64 %174
  %176 = bitcast %class.KV.2* %175 to i8*
  %177 = load i32, i32* %15, align 4
  %178 = load i32, i32* %16, align 4
  %179 = sub i32 %177, %178
  %180 = zext i32 %179 to i64
  %181 = mul i64 %180, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %171, i8* %176, i64 %181, i32 8, i1 false)
  %182 = load %class.KV.2*, %class.KV.2** %24, align 8
  %183 = load i32, i32* %16, align 4
  %184 = zext i32 %183 to i64
  %185 = getelementptr inbounds %class.KV.2, %class.KV.2* %182, i64 %184
  %186 = bitcast %class.KV.2* %185 to i8*
  %187 = bitcast i8* %186 to %class.KV.2*
  %188 = load %class.key*, %class.key** %9, align 8
  %189 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EPKS0_S3_(%class.KV.2* %187, %class.key* %188, %class.key* %189)
  %190 = load i64, i64* %13, align 8
  %191 = load i32, i32* %14, align 4
  %192 = zext i32 %191 to i64
  %193 = shl i64 1, %192
  %194 = or i64 %190, %193
  %195 = shl i64 %194, 1
  %196 = or i64 %195, 1
  %197 = load %class.KV.2*, %class.KV.2** %24, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %0, i64 %196, %class.KV.2* %197)
  br label %198

; <label>:198:                                    ; preds = %149, %130, %90, %78
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.KV.2* @_ZN2KVI3keyS0_Lj3EE11update_nodeEPKS1_jjRS2_(%class.KV.2*, i32, i32, %class.KV.2* dereferenceable(16)) #2 comdat align 2 {
  %5 = alloca %class.KV.2*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %class.KV.2*, align 8
  %9 = alloca %class.KV.2*, align 8
  store %class.KV.2* %0, %class.KV.2** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  store %class.KV.2* %3, %class.KV.2** %8, align 8
  %10 = load i32, i32* %6, align 4
  %11 = zext i32 %10 to i64
  %12 = mul i64 %11, 16
  %13 = call noalias i8* @malloc(i64 %12) #8
  %14 = bitcast i8* %13 to %class.KV.2*
  store %class.KV.2* %14, %class.KV.2** %9, align 8
  %15 = load %class.KV.2*, %class.KV.2** %9, align 8
  %16 = bitcast %class.KV.2* %15 to i8*
  %17 = load %class.KV.2*, %class.KV.2** %5, align 8
  %18 = bitcast %class.KV.2* %17 to i8*
  %19 = load i32, i32* %6, align 4
  %20 = zext i32 %19 to i64
  %21 = mul i64 %20, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %16, i8* %18, i64 %21, i32 8, i1 false)
  %22 = load %class.KV.2*, %class.KV.2** %9, align 8
  %23 = load i32, i32* %7, align 4
  %24 = zext i32 %23 to i64
  %25 = getelementptr inbounds %class.KV.2, %class.KV.2* %22, i64 %24
  %26 = bitcast %class.KV.2* %25 to i8*
  %27 = bitcast i8* %26 to %class.KV.2*
  %28 = load %class.KV.2*, %class.KV.2** %8, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2ERKS1_(%class.KV.2* %27, %class.KV.2* dereferenceable(16) %28)
  %29 = load %class.KV.2*, %class.KV.2** %9, align 8
  ret %class.KV.2* %29
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj3EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.2* noalias sret, %class.KV.2* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV.2*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.3*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.3*, align 8
  %19 = alloca %class.KV.3, align 8
  %20 = alloca %class.KV.3, align 8
  %21 = alloca %class.KV.3*, align 8
  %22 = alloca %class.KV.3, align 8
  %23 = alloca %class.KV.3*, align 8
  %24 = alloca %class.KV.3*, align 8
  store %class.KV.2* %1, %class.KV.2** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.2*, %class.KV.2** %7, align 8
  %26 = getelementptr inbounds %class.KV.2, %class.KV.2* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 3>::Val"* %26 to %class.KV.3**
  %28 = load %class.KV.3*, %class.KV.3** %27, align 8
  store %class.KV.3* %28, %class.KV.3** %12, align 8
  %29 = load %class.KV.2*, %class.KV.2** %7, align 8
  %30 = getelementptr inbounds %class.KV.2, %class.KV.2* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 3>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %14, align 4
  %38 = load i64, i64* %13, align 8
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %15, align 4
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i32, i32* %14, align 4
  %44 = sub i32 63, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %42, %45
  %47 = call i64 @llvm.ctpop.i64(i64 %46)
  %48 = trunc i64 %47 to i32
  store i32 %48, i32* %16, align 4
  %49 = load i64, i64* %13, align 8
  %50 = load i32, i32* %14, align 4
  %51 = zext i32 %50 to i64
  %52 = shl i64 1, %51
  %53 = and i64 %49, %52
  %54 = icmp ne i64 %53, 0
  %55 = zext i1 %54 to i8
  store i8 %55, i8* %17, align 1
  %56 = load i8, i8* %17, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %149

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.3*, %class.KV.3** %12, align 8
  %60 = load i32, i32* %16, align 4
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %class.KV.3, %class.KV.3* %59, i64 %61
  %63 = getelementptr inbounds %class.KV.3, %class.KV.3* %62, i32 0, i32 0
  %64 = bitcast %"union.KV<key, key, 4>::Key"* %63 to i64*
  %65 = load i64, i64* %64, align 8
  %66 = and i64 %65, 1
  %67 = icmp eq i64 %66, 0
  br i1 %67, label %68, label %130

; <label>:68:                                     ; preds = %58
  %69 = load %class.KV.3*, %class.KV.3** %12, align 8
  %70 = load i32, i32* %16, align 4
  %71 = zext i32 %70 to i64
  %72 = getelementptr inbounds %class.KV.3, %class.KV.3* %69, i64 %71
  %73 = getelementptr inbounds %class.KV.3, %class.KV.3* %72, i32 0, i32 0
  %74 = bitcast %"union.KV<key, key, 4>::Key"* %73 to %class.key**
  %75 = load %class.key*, %class.key** %74, align 8
  %76 = load %class.key*, %class.key** %9, align 8
  %77 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %75, %class.key* dereferenceable(8) %76)
  br i1 %77, label %78, label %90

; <label>:78:                                     ; preds = %68
  %79 = load %class.KV.3*, %class.KV.3** %12, align 8
  %80 = load i32, i32* %15, align 4
  %81 = load i32, i32* %16, align 4
  %82 = load %class.key*, %class.key** %9, align 8
  %83 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %19, %class.key* %82, %class.key* %83)
  %84 = call %class.KV.3* @_ZN2KVI3keyS0_Lj4EE11update_nodeEPKS1_jjRS2_(%class.KV.3* %79, i32 %80, i32 %81, %class.KV.3* dereferenceable(16) %19)
  store %class.KV.3* %84, %class.KV.3** %18, align 8
  %85 = load %class.KV.2*, %class.KV.2** %7, align 8
  %86 = getelementptr inbounds %class.KV.2, %class.KV.2* %85, i32 0, i32 0
  %87 = bitcast %"union.KV<key, key, 3>::Key"* %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = load %class.KV.3*, %class.KV.3** %18, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %0, i64 %88, %class.KV.3* %89)
  br label %198

; <label>:90:                                     ; preds = %68
  %91 = load i64*, i64** %11, align 8
  %92 = load i64, i64* %91, align 8
  %93 = add i64 %92, 1
  store i64 %93, i64* %91, align 8
  %94 = load %class.KV.3*, %class.KV.3** %12, align 8
  %95 = load i32, i32* %16, align 4
  %96 = zext i32 %95 to i64
  %97 = getelementptr inbounds %class.KV.3, %class.KV.3* %94, i64 %96
  %98 = getelementptr inbounds %class.KV.3, %class.KV.3* %97, i32 0, i32 0
  %99 = bitcast %"union.KV<key, key, 4>::Key"* %98 to %class.key**
  %100 = load %class.key*, %class.key** %99, align 8
  %101 = call i64 @_ZNK3key4hashEv(%class.key* %100)
  %102 = lshr i64 %101, 28
  %103 = load %class.KV.3*, %class.KV.3** %12, align 8
  %104 = load i32, i32* %16, align 4
  %105 = zext i32 %104 to i64
  %106 = getelementptr inbounds %class.KV.3, %class.KV.3* %103, i64 %105
  %107 = getelementptr inbounds %class.KV.3, %class.KV.3* %106, i32 0, i32 0
  %108 = bitcast %"union.KV<key, key, 4>::Key"* %107 to %class.key**
  %109 = load %class.key*, %class.key** %108, align 8
  %110 = load %class.KV.3*, %class.KV.3** %12, align 8
  %111 = load i32, i32* %16, align 4
  %112 = zext i32 %111 to i64
  %113 = getelementptr inbounds %class.KV.3, %class.KV.3* %110, i64 %112
  %114 = getelementptr inbounds %class.KV.3, %class.KV.3* %113, i32 0, i32 1
  %115 = bitcast %"union.KV<key, key, 4>::Val"* %114 to %class.key**
  %116 = load %class.key*, %class.key** %115, align 8
  %117 = load i64, i64* %8, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.key*, %class.key** %9, align 8
  %120 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj4EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.3* sret %20, i64 %102, %class.key* %109, %class.key* %116, i64 %118, %class.key* %119, %class.key* %120)
  %121 = load %class.KV.3*, %class.KV.3** %12, align 8
  %122 = load i32, i32* %15, align 4
  %123 = load i32, i32* %16, align 4
  %124 = call %class.KV.3* @_ZN2KVI3keyS0_Lj4EE11update_nodeEPKS1_jjRS2_(%class.KV.3* %121, i32 %122, i32 %123, %class.KV.3* dereferenceable(16) %20)
  store %class.KV.3* %124, %class.KV.3** %21, align 8
  %125 = load %class.KV.2*, %class.KV.2** %7, align 8
  %126 = getelementptr inbounds %class.KV.2, %class.KV.2* %125, i32 0, i32 0
  %127 = bitcast %"union.KV<key, key, 3>::Key"* %126 to i64*
  %128 = load i64, i64* %127, align 8
  %129 = load %class.KV.3*, %class.KV.3** %21, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %0, i64 %128, %class.KV.3* %129)
  br label %198

; <label>:130:                                    ; preds = %58
  %131 = load %class.KV.3*, %class.KV.3** %12, align 8
  %132 = load i32, i32* %16, align 4
  %133 = zext i32 %132 to i64
  %134 = getelementptr inbounds %class.KV.3, %class.KV.3* %131, i64 %133
  %135 = load i64, i64* %8, align 8
  %136 = lshr i64 %135, 6
  %137 = load %class.key*, %class.key** %9, align 8
  %138 = load %class.key*, %class.key** %10, align 8
  %139 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3keyS0_Lj4EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.3* sret %22, %class.KV.3* dereferenceable(16) %134, i64 %136, %class.key* %137, %class.key* %138, i64* %139)
  %140 = load %class.KV.3*, %class.KV.3** %12, align 8
  %141 = load i32, i32* %15, align 4
  %142 = load i32, i32* %16, align 4
  %143 = call %class.KV.3* @_ZN2KVI3keyS0_Lj4EE11update_nodeEPKS1_jjRS2_(%class.KV.3* %140, i32 %141, i32 %142, %class.KV.3* dereferenceable(16) %22)
  store %class.KV.3* %143, %class.KV.3** %23, align 8
  %144 = load %class.KV.2*, %class.KV.2** %7, align 8
  %145 = getelementptr inbounds %class.KV.2, %class.KV.2* %144, i32 0, i32 0
  %146 = bitcast %"union.KV<key, key, 3>::Key"* %145 to i64*
  %147 = load i64, i64* %146, align 8
  %148 = load %class.KV.3*, %class.KV.3** %23, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %0, i64 %147, %class.KV.3* %148)
  br label %198

; <label>:149:                                    ; preds = %6
  %150 = load i64*, i64** %11, align 8
  %151 = load i64, i64* %150, align 8
  %152 = add i64 %151, 1
  store i64 %152, i64* %150, align 8
  %153 = load i32, i32* %15, align 4
  %154 = add i32 %153, 1
  %155 = zext i32 %154 to i64
  %156 = mul i64 %155, 16
  %157 = call noalias i8* @malloc(i64 %156) #8
  %158 = bitcast i8* %157 to %class.KV.3*
  store %class.KV.3* %158, %class.KV.3** %24, align 8
  %159 = load %class.KV.3*, %class.KV.3** %24, align 8
  %160 = bitcast %class.KV.3* %159 to i8*
  %161 = load %class.KV.3*, %class.KV.3** %12, align 8
  %162 = bitcast %class.KV.3* %161 to i8*
  %163 = load i32, i32* %16, align 4
  %164 = zext i32 %163 to i64
  %165 = mul i64 %164, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %160, i8* %162, i64 %165, i32 8, i1 false)
  %166 = load %class.KV.3*, %class.KV.3** %24, align 8
  %167 = load i32, i32* %16, align 4
  %168 = add i32 %167, 1
  %169 = zext i32 %168 to i64
  %170 = getelementptr inbounds %class.KV.3, %class.KV.3* %166, i64 %169
  %171 = bitcast %class.KV.3* %170 to i8*
  %172 = load %class.KV.3*, %class.KV.3** %12, align 8
  %173 = load i32, i32* %16, align 4
  %174 = zext i32 %173 to i64
  %175 = getelementptr inbounds %class.KV.3, %class.KV.3* %172, i64 %174
  %176 = bitcast %class.KV.3* %175 to i8*
  %177 = load i32, i32* %15, align 4
  %178 = load i32, i32* %16, align 4
  %179 = sub i32 %177, %178
  %180 = zext i32 %179 to i64
  %181 = mul i64 %180, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %171, i8* %176, i64 %181, i32 8, i1 false)
  %182 = load %class.KV.3*, %class.KV.3** %24, align 8
  %183 = load i32, i32* %16, align 4
  %184 = zext i32 %183 to i64
  %185 = getelementptr inbounds %class.KV.3, %class.KV.3* %182, i64 %184
  %186 = bitcast %class.KV.3* %185 to i8*
  %187 = bitcast i8* %186 to %class.KV.3*
  %188 = load %class.key*, %class.key** %9, align 8
  %189 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EPKS0_S3_(%class.KV.3* %187, %class.key* %188, %class.key* %189)
  %190 = load i64, i64* %13, align 8
  %191 = load i32, i32* %14, align 4
  %192 = zext i32 %191 to i64
  %193 = shl i64 1, %192
  %194 = or i64 %190, %193
  %195 = shl i64 %194, 1
  %196 = or i64 %195, 1
  %197 = load %class.KV.3*, %class.KV.3** %24, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %0, i64 %196, %class.KV.3* %197)
  br label %198

; <label>:198:                                    ; preds = %149, %130, %90, %78
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.KV.3* @_ZN2KVI3keyS0_Lj4EE11update_nodeEPKS1_jjRS2_(%class.KV.3*, i32, i32, %class.KV.3* dereferenceable(16)) #2 comdat align 2 {
  %5 = alloca %class.KV.3*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %class.KV.3*, align 8
  %9 = alloca %class.KV.3*, align 8
  store %class.KV.3* %0, %class.KV.3** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  store %class.KV.3* %3, %class.KV.3** %8, align 8
  %10 = load i32, i32* %6, align 4
  %11 = zext i32 %10 to i64
  %12 = mul i64 %11, 16
  %13 = call noalias i8* @malloc(i64 %12) #8
  %14 = bitcast i8* %13 to %class.KV.3*
  store %class.KV.3* %14, %class.KV.3** %9, align 8
  %15 = load %class.KV.3*, %class.KV.3** %9, align 8
  %16 = bitcast %class.KV.3* %15 to i8*
  %17 = load %class.KV.3*, %class.KV.3** %5, align 8
  %18 = bitcast %class.KV.3* %17 to i8*
  %19 = load i32, i32* %6, align 4
  %20 = zext i32 %19 to i64
  %21 = mul i64 %20, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %16, i8* %18, i64 %21, i32 8, i1 false)
  %22 = load %class.KV.3*, %class.KV.3** %9, align 8
  %23 = load i32, i32* %7, align 4
  %24 = zext i32 %23 to i64
  %25 = getelementptr inbounds %class.KV.3, %class.KV.3* %22, i64 %24
  %26 = bitcast %class.KV.3* %25 to i8*
  %27 = bitcast i8* %26 to %class.KV.3*
  %28 = load %class.KV.3*, %class.KV.3** %8, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2ERKS1_(%class.KV.3* %27, %class.KV.3* dereferenceable(16) %28)
  %29 = load %class.KV.3*, %class.KV.3** %9, align 8
  ret %class.KV.3* %29
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj4EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.3* noalias sret, %class.KV.3* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV.3*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.4*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.4*, align 8
  %19 = alloca %class.KV.4, align 8
  %20 = alloca %class.KV.4, align 8
  %21 = alloca %class.KV.4*, align 8
  %22 = alloca %class.KV.4, align 8
  %23 = alloca %class.KV.4*, align 8
  %24 = alloca %class.KV.4*, align 8
  store %class.KV.3* %1, %class.KV.3** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.3*, %class.KV.3** %7, align 8
  %26 = getelementptr inbounds %class.KV.3, %class.KV.3* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 4>::Val"* %26 to %class.KV.4**
  %28 = load %class.KV.4*, %class.KV.4** %27, align 8
  store %class.KV.4* %28, %class.KV.4** %12, align 8
  %29 = load %class.KV.3*, %class.KV.3** %7, align 8
  %30 = getelementptr inbounds %class.KV.3, %class.KV.3* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 4>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %14, align 4
  %38 = load i64, i64* %13, align 8
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %15, align 4
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i32, i32* %14, align 4
  %44 = sub i32 63, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %42, %45
  %47 = call i64 @llvm.ctpop.i64(i64 %46)
  %48 = trunc i64 %47 to i32
  store i32 %48, i32* %16, align 4
  %49 = load i64, i64* %13, align 8
  %50 = load i32, i32* %14, align 4
  %51 = zext i32 %50 to i64
  %52 = shl i64 1, %51
  %53 = and i64 %49, %52
  %54 = icmp ne i64 %53, 0
  %55 = zext i1 %54 to i8
  store i8 %55, i8* %17, align 1
  %56 = load i8, i8* %17, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %149

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.4*, %class.KV.4** %12, align 8
  %60 = load i32, i32* %16, align 4
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %class.KV.4, %class.KV.4* %59, i64 %61
  %63 = getelementptr inbounds %class.KV.4, %class.KV.4* %62, i32 0, i32 0
  %64 = bitcast %"union.KV<key, key, 5>::Key"* %63 to i64*
  %65 = load i64, i64* %64, align 8
  %66 = and i64 %65, 1
  %67 = icmp eq i64 %66, 0
  br i1 %67, label %68, label %130

; <label>:68:                                     ; preds = %58
  %69 = load %class.KV.4*, %class.KV.4** %12, align 8
  %70 = load i32, i32* %16, align 4
  %71 = zext i32 %70 to i64
  %72 = getelementptr inbounds %class.KV.4, %class.KV.4* %69, i64 %71
  %73 = getelementptr inbounds %class.KV.4, %class.KV.4* %72, i32 0, i32 0
  %74 = bitcast %"union.KV<key, key, 5>::Key"* %73 to %class.key**
  %75 = load %class.key*, %class.key** %74, align 8
  %76 = load %class.key*, %class.key** %9, align 8
  %77 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %75, %class.key* dereferenceable(8) %76)
  br i1 %77, label %78, label %90

; <label>:78:                                     ; preds = %68
  %79 = load %class.KV.4*, %class.KV.4** %12, align 8
  %80 = load i32, i32* %15, align 4
  %81 = load i32, i32* %16, align 4
  %82 = load %class.key*, %class.key** %9, align 8
  %83 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %19, %class.key* %82, %class.key* %83)
  %84 = call %class.KV.4* @_ZN2KVI3keyS0_Lj5EE11update_nodeEPKS1_jjRS2_(%class.KV.4* %79, i32 %80, i32 %81, %class.KV.4* dereferenceable(16) %19)
  store %class.KV.4* %84, %class.KV.4** %18, align 8
  %85 = load %class.KV.3*, %class.KV.3** %7, align 8
  %86 = getelementptr inbounds %class.KV.3, %class.KV.3* %85, i32 0, i32 0
  %87 = bitcast %"union.KV<key, key, 4>::Key"* %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = load %class.KV.4*, %class.KV.4** %18, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %0, i64 %88, %class.KV.4* %89)
  br label %198

; <label>:90:                                     ; preds = %68
  %91 = load i64*, i64** %11, align 8
  %92 = load i64, i64* %91, align 8
  %93 = add i64 %92, 1
  store i64 %93, i64* %91, align 8
  %94 = load %class.KV.4*, %class.KV.4** %12, align 8
  %95 = load i32, i32* %16, align 4
  %96 = zext i32 %95 to i64
  %97 = getelementptr inbounds %class.KV.4, %class.KV.4* %94, i64 %96
  %98 = getelementptr inbounds %class.KV.4, %class.KV.4* %97, i32 0, i32 0
  %99 = bitcast %"union.KV<key, key, 5>::Key"* %98 to %class.key**
  %100 = load %class.key*, %class.key** %99, align 8
  %101 = call i64 @_ZNK3key4hashEv(%class.key* %100)
  %102 = lshr i64 %101, 34
  %103 = load %class.KV.4*, %class.KV.4** %12, align 8
  %104 = load i32, i32* %16, align 4
  %105 = zext i32 %104 to i64
  %106 = getelementptr inbounds %class.KV.4, %class.KV.4* %103, i64 %105
  %107 = getelementptr inbounds %class.KV.4, %class.KV.4* %106, i32 0, i32 0
  %108 = bitcast %"union.KV<key, key, 5>::Key"* %107 to %class.key**
  %109 = load %class.key*, %class.key** %108, align 8
  %110 = load %class.KV.4*, %class.KV.4** %12, align 8
  %111 = load i32, i32* %16, align 4
  %112 = zext i32 %111 to i64
  %113 = getelementptr inbounds %class.KV.4, %class.KV.4* %110, i64 %112
  %114 = getelementptr inbounds %class.KV.4, %class.KV.4* %113, i32 0, i32 1
  %115 = bitcast %"union.KV<key, key, 5>::Val"* %114 to %class.key**
  %116 = load %class.key*, %class.key** %115, align 8
  %117 = load i64, i64* %8, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.key*, %class.key** %9, align 8
  %120 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj5EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.4* sret %20, i64 %102, %class.key* %109, %class.key* %116, i64 %118, %class.key* %119, %class.key* %120)
  %121 = load %class.KV.4*, %class.KV.4** %12, align 8
  %122 = load i32, i32* %15, align 4
  %123 = load i32, i32* %16, align 4
  %124 = call %class.KV.4* @_ZN2KVI3keyS0_Lj5EE11update_nodeEPKS1_jjRS2_(%class.KV.4* %121, i32 %122, i32 %123, %class.KV.4* dereferenceable(16) %20)
  store %class.KV.4* %124, %class.KV.4** %21, align 8
  %125 = load %class.KV.3*, %class.KV.3** %7, align 8
  %126 = getelementptr inbounds %class.KV.3, %class.KV.3* %125, i32 0, i32 0
  %127 = bitcast %"union.KV<key, key, 4>::Key"* %126 to i64*
  %128 = load i64, i64* %127, align 8
  %129 = load %class.KV.4*, %class.KV.4** %21, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %0, i64 %128, %class.KV.4* %129)
  br label %198

; <label>:130:                                    ; preds = %58
  %131 = load %class.KV.4*, %class.KV.4** %12, align 8
  %132 = load i32, i32* %16, align 4
  %133 = zext i32 %132 to i64
  %134 = getelementptr inbounds %class.KV.4, %class.KV.4* %131, i64 %133
  %135 = load i64, i64* %8, align 8
  %136 = lshr i64 %135, 6
  %137 = load %class.key*, %class.key** %9, align 8
  %138 = load %class.key*, %class.key** %10, align 8
  %139 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3keyS0_Lj5EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.4* sret %22, %class.KV.4* dereferenceable(16) %134, i64 %136, %class.key* %137, %class.key* %138, i64* %139)
  %140 = load %class.KV.4*, %class.KV.4** %12, align 8
  %141 = load i32, i32* %15, align 4
  %142 = load i32, i32* %16, align 4
  %143 = call %class.KV.4* @_ZN2KVI3keyS0_Lj5EE11update_nodeEPKS1_jjRS2_(%class.KV.4* %140, i32 %141, i32 %142, %class.KV.4* dereferenceable(16) %22)
  store %class.KV.4* %143, %class.KV.4** %23, align 8
  %144 = load %class.KV.3*, %class.KV.3** %7, align 8
  %145 = getelementptr inbounds %class.KV.3, %class.KV.3* %144, i32 0, i32 0
  %146 = bitcast %"union.KV<key, key, 4>::Key"* %145 to i64*
  %147 = load i64, i64* %146, align 8
  %148 = load %class.KV.4*, %class.KV.4** %23, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %0, i64 %147, %class.KV.4* %148)
  br label %198

; <label>:149:                                    ; preds = %6
  %150 = load i64*, i64** %11, align 8
  %151 = load i64, i64* %150, align 8
  %152 = add i64 %151, 1
  store i64 %152, i64* %150, align 8
  %153 = load i32, i32* %15, align 4
  %154 = add i32 %153, 1
  %155 = zext i32 %154 to i64
  %156 = mul i64 %155, 16
  %157 = call noalias i8* @malloc(i64 %156) #8
  %158 = bitcast i8* %157 to %class.KV.4*
  store %class.KV.4* %158, %class.KV.4** %24, align 8
  %159 = load %class.KV.4*, %class.KV.4** %24, align 8
  %160 = bitcast %class.KV.4* %159 to i8*
  %161 = load %class.KV.4*, %class.KV.4** %12, align 8
  %162 = bitcast %class.KV.4* %161 to i8*
  %163 = load i32, i32* %16, align 4
  %164 = zext i32 %163 to i64
  %165 = mul i64 %164, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %160, i8* %162, i64 %165, i32 8, i1 false)
  %166 = load %class.KV.4*, %class.KV.4** %24, align 8
  %167 = load i32, i32* %16, align 4
  %168 = add i32 %167, 1
  %169 = zext i32 %168 to i64
  %170 = getelementptr inbounds %class.KV.4, %class.KV.4* %166, i64 %169
  %171 = bitcast %class.KV.4* %170 to i8*
  %172 = load %class.KV.4*, %class.KV.4** %12, align 8
  %173 = load i32, i32* %16, align 4
  %174 = zext i32 %173 to i64
  %175 = getelementptr inbounds %class.KV.4, %class.KV.4* %172, i64 %174
  %176 = bitcast %class.KV.4* %175 to i8*
  %177 = load i32, i32* %15, align 4
  %178 = load i32, i32* %16, align 4
  %179 = sub i32 %177, %178
  %180 = zext i32 %179 to i64
  %181 = mul i64 %180, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %171, i8* %176, i64 %181, i32 8, i1 false)
  %182 = load %class.KV.4*, %class.KV.4** %24, align 8
  %183 = load i32, i32* %16, align 4
  %184 = zext i32 %183 to i64
  %185 = getelementptr inbounds %class.KV.4, %class.KV.4* %182, i64 %184
  %186 = bitcast %class.KV.4* %185 to i8*
  %187 = bitcast i8* %186 to %class.KV.4*
  %188 = load %class.key*, %class.key** %9, align 8
  %189 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EPKS0_S3_(%class.KV.4* %187, %class.key* %188, %class.key* %189)
  %190 = load i64, i64* %13, align 8
  %191 = load i32, i32* %14, align 4
  %192 = zext i32 %191 to i64
  %193 = shl i64 1, %192
  %194 = or i64 %190, %193
  %195 = shl i64 %194, 1
  %196 = or i64 %195, 1
  %197 = load %class.KV.4*, %class.KV.4** %24, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %0, i64 %196, %class.KV.4* %197)
  br label %198

; <label>:198:                                    ; preds = %149, %130, %90, %78
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.KV.4* @_ZN2KVI3keyS0_Lj5EE11update_nodeEPKS1_jjRS2_(%class.KV.4*, i32, i32, %class.KV.4* dereferenceable(16)) #2 comdat align 2 {
  %5 = alloca %class.KV.4*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %class.KV.4*, align 8
  %9 = alloca %class.KV.4*, align 8
  store %class.KV.4* %0, %class.KV.4** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  store %class.KV.4* %3, %class.KV.4** %8, align 8
  %10 = load i32, i32* %6, align 4
  %11 = zext i32 %10 to i64
  %12 = mul i64 %11, 16
  %13 = call noalias i8* @malloc(i64 %12) #8
  %14 = bitcast i8* %13 to %class.KV.4*
  store %class.KV.4* %14, %class.KV.4** %9, align 8
  %15 = load %class.KV.4*, %class.KV.4** %9, align 8
  %16 = bitcast %class.KV.4* %15 to i8*
  %17 = load %class.KV.4*, %class.KV.4** %5, align 8
  %18 = bitcast %class.KV.4* %17 to i8*
  %19 = load i32, i32* %6, align 4
  %20 = zext i32 %19 to i64
  %21 = mul i64 %20, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %16, i8* %18, i64 %21, i32 8, i1 false)
  %22 = load %class.KV.4*, %class.KV.4** %9, align 8
  %23 = load i32, i32* %7, align 4
  %24 = zext i32 %23 to i64
  %25 = getelementptr inbounds %class.KV.4, %class.KV.4* %22, i64 %24
  %26 = bitcast %class.KV.4* %25 to i8*
  %27 = bitcast i8* %26 to %class.KV.4*
  %28 = load %class.KV.4*, %class.KV.4** %8, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2ERKS1_(%class.KV.4* %27, %class.KV.4* dereferenceable(16) %28)
  %29 = load %class.KV.4*, %class.KV.4** %9, align 8
  ret %class.KV.4* %29
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj5EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.4* noalias sret, %class.KV.4* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV.4*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.5*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.5*, align 8
  %19 = alloca %class.KV.5, align 8
  %20 = alloca %class.KV.5, align 8
  %21 = alloca %class.KV.5*, align 8
  %22 = alloca %class.KV.5, align 8
  %23 = alloca %class.KV.5*, align 8
  %24 = alloca %class.KV.5*, align 8
  store %class.KV.4* %1, %class.KV.4** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.4*, %class.KV.4** %7, align 8
  %26 = getelementptr inbounds %class.KV.4, %class.KV.4* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 5>::Val"* %26 to %class.KV.5**
  %28 = load %class.KV.5*, %class.KV.5** %27, align 8
  store %class.KV.5* %28, %class.KV.5** %12, align 8
  %29 = load %class.KV.4*, %class.KV.4** %7, align 8
  %30 = getelementptr inbounds %class.KV.4, %class.KV.4* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 5>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %14, align 4
  %38 = load i64, i64* %13, align 8
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %15, align 4
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i32, i32* %14, align 4
  %44 = sub i32 63, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %42, %45
  %47 = call i64 @llvm.ctpop.i64(i64 %46)
  %48 = trunc i64 %47 to i32
  store i32 %48, i32* %16, align 4
  %49 = load i64, i64* %13, align 8
  %50 = load i32, i32* %14, align 4
  %51 = zext i32 %50 to i64
  %52 = shl i64 1, %51
  %53 = and i64 %49, %52
  %54 = icmp ne i64 %53, 0
  %55 = zext i1 %54 to i8
  store i8 %55, i8* %17, align 1
  %56 = load i8, i8* %17, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %149

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.5*, %class.KV.5** %12, align 8
  %60 = load i32, i32* %16, align 4
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %class.KV.5, %class.KV.5* %59, i64 %61
  %63 = getelementptr inbounds %class.KV.5, %class.KV.5* %62, i32 0, i32 0
  %64 = bitcast %"union.KV<key, key, 6>::Key"* %63 to i64*
  %65 = load i64, i64* %64, align 8
  %66 = and i64 %65, 1
  %67 = icmp eq i64 %66, 0
  br i1 %67, label %68, label %130

; <label>:68:                                     ; preds = %58
  %69 = load %class.KV.5*, %class.KV.5** %12, align 8
  %70 = load i32, i32* %16, align 4
  %71 = zext i32 %70 to i64
  %72 = getelementptr inbounds %class.KV.5, %class.KV.5* %69, i64 %71
  %73 = getelementptr inbounds %class.KV.5, %class.KV.5* %72, i32 0, i32 0
  %74 = bitcast %"union.KV<key, key, 6>::Key"* %73 to %class.key**
  %75 = load %class.key*, %class.key** %74, align 8
  %76 = load %class.key*, %class.key** %9, align 8
  %77 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %75, %class.key* dereferenceable(8) %76)
  br i1 %77, label %78, label %90

; <label>:78:                                     ; preds = %68
  %79 = load %class.KV.5*, %class.KV.5** %12, align 8
  %80 = load i32, i32* %15, align 4
  %81 = load i32, i32* %16, align 4
  %82 = load %class.key*, %class.key** %9, align 8
  %83 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %19, %class.key* %82, %class.key* %83)
  %84 = call %class.KV.5* @_ZN2KVI3keyS0_Lj6EE11update_nodeEPKS1_jjRS2_(%class.KV.5* %79, i32 %80, i32 %81, %class.KV.5* dereferenceable(16) %19)
  store %class.KV.5* %84, %class.KV.5** %18, align 8
  %85 = load %class.KV.4*, %class.KV.4** %7, align 8
  %86 = getelementptr inbounds %class.KV.4, %class.KV.4* %85, i32 0, i32 0
  %87 = bitcast %"union.KV<key, key, 5>::Key"* %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = load %class.KV.5*, %class.KV.5** %18, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %0, i64 %88, %class.KV.5* %89)
  br label %198

; <label>:90:                                     ; preds = %68
  %91 = load i64*, i64** %11, align 8
  %92 = load i64, i64* %91, align 8
  %93 = add i64 %92, 1
  store i64 %93, i64* %91, align 8
  %94 = load %class.KV.5*, %class.KV.5** %12, align 8
  %95 = load i32, i32* %16, align 4
  %96 = zext i32 %95 to i64
  %97 = getelementptr inbounds %class.KV.5, %class.KV.5* %94, i64 %96
  %98 = getelementptr inbounds %class.KV.5, %class.KV.5* %97, i32 0, i32 0
  %99 = bitcast %"union.KV<key, key, 6>::Key"* %98 to %class.key**
  %100 = load %class.key*, %class.key** %99, align 8
  %101 = call i64 @_ZNK3key4hashEv(%class.key* %100)
  %102 = lshr i64 %101, 40
  %103 = load %class.KV.5*, %class.KV.5** %12, align 8
  %104 = load i32, i32* %16, align 4
  %105 = zext i32 %104 to i64
  %106 = getelementptr inbounds %class.KV.5, %class.KV.5* %103, i64 %105
  %107 = getelementptr inbounds %class.KV.5, %class.KV.5* %106, i32 0, i32 0
  %108 = bitcast %"union.KV<key, key, 6>::Key"* %107 to %class.key**
  %109 = load %class.key*, %class.key** %108, align 8
  %110 = load %class.KV.5*, %class.KV.5** %12, align 8
  %111 = load i32, i32* %16, align 4
  %112 = zext i32 %111 to i64
  %113 = getelementptr inbounds %class.KV.5, %class.KV.5* %110, i64 %112
  %114 = getelementptr inbounds %class.KV.5, %class.KV.5* %113, i32 0, i32 1
  %115 = bitcast %"union.KV<key, key, 6>::Val"* %114 to %class.key**
  %116 = load %class.key*, %class.key** %115, align 8
  %117 = load i64, i64* %8, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.key*, %class.key** %9, align 8
  %120 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj6EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.5* sret %20, i64 %102, %class.key* %109, %class.key* %116, i64 %118, %class.key* %119, %class.key* %120)
  %121 = load %class.KV.5*, %class.KV.5** %12, align 8
  %122 = load i32, i32* %15, align 4
  %123 = load i32, i32* %16, align 4
  %124 = call %class.KV.5* @_ZN2KVI3keyS0_Lj6EE11update_nodeEPKS1_jjRS2_(%class.KV.5* %121, i32 %122, i32 %123, %class.KV.5* dereferenceable(16) %20)
  store %class.KV.5* %124, %class.KV.5** %21, align 8
  %125 = load %class.KV.4*, %class.KV.4** %7, align 8
  %126 = getelementptr inbounds %class.KV.4, %class.KV.4* %125, i32 0, i32 0
  %127 = bitcast %"union.KV<key, key, 5>::Key"* %126 to i64*
  %128 = load i64, i64* %127, align 8
  %129 = load %class.KV.5*, %class.KV.5** %21, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %0, i64 %128, %class.KV.5* %129)
  br label %198

; <label>:130:                                    ; preds = %58
  %131 = load %class.KV.5*, %class.KV.5** %12, align 8
  %132 = load i32, i32* %16, align 4
  %133 = zext i32 %132 to i64
  %134 = getelementptr inbounds %class.KV.5, %class.KV.5* %131, i64 %133
  %135 = load i64, i64* %8, align 8
  %136 = lshr i64 %135, 6
  %137 = load %class.key*, %class.key** %9, align 8
  %138 = load %class.key*, %class.key** %10, align 8
  %139 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3keyS0_Lj6EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.5* sret %22, %class.KV.5* dereferenceable(16) %134, i64 %136, %class.key* %137, %class.key* %138, i64* %139)
  %140 = load %class.KV.5*, %class.KV.5** %12, align 8
  %141 = load i32, i32* %15, align 4
  %142 = load i32, i32* %16, align 4
  %143 = call %class.KV.5* @_ZN2KVI3keyS0_Lj6EE11update_nodeEPKS1_jjRS2_(%class.KV.5* %140, i32 %141, i32 %142, %class.KV.5* dereferenceable(16) %22)
  store %class.KV.5* %143, %class.KV.5** %23, align 8
  %144 = load %class.KV.4*, %class.KV.4** %7, align 8
  %145 = getelementptr inbounds %class.KV.4, %class.KV.4* %144, i32 0, i32 0
  %146 = bitcast %"union.KV<key, key, 5>::Key"* %145 to i64*
  %147 = load i64, i64* %146, align 8
  %148 = load %class.KV.5*, %class.KV.5** %23, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %0, i64 %147, %class.KV.5* %148)
  br label %198

; <label>:149:                                    ; preds = %6
  %150 = load i64*, i64** %11, align 8
  %151 = load i64, i64* %150, align 8
  %152 = add i64 %151, 1
  store i64 %152, i64* %150, align 8
  %153 = load i32, i32* %15, align 4
  %154 = add i32 %153, 1
  %155 = zext i32 %154 to i64
  %156 = mul i64 %155, 16
  %157 = call noalias i8* @malloc(i64 %156) #8
  %158 = bitcast i8* %157 to %class.KV.5*
  store %class.KV.5* %158, %class.KV.5** %24, align 8
  %159 = load %class.KV.5*, %class.KV.5** %24, align 8
  %160 = bitcast %class.KV.5* %159 to i8*
  %161 = load %class.KV.5*, %class.KV.5** %12, align 8
  %162 = bitcast %class.KV.5* %161 to i8*
  %163 = load i32, i32* %16, align 4
  %164 = zext i32 %163 to i64
  %165 = mul i64 %164, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %160, i8* %162, i64 %165, i32 8, i1 false)
  %166 = load %class.KV.5*, %class.KV.5** %24, align 8
  %167 = load i32, i32* %16, align 4
  %168 = add i32 %167, 1
  %169 = zext i32 %168 to i64
  %170 = getelementptr inbounds %class.KV.5, %class.KV.5* %166, i64 %169
  %171 = bitcast %class.KV.5* %170 to i8*
  %172 = load %class.KV.5*, %class.KV.5** %12, align 8
  %173 = load i32, i32* %16, align 4
  %174 = zext i32 %173 to i64
  %175 = getelementptr inbounds %class.KV.5, %class.KV.5* %172, i64 %174
  %176 = bitcast %class.KV.5* %175 to i8*
  %177 = load i32, i32* %15, align 4
  %178 = load i32, i32* %16, align 4
  %179 = sub i32 %177, %178
  %180 = zext i32 %179 to i64
  %181 = mul i64 %180, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %171, i8* %176, i64 %181, i32 8, i1 false)
  %182 = load %class.KV.5*, %class.KV.5** %24, align 8
  %183 = load i32, i32* %16, align 4
  %184 = zext i32 %183 to i64
  %185 = getelementptr inbounds %class.KV.5, %class.KV.5* %182, i64 %184
  %186 = bitcast %class.KV.5* %185 to i8*
  %187 = bitcast i8* %186 to %class.KV.5*
  %188 = load %class.key*, %class.key** %9, align 8
  %189 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EPKS0_S3_(%class.KV.5* %187, %class.key* %188, %class.key* %189)
  %190 = load i64, i64* %13, align 8
  %191 = load i32, i32* %14, align 4
  %192 = zext i32 %191 to i64
  %193 = shl i64 1, %192
  %194 = or i64 %190, %193
  %195 = shl i64 %194, 1
  %196 = or i64 %195, 1
  %197 = load %class.KV.5*, %class.KV.5** %24, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %0, i64 %196, %class.KV.5* %197)
  br label %198

; <label>:198:                                    ; preds = %149, %130, %90, %78
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.KV.5* @_ZN2KVI3keyS0_Lj6EE11update_nodeEPKS1_jjRS2_(%class.KV.5*, i32, i32, %class.KV.5* dereferenceable(16)) #2 comdat align 2 {
  %5 = alloca %class.KV.5*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %class.KV.5*, align 8
  %9 = alloca %class.KV.5*, align 8
  store %class.KV.5* %0, %class.KV.5** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  store %class.KV.5* %3, %class.KV.5** %8, align 8
  %10 = load i32, i32* %6, align 4
  %11 = zext i32 %10 to i64
  %12 = mul i64 %11, 16
  %13 = call noalias i8* @malloc(i64 %12) #8
  %14 = bitcast i8* %13 to %class.KV.5*
  store %class.KV.5* %14, %class.KV.5** %9, align 8
  %15 = load %class.KV.5*, %class.KV.5** %9, align 8
  %16 = bitcast %class.KV.5* %15 to i8*
  %17 = load %class.KV.5*, %class.KV.5** %5, align 8
  %18 = bitcast %class.KV.5* %17 to i8*
  %19 = load i32, i32* %6, align 4
  %20 = zext i32 %19 to i64
  %21 = mul i64 %20, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %16, i8* %18, i64 %21, i32 8, i1 false)
  %22 = load %class.KV.5*, %class.KV.5** %9, align 8
  %23 = load i32, i32* %7, align 4
  %24 = zext i32 %23 to i64
  %25 = getelementptr inbounds %class.KV.5, %class.KV.5* %22, i64 %24
  %26 = bitcast %class.KV.5* %25 to i8*
  %27 = bitcast i8* %26 to %class.KV.5*
  %28 = load %class.KV.5*, %class.KV.5** %8, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2ERKS1_(%class.KV.5* %27, %class.KV.5* dereferenceable(16) %28)
  %29 = load %class.KV.5*, %class.KV.5** %9, align 8
  ret %class.KV.5* %29
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj6EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.5* noalias sret, %class.KV.5* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV.5*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.6*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.6*, align 8
  %19 = alloca %class.KV.6, align 8
  %20 = alloca %class.KV.6, align 8
  %21 = alloca %class.KV.6*, align 8
  %22 = alloca %class.KV.6, align 8
  %23 = alloca %class.KV.6*, align 8
  %24 = alloca %class.KV.6*, align 8
  store %class.KV.5* %1, %class.KV.5** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.5*, %class.KV.5** %7, align 8
  %26 = getelementptr inbounds %class.KV.5, %class.KV.5* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 6>::Val"* %26 to %class.KV.6**
  %28 = load %class.KV.6*, %class.KV.6** %27, align 8
  store %class.KV.6* %28, %class.KV.6** %12, align 8
  %29 = load %class.KV.5*, %class.KV.5** %7, align 8
  %30 = getelementptr inbounds %class.KV.5, %class.KV.5* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 6>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %14, align 4
  %38 = load i64, i64* %13, align 8
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %15, align 4
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i32, i32* %14, align 4
  %44 = sub i32 63, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %42, %45
  %47 = call i64 @llvm.ctpop.i64(i64 %46)
  %48 = trunc i64 %47 to i32
  store i32 %48, i32* %16, align 4
  %49 = load i64, i64* %13, align 8
  %50 = load i32, i32* %14, align 4
  %51 = zext i32 %50 to i64
  %52 = shl i64 1, %51
  %53 = and i64 %49, %52
  %54 = icmp ne i64 %53, 0
  %55 = zext i1 %54 to i8
  store i8 %55, i8* %17, align 1
  %56 = load i8, i8* %17, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %149

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.6*, %class.KV.6** %12, align 8
  %60 = load i32, i32* %16, align 4
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %class.KV.6, %class.KV.6* %59, i64 %61
  %63 = getelementptr inbounds %class.KV.6, %class.KV.6* %62, i32 0, i32 0
  %64 = bitcast %"union.KV<key, key, 7>::Key"* %63 to i64*
  %65 = load i64, i64* %64, align 8
  %66 = and i64 %65, 1
  %67 = icmp eq i64 %66, 0
  br i1 %67, label %68, label %130

; <label>:68:                                     ; preds = %58
  %69 = load %class.KV.6*, %class.KV.6** %12, align 8
  %70 = load i32, i32* %16, align 4
  %71 = zext i32 %70 to i64
  %72 = getelementptr inbounds %class.KV.6, %class.KV.6* %69, i64 %71
  %73 = getelementptr inbounds %class.KV.6, %class.KV.6* %72, i32 0, i32 0
  %74 = bitcast %"union.KV<key, key, 7>::Key"* %73 to %class.key**
  %75 = load %class.key*, %class.key** %74, align 8
  %76 = load %class.key*, %class.key** %9, align 8
  %77 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %75, %class.key* dereferenceable(8) %76)
  br i1 %77, label %78, label %90

; <label>:78:                                     ; preds = %68
  %79 = load %class.KV.6*, %class.KV.6** %12, align 8
  %80 = load i32, i32* %15, align 4
  %81 = load i32, i32* %16, align 4
  %82 = load %class.key*, %class.key** %9, align 8
  %83 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %19, %class.key* %82, %class.key* %83)
  %84 = call %class.KV.6* @_ZN2KVI3keyS0_Lj7EE11update_nodeEPKS1_jjRS2_(%class.KV.6* %79, i32 %80, i32 %81, %class.KV.6* dereferenceable(16) %19)
  store %class.KV.6* %84, %class.KV.6** %18, align 8
  %85 = load %class.KV.5*, %class.KV.5** %7, align 8
  %86 = getelementptr inbounds %class.KV.5, %class.KV.5* %85, i32 0, i32 0
  %87 = bitcast %"union.KV<key, key, 6>::Key"* %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = load %class.KV.6*, %class.KV.6** %18, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %0, i64 %88, %class.KV.6* %89)
  br label %198

; <label>:90:                                     ; preds = %68
  %91 = load i64*, i64** %11, align 8
  %92 = load i64, i64* %91, align 8
  %93 = add i64 %92, 1
  store i64 %93, i64* %91, align 8
  %94 = load %class.KV.6*, %class.KV.6** %12, align 8
  %95 = load i32, i32* %16, align 4
  %96 = zext i32 %95 to i64
  %97 = getelementptr inbounds %class.KV.6, %class.KV.6* %94, i64 %96
  %98 = getelementptr inbounds %class.KV.6, %class.KV.6* %97, i32 0, i32 0
  %99 = bitcast %"union.KV<key, key, 7>::Key"* %98 to %class.key**
  %100 = load %class.key*, %class.key** %99, align 8
  %101 = call i64 @_ZNK3key4hashEv(%class.key* %100)
  %102 = lshr i64 %101, 46
  %103 = load %class.KV.6*, %class.KV.6** %12, align 8
  %104 = load i32, i32* %16, align 4
  %105 = zext i32 %104 to i64
  %106 = getelementptr inbounds %class.KV.6, %class.KV.6* %103, i64 %105
  %107 = getelementptr inbounds %class.KV.6, %class.KV.6* %106, i32 0, i32 0
  %108 = bitcast %"union.KV<key, key, 7>::Key"* %107 to %class.key**
  %109 = load %class.key*, %class.key** %108, align 8
  %110 = load %class.KV.6*, %class.KV.6** %12, align 8
  %111 = load i32, i32* %16, align 4
  %112 = zext i32 %111 to i64
  %113 = getelementptr inbounds %class.KV.6, %class.KV.6* %110, i64 %112
  %114 = getelementptr inbounds %class.KV.6, %class.KV.6* %113, i32 0, i32 1
  %115 = bitcast %"union.KV<key, key, 7>::Val"* %114 to %class.key**
  %116 = load %class.key*, %class.key** %115, align 8
  %117 = load i64, i64* %8, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.key*, %class.key** %9, align 8
  %120 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj7EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.6* sret %20, i64 %102, %class.key* %109, %class.key* %116, i64 %118, %class.key* %119, %class.key* %120)
  %121 = load %class.KV.6*, %class.KV.6** %12, align 8
  %122 = load i32, i32* %15, align 4
  %123 = load i32, i32* %16, align 4
  %124 = call %class.KV.6* @_ZN2KVI3keyS0_Lj7EE11update_nodeEPKS1_jjRS2_(%class.KV.6* %121, i32 %122, i32 %123, %class.KV.6* dereferenceable(16) %20)
  store %class.KV.6* %124, %class.KV.6** %21, align 8
  %125 = load %class.KV.5*, %class.KV.5** %7, align 8
  %126 = getelementptr inbounds %class.KV.5, %class.KV.5* %125, i32 0, i32 0
  %127 = bitcast %"union.KV<key, key, 6>::Key"* %126 to i64*
  %128 = load i64, i64* %127, align 8
  %129 = load %class.KV.6*, %class.KV.6** %21, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %0, i64 %128, %class.KV.6* %129)
  br label %198

; <label>:130:                                    ; preds = %58
  %131 = load %class.KV.6*, %class.KV.6** %12, align 8
  %132 = load i32, i32* %16, align 4
  %133 = zext i32 %132 to i64
  %134 = getelementptr inbounds %class.KV.6, %class.KV.6* %131, i64 %133
  %135 = load i64, i64* %8, align 8
  %136 = lshr i64 %135, 6
  %137 = load %class.key*, %class.key** %9, align 8
  %138 = load %class.key*, %class.key** %10, align 8
  %139 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3keyS0_Lj7EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.6* sret %22, %class.KV.6* dereferenceable(16) %134, i64 %136, %class.key* %137, %class.key* %138, i64* %139)
  %140 = load %class.KV.6*, %class.KV.6** %12, align 8
  %141 = load i32, i32* %15, align 4
  %142 = load i32, i32* %16, align 4
  %143 = call %class.KV.6* @_ZN2KVI3keyS0_Lj7EE11update_nodeEPKS1_jjRS2_(%class.KV.6* %140, i32 %141, i32 %142, %class.KV.6* dereferenceable(16) %22)
  store %class.KV.6* %143, %class.KV.6** %23, align 8
  %144 = load %class.KV.5*, %class.KV.5** %7, align 8
  %145 = getelementptr inbounds %class.KV.5, %class.KV.5* %144, i32 0, i32 0
  %146 = bitcast %"union.KV<key, key, 6>::Key"* %145 to i64*
  %147 = load i64, i64* %146, align 8
  %148 = load %class.KV.6*, %class.KV.6** %23, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %0, i64 %147, %class.KV.6* %148)
  br label %198

; <label>:149:                                    ; preds = %6
  %150 = load i64*, i64** %11, align 8
  %151 = load i64, i64* %150, align 8
  %152 = add i64 %151, 1
  store i64 %152, i64* %150, align 8
  %153 = load i32, i32* %15, align 4
  %154 = add i32 %153, 1
  %155 = zext i32 %154 to i64
  %156 = mul i64 %155, 16
  %157 = call noalias i8* @malloc(i64 %156) #8
  %158 = bitcast i8* %157 to %class.KV.6*
  store %class.KV.6* %158, %class.KV.6** %24, align 8
  %159 = load %class.KV.6*, %class.KV.6** %24, align 8
  %160 = bitcast %class.KV.6* %159 to i8*
  %161 = load %class.KV.6*, %class.KV.6** %12, align 8
  %162 = bitcast %class.KV.6* %161 to i8*
  %163 = load i32, i32* %16, align 4
  %164 = zext i32 %163 to i64
  %165 = mul i64 %164, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %160, i8* %162, i64 %165, i32 8, i1 false)
  %166 = load %class.KV.6*, %class.KV.6** %24, align 8
  %167 = load i32, i32* %16, align 4
  %168 = add i32 %167, 1
  %169 = zext i32 %168 to i64
  %170 = getelementptr inbounds %class.KV.6, %class.KV.6* %166, i64 %169
  %171 = bitcast %class.KV.6* %170 to i8*
  %172 = load %class.KV.6*, %class.KV.6** %12, align 8
  %173 = load i32, i32* %16, align 4
  %174 = zext i32 %173 to i64
  %175 = getelementptr inbounds %class.KV.6, %class.KV.6* %172, i64 %174
  %176 = bitcast %class.KV.6* %175 to i8*
  %177 = load i32, i32* %15, align 4
  %178 = load i32, i32* %16, align 4
  %179 = sub i32 %177, %178
  %180 = zext i32 %179 to i64
  %181 = mul i64 %180, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %171, i8* %176, i64 %181, i32 8, i1 false)
  %182 = load %class.KV.6*, %class.KV.6** %24, align 8
  %183 = load i32, i32* %16, align 4
  %184 = zext i32 %183 to i64
  %185 = getelementptr inbounds %class.KV.6, %class.KV.6* %182, i64 %184
  %186 = bitcast %class.KV.6* %185 to i8*
  %187 = bitcast i8* %186 to %class.KV.6*
  %188 = load %class.key*, %class.key** %9, align 8
  %189 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EPKS0_S3_(%class.KV.6* %187, %class.key* %188, %class.key* %189)
  %190 = load i64, i64* %13, align 8
  %191 = load i32, i32* %14, align 4
  %192 = zext i32 %191 to i64
  %193 = shl i64 1, %192
  %194 = or i64 %190, %193
  %195 = shl i64 %194, 1
  %196 = or i64 %195, 1
  %197 = load %class.KV.6*, %class.KV.6** %24, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %0, i64 %196, %class.KV.6* %197)
  br label %198

; <label>:198:                                    ; preds = %149, %130, %90, %78
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.KV.6* @_ZN2KVI3keyS0_Lj7EE11update_nodeEPKS1_jjRS2_(%class.KV.6*, i32, i32, %class.KV.6* dereferenceable(16)) #2 comdat align 2 {
  %5 = alloca %class.KV.6*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %class.KV.6*, align 8
  %9 = alloca %class.KV.6*, align 8
  store %class.KV.6* %0, %class.KV.6** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  store %class.KV.6* %3, %class.KV.6** %8, align 8
  %10 = load i32, i32* %6, align 4
  %11 = zext i32 %10 to i64
  %12 = mul i64 %11, 16
  %13 = call noalias i8* @malloc(i64 %12) #8
  %14 = bitcast i8* %13 to %class.KV.6*
  store %class.KV.6* %14, %class.KV.6** %9, align 8
  %15 = load %class.KV.6*, %class.KV.6** %9, align 8
  %16 = bitcast %class.KV.6* %15 to i8*
  %17 = load %class.KV.6*, %class.KV.6** %5, align 8
  %18 = bitcast %class.KV.6* %17 to i8*
  %19 = load i32, i32* %6, align 4
  %20 = zext i32 %19 to i64
  %21 = mul i64 %20, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %16, i8* %18, i64 %21, i32 8, i1 false)
  %22 = load %class.KV.6*, %class.KV.6** %9, align 8
  %23 = load i32, i32* %7, align 4
  %24 = zext i32 %23 to i64
  %25 = getelementptr inbounds %class.KV.6, %class.KV.6* %22, i64 %24
  %26 = bitcast %class.KV.6* %25 to i8*
  %27 = bitcast i8* %26 to %class.KV.6*
  %28 = load %class.KV.6*, %class.KV.6** %8, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2ERKS1_(%class.KV.6* %27, %class.KV.6* dereferenceable(16) %28)
  %29 = load %class.KV.6*, %class.KV.6** %9, align 8
  ret %class.KV.6* %29
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj7EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.6* noalias sret, %class.KV.6* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV.6*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.7*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.7*, align 8
  %19 = alloca %class.KV.7, align 8
  %20 = alloca %class.KV.7, align 8
  %21 = alloca %class.KV.7*, align 8
  %22 = alloca %class.KV.7, align 8
  %23 = alloca %class.KV.7*, align 8
  %24 = alloca %class.KV.7*, align 8
  store %class.KV.6* %1, %class.KV.6** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.6*, %class.KV.6** %7, align 8
  %26 = getelementptr inbounds %class.KV.6, %class.KV.6* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 7>::Val"* %26 to %class.KV.7**
  %28 = load %class.KV.7*, %class.KV.7** %27, align 8
  store %class.KV.7* %28, %class.KV.7** %12, align 8
  %29 = load %class.KV.6*, %class.KV.6** %7, align 8
  %30 = getelementptr inbounds %class.KV.6, %class.KV.6* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 7>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %14, align 4
  %38 = load i64, i64* %13, align 8
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %15, align 4
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i32, i32* %14, align 4
  %44 = sub i32 63, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %42, %45
  %47 = call i64 @llvm.ctpop.i64(i64 %46)
  %48 = trunc i64 %47 to i32
  store i32 %48, i32* %16, align 4
  %49 = load i64, i64* %13, align 8
  %50 = load i32, i32* %14, align 4
  %51 = zext i32 %50 to i64
  %52 = shl i64 1, %51
  %53 = and i64 %49, %52
  %54 = icmp ne i64 %53, 0
  %55 = zext i1 %54 to i8
  store i8 %55, i8* %17, align 1
  %56 = load i8, i8* %17, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %149

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.7*, %class.KV.7** %12, align 8
  %60 = load i32, i32* %16, align 4
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %class.KV.7, %class.KV.7* %59, i64 %61
  %63 = getelementptr inbounds %class.KV.7, %class.KV.7* %62, i32 0, i32 0
  %64 = bitcast %"union.KV<key, key, 8>::Key"* %63 to i64*
  %65 = load i64, i64* %64, align 8
  %66 = and i64 %65, 1
  %67 = icmp eq i64 %66, 0
  br i1 %67, label %68, label %130

; <label>:68:                                     ; preds = %58
  %69 = load %class.KV.7*, %class.KV.7** %12, align 8
  %70 = load i32, i32* %16, align 4
  %71 = zext i32 %70 to i64
  %72 = getelementptr inbounds %class.KV.7, %class.KV.7* %69, i64 %71
  %73 = getelementptr inbounds %class.KV.7, %class.KV.7* %72, i32 0, i32 0
  %74 = bitcast %"union.KV<key, key, 8>::Key"* %73 to %class.key**
  %75 = load %class.key*, %class.key** %74, align 8
  %76 = load %class.key*, %class.key** %9, align 8
  %77 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %75, %class.key* dereferenceable(8) %76)
  br i1 %77, label %78, label %90

; <label>:78:                                     ; preds = %68
  %79 = load %class.KV.7*, %class.KV.7** %12, align 8
  %80 = load i32, i32* %15, align 4
  %81 = load i32, i32* %16, align 4
  %82 = load %class.key*, %class.key** %9, align 8
  %83 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %19, %class.key* %82, %class.key* %83)
  %84 = call %class.KV.7* @_ZN2KVI3keyS0_Lj8EE11update_nodeEPKS1_jjRS2_(%class.KV.7* %79, i32 %80, i32 %81, %class.KV.7* dereferenceable(16) %19)
  store %class.KV.7* %84, %class.KV.7** %18, align 8
  %85 = load %class.KV.6*, %class.KV.6** %7, align 8
  %86 = getelementptr inbounds %class.KV.6, %class.KV.6* %85, i32 0, i32 0
  %87 = bitcast %"union.KV<key, key, 7>::Key"* %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = load %class.KV.7*, %class.KV.7** %18, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %0, i64 %88, %class.KV.7* %89)
  br label %198

; <label>:90:                                     ; preds = %68
  %91 = load i64*, i64** %11, align 8
  %92 = load i64, i64* %91, align 8
  %93 = add i64 %92, 1
  store i64 %93, i64* %91, align 8
  %94 = load %class.KV.7*, %class.KV.7** %12, align 8
  %95 = load i32, i32* %16, align 4
  %96 = zext i32 %95 to i64
  %97 = getelementptr inbounds %class.KV.7, %class.KV.7* %94, i64 %96
  %98 = getelementptr inbounds %class.KV.7, %class.KV.7* %97, i32 0, i32 0
  %99 = bitcast %"union.KV<key, key, 8>::Key"* %98 to %class.key**
  %100 = load %class.key*, %class.key** %99, align 8
  %101 = call i64 @_ZNK3key4hashEv(%class.key* %100)
  %102 = lshr i64 %101, 52
  %103 = load %class.KV.7*, %class.KV.7** %12, align 8
  %104 = load i32, i32* %16, align 4
  %105 = zext i32 %104 to i64
  %106 = getelementptr inbounds %class.KV.7, %class.KV.7* %103, i64 %105
  %107 = getelementptr inbounds %class.KV.7, %class.KV.7* %106, i32 0, i32 0
  %108 = bitcast %"union.KV<key, key, 8>::Key"* %107 to %class.key**
  %109 = load %class.key*, %class.key** %108, align 8
  %110 = load %class.KV.7*, %class.KV.7** %12, align 8
  %111 = load i32, i32* %16, align 4
  %112 = zext i32 %111 to i64
  %113 = getelementptr inbounds %class.KV.7, %class.KV.7* %110, i64 %112
  %114 = getelementptr inbounds %class.KV.7, %class.KV.7* %113, i32 0, i32 1
  %115 = bitcast %"union.KV<key, key, 8>::Val"* %114 to %class.key**
  %116 = load %class.key*, %class.key** %115, align 8
  %117 = load i64, i64* %8, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.key*, %class.key** %9, align 8
  %120 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj8EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.7* sret %20, i64 %102, %class.key* %109, %class.key* %116, i64 %118, %class.key* %119, %class.key* %120)
  %121 = load %class.KV.7*, %class.KV.7** %12, align 8
  %122 = load i32, i32* %15, align 4
  %123 = load i32, i32* %16, align 4
  %124 = call %class.KV.7* @_ZN2KVI3keyS0_Lj8EE11update_nodeEPKS1_jjRS2_(%class.KV.7* %121, i32 %122, i32 %123, %class.KV.7* dereferenceable(16) %20)
  store %class.KV.7* %124, %class.KV.7** %21, align 8
  %125 = load %class.KV.6*, %class.KV.6** %7, align 8
  %126 = getelementptr inbounds %class.KV.6, %class.KV.6* %125, i32 0, i32 0
  %127 = bitcast %"union.KV<key, key, 7>::Key"* %126 to i64*
  %128 = load i64, i64* %127, align 8
  %129 = load %class.KV.7*, %class.KV.7** %21, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %0, i64 %128, %class.KV.7* %129)
  br label %198

; <label>:130:                                    ; preds = %58
  %131 = load %class.KV.7*, %class.KV.7** %12, align 8
  %132 = load i32, i32* %16, align 4
  %133 = zext i32 %132 to i64
  %134 = getelementptr inbounds %class.KV.7, %class.KV.7* %131, i64 %133
  %135 = load i64, i64* %8, align 8
  %136 = lshr i64 %135, 6
  %137 = load %class.key*, %class.key** %9, align 8
  %138 = load %class.key*, %class.key** %10, align 8
  %139 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3keyS0_Lj8EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.7* sret %22, %class.KV.7* dereferenceable(16) %134, i64 %136, %class.key* %137, %class.key* %138, i64* %139)
  %140 = load %class.KV.7*, %class.KV.7** %12, align 8
  %141 = load i32, i32* %15, align 4
  %142 = load i32, i32* %16, align 4
  %143 = call %class.KV.7* @_ZN2KVI3keyS0_Lj8EE11update_nodeEPKS1_jjRS2_(%class.KV.7* %140, i32 %141, i32 %142, %class.KV.7* dereferenceable(16) %22)
  store %class.KV.7* %143, %class.KV.7** %23, align 8
  %144 = load %class.KV.6*, %class.KV.6** %7, align 8
  %145 = getelementptr inbounds %class.KV.6, %class.KV.6* %144, i32 0, i32 0
  %146 = bitcast %"union.KV<key, key, 7>::Key"* %145 to i64*
  %147 = load i64, i64* %146, align 8
  %148 = load %class.KV.7*, %class.KV.7** %23, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %0, i64 %147, %class.KV.7* %148)
  br label %198

; <label>:149:                                    ; preds = %6
  %150 = load i64*, i64** %11, align 8
  %151 = load i64, i64* %150, align 8
  %152 = add i64 %151, 1
  store i64 %152, i64* %150, align 8
  %153 = load i32, i32* %15, align 4
  %154 = add i32 %153, 1
  %155 = zext i32 %154 to i64
  %156 = mul i64 %155, 16
  %157 = call noalias i8* @malloc(i64 %156) #8
  %158 = bitcast i8* %157 to %class.KV.7*
  store %class.KV.7* %158, %class.KV.7** %24, align 8
  %159 = load %class.KV.7*, %class.KV.7** %24, align 8
  %160 = bitcast %class.KV.7* %159 to i8*
  %161 = load %class.KV.7*, %class.KV.7** %12, align 8
  %162 = bitcast %class.KV.7* %161 to i8*
  %163 = load i32, i32* %16, align 4
  %164 = zext i32 %163 to i64
  %165 = mul i64 %164, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %160, i8* %162, i64 %165, i32 8, i1 false)
  %166 = load %class.KV.7*, %class.KV.7** %24, align 8
  %167 = load i32, i32* %16, align 4
  %168 = add i32 %167, 1
  %169 = zext i32 %168 to i64
  %170 = getelementptr inbounds %class.KV.7, %class.KV.7* %166, i64 %169
  %171 = bitcast %class.KV.7* %170 to i8*
  %172 = load %class.KV.7*, %class.KV.7** %12, align 8
  %173 = load i32, i32* %16, align 4
  %174 = zext i32 %173 to i64
  %175 = getelementptr inbounds %class.KV.7, %class.KV.7* %172, i64 %174
  %176 = bitcast %class.KV.7* %175 to i8*
  %177 = load i32, i32* %15, align 4
  %178 = load i32, i32* %16, align 4
  %179 = sub i32 %177, %178
  %180 = zext i32 %179 to i64
  %181 = mul i64 %180, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %171, i8* %176, i64 %181, i32 8, i1 false)
  %182 = load %class.KV.7*, %class.KV.7** %24, align 8
  %183 = load i32, i32* %16, align 4
  %184 = zext i32 %183 to i64
  %185 = getelementptr inbounds %class.KV.7, %class.KV.7* %182, i64 %184
  %186 = bitcast %class.KV.7* %185 to i8*
  %187 = bitcast i8* %186 to %class.KV.7*
  %188 = load %class.key*, %class.key** %9, align 8
  %189 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EPKS0_S3_(%class.KV.7* %187, %class.key* %188, %class.key* %189)
  %190 = load i64, i64* %13, align 8
  %191 = load i32, i32* %14, align 4
  %192 = zext i32 %191 to i64
  %193 = shl i64 1, %192
  %194 = or i64 %190, %193
  %195 = shl i64 %194, 1
  %196 = or i64 %195, 1
  %197 = load %class.KV.7*, %class.KV.7** %24, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %0, i64 %196, %class.KV.7* %197)
  br label %198

; <label>:198:                                    ; preds = %149, %130, %90, %78
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.KV.7* @_ZN2KVI3keyS0_Lj8EE11update_nodeEPKS1_jjRS2_(%class.KV.7*, i32, i32, %class.KV.7* dereferenceable(16)) #2 comdat align 2 {
  %5 = alloca %class.KV.7*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %class.KV.7*, align 8
  %9 = alloca %class.KV.7*, align 8
  store %class.KV.7* %0, %class.KV.7** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  store %class.KV.7* %3, %class.KV.7** %8, align 8
  %10 = load i32, i32* %6, align 4
  %11 = zext i32 %10 to i64
  %12 = mul i64 %11, 16
  %13 = call noalias i8* @malloc(i64 %12) #8
  %14 = bitcast i8* %13 to %class.KV.7*
  store %class.KV.7* %14, %class.KV.7** %9, align 8
  %15 = load %class.KV.7*, %class.KV.7** %9, align 8
  %16 = bitcast %class.KV.7* %15 to i8*
  %17 = load %class.KV.7*, %class.KV.7** %5, align 8
  %18 = bitcast %class.KV.7* %17 to i8*
  %19 = load i32, i32* %6, align 4
  %20 = zext i32 %19 to i64
  %21 = mul i64 %20, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %16, i8* %18, i64 %21, i32 8, i1 false)
  %22 = load %class.KV.7*, %class.KV.7** %9, align 8
  %23 = load i32, i32* %7, align 4
  %24 = zext i32 %23 to i64
  %25 = getelementptr inbounds %class.KV.7, %class.KV.7* %22, i64 %24
  %26 = bitcast %class.KV.7* %25 to i8*
  %27 = bitcast i8* %26 to %class.KV.7*
  %28 = load %class.KV.7*, %class.KV.7** %8, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2ERKS1_(%class.KV.7* %27, %class.KV.7* dereferenceable(16) %28)
  %29 = load %class.KV.7*, %class.KV.7** %9, align 8
  ret %class.KV.7* %29
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj8EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.7* noalias sret, %class.KV.7* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV.7*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.8*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.8*, align 8
  %19 = alloca %class.KV.8, align 8
  %20 = alloca %class.KV.8, align 8
  %21 = alloca %class.KV.8*, align 8
  %22 = alloca %class.KV.8, align 8
  %23 = alloca %class.KV.8*, align 8
  %24 = alloca %class.KV.8*, align 8
  store %class.KV.7* %1, %class.KV.7** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.7*, %class.KV.7** %7, align 8
  %26 = getelementptr inbounds %class.KV.7, %class.KV.7* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 8>::Val"* %26 to %class.KV.8**
  %28 = load %class.KV.8*, %class.KV.8** %27, align 8
  store %class.KV.8* %28, %class.KV.8** %12, align 8
  %29 = load %class.KV.7*, %class.KV.7** %7, align 8
  %30 = getelementptr inbounds %class.KV.7, %class.KV.7* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 8>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %14, align 4
  %38 = load i64, i64* %13, align 8
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %15, align 4
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i32, i32* %14, align 4
  %44 = sub i32 63, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %42, %45
  %47 = call i64 @llvm.ctpop.i64(i64 %46)
  %48 = trunc i64 %47 to i32
  store i32 %48, i32* %16, align 4
  %49 = load i64, i64* %13, align 8
  %50 = load i32, i32* %14, align 4
  %51 = zext i32 %50 to i64
  %52 = shl i64 1, %51
  %53 = and i64 %49, %52
  %54 = icmp ne i64 %53, 0
  %55 = zext i1 %54 to i8
  store i8 %55, i8* %17, align 1
  %56 = load i8, i8* %17, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %149

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.8*, %class.KV.8** %12, align 8
  %60 = load i32, i32* %16, align 4
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %class.KV.8, %class.KV.8* %59, i64 %61
  %63 = getelementptr inbounds %class.KV.8, %class.KV.8* %62, i32 0, i32 0
  %64 = bitcast %"union.KV<key, key, 9>::Key"* %63 to i64*
  %65 = load i64, i64* %64, align 8
  %66 = and i64 %65, 1
  %67 = icmp eq i64 %66, 0
  br i1 %67, label %68, label %130

; <label>:68:                                     ; preds = %58
  %69 = load %class.KV.8*, %class.KV.8** %12, align 8
  %70 = load i32, i32* %16, align 4
  %71 = zext i32 %70 to i64
  %72 = getelementptr inbounds %class.KV.8, %class.KV.8* %69, i64 %71
  %73 = getelementptr inbounds %class.KV.8, %class.KV.8* %72, i32 0, i32 0
  %74 = bitcast %"union.KV<key, key, 9>::Key"* %73 to %class.key**
  %75 = load %class.key*, %class.key** %74, align 8
  %76 = load %class.key*, %class.key** %9, align 8
  %77 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %75, %class.key* dereferenceable(8) %76)
  br i1 %77, label %78, label %90

; <label>:78:                                     ; preds = %68
  %79 = load %class.KV.8*, %class.KV.8** %12, align 8
  %80 = load i32, i32* %15, align 4
  %81 = load i32, i32* %16, align 4
  %82 = load %class.key*, %class.key** %9, align 8
  %83 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %19, %class.key* %82, %class.key* %83)
  %84 = call %class.KV.8* @_ZN2KVI3keyS0_Lj9EE11update_nodeEPKS1_jjRS2_(%class.KV.8* %79, i32 %80, i32 %81, %class.KV.8* dereferenceable(16) %19)
  store %class.KV.8* %84, %class.KV.8** %18, align 8
  %85 = load %class.KV.7*, %class.KV.7** %7, align 8
  %86 = getelementptr inbounds %class.KV.7, %class.KV.7* %85, i32 0, i32 0
  %87 = bitcast %"union.KV<key, key, 8>::Key"* %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = load %class.KV.8*, %class.KV.8** %18, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %0, i64 %88, %class.KV.8* %89)
  br label %198

; <label>:90:                                     ; preds = %68
  %91 = load i64*, i64** %11, align 8
  %92 = load i64, i64* %91, align 8
  %93 = add i64 %92, 1
  store i64 %93, i64* %91, align 8
  %94 = load %class.KV.8*, %class.KV.8** %12, align 8
  %95 = load i32, i32* %16, align 4
  %96 = zext i32 %95 to i64
  %97 = getelementptr inbounds %class.KV.8, %class.KV.8* %94, i64 %96
  %98 = getelementptr inbounds %class.KV.8, %class.KV.8* %97, i32 0, i32 0
  %99 = bitcast %"union.KV<key, key, 9>::Key"* %98 to %class.key**
  %100 = load %class.key*, %class.key** %99, align 8
  %101 = call i64 @_ZNK3key4hashEv(%class.key* %100)
  %102 = lshr i64 %101, 58
  %103 = load %class.KV.8*, %class.KV.8** %12, align 8
  %104 = load i32, i32* %16, align 4
  %105 = zext i32 %104 to i64
  %106 = getelementptr inbounds %class.KV.8, %class.KV.8* %103, i64 %105
  %107 = getelementptr inbounds %class.KV.8, %class.KV.8* %106, i32 0, i32 0
  %108 = bitcast %"union.KV<key, key, 9>::Key"* %107 to %class.key**
  %109 = load %class.key*, %class.key** %108, align 8
  %110 = load %class.KV.8*, %class.KV.8** %12, align 8
  %111 = load i32, i32* %16, align 4
  %112 = zext i32 %111 to i64
  %113 = getelementptr inbounds %class.KV.8, %class.KV.8* %110, i64 %112
  %114 = getelementptr inbounds %class.KV.8, %class.KV.8* %113, i32 0, i32 1
  %115 = bitcast %"union.KV<key, key, 9>::Val"* %114 to %class.key**
  %116 = load %class.key*, %class.key** %115, align 8
  %117 = load i64, i64* %8, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.key*, %class.key** %9, align 8
  %120 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj9EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.8* sret %20, i64 %102, %class.key* %109, %class.key* %116, i64 %118, %class.key* %119, %class.key* %120)
  %121 = load %class.KV.8*, %class.KV.8** %12, align 8
  %122 = load i32, i32* %15, align 4
  %123 = load i32, i32* %16, align 4
  %124 = call %class.KV.8* @_ZN2KVI3keyS0_Lj9EE11update_nodeEPKS1_jjRS2_(%class.KV.8* %121, i32 %122, i32 %123, %class.KV.8* dereferenceable(16) %20)
  store %class.KV.8* %124, %class.KV.8** %21, align 8
  %125 = load %class.KV.7*, %class.KV.7** %7, align 8
  %126 = getelementptr inbounds %class.KV.7, %class.KV.7* %125, i32 0, i32 0
  %127 = bitcast %"union.KV<key, key, 8>::Key"* %126 to i64*
  %128 = load i64, i64* %127, align 8
  %129 = load %class.KV.8*, %class.KV.8** %21, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %0, i64 %128, %class.KV.8* %129)
  br label %198

; <label>:130:                                    ; preds = %58
  %131 = load %class.KV.8*, %class.KV.8** %12, align 8
  %132 = load i32, i32* %16, align 4
  %133 = zext i32 %132 to i64
  %134 = getelementptr inbounds %class.KV.8, %class.KV.8* %131, i64 %133
  %135 = load i64, i64* %8, align 8
  %136 = lshr i64 %135, 6
  %137 = load %class.key*, %class.key** %9, align 8
  %138 = load %class.key*, %class.key** %10, align 8
  %139 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3keyS0_Lj9EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.8* sret %22, %class.KV.8* dereferenceable(16) %134, i64 %136, %class.key* %137, %class.key* %138, i64* %139)
  %140 = load %class.KV.8*, %class.KV.8** %12, align 8
  %141 = load i32, i32* %15, align 4
  %142 = load i32, i32* %16, align 4
  %143 = call %class.KV.8* @_ZN2KVI3keyS0_Lj9EE11update_nodeEPKS1_jjRS2_(%class.KV.8* %140, i32 %141, i32 %142, %class.KV.8* dereferenceable(16) %22)
  store %class.KV.8* %143, %class.KV.8** %23, align 8
  %144 = load %class.KV.7*, %class.KV.7** %7, align 8
  %145 = getelementptr inbounds %class.KV.7, %class.KV.7* %144, i32 0, i32 0
  %146 = bitcast %"union.KV<key, key, 8>::Key"* %145 to i64*
  %147 = load i64, i64* %146, align 8
  %148 = load %class.KV.8*, %class.KV.8** %23, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %0, i64 %147, %class.KV.8* %148)
  br label %198

; <label>:149:                                    ; preds = %6
  %150 = load i64*, i64** %11, align 8
  %151 = load i64, i64* %150, align 8
  %152 = add i64 %151, 1
  store i64 %152, i64* %150, align 8
  %153 = load i32, i32* %15, align 4
  %154 = add i32 %153, 1
  %155 = zext i32 %154 to i64
  %156 = mul i64 %155, 16
  %157 = call noalias i8* @malloc(i64 %156) #8
  %158 = bitcast i8* %157 to %class.KV.8*
  store %class.KV.8* %158, %class.KV.8** %24, align 8
  %159 = load %class.KV.8*, %class.KV.8** %24, align 8
  %160 = bitcast %class.KV.8* %159 to i8*
  %161 = load %class.KV.8*, %class.KV.8** %12, align 8
  %162 = bitcast %class.KV.8* %161 to i8*
  %163 = load i32, i32* %16, align 4
  %164 = zext i32 %163 to i64
  %165 = mul i64 %164, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %160, i8* %162, i64 %165, i32 8, i1 false)
  %166 = load %class.KV.8*, %class.KV.8** %24, align 8
  %167 = load i32, i32* %16, align 4
  %168 = add i32 %167, 1
  %169 = zext i32 %168 to i64
  %170 = getelementptr inbounds %class.KV.8, %class.KV.8* %166, i64 %169
  %171 = bitcast %class.KV.8* %170 to i8*
  %172 = load %class.KV.8*, %class.KV.8** %12, align 8
  %173 = load i32, i32* %16, align 4
  %174 = zext i32 %173 to i64
  %175 = getelementptr inbounds %class.KV.8, %class.KV.8* %172, i64 %174
  %176 = bitcast %class.KV.8* %175 to i8*
  %177 = load i32, i32* %15, align 4
  %178 = load i32, i32* %16, align 4
  %179 = sub i32 %177, %178
  %180 = zext i32 %179 to i64
  %181 = mul i64 %180, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %171, i8* %176, i64 %181, i32 8, i1 false)
  %182 = load %class.KV.8*, %class.KV.8** %24, align 8
  %183 = load i32, i32* %16, align 4
  %184 = zext i32 %183 to i64
  %185 = getelementptr inbounds %class.KV.8, %class.KV.8* %182, i64 %184
  %186 = bitcast %class.KV.8* %185 to i8*
  %187 = bitcast i8* %186 to %class.KV.8*
  %188 = load %class.key*, %class.key** %9, align 8
  %189 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EPKS0_S3_(%class.KV.8* %187, %class.key* %188, %class.key* %189)
  %190 = load i64, i64* %13, align 8
  %191 = load i32, i32* %14, align 4
  %192 = zext i32 %191 to i64
  %193 = shl i64 1, %192
  %194 = or i64 %190, %193
  %195 = shl i64 %194, 1
  %196 = or i64 %195, 1
  %197 = load %class.KV.8*, %class.KV.8** %24, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %0, i64 %196, %class.KV.8* %197)
  br label %198

; <label>:198:                                    ; preds = %149, %130, %90, %78
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.KV.8* @_ZN2KVI3keyS0_Lj9EE11update_nodeEPKS1_jjRS2_(%class.KV.8*, i32, i32, %class.KV.8* dereferenceable(16)) #2 comdat align 2 {
  %5 = alloca %class.KV.8*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %class.KV.8*, align 8
  %9 = alloca %class.KV.8*, align 8
  store %class.KV.8* %0, %class.KV.8** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  store %class.KV.8* %3, %class.KV.8** %8, align 8
  %10 = load i32, i32* %6, align 4
  %11 = zext i32 %10 to i64
  %12 = mul i64 %11, 16
  %13 = call noalias i8* @malloc(i64 %12) #8
  %14 = bitcast i8* %13 to %class.KV.8*
  store %class.KV.8* %14, %class.KV.8** %9, align 8
  %15 = load %class.KV.8*, %class.KV.8** %9, align 8
  %16 = bitcast %class.KV.8* %15 to i8*
  %17 = load %class.KV.8*, %class.KV.8** %5, align 8
  %18 = bitcast %class.KV.8* %17 to i8*
  %19 = load i32, i32* %6, align 4
  %20 = zext i32 %19 to i64
  %21 = mul i64 %20, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %16, i8* %18, i64 %21, i32 8, i1 false)
  %22 = load %class.KV.8*, %class.KV.8** %9, align 8
  %23 = load i32, i32* %7, align 4
  %24 = zext i32 %23 to i64
  %25 = getelementptr inbounds %class.KV.8, %class.KV.8* %22, i64 %24
  %26 = bitcast %class.KV.8* %25 to i8*
  %27 = bitcast i8* %26 to %class.KV.8*
  %28 = load %class.KV.8*, %class.KV.8** %8, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2ERKS1_(%class.KV.8* %27, %class.KV.8* dereferenceable(16) %28)
  %29 = load %class.KV.8*, %class.KV.8** %9, align 8
  ret %class.KV.8* %29
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj9EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.8* noalias sret, %class.KV.8* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV.8*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.KV.9*, align 8
  %13 = alloca i64, align 8
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i8, align 1
  %18 = alloca %class.KV.9*, align 8
  %19 = alloca %class.KV.9, align 8
  %20 = alloca %class.KV.9, align 8
  %21 = alloca %class.KV.9*, align 8
  %22 = alloca %class.KV.9, align 8
  %23 = alloca %class.KV.9*, align 8
  %24 = alloca %class.KV.9*, align 8
  store %class.KV.8* %1, %class.KV.8** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %25 = load %class.KV.8*, %class.KV.8** %7, align 8
  %26 = getelementptr inbounds %class.KV.8, %class.KV.8* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 9>::Val"* %26 to %class.KV.9**
  %28 = load %class.KV.9*, %class.KV.9** %27, align 8
  store %class.KV.9* %28, %class.KV.9** %12, align 8
  %29 = load %class.KV.8*, %class.KV.8** %7, align 8
  %30 = getelementptr inbounds %class.KV.8, %class.KV.8* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 9>::Key"* %30 to i64*
  %32 = load i64, i64* %31, align 8
  %33 = lshr i64 %32, 1
  store i64 %33, i64* %13, align 8
  %34 = load i64, i64* %8, align 8
  %35 = and i64 %34, 63
  %36 = urem i64 %35, 63
  %37 = trunc i64 %36 to i32
  store i32 %37, i32* %14, align 4
  %38 = load i64, i64* %13, align 8
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %15, align 4
  %41 = load i64, i64* %13, align 8
  %42 = shl i64 %41, 1
  %43 = load i32, i32* %14, align 4
  %44 = sub i32 63, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %42, %45
  %47 = call i64 @llvm.ctpop.i64(i64 %46)
  %48 = trunc i64 %47 to i32
  store i32 %48, i32* %16, align 4
  %49 = load i64, i64* %13, align 8
  %50 = load i32, i32* %14, align 4
  %51 = zext i32 %50 to i64
  %52 = shl i64 1, %51
  %53 = and i64 %49, %52
  %54 = icmp ne i64 %53, 0
  %55 = zext i1 %54 to i8
  store i8 %55, i8* %17, align 1
  %56 = load i8, i8* %17, align 1
  %57 = trunc i8 %56 to i1
  br i1 %57, label %58, label %149

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.9*, %class.KV.9** %12, align 8
  %60 = load i32, i32* %16, align 4
  %61 = zext i32 %60 to i64
  %62 = getelementptr inbounds %class.KV.9, %class.KV.9* %59, i64 %61
  %63 = getelementptr inbounds %class.KV.9, %class.KV.9* %62, i32 0, i32 0
  %64 = bitcast %"union.KV<key, key, 10>::Key"* %63 to i64*
  %65 = load i64, i64* %64, align 8
  %66 = and i64 %65, 1
  %67 = icmp eq i64 %66, 0
  br i1 %67, label %68, label %130

; <label>:68:                                     ; preds = %58
  %69 = load %class.KV.9*, %class.KV.9** %12, align 8
  %70 = load i32, i32* %16, align 4
  %71 = zext i32 %70 to i64
  %72 = getelementptr inbounds %class.KV.9, %class.KV.9* %69, i64 %71
  %73 = getelementptr inbounds %class.KV.9, %class.KV.9* %72, i32 0, i32 0
  %74 = bitcast %"union.KV<key, key, 10>::Key"* %73 to %class.key**
  %75 = load %class.key*, %class.key** %74, align 8
  %76 = load %class.key*, %class.key** %9, align 8
  %77 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %75, %class.key* dereferenceable(8) %76)
  br i1 %77, label %78, label %90

; <label>:78:                                     ; preds = %68
  %79 = load %class.KV.9*, %class.KV.9** %12, align 8
  %80 = load i32, i32* %15, align 4
  %81 = load i32, i32* %16, align 4
  %82 = load %class.key*, %class.key** %9, align 8
  %83 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %19, %class.key* %82, %class.key* %83)
  %84 = call %class.KV.9* @_ZN2KVI3keyS0_Lj10EE11update_nodeEPKS1_jjRS2_(%class.KV.9* %79, i32 %80, i32 %81, %class.KV.9* dereferenceable(16) %19)
  store %class.KV.9* %84, %class.KV.9** %18, align 8
  %85 = load %class.KV.8*, %class.KV.8** %7, align 8
  %86 = getelementptr inbounds %class.KV.8, %class.KV.8* %85, i32 0, i32 0
  %87 = bitcast %"union.KV<key, key, 9>::Key"* %86 to i64*
  %88 = load i64, i64* %87, align 8
  %89 = load %class.KV.9*, %class.KV.9** %18, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %0, i64 %88, %class.KV.9* %89)
  br label %198

; <label>:90:                                     ; preds = %68
  %91 = load i64*, i64** %11, align 8
  %92 = load i64, i64* %91, align 8
  %93 = add i64 %92, 1
  store i64 %93, i64* %91, align 8
  %94 = load %class.KV.9*, %class.KV.9** %12, align 8
  %95 = load i32, i32* %16, align 4
  %96 = zext i32 %95 to i64
  %97 = getelementptr inbounds %class.KV.9, %class.KV.9* %94, i64 %96
  %98 = getelementptr inbounds %class.KV.9, %class.KV.9* %97, i32 0, i32 0
  %99 = bitcast %"union.KV<key, key, 10>::Key"* %98 to %class.key**
  %100 = load %class.key*, %class.key** %99, align 8
  %101 = call i64 @_ZNK3key4hashEv(%class.key* %100)
  %102 = lshr i64 %101, 0
  %103 = load %class.KV.9*, %class.KV.9** %12, align 8
  %104 = load i32, i32* %16, align 4
  %105 = zext i32 %104 to i64
  %106 = getelementptr inbounds %class.KV.9, %class.KV.9* %103, i64 %105
  %107 = getelementptr inbounds %class.KV.9, %class.KV.9* %106, i32 0, i32 0
  %108 = bitcast %"union.KV<key, key, 10>::Key"* %107 to %class.key**
  %109 = load %class.key*, %class.key** %108, align 8
  %110 = load %class.KV.9*, %class.KV.9** %12, align 8
  %111 = load i32, i32* %16, align 4
  %112 = zext i32 %111 to i64
  %113 = getelementptr inbounds %class.KV.9, %class.KV.9* %110, i64 %112
  %114 = getelementptr inbounds %class.KV.9, %class.KV.9* %113, i32 0, i32 1
  %115 = bitcast %"union.KV<key, key, 10>::Val"* %114 to %class.key**
  %116 = load %class.key*, %class.key** %115, align 8
  %117 = load i64, i64* %8, align 8
  %118 = lshr i64 %117, 6
  %119 = load %class.key*, %class.key** %9, align 8
  %120 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj10EE14new_inner_nodeEmPKS0_S3_mS3_S3_(%class.KV.9* sret %20, i64 %102, %class.key* %109, %class.key* %116, i64 %118, %class.key* %119, %class.key* %120)
  %121 = load %class.KV.9*, %class.KV.9** %12, align 8
  %122 = load i32, i32* %15, align 4
  %123 = load i32, i32* %16, align 4
  %124 = call %class.KV.9* @_ZN2KVI3keyS0_Lj10EE11update_nodeEPKS1_jjRS2_(%class.KV.9* %121, i32 %122, i32 %123, %class.KV.9* dereferenceable(16) %20)
  store %class.KV.9* %124, %class.KV.9** %21, align 8
  %125 = load %class.KV.8*, %class.KV.8** %7, align 8
  %126 = getelementptr inbounds %class.KV.8, %class.KV.8* %125, i32 0, i32 0
  %127 = bitcast %"union.KV<key, key, 9>::Key"* %126 to i64*
  %128 = load i64, i64* %127, align 8
  %129 = load %class.KV.9*, %class.KV.9** %21, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %0, i64 %128, %class.KV.9* %129)
  br label %198

; <label>:130:                                    ; preds = %58
  %131 = load %class.KV.9*, %class.KV.9** %12, align 8
  %132 = load i32, i32* %16, align 4
  %133 = zext i32 %132 to i64
  %134 = getelementptr inbounds %class.KV.9, %class.KV.9* %131, i64 %133
  %135 = load i64, i64* %8, align 8
  %136 = lshr i64 %135, 6
  %137 = load %class.key*, %class.key** %9, align 8
  %138 = load %class.key*, %class.key** %10, align 8
  %139 = load i64*, i64** %11, align 8
  call void @_ZN2KVI3keyS0_Lj10EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.9* sret %22, %class.KV.9* dereferenceable(16) %134, i64 %136, %class.key* %137, %class.key* %138, i64* %139)
  %140 = load %class.KV.9*, %class.KV.9** %12, align 8
  %141 = load i32, i32* %15, align 4
  %142 = load i32, i32* %16, align 4
  %143 = call %class.KV.9* @_ZN2KVI3keyS0_Lj10EE11update_nodeEPKS1_jjRS2_(%class.KV.9* %140, i32 %141, i32 %142, %class.KV.9* dereferenceable(16) %22)
  store %class.KV.9* %143, %class.KV.9** %23, align 8
  %144 = load %class.KV.8*, %class.KV.8** %7, align 8
  %145 = getelementptr inbounds %class.KV.8, %class.KV.8* %144, i32 0, i32 0
  %146 = bitcast %"union.KV<key, key, 9>::Key"* %145 to i64*
  %147 = load i64, i64* %146, align 8
  %148 = load %class.KV.9*, %class.KV.9** %23, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %0, i64 %147, %class.KV.9* %148)
  br label %198

; <label>:149:                                    ; preds = %6
  %150 = load i64*, i64** %11, align 8
  %151 = load i64, i64* %150, align 8
  %152 = add i64 %151, 1
  store i64 %152, i64* %150, align 8
  %153 = load i32, i32* %15, align 4
  %154 = add i32 %153, 1
  %155 = zext i32 %154 to i64
  %156 = mul i64 %155, 16
  %157 = call noalias i8* @malloc(i64 %156) #8
  %158 = bitcast i8* %157 to %class.KV.9*
  store %class.KV.9* %158, %class.KV.9** %24, align 8
  %159 = load %class.KV.9*, %class.KV.9** %24, align 8
  %160 = bitcast %class.KV.9* %159 to i8*
  %161 = load %class.KV.9*, %class.KV.9** %12, align 8
  %162 = bitcast %class.KV.9* %161 to i8*
  %163 = load i32, i32* %16, align 4
  %164 = zext i32 %163 to i64
  %165 = mul i64 %164, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %160, i8* %162, i64 %165, i32 8, i1 false)
  %166 = load %class.KV.9*, %class.KV.9** %24, align 8
  %167 = load i32, i32* %16, align 4
  %168 = add i32 %167, 1
  %169 = zext i32 %168 to i64
  %170 = getelementptr inbounds %class.KV.9, %class.KV.9* %166, i64 %169
  %171 = bitcast %class.KV.9* %170 to i8*
  %172 = load %class.KV.9*, %class.KV.9** %12, align 8
  %173 = load i32, i32* %16, align 4
  %174 = zext i32 %173 to i64
  %175 = getelementptr inbounds %class.KV.9, %class.KV.9* %172, i64 %174
  %176 = bitcast %class.KV.9* %175 to i8*
  %177 = load i32, i32* %15, align 4
  %178 = load i32, i32* %16, align 4
  %179 = sub i32 %177, %178
  %180 = zext i32 %179 to i64
  %181 = mul i64 %180, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %171, i8* %176, i64 %181, i32 8, i1 false)
  %182 = load %class.KV.9*, %class.KV.9** %24, align 8
  %183 = load i32, i32* %16, align 4
  %184 = zext i32 %183 to i64
  %185 = getelementptr inbounds %class.KV.9, %class.KV.9* %182, i64 %184
  %186 = bitcast %class.KV.9* %185 to i8*
  %187 = bitcast i8* %186 to %class.KV.9*
  %188 = load %class.key*, %class.key** %9, align 8
  %189 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %187, %class.key* %188, %class.key* %189)
  %190 = load i64, i64* %13, align 8
  %191 = load i32, i32* %14, align 4
  %192 = zext i32 %191 to i64
  %193 = shl i64 1, %192
  %194 = or i64 %190, %193
  %195 = shl i64 %194, 1
  %196 = or i64 %195, 1
  %197 = load %class.KV.9*, %class.KV.9** %24, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %0, i64 %196, %class.KV.9* %197)
  br label %198

; <label>:198:                                    ; preds = %149, %130, %90, %78
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.KV.9* @_ZN2KVI3keyS0_Lj10EE11update_nodeEPKS1_jjRS2_(%class.KV.9*, i32, i32, %class.KV.9* dereferenceable(16)) #2 comdat align 2 {
  %5 = alloca %class.KV.9*, align 8
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca %class.KV.9*, align 8
  %9 = alloca %class.KV.9*, align 8
  store %class.KV.9* %0, %class.KV.9** %5, align 8
  store i32 %1, i32* %6, align 4
  store i32 %2, i32* %7, align 4
  store %class.KV.9* %3, %class.KV.9** %8, align 8
  %10 = load i32, i32* %6, align 4
  %11 = zext i32 %10 to i64
  %12 = mul i64 %11, 16
  %13 = call noalias i8* @malloc(i64 %12) #8
  %14 = bitcast i8* %13 to %class.KV.9*
  store %class.KV.9* %14, %class.KV.9** %9, align 8
  %15 = load %class.KV.9*, %class.KV.9** %9, align 8
  %16 = bitcast %class.KV.9* %15 to i8*
  %17 = load %class.KV.9*, %class.KV.9** %5, align 8
  %18 = bitcast %class.KV.9* %17 to i8*
  %19 = load i32, i32* %6, align 4
  %20 = zext i32 %19 to i64
  %21 = mul i64 %20, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %16, i8* %18, i64 %21, i32 8, i1 false)
  %22 = load %class.KV.9*, %class.KV.9** %9, align 8
  %23 = load i32, i32* %7, align 4
  %24 = zext i32 %23 to i64
  %25 = getelementptr inbounds %class.KV.9, %class.KV.9* %22, i64 %24
  %26 = bitcast %class.KV.9* %25 to i8*
  %27 = bitcast i8* %26 to %class.KV.9*
  %28 = load %class.KV.9*, %class.KV.9** %8, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2ERKS1_(%class.KV.9* %27, %class.KV.9* dereferenceable(16) %28)
  %29 = load %class.KV.9*, %class.KV.9** %9, align 8
  ret %class.KV.9* %29
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj10EE12insert_innerERKS1_mPKS0_S5_Pm(%class.KV.9* noalias sret, %class.KV.9* dereferenceable(16), i64, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %7 = alloca %class.KV.9*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.key*, align 8
  %10 = alloca %class.key*, align 8
  %11 = alloca i64*, align 8
  %12 = alloca %class.LL*, align 8
  %13 = alloca %class.LL*, align 8
  %14 = alloca %class.LL*, align 8
  store %class.KV.9* %1, %class.KV.9** %7, align 8
  store i64 %2, i64* %8, align 8
  store %class.key* %3, %class.key** %9, align 8
  store %class.key* %4, %class.key** %10, align 8
  store i64* %5, i64** %11, align 8
  %15 = load %class.KV.9*, %class.KV.9** %7, align 8
  %16 = getelementptr inbounds %class.KV.9, %class.KV.9* %15, i32 0, i32 0
  %17 = bitcast %"union.KV<key, key, 10>::Key"* %16 to i64*
  %18 = load i64, i64* %17, align 8
  %19 = and i64 %18, 1
  %20 = icmp eq i64 %19, 0
  br i1 %20, label %21, label %58

; <label>:21:                                     ; preds = %6
  %22 = load %class.KV.9*, %class.KV.9** %7, align 8
  %23 = getelementptr inbounds %class.KV.9, %class.KV.9* %22, i32 0, i32 0
  %24 = bitcast %"union.KV<key, key, 10>::Key"* %23 to %class.key**
  %25 = load %class.key*, %class.key** %24, align 8
  %26 = load %class.key*, %class.key** %9, align 8
  %27 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %25, %class.key* dereferenceable(8) %26)
  br i1 %27, label %28, label %34

; <label>:28:                                     ; preds = %21
  %29 = load %class.KV.9*, %class.KV.9** %7, align 8
  %30 = getelementptr inbounds %class.KV.9, %class.KV.9* %29, i32 0, i32 0
  %31 = bitcast %"union.KV<key, key, 10>::Key"* %30 to %class.key**
  %32 = load %class.key*, %class.key** %31, align 8
  %33 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EPKS0_S3_(%class.KV.9* %0, %class.key* %32, %class.key* %33)
  br label %84

; <label>:34:                                     ; preds = %21
  %35 = load i64*, i64** %11, align 8
  %36 = load i64, i64* %35, align 8
  %37 = add i64 %36, 1
  store i64 %37, i64* %35, align 8
  %38 = call noalias i8* @malloc(i64 24) #8
  %39 = bitcast i8* %38 to %class.LL*
  %40 = bitcast %class.LL* %39 to i8*
  %41 = bitcast i8* %40 to %class.LL*
  %42 = load %class.key*, %class.key** %9, align 8
  %43 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL* %41, %class.key* %42, %class.key* %43, %class.LL* null)
  store %class.LL* %41, %class.LL** %12, align 8
  %44 = call noalias i8* @malloc(i64 24) #8
  %45 = bitcast i8* %44 to %class.LL*
  %46 = bitcast %class.LL* %45 to i8*
  %47 = bitcast i8* %46 to %class.LL*
  %48 = load %class.KV.9*, %class.KV.9** %7, align 8
  %49 = getelementptr inbounds %class.KV.9, %class.KV.9* %48, i32 0, i32 0
  %50 = bitcast %"union.KV<key, key, 10>::Key"* %49 to %class.key**
  %51 = load %class.key*, %class.key** %50, align 8
  %52 = load %class.KV.9*, %class.KV.9** %7, align 8
  %53 = getelementptr inbounds %class.KV.9, %class.KV.9* %52, i32 0, i32 1
  %54 = bitcast %"union.KV<key, key, 10>::Val"* %53 to %class.key**
  %55 = load %class.key*, %class.key** %54, align 8
  %56 = load %class.LL*, %class.LL** %12, align 8
  call void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL* %47, %class.key* %51, %class.key* %55, %class.LL* %56)
  store %class.LL* %47, %class.LL** %13, align 8
  %57 = load %class.LL*, %class.LL** %13, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EmPK2LLIS0_S0_E(%class.KV.9* %0, i64 1, %class.LL* %57)
  br label %84

; <label>:58:                                     ; preds = %6
  %59 = load %class.KV.9*, %class.KV.9** %7, align 8
  %60 = getelementptr inbounds %class.KV.9, %class.KV.9* %59, i32 0, i32 1
  %61 = bitcast %"union.KV<key, key, 10>::Val"* %60 to %class.LL**
  %62 = load %class.LL*, %class.LL** %61, align 8
  %63 = icmp ne %class.LL* %62, null
  br i1 %63, label %64, label %73

; <label>:64:                                     ; preds = %58
  %65 = load %class.KV.9*, %class.KV.9** %7, align 8
  %66 = getelementptr inbounds %class.KV.9, %class.KV.9* %65, i32 0, i32 1
  %67 = bitcast %"union.KV<key, key, 10>::Val"* %66 to %class.LL**
  %68 = load %class.LL*, %class.LL** %67, align 8
  %69 = load %class.key*, %class.key** %9, align 8
  %70 = load %class.key*, %class.key** %10, align 8
  %71 = load i64*, i64** %11, align 8
  %72 = call %class.LL* @_ZNK2LLI3keyS0_E6insertEPKS0_S3_Pm(%class.LL* %68, %class.key* %69, %class.key* %70, i64* %71)
  call void @_ZN2KVI3keyS0_Lj10EEC2EmPK2LLIS0_S0_E(%class.KV.9* %0, i64 1, %class.LL* %72)
  br label %84

; <label>:73:                                     ; preds = %58
  %74 = load i64*, i64** %11, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, 1
  store i64 %76, i64* %74, align 8
  %77 = call noalias i8* @malloc(i64 24) #8
  %78 = bitcast i8* %77 to %class.LL*
  %79 = bitcast %class.LL* %78 to i8*
  %80 = bitcast i8* %79 to %class.LL*
  %81 = load %class.key*, %class.key** %9, align 8
  %82 = load %class.key*, %class.key** %10, align 8
  call void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL* %80, %class.key* %81, %class.key* %82, %class.LL* null)
  store %class.LL* %80, %class.LL** %14, align 8
  %83 = load %class.LL*, %class.LL** %14, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EmPK2LLIS0_S0_E(%class.KV.9* %0, i64 1, %class.LL* %83)
  br label %84

; <label>:84:                                     ; preds = %73, %64, %34, %28
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.LL* @_ZNK2LLI3keyS0_E6insertEPKS0_S3_Pm(%class.LL*, %class.key*, %class.key*, i64*) #2 comdat align 2 {
  %5 = alloca %class.LL*, align 8
  %6 = alloca %class.LL*, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.LL*, align 8
  %11 = alloca %class.LL*, align 8
  store %class.LL* %0, %class.LL** %6, align 8
  store %class.key* %1, %class.key** %7, align 8
  store %class.key* %2, %class.key** %8, align 8
  store i64* %3, i64** %9, align 8
  %12 = load %class.LL*, %class.LL** %6, align 8
  %13 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 0
  %14 = load %class.key*, %class.key** %13, align 8
  %15 = load %class.key*, %class.key** %7, align 8
  %16 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %14, %class.key* dereferenceable(8) %15)
  br i1 %16, label %17, label %27

; <label>:17:                                     ; preds = %4
  %18 = call noalias i8* @malloc(i64 24) #8
  %19 = bitcast i8* %18 to %class.LL*
  %20 = bitcast %class.LL* %19 to i8*
  %21 = bitcast i8* %20 to %class.LL*
  %22 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 0
  %23 = load %class.key*, %class.key** %22, align 8
  %24 = load %class.key*, %class.key** %8, align 8
  %25 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 2
  %26 = load %class.LL*, %class.LL** %25, align 8
  call void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL* %21, %class.key* %23, %class.key* %24, %class.LL* %26)
  store %class.LL* %21, %class.LL** %5, align 8
  br label %66

; <label>:27:                                     ; preds = %4
  %28 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 2
  %29 = load %class.LL*, %class.LL** %28, align 8
  %30 = icmp ne %class.LL* %29, null
  br i1 %30, label %31, label %46

; <label>:31:                                     ; preds = %27
  %32 = call noalias i8* @malloc(i64 24) #8
  %33 = bitcast i8* %32 to %class.LL*
  %34 = bitcast %class.LL* %33 to i8*
  %35 = bitcast i8* %34 to %class.LL*
  %36 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 0
  %37 = load %class.key*, %class.key** %36, align 8
  %38 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 1
  %39 = load %class.key*, %class.key** %38, align 8
  %40 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 2
  %41 = load %class.LL*, %class.LL** %40, align 8
  %42 = load %class.key*, %class.key** %7, align 8
  %43 = load %class.key*, %class.key** %8, align 8
  %44 = load i64*, i64** %9, align 8
  %45 = call %class.LL* @_ZNK2LLI3keyS0_E6insertEPKS0_S3_Pm(%class.LL* %41, %class.key* %42, %class.key* %43, i64* %44)
  call void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL* %35, %class.key* %37, %class.key* %39, %class.LL* %45)
  store %class.LL* %35, %class.LL** %5, align 8
  br label %66

; <label>:46:                                     ; preds = %27
  %47 = load i64*, i64** %9, align 8
  %48 = load i64, i64* %47, align 8
  %49 = add i64 %48, 1
  store i64 %49, i64* %47, align 8
  %50 = call noalias i8* @malloc(i64 24) #8
  %51 = bitcast i8* %50 to %class.LL*
  %52 = bitcast %class.LL* %51 to i8*
  %53 = bitcast i8* %52 to %class.LL*
  %54 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 0
  %55 = load %class.key*, %class.key** %54, align 8
  %56 = getelementptr inbounds %class.LL, %class.LL* %12, i32 0, i32 1
  %57 = load %class.key*, %class.key** %56, align 8
  call void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL* %53, %class.key* %55, %class.key* %57, %class.LL* null)
  store %class.LL* %53, %class.LL** %10, align 8
  %58 = call noalias i8* @malloc(i64 24) #8
  %59 = bitcast i8* %58 to %class.LL*
  %60 = bitcast %class.LL* %59 to i8*
  %61 = bitcast i8* %60 to %class.LL*
  %62 = load %class.key*, %class.key** %7, align 8
  %63 = load %class.key*, %class.key** %8, align 8
  %64 = load %class.LL*, %class.LL** %10, align 8
  call void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL* %61, %class.key* %62, %class.key* %63, %class.LL* %64)
  store %class.LL* %61, %class.LL** %11, align 8
  %65 = load %class.LL*, %class.LL** %11, align 8
  store %class.LL* %65, %class.LL** %5, align 8
  br label %66

; <label>:66:                                     ; preds = %46, %31, %17
  %67 = load %class.LL*, %class.LL** %5, align 8
  ret %class.LL* %67
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj0EE10inner_findERKS1_mPKS0_(%class.KV* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.0*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  store %class.KV* %0, %class.KV** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV*, %class.KV** %5, align 8
  %17 = getelementptr inbounds %class.KV, %class.KV* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, key, 0>::Val"* %17 to %class.KV.0**
  %19 = load %class.KV.0*, %class.KV.0** %18, align 8
  store %class.KV.0* %19, %class.KV.0** %9, align 8
  %20 = load %class.KV*, %class.KV** %5, align 8
  %21 = getelementptr inbounds %class.KV, %class.KV* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 0>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %78

; <label>:33:                                     ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %12, align 4
  %41 = load %class.KV.0*, %class.KV.0** %9, align 8
  %42 = load i32, i32* %12, align 4
  %43 = zext i32 %42 to i64
  %44 = getelementptr inbounds %class.KV.0, %class.KV.0* %41, i64 %43
  %45 = getelementptr inbounds %class.KV.0, %class.KV.0* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 1>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %69

; <label>:50:                                     ; preds = %33
  %51 = load %class.KV.0*, %class.KV.0** %9, align 8
  %52 = load i32, i32* %12, align 4
  %53 = zext i32 %52 to i64
  %54 = getelementptr inbounds %class.KV.0, %class.KV.0* %51, i64 %53
  %55 = getelementptr inbounds %class.KV.0, %class.KV.0* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<key, key, 1>::Key"* %55 to %class.key**
  %57 = load %class.key*, %class.key** %56, align 8
  %58 = load %class.key*, %class.key** %7, align 8
  %59 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %57, %class.key* dereferenceable(8) %58)
  br i1 %59, label %60, label %68

; <label>:60:                                     ; preds = %50
  %61 = load %class.KV.0*, %class.KV.0** %9, align 8
  %62 = load i32, i32* %12, align 4
  %63 = zext i32 %62 to i64
  %64 = getelementptr inbounds %class.KV.0, %class.KV.0* %61, i64 %63
  %65 = getelementptr inbounds %class.KV.0, %class.KV.0* %64, i32 0, i32 1
  %66 = bitcast %"union.KV<key, key, 1>::Val"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  store %class.key* %67, %class.key** %4, align 8
  br label %79

; <label>:68:                                     ; preds = %50
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:69:                                     ; preds = %33
  %70 = load %class.KV.0*, %class.KV.0** %9, align 8
  %71 = load i32, i32* %12, align 4
  %72 = zext i32 %71 to i64
  %73 = getelementptr inbounds %class.KV.0, %class.KV.0* %70, i64 %72
  %74 = load i64, i64* %6, align 8
  %75 = lshr i64 %74, 6
  %76 = load %class.key*, %class.key** %7, align 8
  %77 = call %class.key* @_ZN2KVI3keyS0_Lj1EE10inner_findERKS1_mPKS0_(%class.KV.0* dereferenceable(16) %73, i64 %75, %class.key* %76)
  store %class.key* %77, %class.key** %4, align 8
  br label %79

; <label>:78:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:79:                                     ; preds = %78, %69, %68, %60
  %80 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %80
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj1EE10inner_findERKS1_mPKS0_(%class.KV.0* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV.0*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.1*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  store %class.KV.0* %0, %class.KV.0** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.0*, %class.KV.0** %5, align 8
  %17 = getelementptr inbounds %class.KV.0, %class.KV.0* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, key, 1>::Val"* %17 to %class.KV.1**
  %19 = load %class.KV.1*, %class.KV.1** %18, align 8
  store %class.KV.1* %19, %class.KV.1** %9, align 8
  %20 = load %class.KV.0*, %class.KV.0** %5, align 8
  %21 = getelementptr inbounds %class.KV.0, %class.KV.0* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 1>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %78

; <label>:33:                                     ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %12, align 4
  %41 = load %class.KV.1*, %class.KV.1** %9, align 8
  %42 = load i32, i32* %12, align 4
  %43 = zext i32 %42 to i64
  %44 = getelementptr inbounds %class.KV.1, %class.KV.1* %41, i64 %43
  %45 = getelementptr inbounds %class.KV.1, %class.KV.1* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 2>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %69

; <label>:50:                                     ; preds = %33
  %51 = load %class.KV.1*, %class.KV.1** %9, align 8
  %52 = load i32, i32* %12, align 4
  %53 = zext i32 %52 to i64
  %54 = getelementptr inbounds %class.KV.1, %class.KV.1* %51, i64 %53
  %55 = getelementptr inbounds %class.KV.1, %class.KV.1* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<key, key, 2>::Key"* %55 to %class.key**
  %57 = load %class.key*, %class.key** %56, align 8
  %58 = load %class.key*, %class.key** %7, align 8
  %59 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %57, %class.key* dereferenceable(8) %58)
  br i1 %59, label %60, label %68

; <label>:60:                                     ; preds = %50
  %61 = load %class.KV.1*, %class.KV.1** %9, align 8
  %62 = load i32, i32* %12, align 4
  %63 = zext i32 %62 to i64
  %64 = getelementptr inbounds %class.KV.1, %class.KV.1* %61, i64 %63
  %65 = getelementptr inbounds %class.KV.1, %class.KV.1* %64, i32 0, i32 1
  %66 = bitcast %"union.KV<key, key, 2>::Val"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  store %class.key* %67, %class.key** %4, align 8
  br label %79

; <label>:68:                                     ; preds = %50
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:69:                                     ; preds = %33
  %70 = load %class.KV.1*, %class.KV.1** %9, align 8
  %71 = load i32, i32* %12, align 4
  %72 = zext i32 %71 to i64
  %73 = getelementptr inbounds %class.KV.1, %class.KV.1* %70, i64 %72
  %74 = load i64, i64* %6, align 8
  %75 = lshr i64 %74, 6
  %76 = load %class.key*, %class.key** %7, align 8
  %77 = call %class.key* @_ZN2KVI3keyS0_Lj2EE10inner_findERKS1_mPKS0_(%class.KV.1* dereferenceable(16) %73, i64 %75, %class.key* %76)
  store %class.key* %77, %class.key** %4, align 8
  br label %79

; <label>:78:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:79:                                     ; preds = %78, %69, %68, %60
  %80 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %80
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj2EE10inner_findERKS1_mPKS0_(%class.KV.1* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV.1*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.2*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  store %class.KV.1* %0, %class.KV.1** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.1*, %class.KV.1** %5, align 8
  %17 = getelementptr inbounds %class.KV.1, %class.KV.1* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, key, 2>::Val"* %17 to %class.KV.2**
  %19 = load %class.KV.2*, %class.KV.2** %18, align 8
  store %class.KV.2* %19, %class.KV.2** %9, align 8
  %20 = load %class.KV.1*, %class.KV.1** %5, align 8
  %21 = getelementptr inbounds %class.KV.1, %class.KV.1* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 2>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %78

; <label>:33:                                     ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %12, align 4
  %41 = load %class.KV.2*, %class.KV.2** %9, align 8
  %42 = load i32, i32* %12, align 4
  %43 = zext i32 %42 to i64
  %44 = getelementptr inbounds %class.KV.2, %class.KV.2* %41, i64 %43
  %45 = getelementptr inbounds %class.KV.2, %class.KV.2* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 3>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %69

; <label>:50:                                     ; preds = %33
  %51 = load %class.KV.2*, %class.KV.2** %9, align 8
  %52 = load i32, i32* %12, align 4
  %53 = zext i32 %52 to i64
  %54 = getelementptr inbounds %class.KV.2, %class.KV.2* %51, i64 %53
  %55 = getelementptr inbounds %class.KV.2, %class.KV.2* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<key, key, 3>::Key"* %55 to %class.key**
  %57 = load %class.key*, %class.key** %56, align 8
  %58 = load %class.key*, %class.key** %7, align 8
  %59 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %57, %class.key* dereferenceable(8) %58)
  br i1 %59, label %60, label %68

; <label>:60:                                     ; preds = %50
  %61 = load %class.KV.2*, %class.KV.2** %9, align 8
  %62 = load i32, i32* %12, align 4
  %63 = zext i32 %62 to i64
  %64 = getelementptr inbounds %class.KV.2, %class.KV.2* %61, i64 %63
  %65 = getelementptr inbounds %class.KV.2, %class.KV.2* %64, i32 0, i32 1
  %66 = bitcast %"union.KV<key, key, 3>::Val"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  store %class.key* %67, %class.key** %4, align 8
  br label %79

; <label>:68:                                     ; preds = %50
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:69:                                     ; preds = %33
  %70 = load %class.KV.2*, %class.KV.2** %9, align 8
  %71 = load i32, i32* %12, align 4
  %72 = zext i32 %71 to i64
  %73 = getelementptr inbounds %class.KV.2, %class.KV.2* %70, i64 %72
  %74 = load i64, i64* %6, align 8
  %75 = lshr i64 %74, 6
  %76 = load %class.key*, %class.key** %7, align 8
  %77 = call %class.key* @_ZN2KVI3keyS0_Lj3EE10inner_findERKS1_mPKS0_(%class.KV.2* dereferenceable(16) %73, i64 %75, %class.key* %76)
  store %class.key* %77, %class.key** %4, align 8
  br label %79

; <label>:78:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:79:                                     ; preds = %78, %69, %68, %60
  %80 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %80
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj3EE10inner_findERKS1_mPKS0_(%class.KV.2* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV.2*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.3*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  store %class.KV.2* %0, %class.KV.2** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.2*, %class.KV.2** %5, align 8
  %17 = getelementptr inbounds %class.KV.2, %class.KV.2* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, key, 3>::Val"* %17 to %class.KV.3**
  %19 = load %class.KV.3*, %class.KV.3** %18, align 8
  store %class.KV.3* %19, %class.KV.3** %9, align 8
  %20 = load %class.KV.2*, %class.KV.2** %5, align 8
  %21 = getelementptr inbounds %class.KV.2, %class.KV.2* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 3>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %78

; <label>:33:                                     ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %12, align 4
  %41 = load %class.KV.3*, %class.KV.3** %9, align 8
  %42 = load i32, i32* %12, align 4
  %43 = zext i32 %42 to i64
  %44 = getelementptr inbounds %class.KV.3, %class.KV.3* %41, i64 %43
  %45 = getelementptr inbounds %class.KV.3, %class.KV.3* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 4>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %69

; <label>:50:                                     ; preds = %33
  %51 = load %class.KV.3*, %class.KV.3** %9, align 8
  %52 = load i32, i32* %12, align 4
  %53 = zext i32 %52 to i64
  %54 = getelementptr inbounds %class.KV.3, %class.KV.3* %51, i64 %53
  %55 = getelementptr inbounds %class.KV.3, %class.KV.3* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<key, key, 4>::Key"* %55 to %class.key**
  %57 = load %class.key*, %class.key** %56, align 8
  %58 = load %class.key*, %class.key** %7, align 8
  %59 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %57, %class.key* dereferenceable(8) %58)
  br i1 %59, label %60, label %68

; <label>:60:                                     ; preds = %50
  %61 = load %class.KV.3*, %class.KV.3** %9, align 8
  %62 = load i32, i32* %12, align 4
  %63 = zext i32 %62 to i64
  %64 = getelementptr inbounds %class.KV.3, %class.KV.3* %61, i64 %63
  %65 = getelementptr inbounds %class.KV.3, %class.KV.3* %64, i32 0, i32 1
  %66 = bitcast %"union.KV<key, key, 4>::Val"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  store %class.key* %67, %class.key** %4, align 8
  br label %79

; <label>:68:                                     ; preds = %50
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:69:                                     ; preds = %33
  %70 = load %class.KV.3*, %class.KV.3** %9, align 8
  %71 = load i32, i32* %12, align 4
  %72 = zext i32 %71 to i64
  %73 = getelementptr inbounds %class.KV.3, %class.KV.3* %70, i64 %72
  %74 = load i64, i64* %6, align 8
  %75 = lshr i64 %74, 6
  %76 = load %class.key*, %class.key** %7, align 8
  %77 = call %class.key* @_ZN2KVI3keyS0_Lj4EE10inner_findERKS1_mPKS0_(%class.KV.3* dereferenceable(16) %73, i64 %75, %class.key* %76)
  store %class.key* %77, %class.key** %4, align 8
  br label %79

; <label>:78:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:79:                                     ; preds = %78, %69, %68, %60
  %80 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %80
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj4EE10inner_findERKS1_mPKS0_(%class.KV.3* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV.3*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.4*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  store %class.KV.3* %0, %class.KV.3** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.3*, %class.KV.3** %5, align 8
  %17 = getelementptr inbounds %class.KV.3, %class.KV.3* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, key, 4>::Val"* %17 to %class.KV.4**
  %19 = load %class.KV.4*, %class.KV.4** %18, align 8
  store %class.KV.4* %19, %class.KV.4** %9, align 8
  %20 = load %class.KV.3*, %class.KV.3** %5, align 8
  %21 = getelementptr inbounds %class.KV.3, %class.KV.3* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 4>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %78

; <label>:33:                                     ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %12, align 4
  %41 = load %class.KV.4*, %class.KV.4** %9, align 8
  %42 = load i32, i32* %12, align 4
  %43 = zext i32 %42 to i64
  %44 = getelementptr inbounds %class.KV.4, %class.KV.4* %41, i64 %43
  %45 = getelementptr inbounds %class.KV.4, %class.KV.4* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 5>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %69

; <label>:50:                                     ; preds = %33
  %51 = load %class.KV.4*, %class.KV.4** %9, align 8
  %52 = load i32, i32* %12, align 4
  %53 = zext i32 %52 to i64
  %54 = getelementptr inbounds %class.KV.4, %class.KV.4* %51, i64 %53
  %55 = getelementptr inbounds %class.KV.4, %class.KV.4* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<key, key, 5>::Key"* %55 to %class.key**
  %57 = load %class.key*, %class.key** %56, align 8
  %58 = load %class.key*, %class.key** %7, align 8
  %59 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %57, %class.key* dereferenceable(8) %58)
  br i1 %59, label %60, label %68

; <label>:60:                                     ; preds = %50
  %61 = load %class.KV.4*, %class.KV.4** %9, align 8
  %62 = load i32, i32* %12, align 4
  %63 = zext i32 %62 to i64
  %64 = getelementptr inbounds %class.KV.4, %class.KV.4* %61, i64 %63
  %65 = getelementptr inbounds %class.KV.4, %class.KV.4* %64, i32 0, i32 1
  %66 = bitcast %"union.KV<key, key, 5>::Val"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  store %class.key* %67, %class.key** %4, align 8
  br label %79

; <label>:68:                                     ; preds = %50
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:69:                                     ; preds = %33
  %70 = load %class.KV.4*, %class.KV.4** %9, align 8
  %71 = load i32, i32* %12, align 4
  %72 = zext i32 %71 to i64
  %73 = getelementptr inbounds %class.KV.4, %class.KV.4* %70, i64 %72
  %74 = load i64, i64* %6, align 8
  %75 = lshr i64 %74, 6
  %76 = load %class.key*, %class.key** %7, align 8
  %77 = call %class.key* @_ZN2KVI3keyS0_Lj5EE10inner_findERKS1_mPKS0_(%class.KV.4* dereferenceable(16) %73, i64 %75, %class.key* %76)
  store %class.key* %77, %class.key** %4, align 8
  br label %79

; <label>:78:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:79:                                     ; preds = %78, %69, %68, %60
  %80 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %80
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj5EE10inner_findERKS1_mPKS0_(%class.KV.4* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV.4*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.5*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  store %class.KV.4* %0, %class.KV.4** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.4*, %class.KV.4** %5, align 8
  %17 = getelementptr inbounds %class.KV.4, %class.KV.4* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, key, 5>::Val"* %17 to %class.KV.5**
  %19 = load %class.KV.5*, %class.KV.5** %18, align 8
  store %class.KV.5* %19, %class.KV.5** %9, align 8
  %20 = load %class.KV.4*, %class.KV.4** %5, align 8
  %21 = getelementptr inbounds %class.KV.4, %class.KV.4* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 5>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %78

; <label>:33:                                     ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %12, align 4
  %41 = load %class.KV.5*, %class.KV.5** %9, align 8
  %42 = load i32, i32* %12, align 4
  %43 = zext i32 %42 to i64
  %44 = getelementptr inbounds %class.KV.5, %class.KV.5* %41, i64 %43
  %45 = getelementptr inbounds %class.KV.5, %class.KV.5* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 6>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %69

; <label>:50:                                     ; preds = %33
  %51 = load %class.KV.5*, %class.KV.5** %9, align 8
  %52 = load i32, i32* %12, align 4
  %53 = zext i32 %52 to i64
  %54 = getelementptr inbounds %class.KV.5, %class.KV.5* %51, i64 %53
  %55 = getelementptr inbounds %class.KV.5, %class.KV.5* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<key, key, 6>::Key"* %55 to %class.key**
  %57 = load %class.key*, %class.key** %56, align 8
  %58 = load %class.key*, %class.key** %7, align 8
  %59 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %57, %class.key* dereferenceable(8) %58)
  br i1 %59, label %60, label %68

; <label>:60:                                     ; preds = %50
  %61 = load %class.KV.5*, %class.KV.5** %9, align 8
  %62 = load i32, i32* %12, align 4
  %63 = zext i32 %62 to i64
  %64 = getelementptr inbounds %class.KV.5, %class.KV.5* %61, i64 %63
  %65 = getelementptr inbounds %class.KV.5, %class.KV.5* %64, i32 0, i32 1
  %66 = bitcast %"union.KV<key, key, 6>::Val"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  store %class.key* %67, %class.key** %4, align 8
  br label %79

; <label>:68:                                     ; preds = %50
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:69:                                     ; preds = %33
  %70 = load %class.KV.5*, %class.KV.5** %9, align 8
  %71 = load i32, i32* %12, align 4
  %72 = zext i32 %71 to i64
  %73 = getelementptr inbounds %class.KV.5, %class.KV.5* %70, i64 %72
  %74 = load i64, i64* %6, align 8
  %75 = lshr i64 %74, 6
  %76 = load %class.key*, %class.key** %7, align 8
  %77 = call %class.key* @_ZN2KVI3keyS0_Lj6EE10inner_findERKS1_mPKS0_(%class.KV.5* dereferenceable(16) %73, i64 %75, %class.key* %76)
  store %class.key* %77, %class.key** %4, align 8
  br label %79

; <label>:78:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:79:                                     ; preds = %78, %69, %68, %60
  %80 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %80
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj6EE10inner_findERKS1_mPKS0_(%class.KV.5* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV.5*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.6*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  store %class.KV.5* %0, %class.KV.5** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.5*, %class.KV.5** %5, align 8
  %17 = getelementptr inbounds %class.KV.5, %class.KV.5* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, key, 6>::Val"* %17 to %class.KV.6**
  %19 = load %class.KV.6*, %class.KV.6** %18, align 8
  store %class.KV.6* %19, %class.KV.6** %9, align 8
  %20 = load %class.KV.5*, %class.KV.5** %5, align 8
  %21 = getelementptr inbounds %class.KV.5, %class.KV.5* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 6>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %78

; <label>:33:                                     ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %12, align 4
  %41 = load %class.KV.6*, %class.KV.6** %9, align 8
  %42 = load i32, i32* %12, align 4
  %43 = zext i32 %42 to i64
  %44 = getelementptr inbounds %class.KV.6, %class.KV.6* %41, i64 %43
  %45 = getelementptr inbounds %class.KV.6, %class.KV.6* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 7>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %69

; <label>:50:                                     ; preds = %33
  %51 = load %class.KV.6*, %class.KV.6** %9, align 8
  %52 = load i32, i32* %12, align 4
  %53 = zext i32 %52 to i64
  %54 = getelementptr inbounds %class.KV.6, %class.KV.6* %51, i64 %53
  %55 = getelementptr inbounds %class.KV.6, %class.KV.6* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<key, key, 7>::Key"* %55 to %class.key**
  %57 = load %class.key*, %class.key** %56, align 8
  %58 = load %class.key*, %class.key** %7, align 8
  %59 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %57, %class.key* dereferenceable(8) %58)
  br i1 %59, label %60, label %68

; <label>:60:                                     ; preds = %50
  %61 = load %class.KV.6*, %class.KV.6** %9, align 8
  %62 = load i32, i32* %12, align 4
  %63 = zext i32 %62 to i64
  %64 = getelementptr inbounds %class.KV.6, %class.KV.6* %61, i64 %63
  %65 = getelementptr inbounds %class.KV.6, %class.KV.6* %64, i32 0, i32 1
  %66 = bitcast %"union.KV<key, key, 7>::Val"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  store %class.key* %67, %class.key** %4, align 8
  br label %79

; <label>:68:                                     ; preds = %50
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:69:                                     ; preds = %33
  %70 = load %class.KV.6*, %class.KV.6** %9, align 8
  %71 = load i32, i32* %12, align 4
  %72 = zext i32 %71 to i64
  %73 = getelementptr inbounds %class.KV.6, %class.KV.6* %70, i64 %72
  %74 = load i64, i64* %6, align 8
  %75 = lshr i64 %74, 6
  %76 = load %class.key*, %class.key** %7, align 8
  %77 = call %class.key* @_ZN2KVI3keyS0_Lj7EE10inner_findERKS1_mPKS0_(%class.KV.6* dereferenceable(16) %73, i64 %75, %class.key* %76)
  store %class.key* %77, %class.key** %4, align 8
  br label %79

; <label>:78:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:79:                                     ; preds = %78, %69, %68, %60
  %80 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %80
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj7EE10inner_findERKS1_mPKS0_(%class.KV.6* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV.6*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.7*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  store %class.KV.6* %0, %class.KV.6** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.6*, %class.KV.6** %5, align 8
  %17 = getelementptr inbounds %class.KV.6, %class.KV.6* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, key, 7>::Val"* %17 to %class.KV.7**
  %19 = load %class.KV.7*, %class.KV.7** %18, align 8
  store %class.KV.7* %19, %class.KV.7** %9, align 8
  %20 = load %class.KV.6*, %class.KV.6** %5, align 8
  %21 = getelementptr inbounds %class.KV.6, %class.KV.6* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 7>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %78

; <label>:33:                                     ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %12, align 4
  %41 = load %class.KV.7*, %class.KV.7** %9, align 8
  %42 = load i32, i32* %12, align 4
  %43 = zext i32 %42 to i64
  %44 = getelementptr inbounds %class.KV.7, %class.KV.7* %41, i64 %43
  %45 = getelementptr inbounds %class.KV.7, %class.KV.7* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 8>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %69

; <label>:50:                                     ; preds = %33
  %51 = load %class.KV.7*, %class.KV.7** %9, align 8
  %52 = load i32, i32* %12, align 4
  %53 = zext i32 %52 to i64
  %54 = getelementptr inbounds %class.KV.7, %class.KV.7* %51, i64 %53
  %55 = getelementptr inbounds %class.KV.7, %class.KV.7* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<key, key, 8>::Key"* %55 to %class.key**
  %57 = load %class.key*, %class.key** %56, align 8
  %58 = load %class.key*, %class.key** %7, align 8
  %59 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %57, %class.key* dereferenceable(8) %58)
  br i1 %59, label %60, label %68

; <label>:60:                                     ; preds = %50
  %61 = load %class.KV.7*, %class.KV.7** %9, align 8
  %62 = load i32, i32* %12, align 4
  %63 = zext i32 %62 to i64
  %64 = getelementptr inbounds %class.KV.7, %class.KV.7* %61, i64 %63
  %65 = getelementptr inbounds %class.KV.7, %class.KV.7* %64, i32 0, i32 1
  %66 = bitcast %"union.KV<key, key, 8>::Val"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  store %class.key* %67, %class.key** %4, align 8
  br label %79

; <label>:68:                                     ; preds = %50
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:69:                                     ; preds = %33
  %70 = load %class.KV.7*, %class.KV.7** %9, align 8
  %71 = load i32, i32* %12, align 4
  %72 = zext i32 %71 to i64
  %73 = getelementptr inbounds %class.KV.7, %class.KV.7* %70, i64 %72
  %74 = load i64, i64* %6, align 8
  %75 = lshr i64 %74, 6
  %76 = load %class.key*, %class.key** %7, align 8
  %77 = call %class.key* @_ZN2KVI3keyS0_Lj8EE10inner_findERKS1_mPKS0_(%class.KV.7* dereferenceable(16) %73, i64 %75, %class.key* %76)
  store %class.key* %77, %class.key** %4, align 8
  br label %79

; <label>:78:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:79:                                     ; preds = %78, %69, %68, %60
  %80 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %80
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj8EE10inner_findERKS1_mPKS0_(%class.KV.7* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV.7*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.8*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  store %class.KV.7* %0, %class.KV.7** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.7*, %class.KV.7** %5, align 8
  %17 = getelementptr inbounds %class.KV.7, %class.KV.7* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, key, 8>::Val"* %17 to %class.KV.8**
  %19 = load %class.KV.8*, %class.KV.8** %18, align 8
  store %class.KV.8* %19, %class.KV.8** %9, align 8
  %20 = load %class.KV.7*, %class.KV.7** %5, align 8
  %21 = getelementptr inbounds %class.KV.7, %class.KV.7* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 8>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %78

; <label>:33:                                     ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %12, align 4
  %41 = load %class.KV.8*, %class.KV.8** %9, align 8
  %42 = load i32, i32* %12, align 4
  %43 = zext i32 %42 to i64
  %44 = getelementptr inbounds %class.KV.8, %class.KV.8* %41, i64 %43
  %45 = getelementptr inbounds %class.KV.8, %class.KV.8* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 9>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %69

; <label>:50:                                     ; preds = %33
  %51 = load %class.KV.8*, %class.KV.8** %9, align 8
  %52 = load i32, i32* %12, align 4
  %53 = zext i32 %52 to i64
  %54 = getelementptr inbounds %class.KV.8, %class.KV.8* %51, i64 %53
  %55 = getelementptr inbounds %class.KV.8, %class.KV.8* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<key, key, 9>::Key"* %55 to %class.key**
  %57 = load %class.key*, %class.key** %56, align 8
  %58 = load %class.key*, %class.key** %7, align 8
  %59 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %57, %class.key* dereferenceable(8) %58)
  br i1 %59, label %60, label %68

; <label>:60:                                     ; preds = %50
  %61 = load %class.KV.8*, %class.KV.8** %9, align 8
  %62 = load i32, i32* %12, align 4
  %63 = zext i32 %62 to i64
  %64 = getelementptr inbounds %class.KV.8, %class.KV.8* %61, i64 %63
  %65 = getelementptr inbounds %class.KV.8, %class.KV.8* %64, i32 0, i32 1
  %66 = bitcast %"union.KV<key, key, 9>::Val"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  store %class.key* %67, %class.key** %4, align 8
  br label %79

; <label>:68:                                     ; preds = %50
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:69:                                     ; preds = %33
  %70 = load %class.KV.8*, %class.KV.8** %9, align 8
  %71 = load i32, i32* %12, align 4
  %72 = zext i32 %71 to i64
  %73 = getelementptr inbounds %class.KV.8, %class.KV.8* %70, i64 %72
  %74 = load i64, i64* %6, align 8
  %75 = lshr i64 %74, 6
  %76 = load %class.key*, %class.key** %7, align 8
  %77 = call %class.key* @_ZN2KVI3keyS0_Lj9EE10inner_findERKS1_mPKS0_(%class.KV.8* dereferenceable(16) %73, i64 %75, %class.key* %76)
  store %class.key* %77, %class.key** %4, align 8
  br label %79

; <label>:78:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:79:                                     ; preds = %78, %69, %68, %60
  %80 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %80
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj9EE10inner_findERKS1_mPKS0_(%class.KV.8* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV.8*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  %8 = alloca i64, align 8
  %9 = alloca %class.KV.9*, align 8
  %10 = alloca i64, align 8
  %11 = alloca i8, align 1
  %12 = alloca i32, align 4
  store %class.KV.8* %0, %class.KV.8** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %13 = load i64, i64* %6, align 8
  %14 = and i64 %13, 63
  %15 = urem i64 %14, 63
  store i64 %15, i64* %8, align 8
  %16 = load %class.KV.8*, %class.KV.8** %5, align 8
  %17 = getelementptr inbounds %class.KV.8, %class.KV.8* %16, i32 0, i32 1
  %18 = bitcast %"union.KV<key, key, 9>::Val"* %17 to %class.KV.9**
  %19 = load %class.KV.9*, %class.KV.9** %18, align 8
  store %class.KV.9* %19, %class.KV.9** %9, align 8
  %20 = load %class.KV.8*, %class.KV.8** %5, align 8
  %21 = getelementptr inbounds %class.KV.8, %class.KV.8* %20, i32 0, i32 0
  %22 = bitcast %"union.KV<key, key, 9>::Key"* %21 to i64*
  %23 = load i64, i64* %22, align 8
  %24 = lshr i64 %23, 1
  store i64 %24, i64* %10, align 8
  %25 = load i64, i64* %10, align 8
  %26 = load i64, i64* %8, align 8
  %27 = shl i64 1, %26
  %28 = and i64 %25, %27
  %29 = icmp ne i64 %28, 0
  %30 = zext i1 %29 to i8
  store i8 %30, i8* %11, align 1
  %31 = load i8, i8* %11, align 1
  %32 = trunc i8 %31 to i1
  br i1 %32, label %33, label %78

; <label>:33:                                     ; preds = %3
  %34 = load i64, i64* %10, align 8
  %35 = shl i64 %34, 1
  %36 = load i64, i64* %8, align 8
  %37 = sub i64 63, %36
  %38 = shl i64 %35, %37
  %39 = call i64 @llvm.ctpop.i64(i64 %38)
  %40 = trunc i64 %39 to i32
  store i32 %40, i32* %12, align 4
  %41 = load %class.KV.9*, %class.KV.9** %9, align 8
  %42 = load i32, i32* %12, align 4
  %43 = zext i32 %42 to i64
  %44 = getelementptr inbounds %class.KV.9, %class.KV.9* %41, i64 %43
  %45 = getelementptr inbounds %class.KV.9, %class.KV.9* %44, i32 0, i32 0
  %46 = bitcast %"union.KV<key, key, 10>::Key"* %45 to i64*
  %47 = load i64, i64* %46, align 8
  %48 = and i64 %47, 1
  %49 = icmp eq i64 %48, 0
  br i1 %49, label %50, label %69

; <label>:50:                                     ; preds = %33
  %51 = load %class.KV.9*, %class.KV.9** %9, align 8
  %52 = load i32, i32* %12, align 4
  %53 = zext i32 %52 to i64
  %54 = getelementptr inbounds %class.KV.9, %class.KV.9* %51, i64 %53
  %55 = getelementptr inbounds %class.KV.9, %class.KV.9* %54, i32 0, i32 0
  %56 = bitcast %"union.KV<key, key, 10>::Key"* %55 to %class.key**
  %57 = load %class.key*, %class.key** %56, align 8
  %58 = load %class.key*, %class.key** %7, align 8
  %59 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %57, %class.key* dereferenceable(8) %58)
  br i1 %59, label %60, label %68

; <label>:60:                                     ; preds = %50
  %61 = load %class.KV.9*, %class.KV.9** %9, align 8
  %62 = load i32, i32* %12, align 4
  %63 = zext i32 %62 to i64
  %64 = getelementptr inbounds %class.KV.9, %class.KV.9* %61, i64 %63
  %65 = getelementptr inbounds %class.KV.9, %class.KV.9* %64, i32 0, i32 1
  %66 = bitcast %"union.KV<key, key, 10>::Val"* %65 to %class.key**
  %67 = load %class.key*, %class.key** %66, align 8
  store %class.key* %67, %class.key** %4, align 8
  br label %79

; <label>:68:                                     ; preds = %50
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:69:                                     ; preds = %33
  %70 = load %class.KV.9*, %class.KV.9** %9, align 8
  %71 = load i32, i32* %12, align 4
  %72 = zext i32 %71 to i64
  %73 = getelementptr inbounds %class.KV.9, %class.KV.9* %70, i64 %72
  %74 = load i64, i64* %6, align 8
  %75 = lshr i64 %74, 6
  %76 = load %class.key*, %class.key** %7, align 8
  %77 = call %class.key* @_ZN2KVI3keyS0_Lj10EE10inner_findERKS1_mPKS0_(%class.KV.9* dereferenceable(16) %73, i64 %75, %class.key* %76)
  store %class.key* %77, %class.key** %4, align 8
  br label %79

; <label>:78:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %79

; <label>:79:                                     ; preds = %78, %69, %68, %60
  %80 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %80
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZN2KVI3keyS0_Lj10EE10inner_findERKS1_mPKS0_(%class.KV.9* dereferenceable(16), i64, %class.key*) #2 comdat align 2 {
  %4 = alloca %class.key*, align 8
  %5 = alloca %class.KV.9*, align 8
  %6 = alloca i64, align 8
  %7 = alloca %class.key*, align 8
  store %class.KV.9* %0, %class.KV.9** %5, align 8
  store i64 %1, i64* %6, align 8
  store %class.key* %2, %class.key** %7, align 8
  %8 = load %class.KV.9*, %class.KV.9** %5, align 8
  %9 = getelementptr inbounds %class.KV.9, %class.KV.9* %8, i32 0, i32 1
  %10 = bitcast %"union.KV<key, key, 10>::Val"* %9 to %class.LL**
  %11 = load %class.LL*, %class.LL** %10, align 8
  %12 = icmp ne %class.LL* %11, null
  br i1 %12, label %13, label %20

; <label>:13:                                     ; preds = %3
  %14 = load %class.KV.9*, %class.KV.9** %5, align 8
  %15 = getelementptr inbounds %class.KV.9, %class.KV.9* %14, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 10>::Val"* %15 to %class.LL**
  %17 = load %class.LL*, %class.LL** %16, align 8
  %18 = load %class.key*, %class.key** %7, align 8
  %19 = call %class.key* @_ZNK2LLI3keyS0_E4findEPKS0_(%class.LL* %17, %class.key* %18)
  store %class.key* %19, %class.key** %4, align 8
  br label %21

; <label>:20:                                     ; preds = %3
  store %class.key* null, %class.key** %4, align 8
  br label %21

; <label>:21:                                     ; preds = %20, %13
  %22 = load %class.key*, %class.key** %4, align 8
  ret %class.key* %22
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.key* @_ZNK2LLI3keyS0_E4findEPKS0_(%class.LL*, %class.key*) #2 comdat align 2 {
  %3 = alloca %class.key*, align 8
  %4 = alloca %class.LL*, align 8
  %5 = alloca %class.key*, align 8
  store %class.LL* %0, %class.LL** %4, align 8
  store %class.key* %1, %class.key** %5, align 8
  %6 = load %class.LL*, %class.LL** %4, align 8
  %7 = getelementptr inbounds %class.LL, %class.LL* %6, i32 0, i32 0
  %8 = load %class.key*, %class.key** %7, align 8
  %9 = load %class.key*, %class.key** %5, align 8
  %10 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %8, %class.key* dereferenceable(8) %9)
  br i1 %10, label %11, label %14

; <label>:11:                                     ; preds = %2
  %12 = getelementptr inbounds %class.LL, %class.LL* %6, i32 0, i32 1
  %13 = load %class.key*, %class.key** %12, align 8
  store %class.key* %13, %class.key** %3, align 8
  br label %24

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.LL, %class.LL* %6, i32 0, i32 2
  %16 = load %class.LL*, %class.LL** %15, align 8
  %17 = icmp ne %class.LL* %16, null
  br i1 %17, label %18, label %23

; <label>:18:                                     ; preds = %14
  %19 = getelementptr inbounds %class.LL, %class.LL* %6, i32 0, i32 2
  %20 = load %class.LL*, %class.LL** %19, align 8
  %21 = load %class.key*, %class.key** %5, align 8
  %22 = call %class.key* @_ZNK2LLI3keyS0_E4findEPKS0_(%class.LL* %20, %class.key* %21)
  store %class.key* %22, %class.key** %3, align 8
  br label %24

; <label>:23:                                     ; preds = %14
  store %class.key* null, %class.key** %3, align 8
  br label %24

; <label>:24:                                     ; preds = %23, %18, %11
  %25 = load %class.key*, %class.key** %3, align 8
  ret %class.key* %25
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EE12remove_innerERKS1_mPKS0_Pm(%class.KV* noalias sret, %class.KV* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.0*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i8, align 1
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.0*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.0, align 8
  %19 = alloca %class.KV.0*, align 8
  store %class.KV* %1, %class.KV** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV*, %class.KV** %6, align 8
  %21 = getelementptr inbounds %class.KV, %class.KV* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, key, 0>::Val"* %21 to %class.KV.0**
  %23 = load %class.KV.0*, %class.KV.0** %22, align 8
  store %class.KV.0* %23, %class.KV.0** %10, align 8
  %24 = load %class.KV*, %class.KV** %6, align 8
  %25 = getelementptr inbounds %class.KV, %class.KV* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, key, 0>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %12, align 4
  %33 = load i64, i64* %11, align 8
  %34 = call i64 @llvm.ctpop.i64(i64 %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %13, align 4
  %36 = load i64, i64* %11, align 8
  %37 = load i32, i32* %12, align 4
  %38 = zext i32 %37 to i64
  %39 = shl i64 1, %38
  %40 = and i64 %36, %39
  %41 = icmp ne i64 %40, 0
  %42 = zext i1 %41 to i8
  store i8 %42, i8* %14, align 1
  %43 = load i8, i8* %14, align 1
  %44 = trunc i8 %43 to i1
  br i1 %44, label %45, label %145

; <label>:45:                                     ; preds = %5
  %46 = load i64, i64* %11, align 8
  %47 = shl i64 %46, 1
  %48 = load i32, i32* %12, align 4
  %49 = sub i32 63, %48
  %50 = zext i32 %49 to i64
  %51 = shl i64 %47, %50
  %52 = call i64 @llvm.ctpop.i64(i64 %51)
  %53 = trunc i64 %52 to i32
  store i32 %53, i32* %15, align 4
  %54 = load %class.KV.0*, %class.KV.0** %10, align 8
  %55 = load i32, i32* %15, align 4
  %56 = zext i32 %55 to i64
  %57 = getelementptr inbounds %class.KV.0, %class.KV.0* %54, i64 %56
  %58 = getelementptr inbounds %class.KV.0, %class.KV.0* %57, i32 0, i32 0
  %59 = bitcast %"union.KV<key, key, 1>::Key"* %58 to i64*
  %60 = load i64, i64* %59, align 8
  %61 = and i64 %60, 1
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %63, label %119

; <label>:63:                                     ; preds = %45
  %64 = load %class.KV.0*, %class.KV.0** %10, align 8
  %65 = load i32, i32* %15, align 4
  %66 = zext i32 %65 to i64
  %67 = getelementptr inbounds %class.KV.0, %class.KV.0* %64, i64 %66
  %68 = getelementptr inbounds %class.KV.0, %class.KV.0* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<key, key, 1>::Key"* %68 to %class.key**
  %70 = load %class.key*, %class.key** %69, align 8
  %71 = load %class.key*, %class.key** %8, align 8
  %72 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %70, %class.key* dereferenceable(8) %71)
  br i1 %72, label %73, label %117

; <label>:73:                                     ; preds = %63
  %74 = load i64*, i64** %9, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, -1
  store i64 %76, i64* %74, align 8
  %77 = load i32, i32* %13, align 4
  %78 = sub i32 %77, 1
  %79 = zext i32 %78 to i64
  %80 = mul i64 %79, 16
  %81 = call noalias i8* @malloc(i64 %80) #8
  %82 = bitcast i8* %81 to %class.KV.0*
  store %class.KV.0* %82, %class.KV.0** %16, align 8
  %83 = load %class.KV.0*, %class.KV.0** %16, align 8
  %84 = bitcast %class.KV.0* %83 to i8*
  %85 = load %class.KV.0*, %class.KV.0** %10, align 8
  %86 = bitcast %class.KV.0* %85 to i8*
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = mul i64 %88, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %84, i8* %86, i64 %89, i32 8, i1 false)
  %90 = load %class.KV.0*, %class.KV.0** %16, align 8
  %91 = load i32, i32* %15, align 4
  %92 = zext i32 %91 to i64
  %93 = getelementptr inbounds %class.KV.0, %class.KV.0* %90, i64 %92
  %94 = bitcast %class.KV.0* %93 to i8*
  %95 = load %class.KV.0*, %class.KV.0** %10, align 8
  %96 = load i32, i32* %15, align 4
  %97 = add i32 %96, 1
  %98 = zext i32 %97 to i64
  %99 = getelementptr inbounds %class.KV.0, %class.KV.0* %95, i64 %98
  %100 = bitcast %class.KV.0* %99 to i8*
  %101 = load i32, i32* %13, align 4
  %102 = sub i32 %101, 1
  %103 = load i32, i32* %15, align 4
  %104 = sub i32 %102, %103
  %105 = zext i32 %104 to i64
  %106 = mul i64 %105, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %94, i8* %100, i64 %106, i32 8, i1 false)
  %107 = load i64, i64* %11, align 8
  %108 = load i32, i32* %12, align 4
  %109 = zext i32 %108 to i64
  %110 = shl i64 1, %109
  %111 = xor i64 -1, %110
  %112 = and i64 %107, %111
  %113 = shl i64 %112, 1
  %114 = or i64 %113, 1
  store i64 %114, i64* %17, align 8
  %115 = load i64, i64* %17, align 8
  %116 = load %class.KV.0*, %class.KV.0** %16, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %0, i64 %115, %class.KV.0* %116)
  br label %147

; <label>:117:                                    ; preds = %63
  %118 = load %class.KV*, %class.KV** %6, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2ERKS1_(%class.KV* %0, %class.KV* dereferenceable(16) %118)
  br label %147

; <label>:119:                                    ; preds = %45
  %120 = load %class.KV.0*, %class.KV.0** %10, align 8
  %121 = load i32, i32* %15, align 4
  %122 = zext i32 %121 to i64
  %123 = getelementptr inbounds %class.KV.0, %class.KV.0* %120, i64 %122
  %124 = load i64, i64* %7, align 8
  %125 = lshr i64 %124, 6
  %126 = load %class.key*, %class.key** %8, align 8
  %127 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3keyS0_Lj1EE12remove_innerERKS1_mPKS0_Pm(%class.KV.0* sret %18, %class.KV.0* dereferenceable(16) %123, i64 %125, %class.key* %126, i64* %127)
  %128 = load %class.KV.0*, %class.KV.0** %10, align 8
  %129 = load i32, i32* %15, align 4
  %130 = zext i32 %129 to i64
  %131 = getelementptr inbounds %class.KV.0, %class.KV.0* %128, i64 %130
  %132 = call zeroext i1 @_ZNK2KVI3keyS0_Lj1EEeqERKS1_(%class.KV.0* %18, %class.KV.0* dereferenceable(16) %131)
  br i1 %132, label %133, label %135

; <label>:133:                                    ; preds = %119
  %134 = load %class.KV*, %class.KV** %6, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2ERKS1_(%class.KV* %0, %class.KV* dereferenceable(16) %134)
  br label %147

; <label>:135:                                    ; preds = %119
  %136 = load %class.KV.0*, %class.KV.0** %10, align 8
  %137 = load i32, i32* %13, align 4
  %138 = load i32, i32* %15, align 4
  %139 = call %class.KV.0* @_ZN2KVI3keyS0_Lj1EE11update_nodeEPKS1_jjRS2_(%class.KV.0* %136, i32 %137, i32 %138, %class.KV.0* dereferenceable(16) %18)
  store %class.KV.0* %139, %class.KV.0** %19, align 8
  %140 = load %class.KV*, %class.KV** %6, align 8
  %141 = getelementptr inbounds %class.KV, %class.KV* %140, i32 0, i32 0
  %142 = bitcast %"union.KV<key, key, 0>::Key"* %141 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = load %class.KV.0*, %class.KV.0** %19, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2EmPKS_IS0_S0_Lj1EE(%class.KV* %0, i64 %143, %class.KV.0* %144)
  br label %147

; <label>:145:                                    ; preds = %5
  %146 = load %class.KV*, %class.KV** %6, align 8
  call void @_ZN2KVI3keyS0_Lj0EEC2ERKS1_(%class.KV* %0, %class.KV* dereferenceable(16) %146)
  br label %147

; <label>:147:                                    ; preds = %145, %135, %133, %117, %73
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj0EEeqERKS1_(%class.KV*, %class.KV* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV*, align 8
  %4 = alloca %class.KV*, align 8
  store %class.KV* %0, %class.KV** %3, align 8
  store %class.KV* %1, %class.KV** %4, align 8
  %5 = load %class.KV*, %class.KV** %3, align 8
  %6 = getelementptr inbounds %class.KV, %class.KV* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 0>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV*, %class.KV** %4, align 8
  %10 = getelementptr inbounds %class.KV, %class.KV* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 0>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV, %class.KV* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 0>::Val"* %15 to %class.KV.0**
  %17 = load %class.KV.0*, %class.KV.0** %16, align 8
  %18 = load %class.KV*, %class.KV** %4, align 8
  %19 = getelementptr inbounds %class.KV, %class.KV* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 0>::Val"* %19 to %class.KV.0**
  %21 = load %class.KV.0*, %class.KV.0** %20, align 8
  %22 = icmp eq %class.KV.0* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj0EEC2ERKS1_(%class.KV*, %class.KV* dereferenceable(16)) unnamed_addr #0 comdat align 2 {
  %3 = alloca %class.KV*, align 8
  %4 = alloca %class.KV*, align 8
  store %class.KV* %0, %class.KV** %3, align 8
  store %class.KV* %1, %class.KV** %4, align 8
  %5 = load %class.KV*, %class.KV** %3, align 8
  %6 = getelementptr inbounds %class.KV, %class.KV* %5, i32 0, i32 0
  %7 = load %class.KV*, %class.KV** %4, align 8
  %8 = getelementptr inbounds %class.KV, %class.KV* %7, i32 0, i32 0
  %9 = bitcast %"union.KV<key, key, 0>::Key"* %6 to i8*
  %10 = bitcast %"union.KV<key, key, 0>::Key"* %8 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %9, i8* %10, i64 8, i32 8, i1 false)
  %11 = getelementptr inbounds %class.KV, %class.KV* %5, i32 0, i32 1
  %12 = load %class.KV*, %class.KV** %4, align 8
  %13 = getelementptr inbounds %class.KV, %class.KV* %12, i32 0, i32 1
  %14 = bitcast %"union.KV<key, key, 0>::Val"* %11 to i8*
  %15 = bitcast %"union.KV<key, key, 0>::Val"* %13 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %14, i8* %15, i64 8, i32 8, i1 false)
  ret void
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj1EE12remove_innerERKS1_mPKS0_Pm(%class.KV.0* noalias sret, %class.KV.0* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV.0*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.1*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i8, align 1
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.1*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.1, align 8
  %19 = alloca %class.KV.1*, align 8
  store %class.KV.0* %1, %class.KV.0** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.0*, %class.KV.0** %6, align 8
  %21 = getelementptr inbounds %class.KV.0, %class.KV.0* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, key, 1>::Val"* %21 to %class.KV.1**
  %23 = load %class.KV.1*, %class.KV.1** %22, align 8
  store %class.KV.1* %23, %class.KV.1** %10, align 8
  %24 = load %class.KV.0*, %class.KV.0** %6, align 8
  %25 = getelementptr inbounds %class.KV.0, %class.KV.0* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, key, 1>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %12, align 4
  %33 = load i64, i64* %11, align 8
  %34 = call i64 @llvm.ctpop.i64(i64 %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %13, align 4
  %36 = load i64, i64* %11, align 8
  %37 = load i32, i32* %12, align 4
  %38 = zext i32 %37 to i64
  %39 = shl i64 1, %38
  %40 = and i64 %36, %39
  %41 = icmp ne i64 %40, 0
  %42 = zext i1 %41 to i8
  store i8 %42, i8* %14, align 1
  %43 = load i8, i8* %14, align 1
  %44 = trunc i8 %43 to i1
  br i1 %44, label %45, label %145

; <label>:45:                                     ; preds = %5
  %46 = load i64, i64* %11, align 8
  %47 = shl i64 %46, 1
  %48 = load i32, i32* %12, align 4
  %49 = sub i32 63, %48
  %50 = zext i32 %49 to i64
  %51 = shl i64 %47, %50
  %52 = call i64 @llvm.ctpop.i64(i64 %51)
  %53 = trunc i64 %52 to i32
  store i32 %53, i32* %15, align 4
  %54 = load %class.KV.1*, %class.KV.1** %10, align 8
  %55 = load i32, i32* %15, align 4
  %56 = zext i32 %55 to i64
  %57 = getelementptr inbounds %class.KV.1, %class.KV.1* %54, i64 %56
  %58 = getelementptr inbounds %class.KV.1, %class.KV.1* %57, i32 0, i32 0
  %59 = bitcast %"union.KV<key, key, 2>::Key"* %58 to i64*
  %60 = load i64, i64* %59, align 8
  %61 = and i64 %60, 1
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %63, label %119

; <label>:63:                                     ; preds = %45
  %64 = load %class.KV.1*, %class.KV.1** %10, align 8
  %65 = load i32, i32* %15, align 4
  %66 = zext i32 %65 to i64
  %67 = getelementptr inbounds %class.KV.1, %class.KV.1* %64, i64 %66
  %68 = getelementptr inbounds %class.KV.1, %class.KV.1* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<key, key, 2>::Key"* %68 to %class.key**
  %70 = load %class.key*, %class.key** %69, align 8
  %71 = load %class.key*, %class.key** %8, align 8
  %72 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %70, %class.key* dereferenceable(8) %71)
  br i1 %72, label %73, label %117

; <label>:73:                                     ; preds = %63
  %74 = load i64*, i64** %9, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, -1
  store i64 %76, i64* %74, align 8
  %77 = load i32, i32* %13, align 4
  %78 = sub i32 %77, 1
  %79 = zext i32 %78 to i64
  %80 = mul i64 %79, 16
  %81 = call noalias i8* @malloc(i64 %80) #8
  %82 = bitcast i8* %81 to %class.KV.1*
  store %class.KV.1* %82, %class.KV.1** %16, align 8
  %83 = load %class.KV.1*, %class.KV.1** %16, align 8
  %84 = bitcast %class.KV.1* %83 to i8*
  %85 = load %class.KV.1*, %class.KV.1** %10, align 8
  %86 = bitcast %class.KV.1* %85 to i8*
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = mul i64 %88, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %84, i8* %86, i64 %89, i32 8, i1 false)
  %90 = load %class.KV.1*, %class.KV.1** %16, align 8
  %91 = load i32, i32* %15, align 4
  %92 = zext i32 %91 to i64
  %93 = getelementptr inbounds %class.KV.1, %class.KV.1* %90, i64 %92
  %94 = bitcast %class.KV.1* %93 to i8*
  %95 = load %class.KV.1*, %class.KV.1** %10, align 8
  %96 = load i32, i32* %15, align 4
  %97 = add i32 %96, 1
  %98 = zext i32 %97 to i64
  %99 = getelementptr inbounds %class.KV.1, %class.KV.1* %95, i64 %98
  %100 = bitcast %class.KV.1* %99 to i8*
  %101 = load i32, i32* %13, align 4
  %102 = sub i32 %101, 1
  %103 = load i32, i32* %15, align 4
  %104 = sub i32 %102, %103
  %105 = zext i32 %104 to i64
  %106 = mul i64 %105, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %94, i8* %100, i64 %106, i32 8, i1 false)
  %107 = load i64, i64* %11, align 8
  %108 = load i32, i32* %12, align 4
  %109 = zext i32 %108 to i64
  %110 = shl i64 1, %109
  %111 = xor i64 -1, %110
  %112 = and i64 %107, %111
  %113 = shl i64 %112, 1
  %114 = or i64 %113, 1
  store i64 %114, i64* %17, align 8
  %115 = load i64, i64* %17, align 8
  %116 = load %class.KV.1*, %class.KV.1** %16, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %0, i64 %115, %class.KV.1* %116)
  br label %147

; <label>:117:                                    ; preds = %63
  %118 = load %class.KV.0*, %class.KV.0** %6, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2ERKS1_(%class.KV.0* %0, %class.KV.0* dereferenceable(16) %118)
  br label %147

; <label>:119:                                    ; preds = %45
  %120 = load %class.KV.1*, %class.KV.1** %10, align 8
  %121 = load i32, i32* %15, align 4
  %122 = zext i32 %121 to i64
  %123 = getelementptr inbounds %class.KV.1, %class.KV.1* %120, i64 %122
  %124 = load i64, i64* %7, align 8
  %125 = lshr i64 %124, 6
  %126 = load %class.key*, %class.key** %8, align 8
  %127 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3keyS0_Lj2EE12remove_innerERKS1_mPKS0_Pm(%class.KV.1* sret %18, %class.KV.1* dereferenceable(16) %123, i64 %125, %class.key* %126, i64* %127)
  %128 = load %class.KV.1*, %class.KV.1** %10, align 8
  %129 = load i32, i32* %15, align 4
  %130 = zext i32 %129 to i64
  %131 = getelementptr inbounds %class.KV.1, %class.KV.1* %128, i64 %130
  %132 = call zeroext i1 @_ZNK2KVI3keyS0_Lj2EEeqERKS1_(%class.KV.1* %18, %class.KV.1* dereferenceable(16) %131)
  br i1 %132, label %133, label %135

; <label>:133:                                    ; preds = %119
  %134 = load %class.KV.0*, %class.KV.0** %6, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2ERKS1_(%class.KV.0* %0, %class.KV.0* dereferenceable(16) %134)
  br label %147

; <label>:135:                                    ; preds = %119
  %136 = load %class.KV.1*, %class.KV.1** %10, align 8
  %137 = load i32, i32* %13, align 4
  %138 = load i32, i32* %15, align 4
  %139 = call %class.KV.1* @_ZN2KVI3keyS0_Lj2EE11update_nodeEPKS1_jjRS2_(%class.KV.1* %136, i32 %137, i32 %138, %class.KV.1* dereferenceable(16) %18)
  store %class.KV.1* %139, %class.KV.1** %19, align 8
  %140 = load %class.KV.0*, %class.KV.0** %6, align 8
  %141 = getelementptr inbounds %class.KV.0, %class.KV.0* %140, i32 0, i32 0
  %142 = bitcast %"union.KV<key, key, 1>::Key"* %141 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = load %class.KV.1*, %class.KV.1** %19, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2EmPKS_IS0_S0_Lj2EE(%class.KV.0* %0, i64 %143, %class.KV.1* %144)
  br label %147

; <label>:145:                                    ; preds = %5
  %146 = load %class.KV.0*, %class.KV.0** %6, align 8
  call void @_ZN2KVI3keyS0_Lj1EEC2ERKS1_(%class.KV.0* %0, %class.KV.0* dereferenceable(16) %146)
  br label %147

; <label>:147:                                    ; preds = %145, %135, %133, %117, %73
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj1EEeqERKS1_(%class.KV.0*, %class.KV.0* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV.0*, align 8
  %4 = alloca %class.KV.0*, align 8
  store %class.KV.0* %0, %class.KV.0** %3, align 8
  store %class.KV.0* %1, %class.KV.0** %4, align 8
  %5 = load %class.KV.0*, %class.KV.0** %3, align 8
  %6 = getelementptr inbounds %class.KV.0, %class.KV.0* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 1>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.0*, %class.KV.0** %4, align 8
  %10 = getelementptr inbounds %class.KV.0, %class.KV.0* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 1>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV.0, %class.KV.0* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 1>::Val"* %15 to %class.KV.1**
  %17 = load %class.KV.1*, %class.KV.1** %16, align 8
  %18 = load %class.KV.0*, %class.KV.0** %4, align 8
  %19 = getelementptr inbounds %class.KV.0, %class.KV.0* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 1>::Val"* %19 to %class.KV.1**
  %21 = load %class.KV.1*, %class.KV.1** %20, align 8
  %22 = icmp eq %class.KV.1* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj2EE12remove_innerERKS1_mPKS0_Pm(%class.KV.1* noalias sret, %class.KV.1* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV.1*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.2*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i8, align 1
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.2*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.2, align 8
  %19 = alloca %class.KV.2*, align 8
  store %class.KV.1* %1, %class.KV.1** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.1*, %class.KV.1** %6, align 8
  %21 = getelementptr inbounds %class.KV.1, %class.KV.1* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, key, 2>::Val"* %21 to %class.KV.2**
  %23 = load %class.KV.2*, %class.KV.2** %22, align 8
  store %class.KV.2* %23, %class.KV.2** %10, align 8
  %24 = load %class.KV.1*, %class.KV.1** %6, align 8
  %25 = getelementptr inbounds %class.KV.1, %class.KV.1* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, key, 2>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %12, align 4
  %33 = load i64, i64* %11, align 8
  %34 = call i64 @llvm.ctpop.i64(i64 %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %13, align 4
  %36 = load i64, i64* %11, align 8
  %37 = load i32, i32* %12, align 4
  %38 = zext i32 %37 to i64
  %39 = shl i64 1, %38
  %40 = and i64 %36, %39
  %41 = icmp ne i64 %40, 0
  %42 = zext i1 %41 to i8
  store i8 %42, i8* %14, align 1
  %43 = load i8, i8* %14, align 1
  %44 = trunc i8 %43 to i1
  br i1 %44, label %45, label %145

; <label>:45:                                     ; preds = %5
  %46 = load i64, i64* %11, align 8
  %47 = shl i64 %46, 1
  %48 = load i32, i32* %12, align 4
  %49 = sub i32 63, %48
  %50 = zext i32 %49 to i64
  %51 = shl i64 %47, %50
  %52 = call i64 @llvm.ctpop.i64(i64 %51)
  %53 = trunc i64 %52 to i32
  store i32 %53, i32* %15, align 4
  %54 = load %class.KV.2*, %class.KV.2** %10, align 8
  %55 = load i32, i32* %15, align 4
  %56 = zext i32 %55 to i64
  %57 = getelementptr inbounds %class.KV.2, %class.KV.2* %54, i64 %56
  %58 = getelementptr inbounds %class.KV.2, %class.KV.2* %57, i32 0, i32 0
  %59 = bitcast %"union.KV<key, key, 3>::Key"* %58 to i64*
  %60 = load i64, i64* %59, align 8
  %61 = and i64 %60, 1
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %63, label %119

; <label>:63:                                     ; preds = %45
  %64 = load %class.KV.2*, %class.KV.2** %10, align 8
  %65 = load i32, i32* %15, align 4
  %66 = zext i32 %65 to i64
  %67 = getelementptr inbounds %class.KV.2, %class.KV.2* %64, i64 %66
  %68 = getelementptr inbounds %class.KV.2, %class.KV.2* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<key, key, 3>::Key"* %68 to %class.key**
  %70 = load %class.key*, %class.key** %69, align 8
  %71 = load %class.key*, %class.key** %8, align 8
  %72 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %70, %class.key* dereferenceable(8) %71)
  br i1 %72, label %73, label %117

; <label>:73:                                     ; preds = %63
  %74 = load i64*, i64** %9, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, -1
  store i64 %76, i64* %74, align 8
  %77 = load i32, i32* %13, align 4
  %78 = sub i32 %77, 1
  %79 = zext i32 %78 to i64
  %80 = mul i64 %79, 16
  %81 = call noalias i8* @malloc(i64 %80) #8
  %82 = bitcast i8* %81 to %class.KV.2*
  store %class.KV.2* %82, %class.KV.2** %16, align 8
  %83 = load %class.KV.2*, %class.KV.2** %16, align 8
  %84 = bitcast %class.KV.2* %83 to i8*
  %85 = load %class.KV.2*, %class.KV.2** %10, align 8
  %86 = bitcast %class.KV.2* %85 to i8*
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = mul i64 %88, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %84, i8* %86, i64 %89, i32 8, i1 false)
  %90 = load %class.KV.2*, %class.KV.2** %16, align 8
  %91 = load i32, i32* %15, align 4
  %92 = zext i32 %91 to i64
  %93 = getelementptr inbounds %class.KV.2, %class.KV.2* %90, i64 %92
  %94 = bitcast %class.KV.2* %93 to i8*
  %95 = load %class.KV.2*, %class.KV.2** %10, align 8
  %96 = load i32, i32* %15, align 4
  %97 = add i32 %96, 1
  %98 = zext i32 %97 to i64
  %99 = getelementptr inbounds %class.KV.2, %class.KV.2* %95, i64 %98
  %100 = bitcast %class.KV.2* %99 to i8*
  %101 = load i32, i32* %13, align 4
  %102 = sub i32 %101, 1
  %103 = load i32, i32* %15, align 4
  %104 = sub i32 %102, %103
  %105 = zext i32 %104 to i64
  %106 = mul i64 %105, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %94, i8* %100, i64 %106, i32 8, i1 false)
  %107 = load i64, i64* %11, align 8
  %108 = load i32, i32* %12, align 4
  %109 = zext i32 %108 to i64
  %110 = shl i64 1, %109
  %111 = xor i64 -1, %110
  %112 = and i64 %107, %111
  %113 = shl i64 %112, 1
  %114 = or i64 %113, 1
  store i64 %114, i64* %17, align 8
  %115 = load i64, i64* %17, align 8
  %116 = load %class.KV.2*, %class.KV.2** %16, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %0, i64 %115, %class.KV.2* %116)
  br label %147

; <label>:117:                                    ; preds = %63
  %118 = load %class.KV.1*, %class.KV.1** %6, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2ERKS1_(%class.KV.1* %0, %class.KV.1* dereferenceable(16) %118)
  br label %147

; <label>:119:                                    ; preds = %45
  %120 = load %class.KV.2*, %class.KV.2** %10, align 8
  %121 = load i32, i32* %15, align 4
  %122 = zext i32 %121 to i64
  %123 = getelementptr inbounds %class.KV.2, %class.KV.2* %120, i64 %122
  %124 = load i64, i64* %7, align 8
  %125 = lshr i64 %124, 6
  %126 = load %class.key*, %class.key** %8, align 8
  %127 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3keyS0_Lj3EE12remove_innerERKS1_mPKS0_Pm(%class.KV.2* sret %18, %class.KV.2* dereferenceable(16) %123, i64 %125, %class.key* %126, i64* %127)
  %128 = load %class.KV.2*, %class.KV.2** %10, align 8
  %129 = load i32, i32* %15, align 4
  %130 = zext i32 %129 to i64
  %131 = getelementptr inbounds %class.KV.2, %class.KV.2* %128, i64 %130
  %132 = call zeroext i1 @_ZNK2KVI3keyS0_Lj3EEeqERKS1_(%class.KV.2* %18, %class.KV.2* dereferenceable(16) %131)
  br i1 %132, label %133, label %135

; <label>:133:                                    ; preds = %119
  %134 = load %class.KV.1*, %class.KV.1** %6, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2ERKS1_(%class.KV.1* %0, %class.KV.1* dereferenceable(16) %134)
  br label %147

; <label>:135:                                    ; preds = %119
  %136 = load %class.KV.2*, %class.KV.2** %10, align 8
  %137 = load i32, i32* %13, align 4
  %138 = load i32, i32* %15, align 4
  %139 = call %class.KV.2* @_ZN2KVI3keyS0_Lj3EE11update_nodeEPKS1_jjRS2_(%class.KV.2* %136, i32 %137, i32 %138, %class.KV.2* dereferenceable(16) %18)
  store %class.KV.2* %139, %class.KV.2** %19, align 8
  %140 = load %class.KV.1*, %class.KV.1** %6, align 8
  %141 = getelementptr inbounds %class.KV.1, %class.KV.1* %140, i32 0, i32 0
  %142 = bitcast %"union.KV<key, key, 2>::Key"* %141 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = load %class.KV.2*, %class.KV.2** %19, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2EmPKS_IS0_S0_Lj3EE(%class.KV.1* %0, i64 %143, %class.KV.2* %144)
  br label %147

; <label>:145:                                    ; preds = %5
  %146 = load %class.KV.1*, %class.KV.1** %6, align 8
  call void @_ZN2KVI3keyS0_Lj2EEC2ERKS1_(%class.KV.1* %0, %class.KV.1* dereferenceable(16) %146)
  br label %147

; <label>:147:                                    ; preds = %145, %135, %133, %117, %73
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj2EEeqERKS1_(%class.KV.1*, %class.KV.1* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV.1*, align 8
  %4 = alloca %class.KV.1*, align 8
  store %class.KV.1* %0, %class.KV.1** %3, align 8
  store %class.KV.1* %1, %class.KV.1** %4, align 8
  %5 = load %class.KV.1*, %class.KV.1** %3, align 8
  %6 = getelementptr inbounds %class.KV.1, %class.KV.1* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 2>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.1*, %class.KV.1** %4, align 8
  %10 = getelementptr inbounds %class.KV.1, %class.KV.1* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 2>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV.1, %class.KV.1* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 2>::Val"* %15 to %class.KV.2**
  %17 = load %class.KV.2*, %class.KV.2** %16, align 8
  %18 = load %class.KV.1*, %class.KV.1** %4, align 8
  %19 = getelementptr inbounds %class.KV.1, %class.KV.1* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 2>::Val"* %19 to %class.KV.2**
  %21 = load %class.KV.2*, %class.KV.2** %20, align 8
  %22 = icmp eq %class.KV.2* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj3EE12remove_innerERKS1_mPKS0_Pm(%class.KV.2* noalias sret, %class.KV.2* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV.2*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.3*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i8, align 1
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.3*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.3, align 8
  %19 = alloca %class.KV.3*, align 8
  store %class.KV.2* %1, %class.KV.2** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.2*, %class.KV.2** %6, align 8
  %21 = getelementptr inbounds %class.KV.2, %class.KV.2* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, key, 3>::Val"* %21 to %class.KV.3**
  %23 = load %class.KV.3*, %class.KV.3** %22, align 8
  store %class.KV.3* %23, %class.KV.3** %10, align 8
  %24 = load %class.KV.2*, %class.KV.2** %6, align 8
  %25 = getelementptr inbounds %class.KV.2, %class.KV.2* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, key, 3>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %12, align 4
  %33 = load i64, i64* %11, align 8
  %34 = call i64 @llvm.ctpop.i64(i64 %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %13, align 4
  %36 = load i64, i64* %11, align 8
  %37 = load i32, i32* %12, align 4
  %38 = zext i32 %37 to i64
  %39 = shl i64 1, %38
  %40 = and i64 %36, %39
  %41 = icmp ne i64 %40, 0
  %42 = zext i1 %41 to i8
  store i8 %42, i8* %14, align 1
  %43 = load i8, i8* %14, align 1
  %44 = trunc i8 %43 to i1
  br i1 %44, label %45, label %145

; <label>:45:                                     ; preds = %5
  %46 = load i64, i64* %11, align 8
  %47 = shl i64 %46, 1
  %48 = load i32, i32* %12, align 4
  %49 = sub i32 63, %48
  %50 = zext i32 %49 to i64
  %51 = shl i64 %47, %50
  %52 = call i64 @llvm.ctpop.i64(i64 %51)
  %53 = trunc i64 %52 to i32
  store i32 %53, i32* %15, align 4
  %54 = load %class.KV.3*, %class.KV.3** %10, align 8
  %55 = load i32, i32* %15, align 4
  %56 = zext i32 %55 to i64
  %57 = getelementptr inbounds %class.KV.3, %class.KV.3* %54, i64 %56
  %58 = getelementptr inbounds %class.KV.3, %class.KV.3* %57, i32 0, i32 0
  %59 = bitcast %"union.KV<key, key, 4>::Key"* %58 to i64*
  %60 = load i64, i64* %59, align 8
  %61 = and i64 %60, 1
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %63, label %119

; <label>:63:                                     ; preds = %45
  %64 = load %class.KV.3*, %class.KV.3** %10, align 8
  %65 = load i32, i32* %15, align 4
  %66 = zext i32 %65 to i64
  %67 = getelementptr inbounds %class.KV.3, %class.KV.3* %64, i64 %66
  %68 = getelementptr inbounds %class.KV.3, %class.KV.3* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<key, key, 4>::Key"* %68 to %class.key**
  %70 = load %class.key*, %class.key** %69, align 8
  %71 = load %class.key*, %class.key** %8, align 8
  %72 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %70, %class.key* dereferenceable(8) %71)
  br i1 %72, label %73, label %117

; <label>:73:                                     ; preds = %63
  %74 = load i64*, i64** %9, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, -1
  store i64 %76, i64* %74, align 8
  %77 = load i32, i32* %13, align 4
  %78 = sub i32 %77, 1
  %79 = zext i32 %78 to i64
  %80 = mul i64 %79, 16
  %81 = call noalias i8* @malloc(i64 %80) #8
  %82 = bitcast i8* %81 to %class.KV.3*
  store %class.KV.3* %82, %class.KV.3** %16, align 8
  %83 = load %class.KV.3*, %class.KV.3** %16, align 8
  %84 = bitcast %class.KV.3* %83 to i8*
  %85 = load %class.KV.3*, %class.KV.3** %10, align 8
  %86 = bitcast %class.KV.3* %85 to i8*
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = mul i64 %88, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %84, i8* %86, i64 %89, i32 8, i1 false)
  %90 = load %class.KV.3*, %class.KV.3** %16, align 8
  %91 = load i32, i32* %15, align 4
  %92 = zext i32 %91 to i64
  %93 = getelementptr inbounds %class.KV.3, %class.KV.3* %90, i64 %92
  %94 = bitcast %class.KV.3* %93 to i8*
  %95 = load %class.KV.3*, %class.KV.3** %10, align 8
  %96 = load i32, i32* %15, align 4
  %97 = add i32 %96, 1
  %98 = zext i32 %97 to i64
  %99 = getelementptr inbounds %class.KV.3, %class.KV.3* %95, i64 %98
  %100 = bitcast %class.KV.3* %99 to i8*
  %101 = load i32, i32* %13, align 4
  %102 = sub i32 %101, 1
  %103 = load i32, i32* %15, align 4
  %104 = sub i32 %102, %103
  %105 = zext i32 %104 to i64
  %106 = mul i64 %105, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %94, i8* %100, i64 %106, i32 8, i1 false)
  %107 = load i64, i64* %11, align 8
  %108 = load i32, i32* %12, align 4
  %109 = zext i32 %108 to i64
  %110 = shl i64 1, %109
  %111 = xor i64 -1, %110
  %112 = and i64 %107, %111
  %113 = shl i64 %112, 1
  %114 = or i64 %113, 1
  store i64 %114, i64* %17, align 8
  %115 = load i64, i64* %17, align 8
  %116 = load %class.KV.3*, %class.KV.3** %16, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %0, i64 %115, %class.KV.3* %116)
  br label %147

; <label>:117:                                    ; preds = %63
  %118 = load %class.KV.2*, %class.KV.2** %6, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2ERKS1_(%class.KV.2* %0, %class.KV.2* dereferenceable(16) %118)
  br label %147

; <label>:119:                                    ; preds = %45
  %120 = load %class.KV.3*, %class.KV.3** %10, align 8
  %121 = load i32, i32* %15, align 4
  %122 = zext i32 %121 to i64
  %123 = getelementptr inbounds %class.KV.3, %class.KV.3* %120, i64 %122
  %124 = load i64, i64* %7, align 8
  %125 = lshr i64 %124, 6
  %126 = load %class.key*, %class.key** %8, align 8
  %127 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3keyS0_Lj4EE12remove_innerERKS1_mPKS0_Pm(%class.KV.3* sret %18, %class.KV.3* dereferenceable(16) %123, i64 %125, %class.key* %126, i64* %127)
  %128 = load %class.KV.3*, %class.KV.3** %10, align 8
  %129 = load i32, i32* %15, align 4
  %130 = zext i32 %129 to i64
  %131 = getelementptr inbounds %class.KV.3, %class.KV.3* %128, i64 %130
  %132 = call zeroext i1 @_ZNK2KVI3keyS0_Lj4EEeqERKS1_(%class.KV.3* %18, %class.KV.3* dereferenceable(16) %131)
  br i1 %132, label %133, label %135

; <label>:133:                                    ; preds = %119
  %134 = load %class.KV.2*, %class.KV.2** %6, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2ERKS1_(%class.KV.2* %0, %class.KV.2* dereferenceable(16) %134)
  br label %147

; <label>:135:                                    ; preds = %119
  %136 = load %class.KV.3*, %class.KV.3** %10, align 8
  %137 = load i32, i32* %13, align 4
  %138 = load i32, i32* %15, align 4
  %139 = call %class.KV.3* @_ZN2KVI3keyS0_Lj4EE11update_nodeEPKS1_jjRS2_(%class.KV.3* %136, i32 %137, i32 %138, %class.KV.3* dereferenceable(16) %18)
  store %class.KV.3* %139, %class.KV.3** %19, align 8
  %140 = load %class.KV.2*, %class.KV.2** %6, align 8
  %141 = getelementptr inbounds %class.KV.2, %class.KV.2* %140, i32 0, i32 0
  %142 = bitcast %"union.KV<key, key, 3>::Key"* %141 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = load %class.KV.3*, %class.KV.3** %19, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2EmPKS_IS0_S0_Lj4EE(%class.KV.2* %0, i64 %143, %class.KV.3* %144)
  br label %147

; <label>:145:                                    ; preds = %5
  %146 = load %class.KV.2*, %class.KV.2** %6, align 8
  call void @_ZN2KVI3keyS0_Lj3EEC2ERKS1_(%class.KV.2* %0, %class.KV.2* dereferenceable(16) %146)
  br label %147

; <label>:147:                                    ; preds = %145, %135, %133, %117, %73
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj3EEeqERKS1_(%class.KV.2*, %class.KV.2* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV.2*, align 8
  %4 = alloca %class.KV.2*, align 8
  store %class.KV.2* %0, %class.KV.2** %3, align 8
  store %class.KV.2* %1, %class.KV.2** %4, align 8
  %5 = load %class.KV.2*, %class.KV.2** %3, align 8
  %6 = getelementptr inbounds %class.KV.2, %class.KV.2* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 3>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.2*, %class.KV.2** %4, align 8
  %10 = getelementptr inbounds %class.KV.2, %class.KV.2* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 3>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV.2, %class.KV.2* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 3>::Val"* %15 to %class.KV.3**
  %17 = load %class.KV.3*, %class.KV.3** %16, align 8
  %18 = load %class.KV.2*, %class.KV.2** %4, align 8
  %19 = getelementptr inbounds %class.KV.2, %class.KV.2* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 3>::Val"* %19 to %class.KV.3**
  %21 = load %class.KV.3*, %class.KV.3** %20, align 8
  %22 = icmp eq %class.KV.3* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj4EE12remove_innerERKS1_mPKS0_Pm(%class.KV.3* noalias sret, %class.KV.3* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV.3*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.4*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i8, align 1
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.4*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.4, align 8
  %19 = alloca %class.KV.4*, align 8
  store %class.KV.3* %1, %class.KV.3** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.3*, %class.KV.3** %6, align 8
  %21 = getelementptr inbounds %class.KV.3, %class.KV.3* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, key, 4>::Val"* %21 to %class.KV.4**
  %23 = load %class.KV.4*, %class.KV.4** %22, align 8
  store %class.KV.4* %23, %class.KV.4** %10, align 8
  %24 = load %class.KV.3*, %class.KV.3** %6, align 8
  %25 = getelementptr inbounds %class.KV.3, %class.KV.3* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, key, 4>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %12, align 4
  %33 = load i64, i64* %11, align 8
  %34 = call i64 @llvm.ctpop.i64(i64 %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %13, align 4
  %36 = load i64, i64* %11, align 8
  %37 = load i32, i32* %12, align 4
  %38 = zext i32 %37 to i64
  %39 = shl i64 1, %38
  %40 = and i64 %36, %39
  %41 = icmp ne i64 %40, 0
  %42 = zext i1 %41 to i8
  store i8 %42, i8* %14, align 1
  %43 = load i8, i8* %14, align 1
  %44 = trunc i8 %43 to i1
  br i1 %44, label %45, label %145

; <label>:45:                                     ; preds = %5
  %46 = load i64, i64* %11, align 8
  %47 = shl i64 %46, 1
  %48 = load i32, i32* %12, align 4
  %49 = sub i32 63, %48
  %50 = zext i32 %49 to i64
  %51 = shl i64 %47, %50
  %52 = call i64 @llvm.ctpop.i64(i64 %51)
  %53 = trunc i64 %52 to i32
  store i32 %53, i32* %15, align 4
  %54 = load %class.KV.4*, %class.KV.4** %10, align 8
  %55 = load i32, i32* %15, align 4
  %56 = zext i32 %55 to i64
  %57 = getelementptr inbounds %class.KV.4, %class.KV.4* %54, i64 %56
  %58 = getelementptr inbounds %class.KV.4, %class.KV.4* %57, i32 0, i32 0
  %59 = bitcast %"union.KV<key, key, 5>::Key"* %58 to i64*
  %60 = load i64, i64* %59, align 8
  %61 = and i64 %60, 1
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %63, label %119

; <label>:63:                                     ; preds = %45
  %64 = load %class.KV.4*, %class.KV.4** %10, align 8
  %65 = load i32, i32* %15, align 4
  %66 = zext i32 %65 to i64
  %67 = getelementptr inbounds %class.KV.4, %class.KV.4* %64, i64 %66
  %68 = getelementptr inbounds %class.KV.4, %class.KV.4* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<key, key, 5>::Key"* %68 to %class.key**
  %70 = load %class.key*, %class.key** %69, align 8
  %71 = load %class.key*, %class.key** %8, align 8
  %72 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %70, %class.key* dereferenceable(8) %71)
  br i1 %72, label %73, label %117

; <label>:73:                                     ; preds = %63
  %74 = load i64*, i64** %9, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, -1
  store i64 %76, i64* %74, align 8
  %77 = load i32, i32* %13, align 4
  %78 = sub i32 %77, 1
  %79 = zext i32 %78 to i64
  %80 = mul i64 %79, 16
  %81 = call noalias i8* @malloc(i64 %80) #8
  %82 = bitcast i8* %81 to %class.KV.4*
  store %class.KV.4* %82, %class.KV.4** %16, align 8
  %83 = load %class.KV.4*, %class.KV.4** %16, align 8
  %84 = bitcast %class.KV.4* %83 to i8*
  %85 = load %class.KV.4*, %class.KV.4** %10, align 8
  %86 = bitcast %class.KV.4* %85 to i8*
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = mul i64 %88, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %84, i8* %86, i64 %89, i32 8, i1 false)
  %90 = load %class.KV.4*, %class.KV.4** %16, align 8
  %91 = load i32, i32* %15, align 4
  %92 = zext i32 %91 to i64
  %93 = getelementptr inbounds %class.KV.4, %class.KV.4* %90, i64 %92
  %94 = bitcast %class.KV.4* %93 to i8*
  %95 = load %class.KV.4*, %class.KV.4** %10, align 8
  %96 = load i32, i32* %15, align 4
  %97 = add i32 %96, 1
  %98 = zext i32 %97 to i64
  %99 = getelementptr inbounds %class.KV.4, %class.KV.4* %95, i64 %98
  %100 = bitcast %class.KV.4* %99 to i8*
  %101 = load i32, i32* %13, align 4
  %102 = sub i32 %101, 1
  %103 = load i32, i32* %15, align 4
  %104 = sub i32 %102, %103
  %105 = zext i32 %104 to i64
  %106 = mul i64 %105, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %94, i8* %100, i64 %106, i32 8, i1 false)
  %107 = load i64, i64* %11, align 8
  %108 = load i32, i32* %12, align 4
  %109 = zext i32 %108 to i64
  %110 = shl i64 1, %109
  %111 = xor i64 -1, %110
  %112 = and i64 %107, %111
  %113 = shl i64 %112, 1
  %114 = or i64 %113, 1
  store i64 %114, i64* %17, align 8
  %115 = load i64, i64* %17, align 8
  %116 = load %class.KV.4*, %class.KV.4** %16, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %0, i64 %115, %class.KV.4* %116)
  br label %147

; <label>:117:                                    ; preds = %63
  %118 = load %class.KV.3*, %class.KV.3** %6, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2ERKS1_(%class.KV.3* %0, %class.KV.3* dereferenceable(16) %118)
  br label %147

; <label>:119:                                    ; preds = %45
  %120 = load %class.KV.4*, %class.KV.4** %10, align 8
  %121 = load i32, i32* %15, align 4
  %122 = zext i32 %121 to i64
  %123 = getelementptr inbounds %class.KV.4, %class.KV.4* %120, i64 %122
  %124 = load i64, i64* %7, align 8
  %125 = lshr i64 %124, 6
  %126 = load %class.key*, %class.key** %8, align 8
  %127 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3keyS0_Lj5EE12remove_innerERKS1_mPKS0_Pm(%class.KV.4* sret %18, %class.KV.4* dereferenceable(16) %123, i64 %125, %class.key* %126, i64* %127)
  %128 = load %class.KV.4*, %class.KV.4** %10, align 8
  %129 = load i32, i32* %15, align 4
  %130 = zext i32 %129 to i64
  %131 = getelementptr inbounds %class.KV.4, %class.KV.4* %128, i64 %130
  %132 = call zeroext i1 @_ZNK2KVI3keyS0_Lj5EEeqERKS1_(%class.KV.4* %18, %class.KV.4* dereferenceable(16) %131)
  br i1 %132, label %133, label %135

; <label>:133:                                    ; preds = %119
  %134 = load %class.KV.3*, %class.KV.3** %6, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2ERKS1_(%class.KV.3* %0, %class.KV.3* dereferenceable(16) %134)
  br label %147

; <label>:135:                                    ; preds = %119
  %136 = load %class.KV.4*, %class.KV.4** %10, align 8
  %137 = load i32, i32* %13, align 4
  %138 = load i32, i32* %15, align 4
  %139 = call %class.KV.4* @_ZN2KVI3keyS0_Lj5EE11update_nodeEPKS1_jjRS2_(%class.KV.4* %136, i32 %137, i32 %138, %class.KV.4* dereferenceable(16) %18)
  store %class.KV.4* %139, %class.KV.4** %19, align 8
  %140 = load %class.KV.3*, %class.KV.3** %6, align 8
  %141 = getelementptr inbounds %class.KV.3, %class.KV.3* %140, i32 0, i32 0
  %142 = bitcast %"union.KV<key, key, 4>::Key"* %141 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = load %class.KV.4*, %class.KV.4** %19, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2EmPKS_IS0_S0_Lj5EE(%class.KV.3* %0, i64 %143, %class.KV.4* %144)
  br label %147

; <label>:145:                                    ; preds = %5
  %146 = load %class.KV.3*, %class.KV.3** %6, align 8
  call void @_ZN2KVI3keyS0_Lj4EEC2ERKS1_(%class.KV.3* %0, %class.KV.3* dereferenceable(16) %146)
  br label %147

; <label>:147:                                    ; preds = %145, %135, %133, %117, %73
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj4EEeqERKS1_(%class.KV.3*, %class.KV.3* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV.3*, align 8
  %4 = alloca %class.KV.3*, align 8
  store %class.KV.3* %0, %class.KV.3** %3, align 8
  store %class.KV.3* %1, %class.KV.3** %4, align 8
  %5 = load %class.KV.3*, %class.KV.3** %3, align 8
  %6 = getelementptr inbounds %class.KV.3, %class.KV.3* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 4>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.3*, %class.KV.3** %4, align 8
  %10 = getelementptr inbounds %class.KV.3, %class.KV.3* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 4>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV.3, %class.KV.3* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 4>::Val"* %15 to %class.KV.4**
  %17 = load %class.KV.4*, %class.KV.4** %16, align 8
  %18 = load %class.KV.3*, %class.KV.3** %4, align 8
  %19 = getelementptr inbounds %class.KV.3, %class.KV.3* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 4>::Val"* %19 to %class.KV.4**
  %21 = load %class.KV.4*, %class.KV.4** %20, align 8
  %22 = icmp eq %class.KV.4* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj5EE12remove_innerERKS1_mPKS0_Pm(%class.KV.4* noalias sret, %class.KV.4* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV.4*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.5*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i8, align 1
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.5*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.5, align 8
  %19 = alloca %class.KV.5*, align 8
  store %class.KV.4* %1, %class.KV.4** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.4*, %class.KV.4** %6, align 8
  %21 = getelementptr inbounds %class.KV.4, %class.KV.4* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, key, 5>::Val"* %21 to %class.KV.5**
  %23 = load %class.KV.5*, %class.KV.5** %22, align 8
  store %class.KV.5* %23, %class.KV.5** %10, align 8
  %24 = load %class.KV.4*, %class.KV.4** %6, align 8
  %25 = getelementptr inbounds %class.KV.4, %class.KV.4* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, key, 5>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %12, align 4
  %33 = load i64, i64* %11, align 8
  %34 = call i64 @llvm.ctpop.i64(i64 %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %13, align 4
  %36 = load i64, i64* %11, align 8
  %37 = load i32, i32* %12, align 4
  %38 = zext i32 %37 to i64
  %39 = shl i64 1, %38
  %40 = and i64 %36, %39
  %41 = icmp ne i64 %40, 0
  %42 = zext i1 %41 to i8
  store i8 %42, i8* %14, align 1
  %43 = load i8, i8* %14, align 1
  %44 = trunc i8 %43 to i1
  br i1 %44, label %45, label %145

; <label>:45:                                     ; preds = %5
  %46 = load i64, i64* %11, align 8
  %47 = shl i64 %46, 1
  %48 = load i32, i32* %12, align 4
  %49 = sub i32 63, %48
  %50 = zext i32 %49 to i64
  %51 = shl i64 %47, %50
  %52 = call i64 @llvm.ctpop.i64(i64 %51)
  %53 = trunc i64 %52 to i32
  store i32 %53, i32* %15, align 4
  %54 = load %class.KV.5*, %class.KV.5** %10, align 8
  %55 = load i32, i32* %15, align 4
  %56 = zext i32 %55 to i64
  %57 = getelementptr inbounds %class.KV.5, %class.KV.5* %54, i64 %56
  %58 = getelementptr inbounds %class.KV.5, %class.KV.5* %57, i32 0, i32 0
  %59 = bitcast %"union.KV<key, key, 6>::Key"* %58 to i64*
  %60 = load i64, i64* %59, align 8
  %61 = and i64 %60, 1
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %63, label %119

; <label>:63:                                     ; preds = %45
  %64 = load %class.KV.5*, %class.KV.5** %10, align 8
  %65 = load i32, i32* %15, align 4
  %66 = zext i32 %65 to i64
  %67 = getelementptr inbounds %class.KV.5, %class.KV.5* %64, i64 %66
  %68 = getelementptr inbounds %class.KV.5, %class.KV.5* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<key, key, 6>::Key"* %68 to %class.key**
  %70 = load %class.key*, %class.key** %69, align 8
  %71 = load %class.key*, %class.key** %8, align 8
  %72 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %70, %class.key* dereferenceable(8) %71)
  br i1 %72, label %73, label %117

; <label>:73:                                     ; preds = %63
  %74 = load i64*, i64** %9, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, -1
  store i64 %76, i64* %74, align 8
  %77 = load i32, i32* %13, align 4
  %78 = sub i32 %77, 1
  %79 = zext i32 %78 to i64
  %80 = mul i64 %79, 16
  %81 = call noalias i8* @malloc(i64 %80) #8
  %82 = bitcast i8* %81 to %class.KV.5*
  store %class.KV.5* %82, %class.KV.5** %16, align 8
  %83 = load %class.KV.5*, %class.KV.5** %16, align 8
  %84 = bitcast %class.KV.5* %83 to i8*
  %85 = load %class.KV.5*, %class.KV.5** %10, align 8
  %86 = bitcast %class.KV.5* %85 to i8*
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = mul i64 %88, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %84, i8* %86, i64 %89, i32 8, i1 false)
  %90 = load %class.KV.5*, %class.KV.5** %16, align 8
  %91 = load i32, i32* %15, align 4
  %92 = zext i32 %91 to i64
  %93 = getelementptr inbounds %class.KV.5, %class.KV.5* %90, i64 %92
  %94 = bitcast %class.KV.5* %93 to i8*
  %95 = load %class.KV.5*, %class.KV.5** %10, align 8
  %96 = load i32, i32* %15, align 4
  %97 = add i32 %96, 1
  %98 = zext i32 %97 to i64
  %99 = getelementptr inbounds %class.KV.5, %class.KV.5* %95, i64 %98
  %100 = bitcast %class.KV.5* %99 to i8*
  %101 = load i32, i32* %13, align 4
  %102 = sub i32 %101, 1
  %103 = load i32, i32* %15, align 4
  %104 = sub i32 %102, %103
  %105 = zext i32 %104 to i64
  %106 = mul i64 %105, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %94, i8* %100, i64 %106, i32 8, i1 false)
  %107 = load i64, i64* %11, align 8
  %108 = load i32, i32* %12, align 4
  %109 = zext i32 %108 to i64
  %110 = shl i64 1, %109
  %111 = xor i64 -1, %110
  %112 = and i64 %107, %111
  %113 = shl i64 %112, 1
  %114 = or i64 %113, 1
  store i64 %114, i64* %17, align 8
  %115 = load i64, i64* %17, align 8
  %116 = load %class.KV.5*, %class.KV.5** %16, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %0, i64 %115, %class.KV.5* %116)
  br label %147

; <label>:117:                                    ; preds = %63
  %118 = load %class.KV.4*, %class.KV.4** %6, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2ERKS1_(%class.KV.4* %0, %class.KV.4* dereferenceable(16) %118)
  br label %147

; <label>:119:                                    ; preds = %45
  %120 = load %class.KV.5*, %class.KV.5** %10, align 8
  %121 = load i32, i32* %15, align 4
  %122 = zext i32 %121 to i64
  %123 = getelementptr inbounds %class.KV.5, %class.KV.5* %120, i64 %122
  %124 = load i64, i64* %7, align 8
  %125 = lshr i64 %124, 6
  %126 = load %class.key*, %class.key** %8, align 8
  %127 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3keyS0_Lj6EE12remove_innerERKS1_mPKS0_Pm(%class.KV.5* sret %18, %class.KV.5* dereferenceable(16) %123, i64 %125, %class.key* %126, i64* %127)
  %128 = load %class.KV.5*, %class.KV.5** %10, align 8
  %129 = load i32, i32* %15, align 4
  %130 = zext i32 %129 to i64
  %131 = getelementptr inbounds %class.KV.5, %class.KV.5* %128, i64 %130
  %132 = call zeroext i1 @_ZNK2KVI3keyS0_Lj6EEeqERKS1_(%class.KV.5* %18, %class.KV.5* dereferenceable(16) %131)
  br i1 %132, label %133, label %135

; <label>:133:                                    ; preds = %119
  %134 = load %class.KV.4*, %class.KV.4** %6, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2ERKS1_(%class.KV.4* %0, %class.KV.4* dereferenceable(16) %134)
  br label %147

; <label>:135:                                    ; preds = %119
  %136 = load %class.KV.5*, %class.KV.5** %10, align 8
  %137 = load i32, i32* %13, align 4
  %138 = load i32, i32* %15, align 4
  %139 = call %class.KV.5* @_ZN2KVI3keyS0_Lj6EE11update_nodeEPKS1_jjRS2_(%class.KV.5* %136, i32 %137, i32 %138, %class.KV.5* dereferenceable(16) %18)
  store %class.KV.5* %139, %class.KV.5** %19, align 8
  %140 = load %class.KV.4*, %class.KV.4** %6, align 8
  %141 = getelementptr inbounds %class.KV.4, %class.KV.4* %140, i32 0, i32 0
  %142 = bitcast %"union.KV<key, key, 5>::Key"* %141 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = load %class.KV.5*, %class.KV.5** %19, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2EmPKS_IS0_S0_Lj6EE(%class.KV.4* %0, i64 %143, %class.KV.5* %144)
  br label %147

; <label>:145:                                    ; preds = %5
  %146 = load %class.KV.4*, %class.KV.4** %6, align 8
  call void @_ZN2KVI3keyS0_Lj5EEC2ERKS1_(%class.KV.4* %0, %class.KV.4* dereferenceable(16) %146)
  br label %147

; <label>:147:                                    ; preds = %145, %135, %133, %117, %73
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj5EEeqERKS1_(%class.KV.4*, %class.KV.4* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV.4*, align 8
  %4 = alloca %class.KV.4*, align 8
  store %class.KV.4* %0, %class.KV.4** %3, align 8
  store %class.KV.4* %1, %class.KV.4** %4, align 8
  %5 = load %class.KV.4*, %class.KV.4** %3, align 8
  %6 = getelementptr inbounds %class.KV.4, %class.KV.4* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 5>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.4*, %class.KV.4** %4, align 8
  %10 = getelementptr inbounds %class.KV.4, %class.KV.4* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 5>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV.4, %class.KV.4* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 5>::Val"* %15 to %class.KV.5**
  %17 = load %class.KV.5*, %class.KV.5** %16, align 8
  %18 = load %class.KV.4*, %class.KV.4** %4, align 8
  %19 = getelementptr inbounds %class.KV.4, %class.KV.4* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 5>::Val"* %19 to %class.KV.5**
  %21 = load %class.KV.5*, %class.KV.5** %20, align 8
  %22 = icmp eq %class.KV.5* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj6EE12remove_innerERKS1_mPKS0_Pm(%class.KV.5* noalias sret, %class.KV.5* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV.5*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.6*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i8, align 1
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.6*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.6, align 8
  %19 = alloca %class.KV.6*, align 8
  store %class.KV.5* %1, %class.KV.5** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.5*, %class.KV.5** %6, align 8
  %21 = getelementptr inbounds %class.KV.5, %class.KV.5* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, key, 6>::Val"* %21 to %class.KV.6**
  %23 = load %class.KV.6*, %class.KV.6** %22, align 8
  store %class.KV.6* %23, %class.KV.6** %10, align 8
  %24 = load %class.KV.5*, %class.KV.5** %6, align 8
  %25 = getelementptr inbounds %class.KV.5, %class.KV.5* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, key, 6>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %12, align 4
  %33 = load i64, i64* %11, align 8
  %34 = call i64 @llvm.ctpop.i64(i64 %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %13, align 4
  %36 = load i64, i64* %11, align 8
  %37 = load i32, i32* %12, align 4
  %38 = zext i32 %37 to i64
  %39 = shl i64 1, %38
  %40 = and i64 %36, %39
  %41 = icmp ne i64 %40, 0
  %42 = zext i1 %41 to i8
  store i8 %42, i8* %14, align 1
  %43 = load i8, i8* %14, align 1
  %44 = trunc i8 %43 to i1
  br i1 %44, label %45, label %145

; <label>:45:                                     ; preds = %5
  %46 = load i64, i64* %11, align 8
  %47 = shl i64 %46, 1
  %48 = load i32, i32* %12, align 4
  %49 = sub i32 63, %48
  %50 = zext i32 %49 to i64
  %51 = shl i64 %47, %50
  %52 = call i64 @llvm.ctpop.i64(i64 %51)
  %53 = trunc i64 %52 to i32
  store i32 %53, i32* %15, align 4
  %54 = load %class.KV.6*, %class.KV.6** %10, align 8
  %55 = load i32, i32* %15, align 4
  %56 = zext i32 %55 to i64
  %57 = getelementptr inbounds %class.KV.6, %class.KV.6* %54, i64 %56
  %58 = getelementptr inbounds %class.KV.6, %class.KV.6* %57, i32 0, i32 0
  %59 = bitcast %"union.KV<key, key, 7>::Key"* %58 to i64*
  %60 = load i64, i64* %59, align 8
  %61 = and i64 %60, 1
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %63, label %119

; <label>:63:                                     ; preds = %45
  %64 = load %class.KV.6*, %class.KV.6** %10, align 8
  %65 = load i32, i32* %15, align 4
  %66 = zext i32 %65 to i64
  %67 = getelementptr inbounds %class.KV.6, %class.KV.6* %64, i64 %66
  %68 = getelementptr inbounds %class.KV.6, %class.KV.6* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<key, key, 7>::Key"* %68 to %class.key**
  %70 = load %class.key*, %class.key** %69, align 8
  %71 = load %class.key*, %class.key** %8, align 8
  %72 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %70, %class.key* dereferenceable(8) %71)
  br i1 %72, label %73, label %117

; <label>:73:                                     ; preds = %63
  %74 = load i64*, i64** %9, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, -1
  store i64 %76, i64* %74, align 8
  %77 = load i32, i32* %13, align 4
  %78 = sub i32 %77, 1
  %79 = zext i32 %78 to i64
  %80 = mul i64 %79, 16
  %81 = call noalias i8* @malloc(i64 %80) #8
  %82 = bitcast i8* %81 to %class.KV.6*
  store %class.KV.6* %82, %class.KV.6** %16, align 8
  %83 = load %class.KV.6*, %class.KV.6** %16, align 8
  %84 = bitcast %class.KV.6* %83 to i8*
  %85 = load %class.KV.6*, %class.KV.6** %10, align 8
  %86 = bitcast %class.KV.6* %85 to i8*
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = mul i64 %88, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %84, i8* %86, i64 %89, i32 8, i1 false)
  %90 = load %class.KV.6*, %class.KV.6** %16, align 8
  %91 = load i32, i32* %15, align 4
  %92 = zext i32 %91 to i64
  %93 = getelementptr inbounds %class.KV.6, %class.KV.6* %90, i64 %92
  %94 = bitcast %class.KV.6* %93 to i8*
  %95 = load %class.KV.6*, %class.KV.6** %10, align 8
  %96 = load i32, i32* %15, align 4
  %97 = add i32 %96, 1
  %98 = zext i32 %97 to i64
  %99 = getelementptr inbounds %class.KV.6, %class.KV.6* %95, i64 %98
  %100 = bitcast %class.KV.6* %99 to i8*
  %101 = load i32, i32* %13, align 4
  %102 = sub i32 %101, 1
  %103 = load i32, i32* %15, align 4
  %104 = sub i32 %102, %103
  %105 = zext i32 %104 to i64
  %106 = mul i64 %105, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %94, i8* %100, i64 %106, i32 8, i1 false)
  %107 = load i64, i64* %11, align 8
  %108 = load i32, i32* %12, align 4
  %109 = zext i32 %108 to i64
  %110 = shl i64 1, %109
  %111 = xor i64 -1, %110
  %112 = and i64 %107, %111
  %113 = shl i64 %112, 1
  %114 = or i64 %113, 1
  store i64 %114, i64* %17, align 8
  %115 = load i64, i64* %17, align 8
  %116 = load %class.KV.6*, %class.KV.6** %16, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %0, i64 %115, %class.KV.6* %116)
  br label %147

; <label>:117:                                    ; preds = %63
  %118 = load %class.KV.5*, %class.KV.5** %6, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2ERKS1_(%class.KV.5* %0, %class.KV.5* dereferenceable(16) %118)
  br label %147

; <label>:119:                                    ; preds = %45
  %120 = load %class.KV.6*, %class.KV.6** %10, align 8
  %121 = load i32, i32* %15, align 4
  %122 = zext i32 %121 to i64
  %123 = getelementptr inbounds %class.KV.6, %class.KV.6* %120, i64 %122
  %124 = load i64, i64* %7, align 8
  %125 = lshr i64 %124, 6
  %126 = load %class.key*, %class.key** %8, align 8
  %127 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3keyS0_Lj7EE12remove_innerERKS1_mPKS0_Pm(%class.KV.6* sret %18, %class.KV.6* dereferenceable(16) %123, i64 %125, %class.key* %126, i64* %127)
  %128 = load %class.KV.6*, %class.KV.6** %10, align 8
  %129 = load i32, i32* %15, align 4
  %130 = zext i32 %129 to i64
  %131 = getelementptr inbounds %class.KV.6, %class.KV.6* %128, i64 %130
  %132 = call zeroext i1 @_ZNK2KVI3keyS0_Lj7EEeqERKS1_(%class.KV.6* %18, %class.KV.6* dereferenceable(16) %131)
  br i1 %132, label %133, label %135

; <label>:133:                                    ; preds = %119
  %134 = load %class.KV.5*, %class.KV.5** %6, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2ERKS1_(%class.KV.5* %0, %class.KV.5* dereferenceable(16) %134)
  br label %147

; <label>:135:                                    ; preds = %119
  %136 = load %class.KV.6*, %class.KV.6** %10, align 8
  %137 = load i32, i32* %13, align 4
  %138 = load i32, i32* %15, align 4
  %139 = call %class.KV.6* @_ZN2KVI3keyS0_Lj7EE11update_nodeEPKS1_jjRS2_(%class.KV.6* %136, i32 %137, i32 %138, %class.KV.6* dereferenceable(16) %18)
  store %class.KV.6* %139, %class.KV.6** %19, align 8
  %140 = load %class.KV.5*, %class.KV.5** %6, align 8
  %141 = getelementptr inbounds %class.KV.5, %class.KV.5* %140, i32 0, i32 0
  %142 = bitcast %"union.KV<key, key, 6>::Key"* %141 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = load %class.KV.6*, %class.KV.6** %19, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2EmPKS_IS0_S0_Lj7EE(%class.KV.5* %0, i64 %143, %class.KV.6* %144)
  br label %147

; <label>:145:                                    ; preds = %5
  %146 = load %class.KV.5*, %class.KV.5** %6, align 8
  call void @_ZN2KVI3keyS0_Lj6EEC2ERKS1_(%class.KV.5* %0, %class.KV.5* dereferenceable(16) %146)
  br label %147

; <label>:147:                                    ; preds = %145, %135, %133, %117, %73
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj6EEeqERKS1_(%class.KV.5*, %class.KV.5* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV.5*, align 8
  %4 = alloca %class.KV.5*, align 8
  store %class.KV.5* %0, %class.KV.5** %3, align 8
  store %class.KV.5* %1, %class.KV.5** %4, align 8
  %5 = load %class.KV.5*, %class.KV.5** %3, align 8
  %6 = getelementptr inbounds %class.KV.5, %class.KV.5* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 6>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.5*, %class.KV.5** %4, align 8
  %10 = getelementptr inbounds %class.KV.5, %class.KV.5* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 6>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV.5, %class.KV.5* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 6>::Val"* %15 to %class.KV.6**
  %17 = load %class.KV.6*, %class.KV.6** %16, align 8
  %18 = load %class.KV.5*, %class.KV.5** %4, align 8
  %19 = getelementptr inbounds %class.KV.5, %class.KV.5* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 6>::Val"* %19 to %class.KV.6**
  %21 = load %class.KV.6*, %class.KV.6** %20, align 8
  %22 = icmp eq %class.KV.6* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj7EE12remove_innerERKS1_mPKS0_Pm(%class.KV.6* noalias sret, %class.KV.6* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV.6*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.7*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i8, align 1
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.7*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.7, align 8
  %19 = alloca %class.KV.7*, align 8
  store %class.KV.6* %1, %class.KV.6** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.6*, %class.KV.6** %6, align 8
  %21 = getelementptr inbounds %class.KV.6, %class.KV.6* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, key, 7>::Val"* %21 to %class.KV.7**
  %23 = load %class.KV.7*, %class.KV.7** %22, align 8
  store %class.KV.7* %23, %class.KV.7** %10, align 8
  %24 = load %class.KV.6*, %class.KV.6** %6, align 8
  %25 = getelementptr inbounds %class.KV.6, %class.KV.6* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, key, 7>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %12, align 4
  %33 = load i64, i64* %11, align 8
  %34 = call i64 @llvm.ctpop.i64(i64 %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %13, align 4
  %36 = load i64, i64* %11, align 8
  %37 = load i32, i32* %12, align 4
  %38 = zext i32 %37 to i64
  %39 = shl i64 1, %38
  %40 = and i64 %36, %39
  %41 = icmp ne i64 %40, 0
  %42 = zext i1 %41 to i8
  store i8 %42, i8* %14, align 1
  %43 = load i8, i8* %14, align 1
  %44 = trunc i8 %43 to i1
  br i1 %44, label %45, label %145

; <label>:45:                                     ; preds = %5
  %46 = load i64, i64* %11, align 8
  %47 = shl i64 %46, 1
  %48 = load i32, i32* %12, align 4
  %49 = sub i32 63, %48
  %50 = zext i32 %49 to i64
  %51 = shl i64 %47, %50
  %52 = call i64 @llvm.ctpop.i64(i64 %51)
  %53 = trunc i64 %52 to i32
  store i32 %53, i32* %15, align 4
  %54 = load %class.KV.7*, %class.KV.7** %10, align 8
  %55 = load i32, i32* %15, align 4
  %56 = zext i32 %55 to i64
  %57 = getelementptr inbounds %class.KV.7, %class.KV.7* %54, i64 %56
  %58 = getelementptr inbounds %class.KV.7, %class.KV.7* %57, i32 0, i32 0
  %59 = bitcast %"union.KV<key, key, 8>::Key"* %58 to i64*
  %60 = load i64, i64* %59, align 8
  %61 = and i64 %60, 1
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %63, label %119

; <label>:63:                                     ; preds = %45
  %64 = load %class.KV.7*, %class.KV.7** %10, align 8
  %65 = load i32, i32* %15, align 4
  %66 = zext i32 %65 to i64
  %67 = getelementptr inbounds %class.KV.7, %class.KV.7* %64, i64 %66
  %68 = getelementptr inbounds %class.KV.7, %class.KV.7* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<key, key, 8>::Key"* %68 to %class.key**
  %70 = load %class.key*, %class.key** %69, align 8
  %71 = load %class.key*, %class.key** %8, align 8
  %72 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %70, %class.key* dereferenceable(8) %71)
  br i1 %72, label %73, label %117

; <label>:73:                                     ; preds = %63
  %74 = load i64*, i64** %9, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, -1
  store i64 %76, i64* %74, align 8
  %77 = load i32, i32* %13, align 4
  %78 = sub i32 %77, 1
  %79 = zext i32 %78 to i64
  %80 = mul i64 %79, 16
  %81 = call noalias i8* @malloc(i64 %80) #8
  %82 = bitcast i8* %81 to %class.KV.7*
  store %class.KV.7* %82, %class.KV.7** %16, align 8
  %83 = load %class.KV.7*, %class.KV.7** %16, align 8
  %84 = bitcast %class.KV.7* %83 to i8*
  %85 = load %class.KV.7*, %class.KV.7** %10, align 8
  %86 = bitcast %class.KV.7* %85 to i8*
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = mul i64 %88, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %84, i8* %86, i64 %89, i32 8, i1 false)
  %90 = load %class.KV.7*, %class.KV.7** %16, align 8
  %91 = load i32, i32* %15, align 4
  %92 = zext i32 %91 to i64
  %93 = getelementptr inbounds %class.KV.7, %class.KV.7* %90, i64 %92
  %94 = bitcast %class.KV.7* %93 to i8*
  %95 = load %class.KV.7*, %class.KV.7** %10, align 8
  %96 = load i32, i32* %15, align 4
  %97 = add i32 %96, 1
  %98 = zext i32 %97 to i64
  %99 = getelementptr inbounds %class.KV.7, %class.KV.7* %95, i64 %98
  %100 = bitcast %class.KV.7* %99 to i8*
  %101 = load i32, i32* %13, align 4
  %102 = sub i32 %101, 1
  %103 = load i32, i32* %15, align 4
  %104 = sub i32 %102, %103
  %105 = zext i32 %104 to i64
  %106 = mul i64 %105, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %94, i8* %100, i64 %106, i32 8, i1 false)
  %107 = load i64, i64* %11, align 8
  %108 = load i32, i32* %12, align 4
  %109 = zext i32 %108 to i64
  %110 = shl i64 1, %109
  %111 = xor i64 -1, %110
  %112 = and i64 %107, %111
  %113 = shl i64 %112, 1
  %114 = or i64 %113, 1
  store i64 %114, i64* %17, align 8
  %115 = load i64, i64* %17, align 8
  %116 = load %class.KV.7*, %class.KV.7** %16, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %0, i64 %115, %class.KV.7* %116)
  br label %147

; <label>:117:                                    ; preds = %63
  %118 = load %class.KV.6*, %class.KV.6** %6, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2ERKS1_(%class.KV.6* %0, %class.KV.6* dereferenceable(16) %118)
  br label %147

; <label>:119:                                    ; preds = %45
  %120 = load %class.KV.7*, %class.KV.7** %10, align 8
  %121 = load i32, i32* %15, align 4
  %122 = zext i32 %121 to i64
  %123 = getelementptr inbounds %class.KV.7, %class.KV.7* %120, i64 %122
  %124 = load i64, i64* %7, align 8
  %125 = lshr i64 %124, 6
  %126 = load %class.key*, %class.key** %8, align 8
  %127 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3keyS0_Lj8EE12remove_innerERKS1_mPKS0_Pm(%class.KV.7* sret %18, %class.KV.7* dereferenceable(16) %123, i64 %125, %class.key* %126, i64* %127)
  %128 = load %class.KV.7*, %class.KV.7** %10, align 8
  %129 = load i32, i32* %15, align 4
  %130 = zext i32 %129 to i64
  %131 = getelementptr inbounds %class.KV.7, %class.KV.7* %128, i64 %130
  %132 = call zeroext i1 @_ZNK2KVI3keyS0_Lj8EEeqERKS1_(%class.KV.7* %18, %class.KV.7* dereferenceable(16) %131)
  br i1 %132, label %133, label %135

; <label>:133:                                    ; preds = %119
  %134 = load %class.KV.6*, %class.KV.6** %6, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2ERKS1_(%class.KV.6* %0, %class.KV.6* dereferenceable(16) %134)
  br label %147

; <label>:135:                                    ; preds = %119
  %136 = load %class.KV.7*, %class.KV.7** %10, align 8
  %137 = load i32, i32* %13, align 4
  %138 = load i32, i32* %15, align 4
  %139 = call %class.KV.7* @_ZN2KVI3keyS0_Lj8EE11update_nodeEPKS1_jjRS2_(%class.KV.7* %136, i32 %137, i32 %138, %class.KV.7* dereferenceable(16) %18)
  store %class.KV.7* %139, %class.KV.7** %19, align 8
  %140 = load %class.KV.6*, %class.KV.6** %6, align 8
  %141 = getelementptr inbounds %class.KV.6, %class.KV.6* %140, i32 0, i32 0
  %142 = bitcast %"union.KV<key, key, 7>::Key"* %141 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = load %class.KV.7*, %class.KV.7** %19, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2EmPKS_IS0_S0_Lj8EE(%class.KV.6* %0, i64 %143, %class.KV.7* %144)
  br label %147

; <label>:145:                                    ; preds = %5
  %146 = load %class.KV.6*, %class.KV.6** %6, align 8
  call void @_ZN2KVI3keyS0_Lj7EEC2ERKS1_(%class.KV.6* %0, %class.KV.6* dereferenceable(16) %146)
  br label %147

; <label>:147:                                    ; preds = %145, %135, %133, %117, %73
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj7EEeqERKS1_(%class.KV.6*, %class.KV.6* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV.6*, align 8
  %4 = alloca %class.KV.6*, align 8
  store %class.KV.6* %0, %class.KV.6** %3, align 8
  store %class.KV.6* %1, %class.KV.6** %4, align 8
  %5 = load %class.KV.6*, %class.KV.6** %3, align 8
  %6 = getelementptr inbounds %class.KV.6, %class.KV.6* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 7>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.6*, %class.KV.6** %4, align 8
  %10 = getelementptr inbounds %class.KV.6, %class.KV.6* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 7>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV.6, %class.KV.6* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 7>::Val"* %15 to %class.KV.7**
  %17 = load %class.KV.7*, %class.KV.7** %16, align 8
  %18 = load %class.KV.6*, %class.KV.6** %4, align 8
  %19 = getelementptr inbounds %class.KV.6, %class.KV.6* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 7>::Val"* %19 to %class.KV.7**
  %21 = load %class.KV.7*, %class.KV.7** %20, align 8
  %22 = icmp eq %class.KV.7* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj8EE12remove_innerERKS1_mPKS0_Pm(%class.KV.7* noalias sret, %class.KV.7* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV.7*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.8*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i8, align 1
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.8*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.8, align 8
  %19 = alloca %class.KV.8*, align 8
  store %class.KV.7* %1, %class.KV.7** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.7*, %class.KV.7** %6, align 8
  %21 = getelementptr inbounds %class.KV.7, %class.KV.7* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, key, 8>::Val"* %21 to %class.KV.8**
  %23 = load %class.KV.8*, %class.KV.8** %22, align 8
  store %class.KV.8* %23, %class.KV.8** %10, align 8
  %24 = load %class.KV.7*, %class.KV.7** %6, align 8
  %25 = getelementptr inbounds %class.KV.7, %class.KV.7* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, key, 8>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %12, align 4
  %33 = load i64, i64* %11, align 8
  %34 = call i64 @llvm.ctpop.i64(i64 %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %13, align 4
  %36 = load i64, i64* %11, align 8
  %37 = load i32, i32* %12, align 4
  %38 = zext i32 %37 to i64
  %39 = shl i64 1, %38
  %40 = and i64 %36, %39
  %41 = icmp ne i64 %40, 0
  %42 = zext i1 %41 to i8
  store i8 %42, i8* %14, align 1
  %43 = load i8, i8* %14, align 1
  %44 = trunc i8 %43 to i1
  br i1 %44, label %45, label %145

; <label>:45:                                     ; preds = %5
  %46 = load i64, i64* %11, align 8
  %47 = shl i64 %46, 1
  %48 = load i32, i32* %12, align 4
  %49 = sub i32 63, %48
  %50 = zext i32 %49 to i64
  %51 = shl i64 %47, %50
  %52 = call i64 @llvm.ctpop.i64(i64 %51)
  %53 = trunc i64 %52 to i32
  store i32 %53, i32* %15, align 4
  %54 = load %class.KV.8*, %class.KV.8** %10, align 8
  %55 = load i32, i32* %15, align 4
  %56 = zext i32 %55 to i64
  %57 = getelementptr inbounds %class.KV.8, %class.KV.8* %54, i64 %56
  %58 = getelementptr inbounds %class.KV.8, %class.KV.8* %57, i32 0, i32 0
  %59 = bitcast %"union.KV<key, key, 9>::Key"* %58 to i64*
  %60 = load i64, i64* %59, align 8
  %61 = and i64 %60, 1
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %63, label %119

; <label>:63:                                     ; preds = %45
  %64 = load %class.KV.8*, %class.KV.8** %10, align 8
  %65 = load i32, i32* %15, align 4
  %66 = zext i32 %65 to i64
  %67 = getelementptr inbounds %class.KV.8, %class.KV.8* %64, i64 %66
  %68 = getelementptr inbounds %class.KV.8, %class.KV.8* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<key, key, 9>::Key"* %68 to %class.key**
  %70 = load %class.key*, %class.key** %69, align 8
  %71 = load %class.key*, %class.key** %8, align 8
  %72 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %70, %class.key* dereferenceable(8) %71)
  br i1 %72, label %73, label %117

; <label>:73:                                     ; preds = %63
  %74 = load i64*, i64** %9, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, -1
  store i64 %76, i64* %74, align 8
  %77 = load i32, i32* %13, align 4
  %78 = sub i32 %77, 1
  %79 = zext i32 %78 to i64
  %80 = mul i64 %79, 16
  %81 = call noalias i8* @malloc(i64 %80) #8
  %82 = bitcast i8* %81 to %class.KV.8*
  store %class.KV.8* %82, %class.KV.8** %16, align 8
  %83 = load %class.KV.8*, %class.KV.8** %16, align 8
  %84 = bitcast %class.KV.8* %83 to i8*
  %85 = load %class.KV.8*, %class.KV.8** %10, align 8
  %86 = bitcast %class.KV.8* %85 to i8*
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = mul i64 %88, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %84, i8* %86, i64 %89, i32 8, i1 false)
  %90 = load %class.KV.8*, %class.KV.8** %16, align 8
  %91 = load i32, i32* %15, align 4
  %92 = zext i32 %91 to i64
  %93 = getelementptr inbounds %class.KV.8, %class.KV.8* %90, i64 %92
  %94 = bitcast %class.KV.8* %93 to i8*
  %95 = load %class.KV.8*, %class.KV.8** %10, align 8
  %96 = load i32, i32* %15, align 4
  %97 = add i32 %96, 1
  %98 = zext i32 %97 to i64
  %99 = getelementptr inbounds %class.KV.8, %class.KV.8* %95, i64 %98
  %100 = bitcast %class.KV.8* %99 to i8*
  %101 = load i32, i32* %13, align 4
  %102 = sub i32 %101, 1
  %103 = load i32, i32* %15, align 4
  %104 = sub i32 %102, %103
  %105 = zext i32 %104 to i64
  %106 = mul i64 %105, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %94, i8* %100, i64 %106, i32 8, i1 false)
  %107 = load i64, i64* %11, align 8
  %108 = load i32, i32* %12, align 4
  %109 = zext i32 %108 to i64
  %110 = shl i64 1, %109
  %111 = xor i64 -1, %110
  %112 = and i64 %107, %111
  %113 = shl i64 %112, 1
  %114 = or i64 %113, 1
  store i64 %114, i64* %17, align 8
  %115 = load i64, i64* %17, align 8
  %116 = load %class.KV.8*, %class.KV.8** %16, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %0, i64 %115, %class.KV.8* %116)
  br label %147

; <label>:117:                                    ; preds = %63
  %118 = load %class.KV.7*, %class.KV.7** %6, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2ERKS1_(%class.KV.7* %0, %class.KV.7* dereferenceable(16) %118)
  br label %147

; <label>:119:                                    ; preds = %45
  %120 = load %class.KV.8*, %class.KV.8** %10, align 8
  %121 = load i32, i32* %15, align 4
  %122 = zext i32 %121 to i64
  %123 = getelementptr inbounds %class.KV.8, %class.KV.8* %120, i64 %122
  %124 = load i64, i64* %7, align 8
  %125 = lshr i64 %124, 6
  %126 = load %class.key*, %class.key** %8, align 8
  %127 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3keyS0_Lj9EE12remove_innerERKS1_mPKS0_Pm(%class.KV.8* sret %18, %class.KV.8* dereferenceable(16) %123, i64 %125, %class.key* %126, i64* %127)
  %128 = load %class.KV.8*, %class.KV.8** %10, align 8
  %129 = load i32, i32* %15, align 4
  %130 = zext i32 %129 to i64
  %131 = getelementptr inbounds %class.KV.8, %class.KV.8* %128, i64 %130
  %132 = call zeroext i1 @_ZNK2KVI3keyS0_Lj9EEeqERKS1_(%class.KV.8* %18, %class.KV.8* dereferenceable(16) %131)
  br i1 %132, label %133, label %135

; <label>:133:                                    ; preds = %119
  %134 = load %class.KV.7*, %class.KV.7** %6, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2ERKS1_(%class.KV.7* %0, %class.KV.7* dereferenceable(16) %134)
  br label %147

; <label>:135:                                    ; preds = %119
  %136 = load %class.KV.8*, %class.KV.8** %10, align 8
  %137 = load i32, i32* %13, align 4
  %138 = load i32, i32* %15, align 4
  %139 = call %class.KV.8* @_ZN2KVI3keyS0_Lj9EE11update_nodeEPKS1_jjRS2_(%class.KV.8* %136, i32 %137, i32 %138, %class.KV.8* dereferenceable(16) %18)
  store %class.KV.8* %139, %class.KV.8** %19, align 8
  %140 = load %class.KV.7*, %class.KV.7** %6, align 8
  %141 = getelementptr inbounds %class.KV.7, %class.KV.7* %140, i32 0, i32 0
  %142 = bitcast %"union.KV<key, key, 8>::Key"* %141 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = load %class.KV.8*, %class.KV.8** %19, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2EmPKS_IS0_S0_Lj9EE(%class.KV.7* %0, i64 %143, %class.KV.8* %144)
  br label %147

; <label>:145:                                    ; preds = %5
  %146 = load %class.KV.7*, %class.KV.7** %6, align 8
  call void @_ZN2KVI3keyS0_Lj8EEC2ERKS1_(%class.KV.7* %0, %class.KV.7* dereferenceable(16) %146)
  br label %147

; <label>:147:                                    ; preds = %145, %135, %133, %117, %73
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj8EEeqERKS1_(%class.KV.7*, %class.KV.7* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV.7*, align 8
  %4 = alloca %class.KV.7*, align 8
  store %class.KV.7* %0, %class.KV.7** %3, align 8
  store %class.KV.7* %1, %class.KV.7** %4, align 8
  %5 = load %class.KV.7*, %class.KV.7** %3, align 8
  %6 = getelementptr inbounds %class.KV.7, %class.KV.7* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 8>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.7*, %class.KV.7** %4, align 8
  %10 = getelementptr inbounds %class.KV.7, %class.KV.7* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 8>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV.7, %class.KV.7* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 8>::Val"* %15 to %class.KV.8**
  %17 = load %class.KV.8*, %class.KV.8** %16, align 8
  %18 = load %class.KV.7*, %class.KV.7** %4, align 8
  %19 = getelementptr inbounds %class.KV.7, %class.KV.7* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 8>::Val"* %19 to %class.KV.8**
  %21 = load %class.KV.8*, %class.KV.8** %20, align 8
  %22 = icmp eq %class.KV.8* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj9EE12remove_innerERKS1_mPKS0_Pm(%class.KV.8* noalias sret, %class.KV.8* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV.8*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.KV.9*, align 8
  %11 = alloca i64, align 8
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i8, align 1
  %15 = alloca i32, align 4
  %16 = alloca %class.KV.9*, align 8
  %17 = alloca i64, align 8
  %18 = alloca %class.KV.9, align 8
  %19 = alloca %class.KV.9*, align 8
  store %class.KV.8* %1, %class.KV.8** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %20 = load %class.KV.8*, %class.KV.8** %6, align 8
  %21 = getelementptr inbounds %class.KV.8, %class.KV.8* %20, i32 0, i32 1
  %22 = bitcast %"union.KV<key, key, 9>::Val"* %21 to %class.KV.9**
  %23 = load %class.KV.9*, %class.KV.9** %22, align 8
  store %class.KV.9* %23, %class.KV.9** %10, align 8
  %24 = load %class.KV.8*, %class.KV.8** %6, align 8
  %25 = getelementptr inbounds %class.KV.8, %class.KV.8* %24, i32 0, i32 0
  %26 = bitcast %"union.KV<key, key, 9>::Key"* %25 to i64*
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 1
  store i64 %28, i64* %11, align 8
  %29 = load i64, i64* %7, align 8
  %30 = and i64 %29, 63
  %31 = urem i64 %30, 63
  %32 = trunc i64 %31 to i32
  store i32 %32, i32* %12, align 4
  %33 = load i64, i64* %11, align 8
  %34 = call i64 @llvm.ctpop.i64(i64 %33)
  %35 = trunc i64 %34 to i32
  store i32 %35, i32* %13, align 4
  %36 = load i64, i64* %11, align 8
  %37 = load i32, i32* %12, align 4
  %38 = zext i32 %37 to i64
  %39 = shl i64 1, %38
  %40 = and i64 %36, %39
  %41 = icmp ne i64 %40, 0
  %42 = zext i1 %41 to i8
  store i8 %42, i8* %14, align 1
  %43 = load i8, i8* %14, align 1
  %44 = trunc i8 %43 to i1
  br i1 %44, label %45, label %145

; <label>:45:                                     ; preds = %5
  %46 = load i64, i64* %11, align 8
  %47 = shl i64 %46, 1
  %48 = load i32, i32* %12, align 4
  %49 = sub i32 63, %48
  %50 = zext i32 %49 to i64
  %51 = shl i64 %47, %50
  %52 = call i64 @llvm.ctpop.i64(i64 %51)
  %53 = trunc i64 %52 to i32
  store i32 %53, i32* %15, align 4
  %54 = load %class.KV.9*, %class.KV.9** %10, align 8
  %55 = load i32, i32* %15, align 4
  %56 = zext i32 %55 to i64
  %57 = getelementptr inbounds %class.KV.9, %class.KV.9* %54, i64 %56
  %58 = getelementptr inbounds %class.KV.9, %class.KV.9* %57, i32 0, i32 0
  %59 = bitcast %"union.KV<key, key, 10>::Key"* %58 to i64*
  %60 = load i64, i64* %59, align 8
  %61 = and i64 %60, 1
  %62 = icmp eq i64 %61, 0
  br i1 %62, label %63, label %119

; <label>:63:                                     ; preds = %45
  %64 = load %class.KV.9*, %class.KV.9** %10, align 8
  %65 = load i32, i32* %15, align 4
  %66 = zext i32 %65 to i64
  %67 = getelementptr inbounds %class.KV.9, %class.KV.9* %64, i64 %66
  %68 = getelementptr inbounds %class.KV.9, %class.KV.9* %67, i32 0, i32 0
  %69 = bitcast %"union.KV<key, key, 10>::Key"* %68 to %class.key**
  %70 = load %class.key*, %class.key** %69, align 8
  %71 = load %class.key*, %class.key** %8, align 8
  %72 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %70, %class.key* dereferenceable(8) %71)
  br i1 %72, label %73, label %117

; <label>:73:                                     ; preds = %63
  %74 = load i64*, i64** %9, align 8
  %75 = load i64, i64* %74, align 8
  %76 = add i64 %75, -1
  store i64 %76, i64* %74, align 8
  %77 = load i32, i32* %13, align 4
  %78 = sub i32 %77, 1
  %79 = zext i32 %78 to i64
  %80 = mul i64 %79, 16
  %81 = call noalias i8* @malloc(i64 %80) #8
  %82 = bitcast i8* %81 to %class.KV.9*
  store %class.KV.9* %82, %class.KV.9** %16, align 8
  %83 = load %class.KV.9*, %class.KV.9** %16, align 8
  %84 = bitcast %class.KV.9* %83 to i8*
  %85 = load %class.KV.9*, %class.KV.9** %10, align 8
  %86 = bitcast %class.KV.9* %85 to i8*
  %87 = load i32, i32* %15, align 4
  %88 = zext i32 %87 to i64
  %89 = mul i64 %88, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %84, i8* %86, i64 %89, i32 8, i1 false)
  %90 = load %class.KV.9*, %class.KV.9** %16, align 8
  %91 = load i32, i32* %15, align 4
  %92 = zext i32 %91 to i64
  %93 = getelementptr inbounds %class.KV.9, %class.KV.9* %90, i64 %92
  %94 = bitcast %class.KV.9* %93 to i8*
  %95 = load %class.KV.9*, %class.KV.9** %10, align 8
  %96 = load i32, i32* %15, align 4
  %97 = add i32 %96, 1
  %98 = zext i32 %97 to i64
  %99 = getelementptr inbounds %class.KV.9, %class.KV.9* %95, i64 %98
  %100 = bitcast %class.KV.9* %99 to i8*
  %101 = load i32, i32* %13, align 4
  %102 = sub i32 %101, 1
  %103 = load i32, i32* %15, align 4
  %104 = sub i32 %102, %103
  %105 = zext i32 %104 to i64
  %106 = mul i64 %105, 16
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %94, i8* %100, i64 %106, i32 8, i1 false)
  %107 = load i64, i64* %11, align 8
  %108 = load i32, i32* %12, align 4
  %109 = zext i32 %108 to i64
  %110 = shl i64 1, %109
  %111 = xor i64 -1, %110
  %112 = and i64 %107, %111
  %113 = shl i64 %112, 1
  %114 = or i64 %113, 1
  store i64 %114, i64* %17, align 8
  %115 = load i64, i64* %17, align 8
  %116 = load %class.KV.9*, %class.KV.9** %16, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %0, i64 %115, %class.KV.9* %116)
  br label %147

; <label>:117:                                    ; preds = %63
  %118 = load %class.KV.8*, %class.KV.8** %6, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2ERKS1_(%class.KV.8* %0, %class.KV.8* dereferenceable(16) %118)
  br label %147

; <label>:119:                                    ; preds = %45
  %120 = load %class.KV.9*, %class.KV.9** %10, align 8
  %121 = load i32, i32* %15, align 4
  %122 = zext i32 %121 to i64
  %123 = getelementptr inbounds %class.KV.9, %class.KV.9* %120, i64 %122
  %124 = load i64, i64* %7, align 8
  %125 = lshr i64 %124, 6
  %126 = load %class.key*, %class.key** %8, align 8
  %127 = load i64*, i64** %9, align 8
  call void @_ZN2KVI3keyS0_Lj10EE12remove_innerERKS1_mPKS0_Pm(%class.KV.9* sret %18, %class.KV.9* dereferenceable(16) %123, i64 %125, %class.key* %126, i64* %127)
  %128 = load %class.KV.9*, %class.KV.9** %10, align 8
  %129 = load i32, i32* %15, align 4
  %130 = zext i32 %129 to i64
  %131 = getelementptr inbounds %class.KV.9, %class.KV.9* %128, i64 %130
  %132 = call zeroext i1 @_ZNK2KVI3keyS0_Lj10EEeqERKS1_(%class.KV.9* %18, %class.KV.9* dereferenceable(16) %131)
  br i1 %132, label %133, label %135

; <label>:133:                                    ; preds = %119
  %134 = load %class.KV.8*, %class.KV.8** %6, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2ERKS1_(%class.KV.8* %0, %class.KV.8* dereferenceable(16) %134)
  br label %147

; <label>:135:                                    ; preds = %119
  %136 = load %class.KV.9*, %class.KV.9** %10, align 8
  %137 = load i32, i32* %13, align 4
  %138 = load i32, i32* %15, align 4
  %139 = call %class.KV.9* @_ZN2KVI3keyS0_Lj10EE11update_nodeEPKS1_jjRS2_(%class.KV.9* %136, i32 %137, i32 %138, %class.KV.9* dereferenceable(16) %18)
  store %class.KV.9* %139, %class.KV.9** %19, align 8
  %140 = load %class.KV.8*, %class.KV.8** %6, align 8
  %141 = getelementptr inbounds %class.KV.8, %class.KV.8* %140, i32 0, i32 0
  %142 = bitcast %"union.KV<key, key, 9>::Key"* %141 to i64*
  %143 = load i64, i64* %142, align 8
  %144 = load %class.KV.9*, %class.KV.9** %19, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2EmPKS_IS0_S0_Lj10EE(%class.KV.8* %0, i64 %143, %class.KV.9* %144)
  br label %147

; <label>:145:                                    ; preds = %5
  %146 = load %class.KV.8*, %class.KV.8** %6, align 8
  call void @_ZN2KVI3keyS0_Lj9EEC2ERKS1_(%class.KV.8* %0, %class.KV.8* dereferenceable(16) %146)
  br label %147

; <label>:147:                                    ; preds = %145, %135, %133, %117, %73
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj9EEeqERKS1_(%class.KV.8*, %class.KV.8* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV.8*, align 8
  %4 = alloca %class.KV.8*, align 8
  store %class.KV.8* %0, %class.KV.8** %3, align 8
  store %class.KV.8* %1, %class.KV.8** %4, align 8
  %5 = load %class.KV.8*, %class.KV.8** %3, align 8
  %6 = getelementptr inbounds %class.KV.8, %class.KV.8* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 9>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.8*, %class.KV.8** %4, align 8
  %10 = getelementptr inbounds %class.KV.8, %class.KV.8* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 9>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV.8, %class.KV.8* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 9>::Val"* %15 to %class.KV.9**
  %17 = load %class.KV.9*, %class.KV.9** %16, align 8
  %18 = load %class.KV.8*, %class.KV.8** %4, align 8
  %19 = getelementptr inbounds %class.KV.8, %class.KV.8* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 9>::Val"* %19 to %class.KV.9**
  %21 = load %class.KV.9*, %class.KV.9** %20, align 8
  %22 = icmp eq %class.KV.9* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr void @_ZN2KVI3keyS0_Lj10EE12remove_innerERKS1_mPKS0_Pm(%class.KV.9* noalias sret, %class.KV.9* dereferenceable(16), i64, %class.key*, i64*) #2 comdat align 2 {
  %6 = alloca %class.KV.9*, align 8
  %7 = alloca i64, align 8
  %8 = alloca %class.key*, align 8
  %9 = alloca i64*, align 8
  %10 = alloca %class.LL*, align 8
  store %class.KV.9* %1, %class.KV.9** %6, align 8
  store i64 %2, i64* %7, align 8
  store %class.key* %3, %class.key** %8, align 8
  store i64* %4, i64** %9, align 8
  %11 = load %class.KV.9*, %class.KV.9** %6, align 8
  %12 = getelementptr inbounds %class.KV.9, %class.KV.9* %11, i32 0, i32 1
  %13 = bitcast %"union.KV<key, key, 10>::Val"* %12 to %class.LL**
  %14 = load %class.LL*, %class.LL** %13, align 8
  %15 = icmp ne %class.LL* %14, null
  br i1 %15, label %16, label %34

; <label>:16:                                     ; preds = %5
  %17 = load %class.KV.9*, %class.KV.9** %6, align 8
  %18 = getelementptr inbounds %class.KV.9, %class.KV.9* %17, i32 0, i32 1
  %19 = bitcast %"union.KV<key, key, 10>::Val"* %18 to %class.LL**
  %20 = load %class.LL*, %class.LL** %19, align 8
  %21 = load %class.key*, %class.key** %8, align 8
  %22 = load i64*, i64** %9, align 8
  %23 = call %class.LL* @_ZNK2LLI3keyS0_E6removeEPKS0_Pm(%class.LL* %20, %class.key* %21, i64* %22)
  store %class.LL* %23, %class.LL** %10, align 8
  %24 = load %class.LL*, %class.LL** %10, align 8
  %25 = load %class.KV.9*, %class.KV.9** %6, align 8
  %26 = getelementptr inbounds %class.KV.9, %class.KV.9* %25, i32 0, i32 1
  %27 = bitcast %"union.KV<key, key, 10>::Val"* %26 to %class.LL**
  %28 = load %class.LL*, %class.LL** %27, align 8
  %29 = icmp eq %class.LL* %24, %28
  br i1 %29, label %30, label %32

; <label>:30:                                     ; preds = %16
  %31 = load %class.KV.9*, %class.KV.9** %6, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2ERKS1_(%class.KV.9* %0, %class.KV.9* dereferenceable(16) %31)
  br label %36

; <label>:32:                                     ; preds = %16
  %33 = load %class.LL*, %class.LL** %10, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2EmPK2LLIS0_S0_E(%class.KV.9* %0, i64 1, %class.LL* %33)
  br label %36

; <label>:34:                                     ; preds = %5
  %35 = load %class.KV.9*, %class.KV.9** %6, align 8
  call void @_ZN2KVI3keyS0_Lj10EEC2ERKS1_(%class.KV.9* %0, %class.KV.9* dereferenceable(16) %35)
  br label %36

; <label>:36:                                     ; preds = %34, %32, %30
  ret void
}

; Function Attrs: noinline nounwind optnone sspstrong uwtable
define linkonce_odr zeroext i1 @_ZNK2KVI3keyS0_Lj10EEeqERKS1_(%class.KV.9*, %class.KV.9* dereferenceable(16)) #0 comdat align 2 {
  %3 = alloca %class.KV.9*, align 8
  %4 = alloca %class.KV.9*, align 8
  store %class.KV.9* %0, %class.KV.9** %3, align 8
  store %class.KV.9* %1, %class.KV.9** %4, align 8
  %5 = load %class.KV.9*, %class.KV.9** %3, align 8
  %6 = getelementptr inbounds %class.KV.9, %class.KV.9* %5, i32 0, i32 0
  %7 = bitcast %"union.KV<key, key, 10>::Key"* %6 to i64*
  %8 = load i64, i64* %7, align 8
  %9 = load %class.KV.9*, %class.KV.9** %4, align 8
  %10 = getelementptr inbounds %class.KV.9, %class.KV.9* %9, i32 0, i32 0
  %11 = bitcast %"union.KV<key, key, 10>::Key"* %10 to i64*
  %12 = load i64, i64* %11, align 8
  %13 = icmp eq i64 %8, %12
  br i1 %13, label %14, label %23

; <label>:14:                                     ; preds = %2
  %15 = getelementptr inbounds %class.KV.9, %class.KV.9* %5, i32 0, i32 1
  %16 = bitcast %"union.KV<key, key, 10>::Val"* %15 to %class.key**
  %17 = load %class.key*, %class.key** %16, align 8
  %18 = load %class.KV.9*, %class.KV.9** %4, align 8
  %19 = getelementptr inbounds %class.KV.9, %class.KV.9* %18, i32 0, i32 1
  %20 = bitcast %"union.KV<key, key, 10>::Val"* %19 to %class.key**
  %21 = load %class.key*, %class.key** %20, align 8
  %22 = icmp eq %class.key* %17, %21
  br label %23

; <label>:23:                                     ; preds = %14, %2
  %24 = phi i1 [ false, %2 ], [ %22, %14 ]
  ret i1 %24
}

; Function Attrs: noinline optnone sspstrong uwtable
define linkonce_odr %class.LL* @_ZNK2LLI3keyS0_E6removeEPKS0_Pm(%class.LL*, %class.key*, i64*) #2 comdat align 2 {
  %4 = alloca %class.LL*, align 8
  %5 = alloca %class.LL*, align 8
  %6 = alloca %class.key*, align 8
  %7 = alloca i64*, align 8
  %8 = alloca %class.LL*, align 8
  store %class.LL* %0, %class.LL** %5, align 8
  store %class.key* %1, %class.key** %6, align 8
  store i64* %2, i64** %7, align 8
  %9 = load %class.LL*, %class.LL** %5, align 8
  %10 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 0
  %11 = load %class.key*, %class.key** %10, align 8
  %12 = load %class.key*, %class.key** %6, align 8
  %13 = call zeroext i1 @_ZNK3keyeqERKS_(%class.key* %11, %class.key* dereferenceable(8) %12)
  br i1 %13, label %14, label %20

; <label>:14:                                     ; preds = %3
  %15 = load i64*, i64** %7, align 8
  %16 = load i64, i64* %15, align 8
  %17 = add i64 %16, -1
  store i64 %17, i64* %15, align 8
  %18 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 2
  %19 = load %class.LL*, %class.LL** %18, align 8
  store %class.LL* %19, %class.LL** %4, align 8
  br label %46

; <label>:20:                                     ; preds = %3
  %21 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 2
  %22 = load %class.LL*, %class.LL** %21, align 8
  %23 = icmp ne %class.LL* %22, null
  br i1 %23, label %24, label %45

; <label>:24:                                     ; preds = %20
  %25 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 2
  %26 = load %class.LL*, %class.LL** %25, align 8
  %27 = load %class.key*, %class.key** %6, align 8
  %28 = load i64*, i64** %7, align 8
  %29 = call %class.LL* @_ZNK2LLI3keyS0_E6removeEPKS0_Pm(%class.LL* %26, %class.key* %27, i64* %28)
  store %class.LL* %29, %class.LL** %8, align 8
  %30 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 2
  %31 = load %class.LL*, %class.LL** %30, align 8
  %32 = load %class.LL*, %class.LL** %8, align 8
  %33 = icmp eq %class.LL* %31, %32
  br i1 %33, label %34, label %35

; <label>:34:                                     ; preds = %24
  store %class.LL* %9, %class.LL** %4, align 8
  br label %46

; <label>:35:                                     ; preds = %24
  %36 = call noalias i8* @malloc(i64 24) #8
  %37 = bitcast i8* %36 to %class.LL*
  %38 = bitcast %class.LL* %37 to i8*
  %39 = bitcast i8* %38 to %class.LL*
  %40 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 0
  %41 = load %class.key*, %class.key** %40, align 8
  %42 = getelementptr inbounds %class.LL, %class.LL* %9, i32 0, i32 1
  %43 = load %class.key*, %class.key** %42, align 8
  %44 = load %class.LL*, %class.LL** %8, align 8
  call void @_ZN2LLI3keyS0_EC2EPKS0_S3_PKS1_(%class.LL* %39, %class.key* %41, %class.key* %43, %class.LL* %44)
  store %class.LL* %39, %class.LL** %4, align 8
  br label %46

; <label>:45:                                     ; preds = %20
  store %class.LL* %9, %class.LL** %4, align 8
  br label %46

; <label>:46:                                     ; preds = %45, %35, %34, %14
  %47 = load %class.LL*, %class.LL** %4, align 8
  ret %class.LL* %47
}

attributes #0 = { noinline nounwind optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { noinline optnone sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { noreturn nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nobuiltin nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { argmemonly nounwind }
attributes #7 = { nounwind readnone speculatable }
attributes #8 = { nounwind }
attributes #9 = { noreturn nounwind }
attributes #10 = { builtin nounwind }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 7, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{!"clang version 5.0.0 (tags/RELEASE_500/final)"}
