Index: src/include/Rinternals.h
===================================================================
--- src/include/Rinternals.h	(revision 59004)
+++ src/include/Rinternals.h	(revision 59009)
@@ -48,6 +48,19 @@
 /* type for length of vectors etc */
 typedef int R_len_t; /* will be long later, LONG64 or ssize_t on Win64 */
 #define R_LEN_T_MAX INT_MAX
+#if ( SIZEOF_SIZE_T > 4 )
+# define LONG_VECTOR_SUPPORT
+#endif
+#ifdef LONG_VECTOR_SUPPORT
+    typedef ptrdiff_t R_xlen_t;
+    typedef struct { R_xlen_t lv_length, lv_truelength; } R_long_vec_hdr_t;
+# define R_XLEN_T_MAX 4503599627370496
+# define R_SHORT_LEN_MAX 2147483647
+# define R_LONG_VEC_TOKEN -1
+#else
+    typedef int R_xlen_t;
+# define R_XLEN_T_MAX R_LEN_T_MAX
+#endif
 
 /* Fundamental Data Types:  These are largely Lisp
  * influenced structures, with the exception of LGLSXP,
@@ -264,10 +277,45 @@
 #define UNSET_S4_OBJECT(x) (((x)->sxpinfo.gp) &= ~S4_OBJECT_MASK)
 
 /* Vector Access Macros */
-#define LENGTH(x)	(((VECSEXP) (x))->vecsxp.length)
-#define TRUELENGTH(x)	(((VECSEXP) (x))->vecsxp.truelength)
-#define SETLENGTH(x,v)		((((VECSEXP) (x))->vecsxp.length)=(v))
-#define SET_TRUELENGTH(x,v)	((((VECSEXP) (x))->vecsxp.truelength)=(v))
+#ifdef LONG_VECTOR_SUPPORT
+    R_len_t R_BadLongVector(SEXP, const char *, int);
+# define IS_LONG_VEC(x) (SHORT_VEC_LENGTH(x) == R_LONG_VEC_TOKEN)
+# define SHORT_VEC_LENGTH(x) (((VECSEXP) (x))->vecsxp.length)
+# define SHORT_VEC_TRUELENGTH(x) (((VECSEXP) (x))->vecsxp.truelength)
+# define LONG_VEC_LENGTH(x) ((R_long_vec_hdr_t *) (x))[-1].lv_length
+# define LONG_VEC_TRUELENGTH(x) ((R_long_vec_hdr_t *) (x))[-1].lv_truelength
+# define XLENGTH(x) (IS_LONG_VEC(x) ? LONG_VEC_LENGTH(x) : SHORT_VEC_LENGTH(x))
+# define XTRUELENGTH(x)	(IS_LONG_VEC(x) ? LONG_VEC_TRUELENGTH(x) : SHORT_VEC_TRUELENGTH(x))
+# define LENGTH(x) (IS_LONG_VEC(x) ? R_BadLongVector(x, __FILE__, __LINE__) : SHORT_VEC_LENGTH(x))
+# define TRUELENGTH(x) (IS_LONG_VEC(x) ? R_BadLongVector(x, __FILE__, __LINE__) : SHORT_VEC_TRUELENGTH(x))
+# define SET_SHORT_VEC_LENGTH(x,v) (SHORT_VEC_LENGTH(x) = (v))
+# define SET_SHORT_VEC_TRUELENGTH(x,v) (SHORT_VEC_TRUELENGTH(x) = (v))
+# define SET_LONG_VEC_LENGTH(x,v) (LONG_VEC_LENGTH(x) = (v))
+# define SET_LONG_VEC_TRUELENGTH(x,v) (LONG_VEC_TRUELENGTH(x) = (v))
+# define SETLENGTH(x,v) do { \
+      SEXP sl__x__ = (x); \
+      R_xlen_t sl__v__ = (v); \
+      if (IS_LONG_VEC(sl__x__)) \
+	  SET_LONG_VEC_LENGTH(sl__x__, sl__v__); \
+      else SET_SHORT_VEC_LENGTH(sl__x__, sl__v__); \
+  } while (0)
+# define SET_TRUELENGTH(x,v) do { \
+      SEXP sl__x__ = (x); \
+      R_xlen_t sl__v__ = (v); \
+      if (IS_LONG_VEC(sl__x__)) \
+	  SET_LONG_VEC_TRUELENGTH(sl__x__, sl__v__); \
+      else SET_SHORT_VEC_TRUELENGTH(sl__x__, sl__v__); \
+  } while (0)
+#else
+# define LENGTH(x)	(((VECSEXP) (x))->vecsxp.length)
+# define TRUELENGTH(x)	(((VECSEXP) (x))->vecsxp.truelength)
+# define XLENGTH(x) LENGTH(x)
+# define XTRUELENGTH(x) TRUELENGTH(x)
+# define SETLENGTH(x,v)		((((VECSEXP) (x))->vecsxp.length)=(v))
+# define SET_TRUELENGTH(x,v)	((((VECSEXP) (x))->vecsxp.truelength)=(v))
+# define SET_SHORT_VEC_LENGTH SETLENGTH
+# define SET_SHORT_VEC_TRUELENGTH SET_TRUELENGTH
+#endif
 
 /* Under the generational allocator the data for vector nodes comes
    immediately after the node structure, so the data address is a
@@ -380,6 +428,9 @@
 int  (TRUELENGTH)(SEXP x);
 void (SETLENGTH)(SEXP x, int v);
 void (SET_TRUELENGTH)(SEXP x, int v);
+R_xlen_t  (XLENGTH)(SEXP x);
+R_xlen_t  (XTRUELENGTH)(SEXP x);
+int  (IS_LONG_VEC)(SEXP x);
 int  (LEVELS)(SEXP x);
 int  (SETLEVELS)(SEXP x, int v);
 
@@ -577,7 +628,7 @@
 SEXP Rf_allocList(int);
 SEXP Rf_allocS4Object(void);
 SEXP Rf_allocSExp(SEXPTYPE);
-SEXP Rf_allocVector(SEXPTYPE, R_len_t);
+SEXP Rf_allocVector(SEXPTYPE, R_xlen_t);
 int  Rf_any_duplicated(SEXP x, Rboolean from_last);
 int  Rf_any_duplicated3(SEXP x, SEXP incomp, Rboolean from_last);
 SEXP Rf_applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP);
