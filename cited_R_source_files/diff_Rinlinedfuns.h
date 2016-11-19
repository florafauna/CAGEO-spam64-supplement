Index: src/include/Rinlinedfuns.h
===================================================================
--- src/include/Rinlinedfuns.h	(revision 59004)
+++ src/include/Rinlinedfuns.h	(revision 59009)
@@ -115,7 +115,39 @@
     }
 }
 
+INLINE_FUN R_xlen_t xlength(SEXP s)
+{
+    int i;
+    switch (TYPEOF(s)) {
+    case NILSXP:
+	return 0;
+    case LGLSXP:
+    case INTSXP:
+    case REALSXP:
+    case CPLXSXP:
+    case STRSXP:
+    case CHARSXP:
+    case VECSXP:
+    case EXPRSXP:
+    case RAWSXP:
+	return XLENGTH(s);
+    case LISTSXP:
+    case LANGSXP:
+    case DOTSXP:
+	i = 0;
+	while (s != NULL && s != R_NilValue) {
+	    i++;
+	    s = CDR(s);
+	}
+	return i;
+    case ENVSXP:
+	return Rf_envlength(s);
+    default:
+	return 1;
+    }
+}
 
+
 /* from list.c */
 /* Return a dotted pair with the given CAR and CDR. */
 /* The (R) TAG slot on the cell is set to NULL. */
