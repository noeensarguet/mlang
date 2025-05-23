#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#define CAML_NAME_SPACE
#include "caml/version.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/custom.h"

#include "mlang.h"

#if OCAML_VERSION < 41200

#define Val_none Val_int(0)

CAMLexport value caml_alloc_some(value v) {
  CAMLparam1(v);
  value some = caml_alloc_small(1, 0);
  Field(some, 0) = v;
  CAMLreturn(some);
}

#endif

#define Tgv_val(v) (*((T_irdata **)Data_custom_val(v)))

static void finalize_tgv(value tgv_block){
  CAMLparam1(tgv_block);
  T_irdata *tgv = Tgv_val(tgv_block);
  detruis_irdata(tgv);
  CAMLreturn0;
}

static struct custom_operations tgv_block_ops = {
  "tgv.custom.block.ops",
  *finalize_tgv,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value ml_tgv_alloc(void){
  CAMLparam0();
  CAMLlocal1(mlTgv);
  T_irdata *tgv = cree_irdata();
  mlTgv = caml_alloc_custom(&tgv_block_ops, sizeof(T_irdata *), 0, 1);
  Tgv_val(mlTgv) = tgv;
  CAMLreturn(mlTgv);
}

CAMLprim value ml_init_errs(value mlTgv) {
  CAMLparam1(mlTgv);
  T_irdata *tgv = Tgv_val(mlTgv);
  for (int i = 0; i < tgv->sz_err_finalise; i++) {
    tgv->err_finalise[i] = NULL;
  }
  tgv->nb_err_finalise = 0;
  for (int i = 0; i < tgv->sz_err_sortie; i++) {
    tgv->err_sortie[i] = NULL;
  }
  tgv->nb_err_sortie = 0;
  for (int i = 0; i < tgv->sz_err_archive; i++) {
    tgv->err_archive[i] = NULL;
  }
  tgv->nb_err_archive = 0;
  CAMLreturn(Val_unit);
}

CAMLprim value ml_export_errs(value mlTgv) {
  CAMLparam1(mlTgv);
  T_irdata *tgv = Tgv_val(mlTgv);
  exporte_erreur(tgv);
  CAMLreturn(Val_unit);
}

CAMLprim value ml_get_err_list(value mlTgv) {
  CAMLparam1(mlTgv);
  CAMLlocal2(res, cons);
  res = Val_emptylist;
  T_irdata *tgv = Tgv_val(mlTgv);
  for (int i = 0; i < tgv->nb_err_sortie; ++i) {
    cons = caml_alloc_small(2, Tag_cons);
    Field(cons, 0) = caml_copy_string(tgv->err_sortie[i]);
    Field(cons, 1) = res;
    res = cons;
  }
  CAMLreturn(res);
}

static T_varinfo *cherche_var(T_irdata *tgv, const char *code) {
  T_varinfo *varinfo = cherche_varinfo(tgv, code);
  if (varinfo == NULL) {
    fprintf(stderr, "La variable %s n'existe pas\n", code);
    exit(1);
  }
  return varinfo;
}

CAMLprim value ml_unalias(value mlTgv, value mlCode) {
  CAMLparam2(mlTgv, mlCode);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  T_varinfo *varinfo = cherche_var(tgv, code);
  CAMLreturn(caml_copy_string(varinfo->name));
}

CAMLprim value ml_tgv_defined(value mlTgv, value mlCode) {
  CAMLparam2(mlTgv, mlCode);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  int def = 0;
  T_varinfo *varinfo = cherche_var(tgv, code);
  def = lis_varinfo_def(tgv, varinfo);
  CAMLreturn(Val_int(def != 0));
}

CAMLprim value ml_tgv_reset(value mlTgv, value mlCode) {
  CAMLparam2(mlTgv, mlCode);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  T_varinfo *varinfo = cherche_var(tgv, code);
  ecris_varinfo(tgv, varinfo, 0, 0.0);
  CAMLreturn(Val_unit);
}

CAMLprim value ml_tgv_get(value mlTgv, value mlCode) {
  CAMLparam2(mlTgv, mlCode);
  CAMLlocal1(optOut);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  T_varinfo *varinfo = cherche_var(tgv, code);
  if (lis_varinfo_def(tgv, varinfo)) {
    optOut = caml_alloc_some(caml_copy_double(lis_varinfo_val(tgv, varinfo)));
  } else {
    optOut = Val_none;
  }
  CAMLreturn(optOut);
}

CAMLprim value ml_tgv_get_array(value mlTgv, value mlCode, value mlIdx) {
  CAMLparam3(mlTgv,mlCode, mlIdx);
  CAMLlocal1(optOut);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  int idx = Int_val(mlIdx);
  T_varinfo *varinfo = cherche_var(tgv, code);
  if (lis_varinfo_tab_def(tgv, varinfo, idx)) {
    double val = lis_varinfo_tab_val(tgv, varinfo, idx);
    optOut = caml_alloc_some(caml_copy_double(val));
  } else {
    optOut = Val_none;
  }
  CAMLreturn(optOut);
}

CAMLprim value ml_tgv_set(value mlTgv, value mlCode, value mlMontant) {
  CAMLparam3(mlTgv, mlCode, mlMontant);

  T_irdata *tgv = Tgv_val(mlTgv);
  const char *code = String_val(mlCode);
  double montant = Double_val(mlMontant);
  T_varinfo *varinfo = cherche_var(tgv, code);
  ecris_varinfo(tgv, varinfo, 1, montant);
  CAMLreturn(Val_unit);
}

CAMLprim value ml_tgv_copy(value mlSTgv, value mlDTgv) {
  CAMLparam2(mlSTgv, mlDTgv);

  T_irdata *stgv = Tgv_val(mlSTgv);
  T_irdata *dtgv = Tgv_val(mlDTgv);
  recopie_saisie(stgv, dtgv);
  recopie_calculee(stgv, dtgv);
  recopie_base(stgv, dtgv);
  CAMLreturn(Val_unit);
}

CAMLprim value ml_annee_calc(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_int(ANNEE_REVENU));
}

CAMLprim value ml_enchainement_primitif(value mlTgv) {
  CAMLparam1(mlTgv);
  CAMLlocal2(mlErrListTemp, mlErrListOut);

  T_irdata *tgv = Tgv_val(mlTgv);
  T_discord *erreurs = enchainement_primitif(tgv);
  mlErrListOut = Val_emptylist;
  while (erreurs != NULL) {
    if (erreurs->erreur != NULL) {
      mlErrListTemp = caml_alloc_small(2, Tag_cons); // add code ?
      Field(mlErrListTemp, 0) = caml_copy_string(erreurs->erreur->nom);
      Field(mlErrListTemp, 1) = mlErrListOut;
      mlErrListOut = mlErrListTemp;
    }
    erreurs = erreurs->suivant;
  }
  CAMLreturn(mlErrListOut);
}

