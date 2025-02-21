
/* Avoid warning about empty compilation unit. */
void keyring_secret_service_dummy(void) { }

#if defined(__linux__) && defined(HAS_LIBSECRET)

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#define SECRET_WITH_UNSTABLE 1
#define SECRET_API_SUBJECT_TO_CHANGE 1
#include <libsecret/secret.h>

const SecretSchema *keyring_secret_service_schema(void) {
  static const SecretSchema schema = {
    "com.rstudio.keyring.password", SECRET_SCHEMA_NONE, {
      {  "service", SECRET_SCHEMA_ATTRIBUTE_STRING },
      {  "username", SECRET_SCHEMA_ATTRIBUTE_STRING },
      {  NULL, 0 },
    }
  };

  return &schema;
}

void keyring_secret_service_handle_status(const char *func, gboolean status,
					  GError *err) {
  if (!status || err) {
    char *msg = R_alloc(1, strlen(err->message) + 1);
    strcpy(msg, err->message);
    g_error_free (err);
    error("Secret service keyring error in '%s': '%s'", func, msg);
  }
}

SEXP keyring_secret_service_is_available(SEXP report_error) {

  GError *err = NULL;

  SecretService *secretservice = secret_service_get_sync(
    /* flags = */ SECRET_SERVICE_LOAD_COLLECTIONS | SECRET_SERVICE_OPEN_SESSION,
    /* cancellable = */ NULL,
    &err);

  if (err || !secretservice) {
    if (LOGICAL(report_error)[0]) {
      keyring_secret_service_handle_status("is_available", TRUE, err);
      error("Cannot connect to secret service");

    } else {
      return ScalarLogical(0);
    }
  }

  return ScalarLogical(1);
}

SecretCollection* keyring_secret_service_get_collection_default(void) {

  SecretCollection *collection = NULL;
  GError *err = NULL;

  SecretService *secretservice = secret_service_get_sync(
    /* flags = */ SECRET_SERVICE_LOAD_COLLECTIONS | SECRET_SERVICE_OPEN_SESSION,
    /* cancellable = */ NULL,
    &err);

  if (err || !secretservice) {
    keyring_secret_service_handle_status("get_keyring", TRUE, err);
    error("Cannot connect to secret service");
  }

  collection = secret_collection_for_alias_sync(
    /* service = */ secretservice,
    /* alias = */ "default",
    /* flags = */ SECRET_COLLECTION_NONE,
    /* cancellable = */ NULL,
    &err);

  g_object_unref(secretservice);

  if (err || !collection) {
    keyring_secret_service_handle_status("get_keyring", TRUE, err);
    error("Cannot find keyring");
  }

  return collection;
}

GList* keyring_secret_service_list_collections(void) {

  GError *err = NULL;
  SecretService *secretservice = secret_service_get_sync(
    /* flags = */ SECRET_SERVICE_LOAD_COLLECTIONS | SECRET_SERVICE_OPEN_SESSION,
    /* cancellable = */ NULL,
    &err);

  if (err || !secretservice) {
    keyring_secret_service_handle_status("create_keyring", TRUE, err);
    error("Cannot connect to secret service");
  }

  gboolean status = secret_service_load_collections_sync(
    secretservice,
    /* cancellable = */ NULL,
    &err);

  if (status || err) {
    keyring_secret_service_handle_status("create_keyring", status, err);
  }

  GList *collections = secret_service_get_collections(secretservice);
  if (!collections) {
    g_object_unref(secretservice);
    error("Cannot query keyrings");
  }

  g_object_unref(secretservice);

  return collections;
}

SecretCollection* keyring_secret_service_get_collection_other(const char *name) {

  GList *collections = keyring_secret_service_list_collections();

  GList *item;
  for (item = g_list_first(collections); item; item = g_list_next(item)) {
    SecretCollection *coll = item->data;
    gchar *label = secret_collection_get_label(coll);
    if (! g_strcmp0(label, name)) {
      SecretCollection *copy = g_object_ref(coll);
      g_list_free(collections);
      return copy;
    }
  }

  g_list_free(collections);
  error("Did not find collection: '%s'", name);
  return NULL;
}

SecretCollection* keyring_secret_service_get_collection(SEXP keyring) {

  if (isNull(keyring)) {
    return keyring_secret_service_get_collection_default();
  } else {
    const char *ckeyring = CHAR(STRING_ELT(keyring, 0));
    return keyring_secret_service_get_collection_other(ckeyring);
  }
}

GList* keyring_secret_service_get_item(SEXP keyring, SEXP service,
				       SEXP username) {

  const char* empty = "";
  const char* cservice = CHAR(STRING_ELT(service, 0));
  const char* cusername =
    isNull(username) ? empty : CHAR(STRING_ELT(username, 0));

  const char *errormsg = NULL;
  SecretCollection *collection = NULL;
  GList *secretlist = NULL;
  GHashTable *attributes = NULL;
  GError *err = NULL;

  collection = keyring_secret_service_get_collection(keyring);
  attributes = g_hash_table_new(
    /* hash_func = */ (GHashFunc) g_str_hash,
    /* key_equal_func = */ (GEqualFunc) g_str_equal);

  g_hash_table_insert(attributes, g_strdup("service"), g_strdup(cservice));
  g_hash_table_insert(attributes, g_strdup("username"), g_strdup(cusername));

  secretlist = secret_collection_search_sync(
    /* self = */ collection,
    /* schema = */ keyring_secret_service_schema(),
    /* attributes = */ attributes,
    /* flags = */ SECRET_SEARCH_ALL | SECRET_SEARCH_UNLOCK |
                  SECRET_SEARCH_LOAD_SECRETS,
    /* cancellable = */ NULL,
    &err);

  if (collection) g_object_unref(collection);
  if (attributes) g_hash_table_unref(attributes);
  keyring_secret_service_handle_status("get", TRUE, err);
  if (errormsg) error("%s", errormsg);

  return secretlist;
}

SEXP keyring_secret_service_get(SEXP keyring, SEXP service, SEXP username) {

  GList *secretlist = keyring_secret_service_get_item(keyring, service, username);

  guint listlength = g_list_length(secretlist);
  if (listlength == 0) {
    g_list_free(secretlist);
    error("keyring item not found");

  } else if (listlength > 1) {
    warning("Multiple matching keyring items found, returning first");
  }

  SecretItem *secretitem = g_list_first(secretlist)->data;
  SecretValue *secretvalue = secret_item_get_secret(secretitem);

  if (!secretvalue) {
    g_list_free(secretlist);
    error("Cannot get password");
  }

  gsize passlength;
  const gchar *password = secret_value_get(secretvalue, &passlength);
  SEXP result = PROTECT(allocVector(RAWSXP, passlength));
  memcpy(RAW(result), password, passlength);

  g_list_free(secretlist);
  UNPROTECT(1);
  return result;
}

SEXP keyring_secret_service_set(SEXP keyring, SEXP service, SEXP username,
				SEXP password) {
  const char* empty = "";
  const char* cservice = CHAR(STRING_ELT(service, 0));
  const char* cusername =
    isNull(username) ? empty : CHAR(STRING_ELT(username, 0));

  SecretCollection *collection = NULL;
  GHashTable *attributes = NULL;
  GError *err = NULL;

  collection = keyring_secret_service_get_collection(keyring);
  attributes = g_hash_table_new(
    /* hash_func = */ (GHashFunc) g_str_hash,
    /* key_equal_func = */ (GEqualFunc) g_str_equal);

  g_hash_table_insert(attributes, g_strdup("service"), g_strdup(cservice));
  g_hash_table_insert(attributes, g_strdup("username"), g_strdup(cusername));

  SecretValue *value = secret_value_new((gchar *)RAW(password),
					LENGTH(password),
					/* content_type = */ "text/plain");

  SecretItem *item = secret_item_create_sync(
    collection,
    keyring_secret_service_schema(),
    attributes,
    /* label = */ cservice,
    value,
    /* flags = */ SECRET_ITEM_CREATE_REPLACE,
    /* cancellable = */ NULL,
    &err);

  if (item) g_object_unref(item);
  keyring_secret_service_handle_status("set", TRUE, err);

  return R_NilValue;
}

SEXP keyring_secret_service_delete(SEXP keyring, SEXP service, SEXP username) {

  GList *secretlist = keyring_secret_service_get_item(keyring, service, username);

  guint listlength = g_list_length(secretlist);
  if (listlength == 0) {
    g_list_free(secretlist);
    error("keyring item not found");

  } else if (listlength > 1) {
    warning("Multiple matching keyring items found, returning first");
  }

  SecretItem *secretitem = g_list_first(secretlist)->data;

  GError *err = NULL;

  gboolean status = secret_item_delete_sync(
    secretitem,
    /* cancellable = */ NULL,
    &err);

  g_list_free(secretlist);

  if (!status) error("Could not delete keyring item");

  keyring_secret_service_handle_status("delete", status, err);

  return R_NilValue;
}

SEXP keyring_secret_service_list(SEXP keyring, SEXP service) {

  const char *cservice = isNull(service) ? NULL : CHAR(STRING_ELT(service, 0));
  const char *errormsg = NULL;

  GList *secretlist = NULL, *iter = NULL;
  guint listlength, i;
  GHashTable *attributes = NULL;
  GError *err = NULL;

  SEXP result = R_NilValue;

  SecretCollection *collection = keyring_secret_service_get_collection(keyring);

  /* If service is not NULL, then we only look for the specified service. */
  attributes = g_hash_table_new(
    /* hash_func = */ (GHashFunc) g_str_hash,
    /* key_equal_func = */ (GEqualFunc) g_str_equal);

  if (cservice) {
    g_hash_table_insert(attributes, g_strdup("service"), g_strdup(cservice));
  }

  secretlist = secret_collection_search_sync(
    /* self = */ collection,
    /* schema = */ keyring_secret_service_schema(),
    /* attributes = */ attributes,
    /* flags = */ SECRET_SEARCH_ALL,
    /* cancellable = */ NULL,
    &err);

  if (err) goto cleanup;

  listlength = g_list_length(secretlist);
  result = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(result, 0, allocVector(STRSXP, listlength));
  SET_VECTOR_ELT(result, 1, allocVector(STRSXP, listlength));
  for (i = 0, iter = g_list_first(secretlist); iter; i++, iter = g_list_next(iter)) {
    SecretItem *secret = iter->data;
    GHashTable *attr = secret_item_get_attributes(secret);
    char *service = g_hash_table_lookup(attr, "service");
    char *username = g_hash_table_lookup(attr, "username");
    SET_STRING_ELT(VECTOR_ELT(result, 0), i, mkChar(service));
    SET_STRING_ELT(VECTOR_ELT(result, 1), i, mkChar(username));
  }

  UNPROTECT(1);

  /* If an error happened, then err is not NULL, and the handler longjumps.
     Otherwise if errormsg is not NULL, then we error out with that. This
     happens for example if the specified keyring is not found. */

 cleanup:
  if (collection) g_object_unref(collection);
  if (secretlist) g_list_free(secretlist);
  if (attributes) g_hash_table_unref(attributes);
  keyring_secret_service_handle_status("list", TRUE, err);
  if (errormsg) error("%s", errormsg);

  return result;
}

SEXP keyring_secret_service_create_keyring(SEXP keyring) {

  const char *ckeyring = CHAR(STRING_ELT(keyring, 0));

  GError *err = NULL;

  SecretService *secretservice = secret_service_get_sync(
    /* flags = */ SECRET_SERVICE_LOAD_COLLECTIONS | SECRET_SERVICE_OPEN_SESSION,
    /* cancellable = */ NULL,
    &err);

  if (err || !secretservice) {
    keyring_secret_service_handle_status("create_keyring", TRUE, err);
    error("Cannot connect to secret service");
  }

  SecretCollection *collection = secret_collection_create_sync(
    /* service = */ secretservice,
    /* label = */ ckeyring,
    /* alias = */ NULL,
    /* flags = */ 0,
    /* cancellable = */ NULL,
    &err);

  g_object_unref(secretservice);
  keyring_secret_service_handle_status("create_keyring", TRUE, err);

  if (collection) g_object_unref(collection);

  /* Need to disconnect here, otherwise the proxy is cached, and the new
     collection is not in this cache. If we disconnect here, a new proxy
     will be created for the next operation, and this will already include
     the new collection. */
  secret_service_disconnect();

  return R_NilValue;
}

SEXP keyring_secret_service_list_keyring(void) {

  GList *collections = keyring_secret_service_list_collections();

  guint num = g_list_length(collections);
  SEXP result = PROTECT(allocVector(VECSXP, 3));
  SET_VECTOR_ELT(result, 0, allocVector(STRSXP, num));
  SET_VECTOR_ELT(result, 1, allocVector(INTSXP, num));
  SET_VECTOR_ELT(result, 2, allocVector(LGLSXP, num));

  GList *item;
  int i = 0;
  for (item = g_list_first(collections); item; item = g_list_next(item), i++) {
    SecretCollection *coll = item->data;
    gchar *label = secret_collection_get_label(coll);
    gboolean locked = secret_collection_get_locked(coll);
    GList *secrets = secret_collection_get_items(coll);
    SET_STRING_ELT(VECTOR_ELT(result, 0), i, mkChar((char*) label));
    INTEGER(VECTOR_ELT(result, 1))[i] = g_list_length(secrets);
    LOGICAL(VECTOR_ELT(result, 2))[i] = locked;
  }

  g_list_free(collections);

  UNPROTECT(1);
  return result;
}

SEXP keyring_secret_service_delete_keyring(SEXP keyring) {
  if (isNull(keyring)) error("Cannot delete the default keyring");

  const char *ckeyring = CHAR(STRING_ELT(keyring, 0));
  SecretCollection* collection =
    keyring_secret_service_get_collection_other(ckeyring);

  GError *err = NULL;

  gboolean status = secret_collection_delete_sync(
    collection,
    /* cancellable = */ NULL,
    &err);

  g_object_unref(collection);
  keyring_secret_service_handle_status("delete_keyring", status, err);

  /* Need to disconnect here, otherwise the proxy is cached, and the deleted
     collection will be still in the cache. If we disconnect here, a new proxy
     will be created for the next operation, and this will not include the
     deleted collection. */
  secret_service_disconnect();

  return R_NilValue;
}

SEXP keyring_secret_service_lock_keyring(SEXP keyring) {

  SecretCollection *collection =
    keyring_secret_service_get_collection(keyring);
  GList *list = g_list_append(NULL, collection);
  GError *err = NULL;

  gint ret = secret_service_lock_sync(
    /* service = */ NULL,
    /* objects = */ list,
    /* cancellable = */ NULL,
    /* locked = */ NULL,
    &err);

  g_list_free(list);
  keyring_secret_service_handle_status("lock_keyring", TRUE, err);

  if (ret == 0) { error("Could not lock keyring"); }

  return R_NilValue;
}

SEXP keyring_secret_service_unlock_keyring(SEXP keyring, SEXP password) {

  SecretCollection *collection =
    keyring_secret_service_get_collection(keyring);
  GList *list = g_list_append(NULL, collection);
  GError *err = NULL;

  gint ret = secret_service_unlock_sync(
    /* service = */ NULL,
    /* objects = */ list,
    /* cancellable = */ NULL,
    /* unlocked = */ NULL,
    &err);

  g_list_free(list);
  keyring_secret_service_handle_status("unlock_keyring", TRUE, err);

  if (ret == 0) { error("Could not unlock keyring"); }

  return R_NilValue;
}

SEXP keyring_secret_service_is_locked_keyring(SEXP keyring) {

  SecretCollection *collection =
    keyring_secret_service_get_collection(keyring);

  gboolean locked = secret_collection_get_locked(collection);

  return ScalarLogical(locked);
}

void R_unload_keyring(DllInfo *dll) {
  secret_service_disconnect();
}

#else

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP keyring_secret_service_is_available(SEXP report_error) {
#ifdef __linux__
  if (LOGICAL(report_error)[0]) {
    error("keyring build has no libsecret support");
  } else {
    return ScalarLogical(0);
  }
#else
  error("only works on Linux");
  return R_NilValue;
#endif
}

SEXP keyring_secret_service_get(SEXP keyring, SEXP service,
                                SEXP username) {
  error("only works on Linux with Secret Service support");
  return R_NilValue;
}

SEXP keyring_secret_service_set(SEXP keyring, SEXP service,
                                SEXP username, SEXP password) {
  error("only works on Linux with Secret Service support");
  return R_NilValue;
}

SEXP keyring_secret_service_delete(SEXP keyring, SEXP service,
                                   SEXP username) {
  error("only works on Linux with Secret Service support");
  return R_NilValue;
}

SEXP keyring_secret_service_list(SEXP keyring, SEXP service) {
  error("only works on Linux with Secret Service support");
  return R_NilValue;
}

SEXP keyring_secret_service_create_keyring(SEXP keyring) {
  error("only works on Linux with Secret Service support");
  return R_NilValue;
}

SEXP keyring_secret_service_list_keyring(void) {
  error("only works on Linux with Secret Service support");
  return R_NilValue;
}

SEXP keyring_secret_service_delete_keyring(SEXP keyring) {
  error("only works on Linux with Secret Service support");
  return R_NilValue;
}

SEXP keyring_secret_service_lock_keyring(SEXP keyring) {
  error("only works on Linux with Secret Service support");
  return R_NilValue;
}

SEXP keyring_secret_service_unlock_keyring(SEXP keyring, SEXP password) {
  error("only works on Linux with Secret Service support");
  return R_NilValue;
}

SEXP keyring_secret_service_is_locked_keyring(SEXP keyring) {
  error("only works on Linux with Secret Service support");
  return R_NilValue;
}

#endif // __linux__
