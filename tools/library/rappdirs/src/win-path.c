#include <R.h>
#include <Rdefines.h>

#ifdef WIN32

#include <shlobj.h>
// SHGetFolderPath documentation:
// http://msdn.microsoft.com/en-us/library/windows/desktop/bb762181.aspx

SEXP win_path_(SEXP _folder) {
    int folder = INTEGER(_folder)[0];
    TCHAR startupFolder[MAX_PATH];
    HRESULT hr = SHGetFolderPath(0, folder, 0, 0, startupFolder);

    if (SUCCEEDED(hr)) {
        // Get short path
        TCHAR shortPath[MAX_PATH];
        GetShortPathName(startupFolder, shortPath, MAX_PATH);

        return mkString(shortPath);
    }
    else {
        // Return NULL if failed
        return R_NilValue;
    }
}

#else

SEXP win_path_(SEXP folder) {
    return R_NilValue;
}

#endif
