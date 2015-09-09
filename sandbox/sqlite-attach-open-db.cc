#include <iostream>
#include <sqlite3.h>

int main() {
  sqlite3 *db = nullptr;
  int rc = sqlite3_open_v2("test.db", &db, SQLITE_OPEN_READWRITE|SQLITE_OPEN_CREATE, nullptr);
  if (rc != SQLITE_OK) {
    std::cout << "sqlite3_open_v2() return error code: " << rc << std::endl;
    return 1;
  }

  char *errmsg = nullptr;
  rc = sqlite3_exec(db, "create table test(id,v1,v2);", nullptr, nullptr, &errmsg);
  if (rc != SQLITE_OK) {
    std::cout << "sqlite3_exec returned error: (" << rc << ") ";
    if (errmsg) {
      std::cout << errmsg;
      sqlite3_free(errmsg);
      errmsg = nullptr;
    }
    std::cout << "\n";
  }

  sqlite3 *other = nullptr;
  rc = sqlite3_open_v2(":memory:", &other, SQLITE_OPEN_READWRITE|SQLITE_OPEN_CREATE, nullptr);
  if (rc != SQLITE_OK) {
    std::cout << "sqlite3_open_v2() return error code: " << rc << std::endl;
    return 1;
  }

  sqlite3_exec(other, "attach database 'test.db' as test;", nullptr, nullptr, &errmsg);
  if (rc != SQLITE_OK) {
    std::cout << "attach database returned error: (" << rc << ") ";
    if (errmsg) {
      std::cout << errmsg;
      sqlite3_free(errmsg);
      errmsg = nullptr;
    }
    std::cout << "\n";
  }

  sqlite3_close(other);
  sqlite3_close(db);

  return 0;
}
