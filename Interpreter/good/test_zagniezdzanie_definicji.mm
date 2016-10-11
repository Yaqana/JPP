int y = 1;

int f() {
  int h() {
    int silnia(int x) {
      int s;
      int ret;
      if (x!=0) {
        s = x - 1;
        ret = x * silnia(s);
      } else {
        ret = 1;
      }
      return ret;
    }
    int s = 4;
    return silnia(s);
  }
  return h();
}
int main() {
  y=f();
  print(y==24);
}