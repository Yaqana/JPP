int y;

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

int main() {
  int e = 6;
  y = silnia(e);
  print(y);
}