int y;

int factorial(int x) {
  int s;
  int ret;
  if (x!=0) {
    s = x - 1;
    ret = x * factorial(s);
  } else {
    ret = 1;
  }
  return ret;
}

int main() {
  int e = 6;
  y = factorial(e);
  print(y);
}
