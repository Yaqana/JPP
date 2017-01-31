int y;

int factorial(int x, fun int dec(int)) {
  int s;
  int ret;
  if (x != 0) {
    s = dec(x);
    ret = x * factorial(s, dec);
  } else {
    ret = 1;
  }
  return ret;
}

int main() {
  int e = 6;
  y = factorial(e, (int a) -> {return a - 1;});
  print(y);
}
