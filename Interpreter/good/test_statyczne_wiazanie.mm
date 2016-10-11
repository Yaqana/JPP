int y = 1;
int a = 2;

int f(int a) {
  a = 8;
}
int main() {
  f(y);
  print(a==2);
}