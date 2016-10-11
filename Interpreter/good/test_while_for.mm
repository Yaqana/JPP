int y = 1;


int main() {
  while(y < 5) {
    ++y;
    y*=1;
    print(y);
  }

  for(y = 1; y < 2; y++) {
    print(y);
  }
}