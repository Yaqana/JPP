int y = 1;

int main() {
  while(y < 5) {
    y++;
    print(y) //missing semicolon
  }

  for(y = 1; y < 2; y++) {
    print(y);
  }
}
